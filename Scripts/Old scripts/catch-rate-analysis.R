# Load the required libraries
library(tidyverse)
library(gratia)
library(janitor)
library(mgcv)
library(DHARMa)
library(visreg)

# Load the data
data <- read_csv("Data/Flathead Env data to model.csv")

# Standardize names of variables
data <- data %>% clean_names()

# Reduce df to just the variables needed
data <- data %>%
  select(
    estuary,
    date = event_date,
    fisher = authorised_fisher_id,
    month,
    fiscal_year,
    catch = response,
    effort = effort_qty, # metres of net
    lunar_phase,
    mean_daily_flow,
    recruit_flow,
    #flow_mean_scaled,
    wind_speed_lag1) %>% mutate(logFlow = log(mean_daily_flow+0.1),
                                logRecruitFlow = log(recruit_flow+0.01)) %>%
  group_by(estuary) %>% mutate(flow_mean_scaled = as.vector(scale(logFlow)),
                               recruit_flow_scaled = as.vector(scale(logRecruitFlow)))

summary(data$mean_daily_flow)
summary(data$flow_mean_scaled)


table(data$estuary, data$month)

# limit to months of overnight setting
data <- data %>% filter((estuary == "Lake Illawarra" & between(month, 6,8))|
                          (estuary == "Tuggerah Lakes" & between(month, 3,11))|
                          (estuary == "Wallis Lake" & between(month, 3,11)))

table(data$estuary, data$month)

# Ensure date is in the correct format
data <- data %>% mutate(date = ymd(date))

# How often do they go fishing
summary <- data %>%
  group_by(fisher) %>%
  summarise(days = length(unique(date)),
            nets = n(),
            effort = sum(effort),
            catch = sum(catch))

# Which fishers to remove? 
# Never caught a flathead or fished less than 10 days
remove <- summary %>%
  filter(catch == 0 | days < 10) %>% 
  pull(fisher)

# Remove them
data <- data %>% filter(!(fisher %in% remove))

days <- summary %>% pull(days)
catch <- summary %>% pull(catch)

mean(days)
par(mfrow = c(2, 1))
hist(days, breaks = max(days) / 10)
abline(v = c(mean(days), median(days)), lty = c("dashed", "dotted"))
boxplot(days, horizontal = TRUE)

mean(catch)
par(mfrow = c(2, 1))
hist(catch, breaks = max(catch) / 10)
abline(v = c(mean(catch), median(catch)), lty = c("dashed", "dotted"))
boxplot(catch, horizontal = TRUE)

# Have a look at the distribution of the variables
dotchart(data$catch) # one probable outlier ~1,500kg
data <- data %>% filter(catch < 500)
dotchart(data$catch) # looks nicer now

dotchart(data$effort) # a couple of crazy outliers ~12,000
data <- data %>% filter(effort < 3000) # even 2 km seems like heaps...
dotchart(data$effort)

dotchart(data$catch / data$effort) # good

dotchart(data$lunar_phase) # all good - random/uniform
dotchart(data$flow_mean_scaled) # > 10?
dotchart(data$wind_speed_lag1) # looks fine

# Generate a net id
data <- data %>% 
  group_by(fisher, date) %>% 
  mutate(net = paste0(fisher, "-", row_number())) %>% 
  ungroup()

# Check how often more than one net is deployed
n <- data %>% 
  group_by(fisher, date) %>% 
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

print(n) # 824

n / length(unique(data$date)) * 100 # 22 % of the time

# Calculate differences between observations
data <- data %>%
  group_by(estuary) %>% # for each fisher
  arrange(date) %>% # order the data
  mutate(t = as.numeric(date - lag(date, 1)), # the difference between dates
         t = if_else(is.na(t), 1, t), # change from NA at t = 1
         t = cumsum(t)) %>% # and now get the cumulative sum of the differences
  ungroup()

# Set up factors for REs
data <- data %>% 
  mutate(estuary = factor(estuary),
         fisher = factor(fisher),
         fiscal_year = factor(fiscal_year),
         monthF = as.factor(as.character(month)))

# Try fitting with {mgcv} first
m <- gam(
  formula = catch ~ # kg?
    estuary*monthF +  # month as factor
    s(lunar_phase, bs = "cc", by = estuary) +
    #s(month, bs = "cc", by = estuary) +
    #s(wind_speed_lag1, bs = "tp", by = estuary) +
    s(flow_mean_scaled, bs = "tp", by = estuary) +
   # s(recruit_flow_scaled, bs = "tp", by = estuary) +
    s(fisher, bs = "re") +
    s(fiscal_year, bs = "re") +
    offset(log(effort)), # metres of net
  data = data,
  family = tw(),
  knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
  select = TRUE,
  method = "REML"
)

# Check out model residuals
appraise(m) 
# Looks OK, heavy upper tail in QQ plot. I'm not really sure if these are the
# right residuals to use to diagnose a Tweedie model - Hayden?

# Simulate residuals with DHARMa
simulationOutput <- simulateResiduals(m)
residuals <- simulationOutput$scaledResiduals
plot(simulationOutput)

# Try DHARMa standard tests
testResiduals(simulationOutput)
# All tests are significant
# QQ plot looks OK to me
# Dispersion looks very bad
# What to do about an overdispersed Tweedie model?

# Heteroscedasticity
testQuantiles(simulationOutput)
# Qunatile regressions are significant, but the patterns don't look terrible...
testQuantiles(simulationOutput, predictor = data$t)
# The unmodelled time aspect doesn't look that bad...

# Autocorrelation
acf <- data.frame(
  t = data$t,
  estuary = data$estuary,
  fisher = data$fisher,
  rqr = residuals,
  deviance = resid(m, type = "deviance")
)

ggplot(data = acf, aes(x = t, y = rqr)) + 
  geom_point(aes(colour = fisher), alpha = 0.3) +
  geom_smooth(method = "gam") +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  facet_wrap(vars(estuary)) +
  coord_cartesian(ylim = c(0, 1))

# Some wiggliness but not sure it's meaningful?

ggplot(data = acf, aes(x = t, y = deviance)) + 
  geom_point(aes(colour = fisher), alpha = 0.3) +
  geom_smooth(method = "gam") +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  facet_wrap(vars(estuary))

# Looks pretty good

# CHeck out the model effects
summary(m)

# Look at the marginal effects
draw(m, fun = inv_link(m))

# Look at the conditional effects
visreg(m, scale = "response", cond = list(effort = 100))

# Estuary-specific conditional effects
visreg(m, 
       xvar = "flow_mean_scaled", 
       by = "estuary", 
       cond = list(effort = 100),
       scale = "response")

# How much variance in the REs?
variance_components <- variance_comp(m)
variance_components

sum(variance_components$variance)
