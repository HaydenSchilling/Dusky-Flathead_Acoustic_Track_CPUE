# Load the required libraries
library(tidyverse)
library(gratia)
library(janitor)
library(mgcv)
library(DHARMa)
library(visreg)

# Load the data
data <- read_csv("Data/Flathead Env data to model Shoalhaven Commercial Monthly.csv")

# Standardize names of variables
data <- data %>% clean_names()

# Reduce df to just the variables needed
data <- data %>%
  dplyr::select(
    #estuary,
    #date = event_date,
    fisher = authorised_fisher_id,
    month,
    cal_year,
    catch = dusky_flathead,
    effort = days_fished, # metres of net
    #lunar_phase,
    mean_daily_flow,
    prop_dusky,
    mean_wind,
    log_flow)
    #recruit_flow,
    #flow_mean_scaled,
    #wind_speed_lag1
   # ) %>% mutate(log_flow = log(mean_daily_flow+0.1)) #%>%
    # group_by(estuary) %>% mutate(flow_mean_scaled = as.vector(scale(log_flow)),
    #                            recruit_flow_scaled = as.vector(scale(logRecruitFlow)))

summary(data$mean_daily_flow)
summary(data$log_flow)


table(data$month)

# # limit to months of overnight setting
# data <- data %>% filter((estuary == "Lake Illawarra" & between(month, 6,8))|
#                           (estuary == "Tuggerah Lakes" & between(month, 3,11))|
#                           (estuary == "Wallis Lake" & between(month, 3,11)))
# 
# table(data$estuary, data$month)

# Ensure date is in the correct format
data <- data %>% mutate(date = dmy(paste0("15-",month,"-",cal_year)))

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
dotchart(data$catch) # looks OK
#data <- data %>% filter(catch < 500)
#dotchart(data$catch) # looks nicer now

dotchart(data$effort) # a couple of crazy outliers >5,000
data <- data %>% filter(effort < 4000) # even 2 km seems like heaps...
dotchart(data$effort)

dotchart(data$catch / data$effort) # good - one possible high ~0.5

#dotchart(data$lunar_phase) # all good - random/uniform
dotchart(data$log_flow) # > mostly ok
dotchart(data$mean_wind) # all good - random/uniform

#dotchart(data$wind_speed_lag1) # looks fine

# Generate a net id
data <- data %>% 
  group_by(fisher, date) %>% 
  mutate(net = paste0(fisher, "-", row_number())) %>% 
  ungroup()

# Check reporting - single row per data event
n <- data %>% 
  group_by(fisher, date) %>% 
  summarise(n = n()) %>%
  filter(n > 1) %>%
  nrow()

print(n) # 824

n / length(unique(data$date)) * 100 # good - no issues - fixed in previous script

# Calculate differences between observations
data <- data %>%
  #group_by(estuary) %>% # for each fisher
  arrange(date) %>% # order the data
  mutate(t = as.numeric(date - lag(date, 1)), # the difference between dates
         t = if_else(is.na(t), 1, t), # change from NA at t = 1
         t = cumsum(t)) %>% # and now get the cumulative sum of the differences
  ungroup()

# Set up factors for REs
data <- data %>% 
  mutate(#estuary = factor(estuary),
         fisher = factor(fisher),
         cal_year = factor(cal_year),
         monthF = as.factor(as.character(month)))

# Try fitting with {mgcv} first
m2 <- gam(
  formula = catch ~ # kg?
    monthF +  # month as factor
    #s(prop_dusky, bs="tp")+
   # s(t, bs = "tp")+
    #s(lunar_phase, bs = "cc") +
    #s(month, bs = "cc", by = estuary) +
    #s(wind_speed_lag1, bs = "tp", by = estuary) +
    s(log_flow, bs = "tp") +
    s(mean_wind, bs = "tp") +
    # s(recruit_flow_scaled, bs = "tp", by = estuary) +
    s(fisher, bs = "re") +
    s(cal_year, bs = "re") +
    offset(log(effort)), # metres of net
  data = data,
  #family = tw(),
  family = tw(link=log),  #tw(),
 # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
  select = TRUE,
  method = "REML"
)


appraise(m2) 
summary(m2)

anova(m2)

source("Scripts/dharma-to-gg_fn.R")

plots <- DHARMa_plot(m2)
plot_a <- plots[[1]]
plot_a

plot_b <- plots[[2]]
plot_b

library(patchwork)
plot_a + plot_b
ggsave("Plots/model checks/Shoalhaven model residuals.png", dpi=300, width = 21, height=14.8, units="cm")



# Check out model residuals
appraise(m2) 
# Looks OK, heavy upper tail in QQ plot. I'm not really sure if these are the
# right residuals to use to diagnose a Tweedie model - Hayden?

# Simulate residuals with DHARMa
simulationOutput <- simulateResiduals(m2)
residuals <- simulationOutput$scaledResiduals
plot(simulationOutput)

plotResiduals(simulationOutput, form = data$prop_dusky)
plotResiduals(simulationOutput, form = data$log_flow)
plotResiduals(simulationOutput, form = data$mean_wind)


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
  #estuary = data$estuary,
  fisher = data$fisher,
  rqr = residuals,
  deviance = resid(m2, type = "deviance")
)

ggplot(data = acf, aes(x = t, y = rqr)) + 
  geom_point(aes(colour = fisher), alpha = 0.3) +
  geom_smooth(method = "gam") +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  #facet_wrap(vars(estuary)) +
  coord_cartesian(ylim = c(0, 1))

# Some wiggliness but not sure it's meaningful?

ggplot(data = acf, aes(x = t, y = deviance)) + 
  geom_point(aes(colour = fisher), alpha = 0.3) +
  geom_smooth(method = "gam") +
  theme(legend.position = "none") +
  scale_color_viridis_d() #+
#  facet_wrap(vars(estuary))

# Looks pretty good

# CHeck out the model effects
summary(m2)

# Look at the marginal effects
draw(m2, fun = inv_link(m2))

# Look at the conditional effects
visreg(m, scale = "response", cond = list(effort = 100))

# Estuary-specific conditional effects
visreg(m, 
       xvar = "log_flow", 
       #by = "estuary", 
       cond = list(effort = 100),
       scale = "response")

# How much variance in the REs?
variance_components <- variance_comp(m)
variance_components

sum(variance_components$variance)

#### Manual predictions to match telemetry plots
new_data <- expand.grid("monthF" = unique(data$monthF),
                        "log_flow" = median(data$log_flow, na.rm = T),
                        "prop_dusky" = median(data$prop_dusky, na.rm=T),
                        #"Array" = unique(daily_move$Array),
                        #"lunar_phase" = median(data$lunar_phase, na.rm = T),
                        "mean_wind" = median(data$mean_wind, na.rm = T),
                        "fisher" = data$fisher[1],
                        "cal_year" = data$cal_year[1],
                        "effort" = median(data$effort, na.rm=T))

new_dat_preds <- predict(m2, newdata = new_data, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data$fit <- new_dat_preds$fit
new_data$se <- new_dat_preds$se.fit

p4 <- ggplot(new_data, aes(monthF, fit)) + geom_point() +# facet_wrap(~Array) +
  geom_errorbar(aes(ymin=fit-se, ymax = fit+se)) + theme_bw()
p4

# log_flow
new_data2 <- expand.grid("monthF" = "6",
                        "log_flow" = seq(min(data$log_flow, na.rm=T), max(data$log_flow, na.rm=T),0.1),
                        "prop_dusky" = median(data$prop_dusky, na.rm=T),
                        #"Array" = unique(daily_move$Array),
                        #"lunar_phase" = median(data$lunar_phase, na.rm = T),
                        "mean_wind" = median(data$mean_wind, na.rm = T),
                        "fisher" = data$fisher[1],
                        "cal_year" = data$cal_year[1],
                        "effort" = median(data$effort, na.rm=T))

new_dat_preds2 <- predict(m2, newdata = new_data2, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data2$fit <- new_dat_preds2$fit
new_data2$se <- new_dat_preds2$se.fit

p1 <- ggplot(new_data2, aes((log_flow), fit)) + geom_line() +# facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1) +
  geom_rug(data=data, aes(x=log_flow), inherit.aes = F)
p1

# lunar
# limits <- daily_move %>% group_by(Array) %>%
#   summarise(min_flow = min(log_flow, na.rm=T),
#             max_flow = max(log_flow, na.rm=T))

# new_data3 <- expand.grid("monthF" = "6",
#                          "log_flow" = median(data$log_flow),
#                          "prop_dusky" = median(data$prop_dusky, na.rm=T),
#                            #"Array" = unique(daily_move$Array),
#                          #"lunar_phase" = seq(min(data$lunar_phase), max(data$lunar_phase), 0.1),
#                          "mean_wind" = median(data$mean_wind, na.rm = T),
#                          "fisher" = data$fisher[1],
#                          "cal_year" = data$cal_year[1],
#                          "effort" = median(data$effort, na.rm=T))
# 
# new_dat_preds3 <- predict(m2, newdata = new_data3, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")
# 
# new_data3$fit <- new_dat_preds3$fit
# new_data3$se <- new_dat_preds3$se.fit
# 
# p2 <- ggplot(new_data3, aes(lunar_phase, fit)) + geom_line() + # facet_wrap(~Array) +
#   geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)
# p2

# wind_speed

new_data4 <- expand.grid("monthF" = "6",
                         "log_flow" = median(data$log_flow),
                         "prop_dusky" = median(data$prop_dusky, na.rm=T),
                          #"Array" = unique(daily_move$Array),
                         #"lunar_phase" = median(data$lunar_phase),
                         "mean_wind" = seq(min(data$mean_wind), max(data$mean_wind), 0.1),
                         "fisher" = data$fisher[1],
                         "cal_year" = data$cal_year[1],
                         "effort" = median(data$effort, na.rm=T))

new_dat_preds4 <- predict(m2, newdata = new_data4, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data4$fit <- new_dat_preds4$fit
new_data4$se <- new_dat_preds4$se.fit

p3 <- ggplot(new_data4, aes(mean_wind, fit)) + geom_line() +# facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)
p3

library(patchwork)
p1/p4 & theme_bw() & ylab("Predicted Catch\n(kg day)")

ggsave("Plots/Shoalhaven model outcomes.png", dpi=300, width = 21, height=20, units="cm")
ggsave("Plots/Shoalhaven model outcomes.pdf", dpi=300, width = 21, height=20, units="cm")

devtools::install_github('eco-stats/ecostats', ref='main')
library(ecostats)

plotenvelope(m2, sim.method = "refit")
