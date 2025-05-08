# Load the required libraries
library(tidyverse)
library(gratia)
library(janitor)
library(mgcv)
library(DHARMa)
library(visreg)

# Load the data
data <- read_csv("Data/Flathead Env data to model Mesh Net Commercial Monthly Other Estuary.csv")

# Standardize names of variables
data <- data %>% clean_names()

# Reduce df to just the variables needed
data <- data %>%
  dplyr::select(
    estuary_name,
    #date = event_date,
    fisher = authorised_fisher_id,
    month,
    cal_year,
    catch = dusky_flathead,
    effort = days_fished, # metres of net
    #lunar_phase,
    mean_daily_flow,
    log_flow,
    mean_wind,
    prop_dusky) %>%
  rename(estuary = estuary_name) %>%
  mutate(C_E = catch/effort)
    #recruit_flow,
    #flow_mean_scaled,
    #wind_speed_lag1
   # ) %>% mutate(log_flow = log(mean_daily_flow+0.1)) #%>%
    # group_by(estuary) %>% mutate(flow_mean_scaled = as.vector(scale(log_flow)),
    #                            recruit_flow_scaled = as.vector(scale(logRecruitFlow)))

summary(data$mean_daily_flow)
#summary(data$log_flow)


table(data$estuary, data$month)

# # limit to months of overnight setting
# data <- data %>% filter((estuary == "Lake Illawarra" & between(month, 6,8))|
#                           (estuary == "Tuggerah Lakes" & between(month, 3,11))|
#                           (estuary == "Wallis Lake" & between(month, 3,11)))
# 
# table(data$estuary, data$month)

# Ensure date is in the correct format
data <- data %>% mutate(date =paste(month, cal_year, sep = "_"))

# How often do they go fishing
summary <- data %>%
  group_by(fisher) %>%
  summarise(days = length(unique(date)),
            nets = n(),
            effort = sum(effort),
            catch = sum(catch))

# Which fishers to remove? 
# Never caught a flathead or fished less than 10 months
remove <- summary %>%
  filter(catch == 0 | days < 10) %>% 
  pull(fisher)

# Remove them
data <- data %>% filter(!(fisher %in% remove))

days <- summary %>% pull(days)
catch <- summary %>% pull(catch)
hist(days)

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
dotchart(data$catch) # one dodgy - checked log sheet - remove
#data <- data %>% filter(catch < 500)
#data <- data %>% filter(catch < 500)
#dotchart(data$catch) # looks nicer now

dotchart(data$effort) # mostly ok
#data <- data %>% filter(effort < 4000) # even 2 km seems like heaps...
#dotchart(data$effort)

dotchart(data$catch / data$effort) # some possible high >200
#data <- data %>% filter(C_E < 0.6)


#dotchart(data$lunar_phase) # all good - random/uniform
dotchart(log(data$mean_daily_flow+0.01)) # not great raw - looks better logged
dotchart(data$mean_wind) # all good - random/uniform - two distinct patterns...

#dotchart(data$wind_speed_lag1) # looks fine

# Generate a net id
data <- data %>% 
  group_by(fisher, date) %>% 
  mutate(net = paste0(fisher, "-", cal_year,"-", row_number())) %>% 
  ungroup()

# Check reporting - single row per data event
n <- data %>% 
  group_by(estuary, fisher, date) %>% 
  summarise(n = n()) %>%
  filter(n > 1) %>% 
  nrow()

print(n) 

write_csv(data, "../../Other/CPUE Seminar/Flathead input data.csv")

# n / length(unique(data$date)) * 100 # 0.16% issues - fixed in next step 
# 
# n <- data %>% 
#   group_by(estuary,fisher, date) %>% 
#   summarise(n = n()) %>%
#   filter(n > 1) %>% mutate(fisher_date_estuary = paste0(estuary, fisher, date, sep="_")) %>%
#   select(-fisher, -date, -estuary)
# 
# data <- data %>% mutate(fisher_date_estuary = paste0(estuary, fisher, date, sep="_")) %>%
#   anti_join(n) %>% select(-fisher_date_estuary)


# Calculate differences between observations
# data <- data %>%
#   group_by(estuary) %>% # for each estuary
#   arrange(date) %>% # order the data
#   mutate(t = as.numeric(date - lag(date, 1)), # the difference between dates
#          t = if_else(is.na(t), 1, t), # change from NA at t = 1
#          t = cumsum(t)) %>% # and now get the cumulative sum of the differences
#   ungroup()

# Set up factors for REs
data <- data %>% 
  mutate(#estuary = factor(estuary),
         fisher = factor(fisher),
         estuary = as.factor(as.character(estuary)),
         cal_year = factor(cal_year),
         monthF = as.factor(as.character(month)),
         log_flow = log(mean_daily_flow+0.05),
         full_date = lubridate::dmy(paste0("15_",date)),
         days_since_start = as.integer(full_date-min(full_date)+1)) %>%
  ungroup() %>% group_by(estuary) %>%
  mutate(scaled_flow = scale(log_flow),
         scaled_wind = scale(mean_wind),
         year_est = paste0(cal_year, "_",estuary))

data2 <- data %>% filter(C_E < 200)# %>% filter(prop_dusky >0.1)# %>% filter(catch>0)

# Try fitting with {mgcv} first
m <- gam(
  formula = catch ~ # kg?
    monthF + estuary+
    #s(days_since_start, bs="tp", by = estuary)+
    #s(prop_dusky, bs="tp")+# month as factor
    #s(lunar_phase, bs = "cc") +
    #s(month, bs = "cc", by = estuary) +
    #s(wind_speed_lag1, bs = "tp", by = estuary) +
    #s(log_flow, bs="tp", by = estuary)+
    s(scaled_flow, bs="tp")+
    s(scaled_flow, estuary, bs = "fs") +
    s(scaled_wind, bs = "tp") +
    s(scaled_wind, estuary, bs = "fs") +
    # s(recruit_flow_scaled, bs = "tp", by = estuary) +
    s(fisher, bs = "re") +
    s(cal_year, bs = "re") +
    offset(log(effort)), # days fishing effort
  data = data2,
  family = tw(),  #tw(),
 # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
  select = TRUE,
  method = "REML"#,
# correlation = corCAR1(form = ~ days_since_start|estuary)
)

AIC(m)



source("Scripts/dharma-to-gg_fn.R")

plots <- DHARMa_plot(m)
plot_a <- plots[[1]]
plot_a

plot_b <- plots[[2]]
plot_b

library(patchwork)
plot_a + plot_b
ggsave("Plots/model checks/Other Estuary model residuals.png", dpi=300, width = 21, height=14.8, units="cm")





appraise(m)

simulationOutput <- simulateResiduals(m, n = 1000)
plot(simulationOutput, quantreg=T)
testResiduals(simulationOutput)

hist(data2$prop_dusky)

summary(m)
anova(m)
draw(m)



# Try DHARMa standard tests
testResiduals(simulationOutput)
# All tests are significant
# QQ plot looks OK to me
# Dispersion looks very bad
# What to do about an overdispersed Tweedie model?

# Heteroscedasticity
testQuantiles(simulationOutput)
# Qunatile regressions are significant, but the patterns don't look terrible...

residuals <- simulateResiduals(m)

# Autocorrelation
acf <- data.frame(
  t = data2$days_since_start,
  estuary = data2$estuary,
  fisher = data2$fisher,
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

# Some wiggliness more in Lake Illawarra - not sure what to do about this?

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
       xvar = "log_flow", 
       by = "estuary", 
       cond = list(effort = 100),
       scale = "response")



# How much variance in the REs?
variance_components <- variance_comp(m)
variance_components

sum(variance_components$variance)

#### Manual predictions to match telemetry plots
new_data <- expand.grid("monthF" = unique(data2$monthF),
                        "prop_dusky" = median(data2$prop_dusky),
                        "scaled_flow" = median(data2$scaled_flow, na.rm = T),
                        "estuary" = "Clarence River", #unique(data2$estuary),
                        #"lunar_phase" = median(data2$lunar_phase, na.rm = T),
                        "scaled_wind" = median(data2$scaled_wind, na.rm = T),
                        "fisher" = data2$fisher[1],
                        "cal_year" = data2$cal_year[1],
                        "days_since_start" = median(data2$days_since_start),
                        "effort" = median(data$effort, na.rm=T))

new_dat_preds <- predict(m, newdata = new_data, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data$fit <- new_dat_preds$fit
new_data$se <- new_dat_preds$se.fit

p4 <- ggplot(new_data, aes(as.numeric(as.character(monthF)), fit)) + geom_point() +# facet_wrap(~Array) +
  geom_errorbar(aes(ymin=fit-se, ymax = fit+se)) + theme_bw()
p4

# log_flow
limits <- data2 %>% group_by(estuary) %>%
  summarise(min_flow = min(scaled_flow, na.rm=T),
            max_flow = max(scaled_flow, na.rm=T))

new_data2 <- expand.grid("monthF" = "6",
                        "scaled_flow" = seq(min(data$scaled_flow, na.rm=T), max(data$scaled_flow, na.rm=T),0.1),
                        "prop_dusky" = median(data2$prop_dusky),
                        "estuary" = unique(data2$estuary),
                        "days_since_start" = median(data2$days_since_start),
                        "scaled_wind" = median(data$scaled_wind, na.rm = T),
                        "fisher" = data$fisher[1],
                        "cal_year" = data$cal_year[1],
                        "effort" = median(data$effort, na.rm=T)) %>% left_join(limits) %>%
  filter(scaled_flow <= max_flow) %>% filter(scaled_flow >= min_flow)

new_dat_preds2 <- predict(m, newdata = new_data2, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data2$fit <- new_dat_preds2$fit
new_data2$se <- new_dat_preds2$se.fit

p1 <- ggplot(new_data2, aes((scaled_flow), fit)) + geom_line() + facet_wrap(~estuary, scales = "free") +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)+
  geom_rug(data=data2, aes(x=scaled_flow), inherit.aes = F)
p1

# # lunar
# # limits <- daily_move %>% group_by(Array) %>%
# #   summarise(min_flow = min(scaled_flow, na.rm=T),
# #             max_flow = max(scaled_flow, na.rm=T))
# 
# new_data3 <- expand.grid("monthF" = "6",
#                          "scaled_flow" = median(data$scaled_flow),
#                          #"Array" = unique(daily_move$Array),
#                          "lunar_phase" = seq(min(data$lunar_phase), max(data$lunar_phase), 0.1),
#                          "mean_wind" = median(data$mean_wind, na.rm = T),
#                          "fisher" = data$fisher[1],
#                          "cal_year" = data$cal_year[1],
#                          "effort" = median(data$effort, na.rm=T))
# 
# new_dat_preds3 <- predict(m, newdata = new_data3, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")
# 
# new_data3$fit <- new_dat_preds3$fit
# new_data3$se <- new_dat_preds3$se.fit
# 
# p2 <- ggplot(new_data3, aes(lunar_phase, fit)) + geom_line() + # facet_wrap(~Array) +
#   geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)
# p2

# wind_speed
limlims <- data2 %>% group_by(estuary) %>%
  summarise(min_wind = min(scaled_wind),
            max_wind = max(scaled_wind))

new_data4 <- expand.grid("monthF" = "6",
                         "scaled_flow" = median(data2$scaled_flow, na.rm=T),
                         "prop_dusky" = median(data2$prop_dusky),
                         "days_since_start" = median(data2$days_since_start),
                         "estuary" = unique(data2$estuary),
                         #"lunar_phase" = median(data2$lunar_phase),
                         "scaled_wind" = seq(min(data2$scaled_wind), max(data$scaled_wind), 0.1),
                         "fisher" = data2$fisher[1],
                         "cal_year" = data2$cal_year[1],
                         "effort" = median(data2$effort, na.rm=T)) %>%
  left_join(limlims) %>% filter(scaled_wind >= min_wind) %>%
  filter(scaled_wind <= max_wind)

new_dat_preds4 <- predict(m, newdata = new_data4, exclude = c("s(fisher)", "s(cal_year)") , se.fit = T,type = "response")

new_data4$fit <- new_dat_preds4$fit
new_data4$se <- new_dat_preds4$se.fit

p3 <- ggplot(new_data4, aes(scaled_wind, fit)) + geom_line() + facet_wrap(~estuary, scales = "free") +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)+
  geom_rug(data=data2, aes(x=scaled_wind), inherit.aes = F)
p3

library(patchwork)
p1/p3/p4 & theme_bw()

ggsave("Plots/Other Estuary Mesh Net Monthly model outcomes.pdf", dpi=300, width = 21, height=28, units="cm")
ggsave("Plots/Other Estuary Mesh Net Monthly model outcomes.png", dpi=300, width = 21, height=28, units="cm")


# 
# 
# ##### STANDARDISATION
# m <- gam(
#   formula = catch ~ # kg?
#     monthF + cal_year*estuary+
#     #s(days_since_start, bs="tp", by = estuary)+
#     #s(prop_dusky, bs="tp")+# month as factor
#     #s(lunar_phase, bs = "cc") +
#     #s(month, bs = "cc", by = estuary) +
#     #s(wind_speed_lag1, bs = "tp", by = estuary) +
#     #s(log_flow, bs="tp", by = estuary)+
#     s(scaled_flow, bs="tp")+
#     s(scaled_flow, estuary, bs = "fs") +
#     s(scaled_wind, bs = "tp") +
#     # s(recruit_flow_scaled, bs = "tp", by = estuary) +
#     s(fisher, bs = "re") +
#     #s(cal_year, bs = "re") +
#     offset(log(effort)), # days fishing effort
#   data = data2,
#   family = tw(),  #tw(),
#   # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
#   select = TRUE,
#   method = "REML"#,
#   # correlation = corCAR1(form = ~ days_since_start|estuary)
# )
# 
# resids <- simulateResiduals(m)
# plot(resids)
# 
# new_dat <- expand.grid(monthF = "6",
#                        cal_year = unique(data2$cal_year),
#                        estuary = unique(data2$estuary),
#                        scaled_flow = 0,
#                        scaled_wind = 0,
#                        #prop_dusky=0.8,
#                        fisher = data2$fisher[5],
#                        effort = median(data2$effort)
#                        )
# 
# preds <- predict(m, newdata = new_dat, exclude = "s(fisher)", se.fit = T)
# 
# new_dat$fit <- preds$fit
# new_dat$se <- preds$se.fit
# 
# ggplot(new_dat, aes(as.numeric(as.character(cal_year)), fit)) + facet_wrap(~estuary) + geom_point() +
#   geom_errorbar(aes(ymin = fit-se, ymax = fit+se))
# 
# nominal <- data2 %>% group_by(estuary, cal_year) %>%
#   summarise(total_catch = sum(catch),
#             total_effort = sum(effort),
#             CPUE = total_catch/total_effort)
# 
# ggplot(nominal, aes(as.numeric(as.character(cal_year)), CPUE)) + facet_wrap(~estuary) + geom_point() #+
#   #geom_errorbar(aes(ymin = fit-se, ymax = fit+se))
# 
# ### Scale to plot together
# new_dat <- new_dat %>% ungroup() %>% group_by(estuary) %>%
#   mutate(mean_fit = mean(fit),
#          lower_se = fit-se,
#          upper_se = fit+se,
#          scaled_fit = fit/mean_fit,
#          scaled_upper = upper_se/mean_fit,
#          scaled_lower = lower_se/mean_fit)
# 
# nominal <- nominal %>% ungroup() %>% group_by(estuary) %>%
#   mutate(scaled_CPUE = CPUE/mean(CPUE))
# 
# ggplot(new_dat, aes(as.numeric(as.character(cal_year)), scaled_fit)) + facet_wrap(~estuary) + geom_point() +
#   geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2) +
#   geom_line(data=nominal, aes(y=scaled_CPUE), col="blue")
# 
# 
# # 
# # ### Combine to single index
# # # Hawkesbury River 114.5 km2
# # # Camden Haven River 32.2 km2
# # # Clarence River 132.3 km2
# # # Hunter River 47 km2
# # 
# # new_dat <- new_dat %>% mutate(Estuary_area = case_when(estuary == "Camden Haven River" ~ 32.2,
# #                                                        estuary == "Clarence River" ~ 132.3,
# #                                                        estuary == "Hawkesbury River" ~ 114.5,
# #                                                        estuary == "Hunter River" ~ 47),
# #                               Total_area = 32.2+132.3+114.5+47,
# #                               area_weight = Estuary_area/Total_area)
# #   
# # combined_ests <- new_dat %>% ungroup() %>% group_by(cal_year) %>%
# #   summarise(weighted_mean = weighted.mean(fit, area_weight),
# #             weighted_se = sqrt(sum(area_weight^2 * se^2))) %>%
# #   ungroup() %>% mutate(mean_CPUE = mean(weighted_mean),
# #                        scaled_weighted_mean = weighted_mean/mean_CPUE,
# #                        lower_se = weighted_mean - weighted_se,
# #                        upper_se = weighted_mean + weighted_se,
# #                        scaled_upper = upper_se/mean_CPUE,
# #                        scaled_lower = lower_se/mean_CPUE)
# # 
# # #var_weighted_mean <- sum(Estuary_area^2 * se^2)
# # #se_weighted_mean <- sqrt(var_weighted_mean)
# # 
# # ggplot(combined_ests, aes(as.numeric(as.character(cal_year)), weighted_mean)) + geom_line() +
# #   geom_ribbon(aes(ymin = weighted_mean- weighted_se, ymax = weighted_mean + weighted_se), alpha=0.2)+
# #   ylim(c(0,4))
# # 
# # 
# # nominal2 <- data2 %>% group_by(cal_year) %>%
# #   summarise(total_catch = sum(catch),
# #             total_effort = sum(effort),
# #             CPUE = total_catch/total_effort) %>%
# #   ungroup() %>% mutate(mean_CPUE = mean(CPUE),
# #                        scaled_CPUE = CPUE/mean_CPUE)
# # 
# # ggplot(combined_ests, aes(as.numeric(as.character(cal_year)), scaled_weighted_mean)) + geom_point() +
# #   geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2) +
# #   geom_line(data=nominal2, aes(y=scaled_CPUE), col="blue") + ylim(c(0,1.5))
# 
# 
# 
# # #### Now without the flow and wind
# m2 <- gam(
#   formula = catch ~ # kg?
#     monthF + cal_year*estuary+
#     #s(days_since_start, bs="tp", by = estuary)+
#     #s(prop_dusky, bs="tp")+# month as factor
#     #s(lunar_phase, bs = "cc") +
#     #s(month, bs = "cc", by = estuary) +
#     #s(wind_speed_lag1, bs = "tp", by = estuary) +
#     #s(log_flow, bs="tp", by = estuary)+
#     #s(scaled_flow, bs="tp")+
#     #s(scaled_flow, estuary, bs = "fs") +
#     #s(scaled_wind, bs = "tp") +
#     # s(recruit_flow_scaled, bs = "tp", by = estuary) +
#     s(fisher, bs = "re") +
#     #s(cal_year, bs = "re") +
#     offset(log(effort)), # days fishing effort
#   data = data2,
#   family = tw(),  #tw(),
#   # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
#   select = TRUE,
#   method = "REML"#,
#   # correlation = corCAR1(form = ~ days_since_start|estuary)
# )
# 
# resids2 <- simulateResiduals(m2)
# plot(resids2)
# 
# new_dat2 <- expand.grid(monthF = "6",
#                        cal_year = unique(data2$cal_year),
#                        estuary = unique(data2$estuary),
#                        scaled_flow = 0,
#                        scaled_wind = 0,
#                        #prop_dusky=0.8,
#                        fisher = data2$fisher[5],
#                        effort = median(data2$effort)
# )
# 
# preds2 <- predict(m2, newdata = new_dat2, exclude = "s(fisher)", se.fit = T)
# 
# new_dat2$fit <- preds2$fit
# new_dat2$se <- preds2$se.fit
# 
# ggplot(new_dat2, aes(as.numeric(as.character(cal_year)), fit)) + facet_wrap(~estuary) + geom_point() +
#   geom_errorbar(aes(ymin = fit-se, ymax = fit+se))
# 
# nominal <- data2 %>% group_by(estuary, cal_year) %>%
#   summarise(total_catch = sum(catch),
#             total_effort = sum(effort),
#             CPUE = total_catch/total_effort)
# 
# ggplot(nominal, aes(as.numeric(as.character(cal_year)), CPUE)) + facet_wrap(~estuary) + geom_point() #+
# #geom_errorbar(aes(ymin = fit-se, ymax = fit+se))
# 
# ### Scale to plot together
# new_dat2 <- new_dat2 %>% ungroup() %>% group_by(estuary) %>%
#   mutate(mean_fit = mean(fit),
#          lower_se = fit-se,
#          upper_se = fit+se,
#          scaled_fit = fit/mean_fit,
#          scaled_upper = upper_se/mean_fit,
#          scaled_lower = lower_se/mean_fit)
# 
# nominal <- nominal %>% ungroup() %>% group_by(estuary) %>%
#   mutate(scaled_CPUE = CPUE/mean(CPUE))
# 
# ggplot(new_dat2, aes(as.numeric(as.character(cal_year)), scaled_fit)) + facet_wrap(~estuary) + geom_point() +
#   geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2, fill="grey10") +
#   geom_line(data=nominal, aes(y=scaled_CPUE), col="blue") +
#   geom_line(data=new_dat, col="red")+
#   geom_ribbon(data=new_dat, aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2, fill="red") 
