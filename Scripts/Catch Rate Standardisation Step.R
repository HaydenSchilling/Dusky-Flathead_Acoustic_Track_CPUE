# Catch standardisation - shoalhaven and other

# Load the required libraries
library(tidyverse)
library(gratia)
library(janitor)
library(mgcv)
library(DHARMa)
library(visreg)

# # Load the data
# dataA <- read_csv("Data/Flathead Env data to model Shoalhaven Commercial Monthly.csv") %>% select(-Estuary)
# 
# # Load the data
# dataB <- read_csv("Data/Flathead Env data to model Mesh Net Commercial Monthly Other Estuary.csv")
# 
# data <- bind_rows(dataA, dataB)
# 
# variable.names(dataA)
# variable.names(dataB)
# 
# 
# # Standardize names of variables
# data <- data %>% clean_names()
# 
# # Reduce df to just the variables needed
# data <- data %>%
#   dplyr::select(
#     estuary_name,
#     #date = event_date,
#     fisher = authorised_fisher_id,
#     month,
#     cal_year,
#     catch = dusky_flathead,
#     effort = days_fished, # metres of net
#     #lunar_phase,
#     mean_daily_flow,
#     log_flow,
#     mean_wind,
#     prop_dusky) %>%
#   rename(estuary = estuary_name) %>%
#   mutate(C_E = catch/effort)
# #recruit_flow,
# #flow_mean_scaled,
# #wind_speed_lag1
# # ) %>% mutate(log_flow = log(mean_daily_flow+0.1)) #%>%
# # group_by(estuary) %>% mutate(flow_mean_scaled = as.vector(scale(log_flow)),
# #                            recruit_flow_scaled = as.vector(scale(logRecruitFlow)))
# 
# summary(data$mean_daily_flow)
# #summary(data$log_flow)
# 
# 
# table(data$estuary, data$month)
# 
# # # limit to months of overnight setting
# # data <- data %>% filter((estuary == "Lake Illawarra" & between(month, 6,8))|
# #                           (estuary == "Tuggerah Lakes" & between(month, 3,11))|
# #                           (estuary == "Wallis Lake" & between(month, 3,11)))
# # 
# # table(data$estuary, data$month)
# 
# # Ensure date is in the correct format
# data <- data %>% mutate(date =paste(month, cal_year, sep = "_"))
# 
# # How often do they go fishing
# summary <- data %>%
#   group_by(fisher) %>%
#   summarise(days = length(unique(date)),
#             nets = n(),
#             effort = sum(effort),
#             catch = sum(catch))
# 
# # Which fishers to remove? 
# # Never caught a flathead or fished less than 10 months
# remove <- summary %>%
#   filter(catch == 0 | days < 10) %>% 
#   pull(fisher)
# 
# # Remove them
# data <- data %>% filter(!(fisher %in% remove))
# 
# days <- summary %>% pull(days)
# catch <- summary %>% pull(catch)
# hist(days)
# 
# mean(days)
# par(mfrow = c(2, 1))
# hist(days, breaks = max(days) / 10)
# abline(v = c(mean(days), median(days)), lty = c("dashed", "dotted"))
# boxplot(days, horizontal = TRUE)
# 
# mean(catch)
# par(mfrow = c(2, 1))
# hist(catch, breaks = max(catch) / 10)
# abline(v = c(mean(catch), median(catch)), lty = c("dashed", "dotted"))
# boxplot(catch, horizontal = TRUE)
# 
# # Have a look at the distribution of the variables
# dotchart(data$catch) # one dodgy - checked log sheet - remove
# #data <- data %>% filter(catch < 500)
# #data <- data %>% filter(catch < 500)
# #dotchart(data$catch) # looks nicer now
# 
# dotchart(data$effort) # mostly ok
# #data <- data %>% filter(effort < 4000) # even 2 km seems like heaps...
# #dotchart(data$effort)
# 
# dotchart(data$catch / data$effort) # some possible high >200
# #data <- data %>% filter(C_E < 0.6)
# 
# 
# #dotchart(data$lunar_phase) # all good - random/uniform
# dotchart(log(data$mean_daily_flow+0.01)) # not great raw - looks better logged
# dotchart(data$mean_wind) # all good - random/uniform - two distinct patterns...
# 
# #dotchart(data$wind_speed_lag1) # looks fine
# 
# # Generate a net id
# data <- data %>% 
#   group_by(fisher, date) %>% 
#   mutate(net = paste0(fisher, "-", cal_year,"-", row_number())) %>% 
#   ungroup()
# 
# # Check reporting - single row per data event
# n <- data %>% 
#   group_by(estuary, fisher, date) %>% 
#   summarise(n = n()) %>%
#   filter(n > 1) %>% 
#   nrow()
# 
# print(n) 
# 
# write_csv(data, "Data/Data for standardisation section.csv")

data <- read_csv("Data/Data for standardisation section.csv")

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

data2 <- data# %>% filter(C_E < 200)# %>% filter(prop_dusky >0.1)# %>% filter(catch>0)

table(data2$estuary, data2$monthF)

##### STANDARDISATION
m <- gam(
  formula = catch ~ # kg?
    monthF + cal_year*estuary+
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
    #s(cal_year, bs = "re") +
    offset(log(effort)), # days fishing effort
  data = data2,
  family = tw(),  #tw(),
  # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
  select = TRUE,
  method = "REML"#,
  # correlation = corCAR1(form = ~ days_since_start|estuary)
)

resids <- simulateResiduals(m)
plot(resids)
summary(m)
anova(m)

new_dat <- expand.grid(monthF = "6",
                       cal_year = unique(data2$cal_year),
                       estuary = unique(data2$estuary),
                       scaled_flow = 0,
                       scaled_wind = 0,
                       #prop_dusky=0.8,
                       fisher = data2$fisher[5],
                       effort = median(data2$effort)
)

preds <- predict(m, newdata = new_dat, exclude = "s(fisher)", se.fit = T)

new_dat$fit <- preds$fit
new_dat$se <- preds$se.fit

ggplot(new_dat, aes(as.numeric(as.character(cal_year)), fit)) + facet_wrap(~estuary) + geom_point() +
  geom_errorbar(aes(ymin = fit-se, ymax = fit+se))

nominal <- data2 %>% group_by(estuary, cal_year) %>%
  summarise(total_catch = sum(catch),
            total_effort = sum(effort),
            CPUE = total_catch/total_effort)

ggplot(nominal, aes(as.numeric(as.character(cal_year)), CPUE)) + facet_wrap(~estuary) + geom_point() #+
#geom_errorbar(aes(ymin = fit-se, ymax = fit+se))

### Scale to plot together
new_dat <- new_dat %>% ungroup() %>% group_by(estuary) %>%
  mutate(mean_fit = mean(fit),
         lower_se = fit-se,
         upper_se = fit+se,
         scaled_fit = fit/mean_fit,
         scaled_upper = upper_se/mean_fit,
         scaled_lower = lower_se/mean_fit)

nominal <- nominal %>% ungroup() %>% group_by(estuary) %>%
  mutate(scaled_CPUE = CPUE/mean(CPUE))

ggplot(new_dat, aes(as.numeric(as.character(cal_year)), scaled_fit)) + facet_wrap(~estuary) + geom_point() +
  geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2) +
  geom_line(data=nominal, aes(y=scaled_CPUE), col="blue")




# #### Now without the flow and wind
m2 <- gam(
  formula = catch ~ # kg?
    monthF + cal_year*estuary+
    #s(days_since_start, bs="tp", by = estuary)+
    #s(prop_dusky, bs="tp")+# month as factor
    #s(lunar_phase, bs = "cc") +
    #s(month, bs = "cc", by = estuary) +
    #s(wind_speed_lag1, bs = "tp", by = estuary) +
    #s(log_flow, bs="tp", by = estuary)+
    #s(scaled_flow, bs="tp")+
    #s(scaled_flow, estuary, bs = "fs") +
    #s(scaled_wind, bs = "tp") +
    # s(recruit_flow_scaled, bs = "tp", by = estuary) +
    s(fisher, bs = "re") +
    #s(cal_year, bs = "re") +
    offset(log(effort)), # days fishing effort
  data = data2,
  family = tw(),  #tw(),
  # knots = list(lunar_phase = c(0, 2 * pi)), #month = c(1, 12), 
  select = TRUE,
  method = "REML"#,
  # correlation = corCAR1(form = ~ days_since_start|estuary)
)

resids2 <- simulateResiduals(m2)
plot(resids2)

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
#ggsave("Plots/model checks/Standardisation model residuals.png", dpi=300, width = 21, height=14.8, units="cm")





new_dat2 <- expand.grid(monthF = "6",
                        cal_year = unique(data2$cal_year),
                        estuary = unique(data2$estuary),
                        scaled_flow = 0,
                        scaled_wind = 0,
                        #prop_dusky=0.8,
                        fisher = data2$fisher[5],
                        effort = median(data2$effort)
)

preds2 <- predict(m2, newdata = new_dat2, exclude = "s(fisher)", se.fit = T)

new_dat2$fit <- preds2$fit
new_dat2$se <- preds2$se.fit

ggplot(new_dat2, aes(as.numeric(as.character(cal_year)), fit)) + facet_wrap(~estuary) + geom_point() +
  geom_errorbar(aes(ymin = fit-se, ymax = fit+se))

nominal <- data2 %>% group_by(estuary, cal_year) %>%
  summarise(total_catch = sum(catch),
            total_effort = sum(effort),
            CPUE = total_catch/total_effort)

ggplot(nominal, aes(as.numeric(as.character(cal_year)), CPUE)) + facet_wrap(~estuary) + geom_point() #+
#geom_errorbar(aes(ymin = fit-se, ymax = fit+se))

### Scale to plot together
new_dat2 <- new_dat2 %>% ungroup() %>% group_by(estuary) %>%
  mutate(mean_fit = mean(fit),
         lower_se = fit-se,
         upper_se = fit+se,
         scaled_fit = fit/mean_fit,
         scaled_upper = upper_se/mean_fit,
         scaled_lower = lower_se/mean_fit)

nominal <- nominal %>% ungroup() %>% group_by(estuary) %>%
  mutate(scaled_CPUE = CPUE/mean(CPUE))

ggplot(new_dat2, aes(as.numeric(as.character(cal_year)), scaled_fit)) + facet_wrap(~estuary) + geom_point() +
  geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2, fill="grey10") +
  geom_line(data=nominal, aes(y=scaled_CPUE), col="blue") +
  geom_line(data=new_dat, col="red")+
  geom_ribbon(data=new_dat, aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2, fill="red") 


new_dat <- new_dat %>% mutate(Method = "Including flow and wind")
new_dat2 <- new_dat2 %>% mutate(Method = "Excluding flow and wind")
nominal <- nominal %>% mutate(Method = "Nominal") %>% 
  rename(scaled_fit=scaled_CPUE)

plot_datX <- bind_rows(new_dat, new_dat2, nominal)
write_csv(as.character(plot_datX), "standardised data for plotting.csv")

ggplot(plot_datX, aes(as.numeric(as.character(cal_year)), scaled_fit,
                      col = Method, fill=Method, lty=Method)) +
  facet_wrap(~estuary, scales="free_y") + geom_point(size=0.7) +
  geom_ribbon(aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2) +
  geom_line() + theme_bw()+ labs(x="Year", y = "Scaled CPUE")+
  theme(legend.position = c(0.8,0.2),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=10, colour = "black"),
        axis.title = element_text(face = "bold", size=14),
        axis.text = element_text(size=12, colour = "black"),
        strip.text = element_text(size=10))+
  scale_fill_manual(values = c("blue", "red","grey"))+
  scale_colour_manual(values = c("blue", "red","grey"))+
  scale_linetype_manual(values=c(NA,NA,"dashed")) +
  ylim(c(0,NA))
  
#geom_line(data=nominal, aes(y=scaled_CPUE), col="blue") +
  #geom_line(data=new_dat, col="red")+
  #geom_ribbon(data=new_dat, aes(ymin = scaled_lower, ymax = scaled_upper), alpha=0.2, fill="red") 

#ggsave("Plots/Standardisation Figure.pdf", dpi=300, width=21, height=14.8, units="cm")
#ggsave("Plots/Standardisation Figure.png", dpi=300, width=21, height=14.8, units="cm")
