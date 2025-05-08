library(tidyverse)
library(mgcv)
library(gratia)
library(DHARMa)

# ## Data Prep
# daily_move <- read_csv("VPS - Hayden/Full CEFT Download/Daily movements.csv")
# 
# # biometric data
# serial_nums_transmitter_key <- read_csv("VPS - Hayden/Full CEFT Download/IMOS_detections.csv") %>%
#   select(transmitter_serial_number, transmitter_id) %>% distinct() %>%
#   rename(SerialNo = transmitter_serial_number)
# 
# 
# bio_dat <- read_csv("VPS - Hayden/Dylan Data/Flathead_TaggingData_240708.csv") %>%
#   left_join(serial_nums_transmitter_key) %>% drop_na(transmitter_id) %>% 
#   select(transmitter_id , SerialNo, Sex, `Fork Length (mm)`) %>%
#   rename(Transmitter = transmitter_id)
# 
# daily_move <- daily_move %>% left_join(bio_dat)
# 
# table(daily_move$Sex)
# 
# daily_move <- daily_move %>% mutate(Sex = case_when(Sex == "Check" ~ "U",
#                                                     Sex == "F2" ~ "F",
#                                                     Sex == "F3" ~ "F",
#                                                     Sex == "M?" ~ "U",
#                                                     T ~ Sex))
# 
# table(daily_move$Sex)
# table(daily_move$Array)
# 
# # Environmental Data
# env_dat <- read_csv("VPS - Hayden/Dylan Data/Environmental_HourlyData_240708.csv") %>%
#   select(DateTime, DateTimeR5, `Clyde_Level(m)`, `Clyde_Discharge(ML/d)`, `ShoalGrassy_Level(m)`,
#          `ShoalGrassy_Discharge (ML/d)`, `Temp_Clyde_003`, `Sal_Clyde_003`, `Temp_Clyde_010`, `Sal_Clyde_010`,
#          `Temp_Clyde_017`, `Sal_Clyde_017`,  `Temp_Clyde_024`, `Sal_Clyde_024`,  `Temp_Clyde_031`, `Sal_Clyde_031`,
#          `CM_009_Temp`, `CM_009_Sal`, `CM_028_Temp`, `CM_028_Sal`, `CM_049_Temp`, `CM_049_Sal`,
#          `CM_071_Temp`, `CM_071_Sal`, `CM_103_Temp`, `CM_103_Sal`, Tide_Batemans, Tide_Greenwell) %>%
#   mutate(DateTime = lubridate::dmy(DateTime))
# 
# tt <- env_dat %>% pivot_longer(cols = 3:28, values_to = "value", names_to = "variable") %>%
#   group_by(DateTime, variable) %>% summarise(Value = mean(value, na.rm=T))
# 
# ggplot(tt, aes(DateTime, Value)) + geom_line() + facet_wrap(~variable, scales="free_y") + 
#   geom_vline(aes(xintercept = as.Date("2010-10-01")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2009-11-23")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2013-08-01")), col="red")
# 
# ggsave("VPS - Hayden/Dylan Data/Environmental Data plot.png", dpi=300, width=38, height=25, units="cm")
# 
# ranges <- daily_move %>% group_by(Array) %>% summarise(min_date = min(Date),
#                                                        max_date = max(Date))
# ranges
# 
# # ok we want to keep Clyde_010 and Clyde_024 - maybe average together
# # Shoalhaven CM_028 and CM_071 - will have some missing later end dates
# 
# vars = c("Temp_Clyde_010", "Sal_Clyde_010", "Temp_Clyde_024", "Sal_Clyde_024",
#          "CM_028_Temp",
#          "CM_028_Sal", 
#          "CM_071_Temp", 
#          "CM_071_Sal" )
# 
# test_dat <- tt %>% filter(variable %in% vars)
# ggplot(test_dat, aes(DateTime, Value, col=variable)) + geom_line() + #facet_wrap(~variable, scales="free_y") + 
#   geom_vline(aes(xintercept = as.Date("2010-10-01")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2009-11-23")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2013-08-01")), col="red")
# 
# temp_sal_dat <- env_dat %>% group_by(DateTime) %>%
#   summarise(Clyde_temp = mean(c( Temp_Clyde_024 ), na.rm=T), #, Temp_Clyde_010
#             Clyde_sal = mean(c(Sal_Clyde_010, Sal_Clyde_024), na.rm=T),
#             Shoalhaven_Temp = mean(c(CM_071_Temp, CM_028_Temp), na.rm=T), #, , CM_028_Temp
#             Shoalhaven_Sal = mean(c(CM_028_Sal, CM_071_Sal), na.rm=T),
#             Tide_Clyde_max = max(Tide_Batemans, na.rm=T),
#             Tide_Shoalhaven_max = max(Tide_Greenwell, na.rm=T))
# 
# ggplot(temp_sal_dat, aes(DateTime)) + 
#   geom_line(aes(y=Clyde_temp), col= "red") + 
#   geom_line(aes(y=Clyde_sal), col= "blue") + 
#   #geom_line(aes(y=Shoalhaven_Temp)) + 
#   #geom_line(aes(y=Shoalhaven_Sal)) + 
#   geom_vline(aes(xintercept = as.Date("2010-10-01")), col="red")+
#   #geom_vline(aes(xintercept = as.Date("2009-11-23")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2013-08-01")), col="red")
# 
# 
# ggplot(temp_sal_dat, aes(DateTime)) + 
#   #geom_line(aes(y=Clyde_temp), col= "red") + 
#   #geom_line(aes(y=Clyde_sal), col= "blue") + 
#   geom_line(aes(y=Shoalhaven_Temp)) + 
#   geom_line(aes(y=Shoalhaven_Sal)) + 
#   #geom_vline(aes(xintercept = as.Date("2010-10-01")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2009-11-23")), col="red")+
#   geom_vline(aes(xintercept = as.Date("2013-08-01")), col="red")
# 
# # match the data now
# Clyde_temp_sal_dat <- temp_sal_dat %>% select(Clyde_temp, Clyde_sal, DateTime, Tide_Clyde_max) %>% #, Shoalhaven_Temp, Shoalhaven_Sal
#   rename(Date = DateTime, Temp_logger = Clyde_temp, Sal_logger = Clyde_sal,
#          Tide_max = Tide_Clyde_max) %>% mutate(Array = "Clyde River")
# 
# Shoalhaven_temp_sal_dat <- temp_sal_dat %>% select(Shoalhaven_Temp, Shoalhaven_Sal, DateTime, Tide_Shoalhaven_max) %>% #, Shoalhaven_Temp, Shoalhaven_Sal
#   rename(Date = DateTime, Temp_logger = Shoalhaven_Temp, Sal_logger = Shoalhaven_Sal,
#          Tide_max = Tide_Shoalhaven_max) %>% mutate(Array = "Shoalhaven River")
# 
# CS_Tem_Sal <- Clyde_temp_sal_dat %>% bind_rows(Shoalhaven_temp_sal_dat)
# 
# #shoalhaven <- read_csv("VPS - Hayden/RSP Cleaned combined data.csv") %>% filter(Array == "Shoalhaven River")
# 
# flowS <- read_csv("VPS - Hayden/Shoalhaven Rv Grassy Gully2 Gauge.csv") %>% select(-6) %>%
#   mutate(Date = as.Date(lubridate::dmy_hm(`Date and time`))) %>% filter(Date > as.Date("2009-06-30")) %>%
#   filter(Date < as.Date("2014-01-01")) %>% mutate(Array = "Shoalhaven River")
# 
# ggplot(flowS, aes(Date, Mean_Discharge)) + geom_line()
# 
# 
# flowC <- read_csv("VPS - Hayden/Clyde Rv Brooman Gauge.csv") %>% select(-6) %>%
#   mutate(Date = as.Date(lubridate::dmy_hm(`Date and time`))) %>% filter(Date > as.Date("2009-06-30")) %>%
#   filter(Date < as.Date("2014-01-01")) %>% mutate(Array = "Clyde River")
# 
# ggplot(flowC, aes(Date, Mean_Discharge)) + geom_line()
# 
# flow <- flowS %>% bind_rows(flowC)
# 
# # Rolling average
# library(zoo)
# library(lunar)
# 
# flow <- flow %>% mutate(day14_flow = rollmean(Mean_Discharge, k = 2, align = "right", fill= NA))
# 
# daily_move <- daily_move %>% left_join(flow) %>% left_join(CS_Tem_Sal) %>%
#   mutate(ID = as.factor(as.character(Transmitter)),
#          #log_dist = log(dist+1),
#          log_flow = log(day14_flow),
#          lunar = lunar.phase(Date),
#          day_of_year = lubridate::yday(Date))
# 
# summary(daily_move)
# 
# 
# plot(daily_move$Temp_logger, daily_move$day_of_year)
# plot(log(daily_move$day14_flow), daily_move$Sal_logger)
# 
# # make daily dataframe to model/interpolate from
# 
# daily_env <- CS_Tem_Sal %>% left_join(flow) %>% mutate(day_of_year = lubridate::yday(Date)) %>%
#   select(-c(7:11))
# 
# plot(daily_env$Temp_logger, daily_env$day_of_year)
# plot(log(daily_env$day14_flow), daily_env$Sal_logger)
# 
# library(mgcv)
# sal_gam <- gam(Sal_logger ~ s(log(day14_flow)), data = daily_env)
# summary(sal_gam)
# 
# gam.check(sal_gam)
# 
# plot(sal_gam)
# 
# temp_gam <- gam(Temp_logger ~ s(day_of_year, bs="cc"), data = daily_env)
# summary(temp_gam)
# 
# gam.check(temp_gam)
# 
# plot(temp_gam)
# 
# # fill NA in logger data in modelling data
# daily_move$Temp_predicted <- as.vector(predict(temp_gam, newdata=daily_move, se.fit=F))
# daily_move$Sal_predicted <- as.vector(predict(sal_gam, newdata=daily_move, se.fit=F))
# 
# daily_move <- daily_move %>% mutate(Temp_filled = case_when(!is.na(Temp_logger) ~ Temp_logger,
#                                                             T ~ Temp_predicted),
#                                     Sal_filled = case_when(!is.na(Sal_logger) ~ Sal_logger,
#                                                            T ~ Sal_predicted),
#                                     Tide_max = case_when(is.infinite(Tide_max) ~ NA_integer_,
#                                                          T ~ Tide_max))
# 
# summary(daily_move)
# 
# 
# ### Wind speed from BARRA
# wind_dat <- read_csv("Dusky_Env_Analysis/Data/Wind Data/Combined files/Combined U V speed direction data.csv") %>%
#   rename(Array = Estuary) %>% group_by(Array) %>%
#   mutate(wind_2_day = rollmean(Speed_km_hr, k = 2, align = "right", fill= NA))
# 
# daily_move <- daily_move %>% left_join(wind_dat)




daily_move <- read_csv("VPS - Hayden/Binary Movement data for modelling.csv")

table(daily_move$Array, daily_move$Movement)

daily_move <- daily_move %>% mutate(Array = as.factor(as.character(Array)),
                                    ID = as.factor(as.character(ID)),
                                    days_since_start = as.numeric(Date - min(Date)),
                                    size_mm = `Fork Length (mm)`,
                                    MonthF = as.character(Month)) %>%
  group_by(Array) %>% mutate(scale_log_flow = scale(log_flow)) %>% ungroup()# %>%
  #mutate(Speed_km_hr = wind_2_day)

## variables:
## Array, lunar,
## Temp_filled (water temp averaged from 2 loggers in each estuary, missing data filled using a gam from day of year, r2 = 0.92),
## Sal_filled (water temp averaged from 2 loggers in each estuary, missing data filled using a gam fromriver inflow r2 = 0.85),
# size_mm (Fish fork length), Sex (correlated to size so dropped),
# Tide_max (max tidal height of day) - some missing data,
# ID (unique fish id),
# day14_flow (2 day rolling average of river inflow)
# wind_2_day (2 day rolling average of wind speed km_hr)

#cor.test(log(daily_move$day14_flow), daily_move$Sal_filled) # highly correlated so removed flow

hist(log(daily_move$wind_2_day))

g3 <- gam(Movement ~ #Array+
            s(Month, bs= "cc")+
            s(Month, Array, bs="fs") + # 
            s(lunar, bs="cc")+ #s(day14_flow, by = Array)+
            #s(Temp_filled, by= Array) +
            s(scale_log_flow)+
            s(scale_log_flow, Array, bs="fs")+ # estuary specific effects - possibly overkill
            # s(Tide_max)+
            s(Speed_km_hr)+
            s(Speed_km_hr, Array, bs="fs")+
            #te(Direction, Speed_km_hr, bs = c("cc", "tp"), by = Array) +
  #          s(log(Speed_km_hr))+
            s(ID, bs="re") , data=daily_move, family="binomial", select = T,
            method= "REML")

AIC(g3)

concurvity(g3) # potential concurvity issue with ID and salinity in Shoalhaven...

#plot(g3)
gam.check(g3) # looks mostly ok
acf(residuals(g3)) # no major temporal autocorrelation

source("Dusky_Env_Analysis/Scripts/dharma-to-gg_fn.R")

plots <- DHARMa_plot(g3)
plot_a <- plots[[1]]
plot_a

plot_b <- plots[[2]]
plot_b

library(patchwork)
plot_a + plot_b
ggsave("Dusky_Env_Analysis/Plots/model checks/Binary Movement model residuals.png", dpi=300, width = 21, height=14.8, units="cm")

### Check original DHARMa
#resids <- simulateResiduals(g3)
#plot(resids) # looks good to me

summary(g3)
appraise(g3)
gratia::draw(g3, fun = inv_link(g3), continuous_fill = ggplot2::scale_fill_continuous(type="viridis", limits=c(0,0.7))) + scale_colour_viridis_c()
performance::r2(g3)

#ggsave("VPS - Hayden/Binary gam wind direction.png", width=28, height=21, dpi=300, units="cm")

### Manual predictions
new_data <- expand.grid("Month" = seq(1,12,0.1),
                       "scale_log_flow" = median(daily_move$scale_log_flow, na.rm = T),
                       "Array" = unique(daily_move$Array),
                       "lunar" = median(daily_move$lunar, na.rm = T),
                       "Speed_km_hr" = median(daily_move$Speed_km_hr, na.rm = T),
                       "ID" = daily_move$ID[1])

new_dat_preds <- predict(g3, newdata = new_data, exclude = "s(ID)" , se.fit = T,type = "response")

new_data$fit <- new_dat_preds$fit
new_data$se <- new_dat_preds$se.fit

p4 <- ggplot(new_data, aes(Month, fit)) + geom_line() + facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1) +
  geom_rug(data=daily_move, aes(x=Month), inherit.aes = F) +
  scale_x_continuous(breaks=seq(2,12,2))

p4

# scale_log_flow
limits <- daily_move %>% group_by(Array) %>%
  summarise(min_flow = min(scale_log_flow, na.rm=T),
            max_flow = max(scale_log_flow, na.rm=T))

new_data2 <- expand.grid("Month" = median(daily_move$Month),
                        "scale_log_flow" = seq(min(daily_move$scale_log_flow, na.rm=T), max(daily_move$log_flow, na.rm=T),0.1),
                        "Array" = unique(daily_move$Array),
                        "lunar" = median(daily_move$lunar, na.rm = T),
                        "Speed_km_hr" = median(daily_move$Speed_km_hr, na.rm = T),
                        "ID" = daily_move$ID[1]) %>% left_join(limits) %>%
  filter(scale_log_flow <= max_flow) %>% filter(scale_log_flow >= min_flow)

new_dat_preds2 <- predict(g3, newdata = new_data2, exclude = "s(ID)" , se.fit = T,type = "response")

new_data2$fit <- new_dat_preds2$fit
new_data2$se <- new_dat_preds2$se.fit

p1 <- ggplot(new_data2, aes((scale_log_flow), fit)) + geom_line() + facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)+
  geom_rug(data=daily_move, aes(x=scale_log_flow), inherit.aes = F)

# lunar
# limits <- daily_move %>% group_by(Array) %>%
#   summarise(min_flow = min(log_flow, na.rm=T),
#             max_flow = max(log_flow, na.rm=T))

new_data3 <- expand.grid("Month" = median(daily_move$Month),
                         "scale_log_flow" = median(daily_move$scale_log_flow, na.rm=T),
                         "Array" = "Clyde River",
                         "lunar" = seq(min(daily_move$lunar), max(daily_move$lunar), 0.1),
                         "Speed_km_hr" = median(daily_move$Speed_km_hr, na.rm = T),
                         "ID" = daily_move$ID[1]) %>% left_join(limits)

new_dat_preds3 <- predict(g3, newdata = new_data3, exclude = "s(ID)" , se.fit = T,type = "response")

new_data3$fit <- new_dat_preds3$fit
new_data3$se <- new_dat_preds3$se.fit

p2 <- ggplot(new_data3, aes(lunar, fit)) + geom_line() + # facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)+
  geom_rug(data=daily_move, aes(x=lunar), inherit.aes = F)+
  scale_x_continuous(breaks = c(0,pi/2,pi,(3*pi)/2), 
                     labels = c("New","1st Quarter","Full","3rd Quarter"))
p2

# wind_speed
limits2 <- daily_move %>% group_by(Array) %>%
  summarise(min_speed = min(Speed_km_hr, na.rm=T),
            max_speed = max(Speed_km_hr, na.rm=T))

new_data4 <- expand.grid("Month" = median(daily_move$Month),
                         "scale_log_flow" = median(daily_move$scale_log_flow, na.rm=T),
                         "Array" = unique(daily_move$Array),
                         "Speed_km_hr" = seq(min(daily_move$Speed_km_hr), max(daily_move$Speed_km_hr), 0.1),
                         "lunar" = median(daily_move$lunar, na.rm = T),
                         "ID" = daily_move$ID[1]) %>% left_join(limits2) %>%
  filter(Speed_km_hr <= max_speed) %>% filter(Speed_km_hr >= min_speed)

new_dat_preds4 <- predict(g3, newdata = new_data4, exclude = "s(ID)" , se.fit = T,type = "response")

new_data4$fit <- new_dat_preds4$fit
new_data4$se <- new_dat_preds4$se.fit

p3 <- ggplot(new_data4, aes(Speed_km_hr, fit)) + geom_line() + facet_wrap(~Array) +
  geom_ribbon(aes(ymin=fit-se, ymax = fit+se), alpha=0.1)+
  geom_rug(data=daily_move, aes(x=Speed_km_hr), inherit.aes = F)

library(patchwork)
p1/p3/p4/p2 & theme_bw() & ylab("P(Movement)")

ggsave("VPS - Hayden/Binary movement model outcomes.png", dpi=300, width = 21, height=28, units="cm")
ggsave("VPS - Hayden/Binary movement model outcomes.pdf", dpi=300, width = 21, height=28, units="cm")
