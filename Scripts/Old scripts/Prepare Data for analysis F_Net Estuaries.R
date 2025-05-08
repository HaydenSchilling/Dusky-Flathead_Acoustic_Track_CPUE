### Prepare data for analysis - shoalhaven and clyde river

library(tidyverse)
library(lubridate)

mydata <- read_tsv("Data/Flathead Net all data.txt")

table(mydata$EstuaryName, mydata$FishingMethod)
table(mydata$FiscalYear)
table(mydata$FishingMethod)

mydata <- mydata %>% filter(EstuaryName %in% c("Wallis Lake", "Tuggerah Lakes", "Lake Illawarra")) %>%
  filter(FishingMethod == "Flathead net") %>% filter(!FiscalYear %in% c("2024/25","2008/09","2023/24")) %>%
  filter(Month >2 & Month <12) %>% filter(EstuaryName == "Tuggerah Lakes" |
                                            EstuaryName == "Wallis Lake" |
                                            (EstuaryName == "Lake Illawarra" & Month>5 & Month <9))

table(mydata$EstuaryName, mydata$FiscalYear)
table(mydata$FiscalYear)
table(mydata$EstuaryName, mydata$Month)

# monthly proportion of flathead
monthly_sum <- mydata %>% group_by(EstuaryName, CalYear, Month) %>% summarise(total_catch = sum(WholeWtFishOnlineKg, na.rm = T))

fh_month_tot <- mydata %>% filter(CommonName == "Dusky Flathead") %>%
  group_by(EstuaryName, CalYear, Month) %>% summarise(total_flathead = sum(WholeWtFishOnlineKg, na.rm = T))


combo <- fh_month_tot %>% left_join(monthly_sum) %>%
  mutate(Prop_flathead = total_flathead/total_catch*100) %>%
  ungroup() %>% group_by(EstuaryName, Month) %>% summarise(Prop_flat_mean = mean(Prop_flathead),
                                              Prop_flat_sd = sd(Prop_flathead),
                                              n=n(),
                                              Prop_flathead_se = Prop_flat_sd/(sqrt(n)))

ggplot(combo, aes(Month, Prop_flat_mean)) + geom_col() + facet_wrap(~EstuaryName)+
  geom_errorbar(aes(ymax = Prop_flat_mean+Prop_flathead_se, ymin= Prop_flat_mean-Prop_flathead_se)) +
  ylab("Percent Catch Flathead")

# Catch_by_estuary <- mydata %>% filter(CommonName == "Dusky Flathead") %>% group_by(EstuaryName) %>%
#   summarise(total_catch = sum(WholeWtFishOnlineKg, na.rm=T))
# Catch_by_estuary <- Catch_by_estuary %>%
#   mutate(Percent = total_catch/sum(Catch_by_estuary$total_catch)*100)

#mydata <- mydata %>% filter(between(Month,6,9))  %>% # only months when overnight setting is allowed
#  filter(CalYear != 2024) # possible incomplete reporting year

table(mydata$FiscalYear)
table(mydata$FishingMethod)
table(mydata$BoatName)
table(mydata$EffortQty)
hist(mydata$EffortQty)
table(mydata$Month)

mydata <- mydata %>% filter(EffortQty >100 & EffortQty <4000) # remove unreasonable efforts (too small)

# lunar calculations
library(lunar)
mydata$EventDate <- lubridate::dmy_hms(mydata$EventDate)
mydata$lunar_phase <- lunar.phase(as.Date(mydata$EventDate))

hist(mydata$lunar_phase)

### NEED TO GO TO WIDE FORMAT - 1 row per "sample"
mydata_wide <- mydata %>%
  dplyr::select(-ScientificName, -SpeciesCode, -CAABCode, -CatchWtKg, -WholeWtComCatchKg, -CatchAmount) %>%
  pivot_wider(names_from = CommonName, values_from= WholeWtFishOnlineKg, values_fill = 0)

## filter out dodgy ones (this could be improved)
# need to merge some split records - maybe just drop the 2% dodgy...
n <- mydata_wide %>% 
  group_by(EstuaryName, AuthorisedFisherID, EventDate) %>% 
  summarise(n = n()) %>%
  filter(n > 1) # 837 records dodgy = 837/23337*100 = 3.6%
n <- n %>% select(-n)

mydata_wide <- mydata_wide %>% anti_join(n)

#xx <- mydata_wide %>% filter(EventDate == as.Date("2013-08-16"))

# remove dodgy 17 - easier than fixing
# dodgy <- mydata_wide %>% filter(`Spanner Crab` >0)

table(mydata$CommonName)

hist(mydata_wide$`Dusky Flathead`)
mydata_wide$Response <- mydata_wide$`Dusky Flathead`


#mydata_wide <- mydata_wide %>% filter(!is.na(`AA-Effort Only`)) ### This is important as if not removed you end up with duplicates in the wide format

catch_only <- mydata_wide[,50:165]
mydata_wide$Total_Catch <- rowSums(catch_only, na.rm = T)
mydata_wide$Proportion_Flathead <- mydata_wide$Response/mydata_wide$Total_Catch

hist(mydata_wide$Proportion_Flathead)
# no_catch <- mydata_wide %>% filter(Total_Catch == 0) # None
mydata_wide <- mydata_wide %>% filter(Total_Catch > 0)

write_csv(mydata_wide, "Data/Flathead Net wide.csv")

# ### Wind Data
# 
# wind_dat <- read_csv("Data/Wind Data/Combined files/Combined U V data.csv") %>%
#   mutate(Estuary = case_when(Estuary == "Tuggerah Lake" ~ "Tuggerah Lakes",
#                              T ~ Estuary))
# 
# mydata_wide$Wind_speed <- -999
# mydata_wide$Wind_speed_lag1 <- -999
# mydata_wide$Date <- as.Date(mydata_wide$EventDate)
# #duds <- mydata_wide %>% filter(is.na(Date))
# 
# pb <- txtProgressBar(min = 0, max = nrow(mydata_wide), style = 3)
# for(i in 1:nrow(mydata_wide)){
#   temp_wind <- wind_dat %>% filter(Estuary == mydata_wide$EstuaryName[i]) %>%
#     filter(Date == mydata_wide$Date[i])
#   temp_wind_lag1 <- wind_dat %>% filter(Estuary == mydata_wide$EstuaryName[i]) %>%
#     filter(Date == mydata_wide$Date[i]-1)
#   mydata_wide$Wind_speed[i] <- temp_wind$Speed_m_s[1]
#   mydata_wide$Wind_speed_lag1[i] <- temp_wind_lag1$Speed_m_s[1]
#   setTxtProgressBar(pb, i)
# }
# 
# mod_dat <- mydata_wide %>% drop_na(EffortQty) %>% filter(EffortQty>400) %>% #filter(Response <300) %>%
#   drop_na(AuthorisedFisherID) %>% mutate(ratio = Response/EffortQty) %>%
#   rename(Estuary = EstuaryName)

mod_dat <- mydata_wide

#### Freshwater flow data
flow_dat1 <- read_csv("Data/Flow Data/Wallis Inflow data/Wallis Gauge Data.csv") %>% dplyr::select(-Comments)  %>%
  mutate(Date = as.Date(dmy_hm(Date)),
         Month = month(Date),
         Year = year(Date),
         Day = day(Date),
         Estuary = "Wallis Lake")

flow_dat2 <- read_csv("Data/Flow Data/Lake Illawarra inflow data/Illawarra Gauge Data.csv") %>% select(-Comments) %>%
  mutate(Date = as.Date(dmy_hm(Date)),
         Month = month(Date),
         Year = year(Date),
         Day = day(Date),
         Estuary = "Lake Illawarra") # Estuary = "Lake Illawarra, Wollongong")

flow_dat3 <- read_csv("Data/Flow Data/Tuggerah Inflow data/Tuggerah Inflow Data.csv") %>% select(-Comments) %>%
  mutate(Date = as.Date(dmy_hm(Date)),
         Month = month(Date),
         Year = year(Date),
         Day = day(Date),
         Estuary = "Tuggerah Lakes") # Estuary = "Tuggerah Lakes, Munmorah, Budgewoi"

flow_dat <- bind_rows(flow_dat1, flow_dat2, flow_dat3)
write_csv(flow_dat, "Data/Flow Data/Combined Flathead Net estuary flow data.csv")

#match_flow <- function(fish_data, flow_data){}

### LOOP

# AIC_dat <- data.frame("Lag" = seq(1,42), "AIC" = rep(NA_integer_,42))
# pb = txtProgressBar(min = 1, max = 42, initial = 1)
#
# for(j in 1:42){

mod_dat$EventDate <- as.Date(mod_dat$EventDate)
mod_dat$days_prior_14 <- mod_dat$EventDate-2
mod_dat$days_prior_1 <- mod_dat$EventDate-1

mod_dat$Mean_daily_Flow <- -999 # dummy value
mod_dat$Max_daily_Flow <- -999 # dummy value

pb <- txtProgressBar(min = 0, max = nrow(mod_dat), style = 3)
for(i in (1:nrow(mod_dat))){
  temp_dat <- flow_dat %>% filter(Estuary == mod_dat$EstuaryName[i]) %>%
    filter(Date>= mod_dat$days_prior_14[i]) %>%
    filter(Date <= mod_dat$days_prior_1[i])
  
  mod_dat$Mean_daily_Flow[i] <- mean(temp_dat$Mean, na.rm=T)
  mod_dat$Max_daily_Flow[i] <- max(temp_dat$Mean, na.rm=T)
  setTxtProgressBar(pb, i)
}

summary(mod_dat$Mean_daily_Flow)

hist(log(mod_dat$Mean_daily_Flow))

mod_dat <- mod_dat %>% ungroup() %>% group_by(EstuaryName) %>%
  mutate(#Flow_mean_scaled = scale(Mean_daily_Flow),
         #Flow_max_scaled = scale(Max_daily_Flow),
    log_flow = log(Mean_daily_Flow)) %>% ungroup() #%>%
 # mutate(Estuary = as.factor(as.character(Estuary)))



### Add number of days since Day1 in each estuary

mod_dat <- mod_dat %>% ungroup() %>% group_by(EstuaryName) %>%
  mutate(Days_since_start = as.numeric(EventDate - min(EventDate))) %>% ungroup()

#mod_dat$Flow_mean_scaled <- as.numeric(mod_dat$Flow_mean_scaled)
#mod_dat$Flow_max_scaled <- as.numeric(mod_dat$Flow_max_scaled)



#### Add lagged flow
#### Make lagged recruitment

# lag_flow <- flow_dat %>% mutate(FiscalYear_match = case_when(Month>6 ~ Year +1,
#                                                              T ~ Year)) %>%
#   filter(Month == 12 | Month <=3) %>%
#   group_by(Estuary, FiscalYear_match) %>%
#   summarise(recruit_flow = mean(Mean, na.rm=T))
# 
# mod_dat$FiscalYear_old <- mod_dat$FiscalYear
# mod_dat$FiscalYear_ending <- as.numeric(substr(mod_dat$FiscalYear, 1, 4))+1
# mod_dat$FiscalYear_match <- mod_dat$FiscalYear_ending - 4
# 
# tt <- mod_dat %>% select(FiscalYear, FiscalYear_ending, FiscalYear_match)
# 
# mod_dat <- mod_dat %>% left_join(lag_flow)
# 
# 
# 
# mod_dat <- mod_dat %>% ungroup() %>% group_by(Estuary) %>%
#   mutate(recruit_flow_scaled = as.numeric(scale(recruit_flow))) %>% ungroup()
# 
# 
# 
### Add  winds
winds <- read_csv("Data/Wind Data/Combined files/Combined U V speed direction data.csv") %>%
  mutate(Estuary = case_when(Estuary == "Tuggerah Lake" ~ "Tuggerah Lakes",
                             T ~ Estuary))
#mod_dat <- mod_dat %>% mutate(Estuary = "Shoalhaven River")

mod_dat$EventDate <- as.Date(mod_dat$EventDate)
mod_dat$days_prior_14 <- mod_dat$EventDate-2
mod_dat$days_prior_1 <- mod_dat$EventDate-1

mod_dat$Mean_wind <- -999 # dummy value

pb <- txtProgressBar(min = 0, max = nrow(mod_dat), style = 3)
for(i in (1:nrow(mod_dat))){
  temp_dat <- winds %>% filter(Estuary == mod_dat$EstuaryName[i]) %>%
    filter(Date>= mod_dat$days_prior_14[i]) %>%
    filter(Date <= mod_dat$days_prior_1[i])
  
  mod_dat$Mean_wind[i] <- mean(temp_dat$Speed_km_hr, na.rm=T)
  setTxtProgressBar(pb, i)
}

hist(mod_dat$Mean_wind)
summary(mod_dat$Mean_wind)

# mod_dat <- mod_dat %>% ungroup()
# mod_dat$Onshore_winds_scaled <- as.numeric(scale(mod_dat$Onshore_winds))
# mod_dat$Offshore_winds_scaled <- as.numeric(scale(mod_dat$Offshore_winds))

write_csv(mod_dat, "Data/Flathead Env data to model Flathead Net Commercial Daily.csv")
