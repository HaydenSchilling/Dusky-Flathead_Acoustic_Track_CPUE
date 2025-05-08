### Prepare data for analysis

library(tidyverse)
library(lubridate)

mydata <- read_tsv("Data/Flathead Net all data.txt")

Catch_by_estuary <- mydata %>% filter(CommonName == "Dusky Flathead") %>% group_by(EstuaryName) %>%
  summarise(total_catch = sum(WholeWtFishOnlineKg, na.rm=T))
Catch_by_estuary <- Catch_by_estuary %>%
  mutate(Percent = total_catch/sum(Catch_by_estuary$total_catch)*100)

# Keep only >1%
Estuaries <- c("Wallis Lake", "Tuggerah Lakes", "Lake Illawarra")

mydata <- mydata %>% filter(EstuaryName %in% Estuaries) %>% filter(FiscalYear != "2023/24") %>%
  filter(FiscalYear != "2008/09")

table(mydata$FiscalYear)
table(mydata$FishingMethod)
table(mydata$BoatName)
table(mydata$EffortQty)
hist(mydata$EffortQty)
table(mydata$Month)

mydata <- mydata %>% filter(Month >2) %>% filter(Month<12)

# lunar calculations
library(lunar)
mydata$EventDate <- lubridate::dmy_hms(mydata$EventDate)
mydata$lunar_phase <- lunar.phase(as.Date(mydata$EventDate))

hist(mydata$lunar_phase)

test2 <- mydata %>% filter(EventDate == as.Date("2013-05-08")) %>% filter(FishingBusinessID==1011)

### NEED TO GO TO WIDE FORMAT - 1 row per "sample"
mydata_wide <- mydata %>%
  select(-ScientificName, -SpeciesCode, -CAABCode, -WholeWtFishOnlineKg, -WholeWtComCatchKg, -CatchAmount) %>%
  pivot_wider(names_from = CommonName, values_from=  CatchWtKg, values_fill = 0)

# ## filter out dodgy ones (this could be improved)
# dodgy <- mydata_wide %>% filter(`Spanner Crab` >0)

table(mydata$CommonName)

hist(mydata_wide$`Dusky Flathead`)
mydata_wide$Response <- mydata_wide$`Dusky Flathead`


mydata_wide <- mydata_wide %>% filter(!is.na(`AA-Effort Only`)) ### This is important as if not removed you end up with duplicates in the wide format

test <- mydata_wide %>% filter(EventDate == as.Date("2013-05-08")) %>% filter(FishingBusinessID==1011)


catch_only <- mydata_wide[,50:171]
mydata_wide$Total_Catch <- rowSums(catch_only)
mydata_wide$Proportion_Flathead <- mydata_wide$Response/mydata_wide$Total_Catch

hist(mydata_wide$Proportion_Flathead)

dat_month <- mydata_wide %>% group_by(EstuaryName, AuthorisedFisherID, FiscalYear, Month, CalYear) %>%
  summarise(Response = sum(Response, na.rm=T), Total_Catch = sum(Total_Catch, na.rm = T), total_bream = sum(`Yellowfin Bream`, na.rm=T),
            Effort_days = n_distinct(LogSheetEventID)) %>% mutate(Proportion_Flathead = Response/Total_Catch) %>%
  filter(Total_Catch>0)

hist(dat_month$Proportion_Flathead)


write_csv(dat_month, "Data/Flathead net key estuary wide Monthly.csv")

### Wind Data
wind_dat <- read_csv("Data/Wind Data/Combined files/Combined U V data.csv") %>%
  mutate(Estuary = case_when(Estuary == "Tuggerah Lake" ~ "Tuggerah Lakes",
                             T ~ Estuary),
         CalYear = year(Date),
         Month = month(Date))

mydata_wide <- read_csv("Data/Flathead net key estuary wide Monthly.csv")

mydata_wide$Wind_speed <- -999
mydata_wide$Date <- as.Date(paste0(mydata_wide$CalYear,"-",mydata_wide$Month,"-15"))
#duds <- mydata_wide %>% filter(is.na(Date))

pb <- txtProgressBar(min = 0, max = nrow(mydata_wide), style = 3)
for(i in 1:nrow(mydata_wide)){
  temp_wind <- wind_dat %>% filter(Estuary == mydata_wide$EstuaryName[i]) %>%
    filter(CalYear == mydata_wide$CalYear[i] & Month == mydata_wide$Month[i])
  mydata_wide$Wind_speed[i] <- mean(temp_wind$Speed_m_s, na.rm=T)
  setTxtProgressBar(pb, i)
}

table(mydata_wide$Effort_days)

mod_dat <- mydata_wide %>% drop_na(Effort_days) %>% filter(Effort_days<32) %>% #filter(Response <300) %>%
  drop_na(AuthorisedFisherID) %>%  rename(Estuary = EstuaryName)

#### Freshwater flow data
flow_dat1 <- read_csv("Data/Flow Data/Wallis Inflow data/Wallis Gauge Data.csv") %>% select(-Comments) %>%
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
write_csv(flow_dat, "Data/Flow Data/Combined estuary flow data.csv")

#match_flow <- function(fish_data, flow_data){}

### LOOP

# AIC_dat <- data.frame("Lag" = seq(1,42), "AIC" = rep(NA_integer_,42))
# pb = txtProgressBar(min = 1, max = 42, initial = 1)
#
# for(j in 1:42){

mod_dat$Mean_daily_Flow <- -999 # dummy value
mod_dat$Max_daily_Flow <- -999 # dummy value

pb <- txtProgressBar(min = 0, max = nrow(mod_dat), style = 3)
for(i in (1:nrow(mod_dat))){
  temp_dat <- flow_dat %>% filter(Estuary == mod_dat$Estuary[i]) %>%
    filter(Year == mod_dat$CalYear[i]) %>%
    filter(Month == mod_dat$Month[i])
  
  mod_dat$Mean_daily_Flow[i] <- mean(temp_dat$Mean, na.rm=T)
  mod_dat$Max_daily_Flow[i] <- max(temp_dat$Mean, na.rm=T)
  setTxtProgressBar(pb, i)
}

hist(mod_dat$Mean_daily_Flow)

mod_dat <- mod_dat %>% ungroup() %>% group_by(Estuary) %>%
  mutate(Flow_mean_scaled = scale(Mean_daily_Flow),
         Flow_max_scaled = scale(Max_daily_Flow)) %>% ungroup() %>%
  mutate(Estuary = as.factor(as.character(Estuary)))

cor.test(mod_dat$Flow_max_scaled, mod_dat$Flow_mean_scaled) # correlation of 0.94


### Add number of days since Day1 in each estuary

mod_dat <- mod_dat %>% ungroup() %>% group_by(Estuary) %>%
  mutate(Days_since_start = as.numeric(Date - min(Date))) %>% ungroup()

mod_dat$Flow_mean_scaled <- as.numeric(mod_dat$Flow_mean_scaled)
mod_dat$Flow_max_scaled <- as.numeric(mod_dat$Flow_max_scaled)



#### Add lagged flow
#### Make lagged recruitment

lag_flow <- flow_dat %>% mutate(FiscalYear_match = case_when(Month>6 ~ Year +1,
                                                             T ~ Year)) %>%
  filter(Month == 12 | Month <=3) %>%
  group_by(Estuary, FiscalYear_match) %>%
  summarise(recruit_flow = mean(Mean, na.rm=T))

mod_dat$FiscalYear_old <- mod_dat$FiscalYear
mod_dat$FiscalYear_ending <- as.numeric(substr(mod_dat$FiscalYear, 1, 4))+1
mod_dat$FiscalYear_match <- mod_dat$FiscalYear_ending - 4

tt <- mod_dat %>% select(FiscalYear, FiscalYear_ending, FiscalYear_match)

mod_dat <- mod_dat %>% left_join(lag_flow)



mod_dat <- mod_dat %>% ungroup() %>% group_by(Estuary) %>%
  mutate(recruit_flow_scaled = as.numeric(scale(recruit_flow))) %>% ungroup()



### Add recruitment winds
winds_offshoreshore <- read_csv("Data/Wind Data/Combined files/Combined U V speed direction data final 45 degree.csv") %>%
  mutate(Estuary = case_when(Estuary == "Tuggerah Lake" ~ "Tuggerah Lakes",
                             T ~ Estuary))

winds_offshoreshore <- winds_offshoreshore %>% mutate(FiscalYear_match = case_when(Month>6 ~ Year +1,
                                                                                   T ~ Year)) %>%
  filter(Month == 12 | Month <=3) %>%
  group_by(Estuary, FiscalYear_match) %>%
  summarise(Offshore_winds = sum(displacement, na.rm=T))


winds_onshoreshore <- read_csv("Data/Wind Data/Combined files/Combined U V speed direction data final 135 degree.csv") %>%
  mutate(Estuary = case_when(Estuary == "Tuggerah Lake" ~ "Tuggerah Lakes",
                             T ~ Estuary))

winds_onshoreshore <- winds_onshoreshore %>% mutate(FiscalYear_match = case_when(Month>6 ~ Year +1,
                                                                                 T ~ Year)) %>%
  filter(Month == 12 | Month <=3) %>%
  group_by(Estuary, FiscalYear_match) %>%
  summarise(Onshore_winds = sum(displacement, na.rm=T))


mod_dat <- mod_dat %>% left_join(winds_offshoreshore) %>% left_join(winds_onshoreshore)

hist(mod_dat$Onshore_winds)
hist(mod_dat$Offshore_winds)

mod_dat <- mod_dat %>% ungroup()
mod_dat$Onshore_winds_scaled <- as.numeric(scale(mod_dat$Onshore_winds))
mod_dat$Offshore_winds_scaled <- as.numeric(scale(mod_dat$Offshore_winds))

write_csv(mod_dat, "Data/Flathead Env data to model Monthly.csv")
