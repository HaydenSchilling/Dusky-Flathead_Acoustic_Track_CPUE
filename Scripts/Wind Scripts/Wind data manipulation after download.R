# Combine Downloaded BARRA Data
library(tidyverse)
library(lubridate)

# step 1 - combine all u viles

file_list <- list.files("Data/Wind Data/", pattern = "u_wind", full.names = T)

mydata_list <- list()
for (i in (1:length(file_list))){
  mydata_list[[i]] <- read_csv(file_list[i])
}

mydata <- bind_rows(mydata_list)
write_csv(mydata, "Data/Wind Data/Combined files/Full u wind.csv")

# now v wind
file_list <- list.files("Data/Wind Data/", pattern = "v_wind", full.names = T)

mydata_list <- list()
for (i in (1:length(file_list))){
  mydata_list[[i]] <- read_csv(file_list[i])
}

mydata <- bind_rows(mydata_list)
write_csv(mydata, "Data/Wind Data/Combined files/Full v wind.csv")


### Merge files
### First make long format
u_dat <- read_csv("Data/Wind Data/Combined files/Full u wind.csv")
u_dat <- u_dat %>% pivot_longer(1:9, names_to = "Estuary", values_to = "u")

v_dat <- read_csv("Data/Wind Data/Combined files/Full v wind.csv")
v_dat <- v_dat %>% pivot_longer(1:9, names_to = "Estuary", values_to = "v")

# Merge and fix date
uv_dat <- u_dat %>% left_join(v_dat) %>% mutate(Date = ymd(str_remove(Date, pattern="X")))
uv_dat <- uv_dat %>% mutate(Speed_m_s = sqrt((u^2)+(v^2)))

hist(uv_dat$Speed_m_s)

write_csv(uv_dat, "Data/Wind Data/Combined files/Combined U V data.csv")
