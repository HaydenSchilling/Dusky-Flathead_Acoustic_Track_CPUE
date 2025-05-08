# movement plots

library(tidyverse)

#library(remotes)
#install_github("YuriNiella/RSP", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
#install.packages("fastmap")
#library(RSP)
#remove.packages("RSP")

#browseVignettes("RSP")

library(actel)
library(janitor)


# run actel

options(actel.auto.convert.codespaces = FALSE)

setwd("VPS - Hayden/Full CEFT Download/actel/")
setwd("../actel/")

#xx <- explore(tz = 'Australia/Sydney', report = T)

xxS <- explore(tz = 'Australia/Sydney', report = T, max.interval = 240)

bio <- xxS$bio %>% clean_names() %>% rename(tag_id = transmitter) %>% select(-signal)

xx2 <- xxS$detections %>% bind_rows(.id = "TagID") %>% janitor::clean_names() %>% left_join(bio)


data_sum <- xx2 %>% mutate(Date = as.Date(timestamp)) %>% group_by(array, tag_id, measurement_value, release_date) %>%
  summarise(first_detect = as.Date(min(timestamp)), last_detect = as.Date(max(timestamp)),
            days_detected = n_distinct(Date)) %>%
  mutate(release_date = as.Date(release_date))
write_csv(data_sum, "Shoalhaven detection bio summary.csv")


# load estuary dists
est_dists <- read_csv("../../../Dusky_Env_Analysis/Scripts/Dan code/shoalhaven-distances_final.csv") %>%
  select(-section, -array)

xx3 <- xx2 %>% left_join(est_dists) %>% filter(station_name != "Clyde_033")

duds <- xx3 %>% filter((tag_id == "A69-1303-55318" & distance_to_mouth>30000))

xx3 <- xx3 %>% anti_join(duds)

#test <- xx3 %>% filter(tag_id == "A69-9002-1345")

ggplot(xx3, aes(timestamp, distance_to_mouth/1000)) + geom_point(size=0.5) +
  geom_line(size=0.5) + facet_wrap(~tag_id, scales = "free_x") +
  theme_bw()+ theme(strip.text = element_text(size=8))+
  labs(x="Date", y = "Distance from Estuary Mouth (km)")

ggsave("../../../Dusky_Env_Analysis/Plots/Shoalhaven movement.png", dpi=300, width =14.8*2, height=21, units="cm")
