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

setwd("VPS - Hayden/Full CEFT Download/Clyde/actel/")
xx <- explore(tz = 'Australia/Sydney', report = T, max.interval = 240)

hh <- getTimes(input = xx, move.type = "array", event.type = "arrival", n.events = "all")
times <- timesToCircular(hh)
plotTimes(times)
plotTimes(times, circular.scale = "area", mean.dash = FALSE,
          file = "Clyde detection time.pdf", height = 4, width=4)

#plotMoves(xx, tags = "A69-9002-1345")

#Detection_plot_dat

bio <- xx$bio %>% clean_names() %>% rename(tag_id = transmitter) %>% select(-signal)

xx2 <- xx$detections %>% bind_rows(.id = "TagID") %>% janitor::clean_names() %>% left_join(bio)


data_sum <- xx2 %>% mutate(Date = as.Date(timestamp)) %>% group_by(array, tag_id, measurement_value, release_date) %>%
  summarise(first_detect = as.Date(min(timestamp)), last_detect = as.Date(max(timestamp)),
            days_detected = n_distinct(Date)) %>%
  mutate(release_date = as.Date(release_date))
write_csv(data_sum, "Clyde detection bio summary.csv")

# load estuary dists
est_dists <- read_csv("../../../../Dusky_Env_Analysis/Scripts/Dan code/clyde-distances_final.csv") %>%
  select(-section, -array)

xx3 <- xx2 %>% left_join(est_dists) %>% filter(station_name != "Clyde_033")

duds <- xx3 %>% filter((tag_id == "A69-1303-198" & distance_to_mouth>30000)|
                        (tag_id == "A69-1303-64275"& distance_to_mouth >30000)|
                         (tag_id == "A69-9001-32481"& distance_to_mouth >30000)|
                         (tag_id == "A69-9001-32490"& distance_to_mouth >30000)|
                         (tag_id == "A69-9001-32499"& distance_to_mouth >30000))

xx3 <- xx3 %>% anti_join(duds)

test <- xx3 %>% filter(tag_id == "A69-9002-1345")

ggplot(xx3, aes(timestamp, distance_to_mouth/1000)) + geom_point(size=0.5) +
  geom_line(size=0.5) + facet_wrap(~tag_id, scales = "free_x") +
  theme_bw()+ theme(strip.text = element_text(size=8))+
  labs(x="Date", y = "Distance from Estuary Mouth (km)")

ggsave("../../../../Dusky_Env_Analysis/Plots/Clyde movement.png", dpi=300, width =14.8*2, height=21, units="cm")
