### Array plots for manuscript
library(tidyverse)
library(ggspatial)
library(janitor)
library(sf)

# Open the array files including locations of each receiver
shoalhaven <- read_csv("Dusky_Env_Analysis/Scripts/Dan code/shoalhaven-distances.csv") %>% clean_names()
clyde <- read_csv("Dusky_Env_Analysis/Scripts/Dan code/clyde-distances.csv") %>% clean_names()

# Open the coastline shapefile
coast <- st_read("Dusky_Env_Analysis/Scripts/Dan code/Coastaline_WGS84_MGA56_NSWOEH.shp") %>%
  mutate(Land = case_when(ID_NAME == "Water" ~ "Water", # make only land and water
                          T ~ "Land"))

# Load locations of estuaries
estuary_locations <- read_csv("Dusky_Env_Analysis/Data/Estuary Locations.csv")

# Shoalhaven array plot
p3 <- ggplot() +
  geom_sf(data = coast, aes(fill=Land)) + # Add coastline
  geom_point(data = shoalhaven, aes(x = longitude, y = latitude)) + # add receivers
  coord_sf(xlim = c(min(shoalhaven$longitude), max(shoalhaven$longitude)), 
           ylim = c(min(shoalhaven$latitude)-0.01, max(shoalhaven$latitude))) + # add map limits
  scale_fill_manual(values=c("grey80", "white"))+ # set colours of land/water
  annotation_scale()+ # add scale bar
  theme_bw()+ # add some default formatting
  labs(y="Latitude", x = "Longitude")+ # axis labels
  theme(legend.position = "none",
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(size=12, face="bold"))+ # custom formatting
  ggtitle("Shoalhaven River") # title
p3 # view plot


# Clyde River Plot
p4 <- ggplot() +
  geom_sf(data = coast, aes(fill=Land)) + # load land/water
  geom_point(data = clyde, aes(x = longitude, y = latitude)) + # add receiver points
  coord_sf(xlim = c(min(clyde$longitude), max(clyde$longitude)+0.01),
           ylim = c(min(clyde$latitude), max(clyde$latitude)))+ # map limits
  scale_fill_manual(values=c("grey80", "white"))+ # land and water colour
  scale_x_continuous(breaks=seq(150.12,150.24,0.06))+ # axis label breaks
  annotation_scale()+ # scale bar
  theme_bw()+ # some default formatting
  labs(y="Latitude", x = "Longitude")+ # axis labels
  theme(legend.position = "none", # custom formatting
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(size=12, face="bold"))+
  ggtitle("Clyde River") # title
p4 # view plot

# make shapefile of only land (no water)
coast2 <- coast %>% filter(Land == "Land")

# Rnaturalearth is a way to get some shapefiles
library("rnaturalearth")
library("rnaturalearthdata")

states <- ne_states(country = 'australia', returnclass = "sf",) # load Australia with states

# NSW Coastline
library(ggrepel)
p1 <- ggplot() +
  geom_sf(data = coast2, aes(fill=Land)) + # add coastline
  geom_point(data = estuary_locations, aes(x = longitude, y = latitude), # add estuary locations
             shape=22, fill="blue", col="white", size=2) +
  geom_text_repel(data = estuary_locations, aes(x = longitude, y = latitude, label=Estuary))+ # add estuary labels next to points
  coord_sf(xlim = c(min(estuary_locations$longitude)+0.05, 
                        max(estuary_locations$longitude)+0.5), # map limits
           ylim = c(min(estuary_locations$latitude)-0.4, max(estuary_locations$latitude)))+
  scale_fill_manual(values=c("grey80", "white"))+ # set land/water colours
  scale_x_continuous(breaks=seq(151,154,1))+ # set axis 
  annotation_scale()+ # scale bar
  theme_classic()+ # some default formatting
  labs(y="Latitude", x = "Longitude")+ # axis labels
  theme(legend.position = "none", # custom formatting
        panel.border = element_rect(colour = "black", fill=NA),
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(size=12, face="bold"))#+
  #ggtitle("Clyde River")
p1

### Inset map

p2 <- ggplot() +
  layer_spatial(data=states,colour="black", lwd=0.2, fill="khaki3")+ # load Australia with states
  # layer_spatial(WRPA_all,col="black", fill = "grey90" ,
  #               alpha=0.9, size=0.5)+
  geom_rect(col="black", aes(xmin=min(estuary_locations$longitude)+0.05, # add rectangle matching NSW map limits
                             xmax= max(estuary_locations$longitude)+0.5, 
                             ymin= min(estuary_locations$latitude)-0.4,
                             ymax = max(estuary_locations$latitude)),
            fill=NA)+
  theme_classic()+ # some default formatting
  ylab("Latitude") + xlab("Longitude")+ # axis labels
  theme(legend.position = "right", # custom formatting
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(colour="black", size=10),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())
p2 # view plot

### Merge all plots together

library(patchwork)
pX <-  (p1+p4)/p3 # p1 and p4 side by side both on top of p3
pX


### Overlay inset on top of p1
PD <- p1 + inset_element(p2, left = 0, bottom = 0.7, right = 0.6, top = 1)


(PD+p4)/p3
#ggsave("Dusky_Env_Analysis/Plots/Map plot.pdf", dpi=300, width=21, height=25, units="cm") # minor manual alterations made
