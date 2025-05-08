# rainfall data download
#Jeffrey, S.J., Carter, J.O., Moodie, K.B. and Beswick, A.R. (2001). Using spatial interpolation to construct a comprehensive archive of Australian climate data, Environmental Modelling and Software, Vol 16/4, pp 309-330. DOI: 10.1016/S1364-8152(01)00008-1.  
#

# # Download  files
# library(curl)
# 
# url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/2015.monthly_rain.nc"
# tmp <- "test monthly raindata.nc"
# curl_download(url, tmp)
# 
# for(i in 1989:2023){
#   url <- paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/",as.character(i),".monthly_rain.nc")
#   tmp <- paste0("Data/Rain Data/",as.character(i)," monthly raindata.nc")
#   curl_download(url, tmp) 
# }
# 
# 
# 



# Catchment boundaries

library(tidyverse)
library(sf)
library(raster)

catchments <- sf::read_sf("Data/EstuaryDrainageCatchment/Data/EstuaryDrainageCatchments.shp")
plot(catchments)

ests_keep <- c("WALLIS LAKE", "TUGGERAH LAKE", "LAKE ILLAWARRA")

ests <- catchments %>% filter(CatchmentN %in% ests_keep)

plot(ests)

# get the actual monthly rain

rain_files <- list.files("Data/Rain Data/", full.names = T)

catchments <- sf::read_sf("Data/EstuaryDrainageCatchment/Data/EstuaryDrainageCatchments.shp")

## Test extract values for Estuary
# xx <- extract(mydata[[1]], northernMDB) # different to above - this one is correct
# yy <- mean(unlist(xx))
year_seq <- seq(1989:2023) # not sure why this doesn't work

extracted_data <- data.frame()

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(rain_files), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=") 

for(e in 1:length(ests_keep)){

  ests <- catchments %>% filter(CatchmentN == ests_keep[e])
  
  
for(i in 1:length(rain_files)){
  mydata <- stack(rain_files[i])
  temp_data <- data.frame(Rainfall = rep(as.numeric(NA),12), 
                          Year = rep(as.numeric(NA),12),
                          Month= rep(as.numeric(NA),12),
                          Estuary = rep(ests_keep[e],12))
  for(k in 1:nlayers(mydata)){
    xx <- raster::extract(mydata[[k]], ests)
    yy <- mean(unlist(xx))
    temp_data$Rainfall[k] <- yy
    temp_data$Month[k] <- k
    temp_data$year[k] <- year_seq[i]
  }
  extracted_data <- extracted_data %>% bind_rows(temp_data)
  setTxtProgressBar(pb, i)
}
}
extracted_data$Year = 1988+extracted_data$year

write_csv(extracted_data, "Data/Monthly Rain Estuaries Drainage.csv")

