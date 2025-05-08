# HAYDEN BARRA 

Surface eastward wind (uas)
https://dapds00.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/day/uas/catalog.html

data url
url <-  "https://dapds00.nci.org.au/thredds/dodsC/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/day/uas/v20231001/uas_AUS-11_ERA5_historical_hres_BOM_BARRA-R2_v1_day_199303-199303.nc"
url <- "https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/day/uas/latest/uas_AUS-11_ERA5_historical_hres_BOM_BARRA-R2_v1_day_198001-198001.nc"
northward wind surface (vas)


library(tidync)

library(raster)
## Need to disable DPI checkploint
mydata <- raster::stack(url)

plot(mydata[1])
summary(mydata)
mydata$X1980.01.01
summary(mydata$X1980.01.01)
plot(mydata$X1980.01.01)

xy <- cbind(130, -20)
#extract(r, xy)

raster::extract(mydata, xy)


#### make vectors of months and years
months <- seq(1:12)
years <- seq(2009,2023,1)

# Barra ends in August 2023
year_month <- crossing(years,months)
duds <- year_month %>% filter(years > 2023)
year_month <- year_month %>% anti_join(duds)

year_month <- year_month %>% mutate(Month_pad = str_pad(as.character(months),width=2, pad="0"),
                                    year_month = paste0(years, Month_pad))


url1 <-  "https://dapds00.nci.org.au/thredds/dodsC/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/day/uas/latest/uas_AUS-11_ERA5_historical_hres_BOM_BARRA-R2_v1_day_"
url2 <-  ".nc"
i=year_month$year_month[1]
url <-  "https://dapds00.nci.org.au/thredds/dodsC/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/day/uas/latest/uas_AUS-11_ERA5_historical_hres_BOM_BARRA-R2_v1_day_199303-199303.nc"
full_url <- paste0(url1,i,"-",i,url2) 
url
full_url


lat_lons <- data.frame("Lat" = c(-32.27560, -33.31052, -34.52192, -34.86250, -35.69744, -32.876619778952865, -33.5024638307079, -29.44183911413873, -31.65979997015673),
                       "Lon" = c(152.49460, 151.48528, 150.84102, 150.60694, 150.1515064 , 151.72565810770107, 151.157008515889, 153.20397383003962, 152.7285029193538),
                       "Estuary" =c("Wallis Lake", "Tuggerah Lake", "Lake Illawarra", "Shoalhaven River", "Clyde River",
                                    "Hunter River", "Hawkesbury River", "Clarence River", "Camden Haven River"))

xy <- lat_lons[,c(2,1)]
lat_lons_sp <- SpatialPointsDataFrame(coords = xy, data = lat_lons,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# Steps
# Download file
pb <- txtProgressBar(min = 0, max = nrow(year_month), style = 3)
for(i in 1:nrow(year_month)){
full_url <- paste0(url1,year_month$year_month[i],"-",year_month$year_month[i],url2) 

mydata <- raster::stack(full_url)
extracted <- raster::extract(mydata, lat_lons_sp, method = "bilinear")

et <- t(extracted)
et2 <- as.data.frame(et)
et2$Date = row.names(et2)
et2 <- et2 %>% rename("Wallis Lake" = V1, "Tuggerah Lake" = V2, "Lake Illawarra" = V3, "Shoalhaven River" = V4, "Clyde River" = V5,
                      "Hunter River" = V6, "Hawkesbury River" = V7, "Clarence River" = V8, "Camden Haven River"=V9)

write_csv(et2, paste0("Data/Wind Data/u_wind_", year_month$year_month[i],".csv"))
setTxtProgressBar(pb, i)
}
