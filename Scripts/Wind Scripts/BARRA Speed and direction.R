# Larval BARRA take U and V vectors and calculate speed and directions

# Combine U and V
library(data.table)
library(tidyverse)
library(lubridate)

Full_dat <- read_csv("Data/Wind Data/Combined files/Combined U V data.csv")

hist(Full_dat$u)
hist(Full_dat$v)

# Now fix time
# Posix with origin: hours since 1970-01-01 00:00:00


head(Full_dat)

Full_dat$Year <- year(Full_dat$Date)
Full_dat$Month <- month(Full_dat$Date)
Full_dat$Day <- day(Full_dat$Date)
#Full_dat$Hour <- hour(Full_dat$Date)

head(Full_dat)
range(Full_dat$Date)
#fwrite(Full_dat, "../BOM Data/BARRA Model/Estuaries_u_v.csv")

# Calculate speed and direction from vectors
library(rWind)
sp_dir <- uv2ds(Full_dat$u, Full_dat$v)
head(sp_dir)

# Assign to dataframe
dir <- sp_dir[,1]
sp <- sp_dir[,2]
Full_dat$Direction <- dir
Full_dat$Speed_m_s <- sp
# Convert to km/hr
Full_dat$Speed_km_hr <- Full_dat$Speed_m_s * 3.6
Full_dat$Speed_m_s <- NULL

head(Full_dat)
table(Full_dat$Year)

fwrite(Full_dat, "Data/Wind Data/Combined files/Combined U V speed direction data.csv", row.names = FALSE)
