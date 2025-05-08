library(tidyverse)
library(REdaS)

mydata <- read_csv("Wind Data/Combined files/Combined U V speed direction data.csv")

# Classify onshore/offshore
mydata$Onshore_Offshore <- "Offshore"

mydata$Onshore_Offshore[mydata$Direction < 135 | mydata$Direction > 315] <- "Onshore"

# Check it worked
table(mydata$Onshore_Offshore, mydata$Estuary)

head(mydata)

# make offshore negative wind speed
mydata$output <- ifelse((mydata$Onshore_Offshore) == "Offshore", -1, 1) # make output vector
mydata$Speed_km_hr <- mydata$Speed_km_hr*mydata$output

str(mydata)

#### Identify coastline angle, Set to 24 for Hawkesbury, Camden, Wallis, Illawarra, St george, Sydney, and others different
mydata$Coastline_angle <- 45

# radians from adjusted coastline (24 deg coastline - need to fix for all estuaries)
mydata$Wind.direction.in.radians.adjusted <- NULL
mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Direction+mydata$Coastline_angle)

# Calculate effective wind speed
mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)
# check (very slow)
#plot(mydata$Direction, mydata$Wind.effect.size)

mydata$Wind.effect.size = abs(mydata$Wind.effect.size)
mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Speed_km_hr * -1
# check again (also slow)
plot(mydata$Direction, mydata$Wind.speed.adjusted)


# Group by Month
datM <- mydata %>% group_by(Estuary, Year, Month) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE) * 24), count = n())
head(datM)

fwrite(datM, file = "Wind Data/Combined files/Combined U V speed direction data final 45 degree.csv")

# mydata$estuary <- as.factor(mydata$estuary)
# estuaries <- levels(mydata$estuary)
# 
# for (i in as.character(estuaries)) {
#   dat2 <- subset(datM, estuary == i)
#   fwrite(dat2, file = paste("../Data/BARRA Data/BARRA_", i, "_Monthly Modelled Wind Data Final 45 degree.csv", sep = ""))
#   
# }