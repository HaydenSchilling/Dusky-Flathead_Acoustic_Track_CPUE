library(tidyverse)
library(REdaS)

mydata <- read_csv("Data/Wind Data/Combined files/Combined U V speed direction data.csv")

# Classify onshore/offshore
mydata$Onshore_Offshore <- "Offshore"

# Adjust onshore offshore for coastline angle

mydata$Onshore_Offshore[mydata$Direction < 225 & mydata$Direction > 45] <- "Onshore"

# Check it worked
table(mydata$Onshore_Offshore, mydata$Estuary)

# make offshore negative wind speed
mydata$output <- ifelse((mydata$Onshore_Offshore) == "Offshore", -1, 1) # make output vector
mydata$Speed_km_hr <- mydata$Speed_km_hr*mydata$output

#### Identify  angle of interest
mydata$Coastline_angle <- 135

# radians from adjusted coastline (24 deg coastline - need to fix for all estuaries)
mydata$Wind.direction.in.radians.adjusted <- NULL
mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Direction+mydata$Coastline_angle)

# Calculate effective wind speed
mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)*-1
plot(mydata$Direction, mydata$Wind.effect.size) # check

mydata$Wind.effect.size = abs(mydata$Wind.effect.size)
mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Speed_km_hr * -1

# Check (is slow)
plot(mydata$Direction, mydata$Wind.speed.adjusted)

# Group by month
datM <- mydata %>% group_by(Estuary, Year, Month) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)*24), count = n()) # *24 as i have speed per hour but want 24 hours
head(datM)

hist(datM$displacement)

fwrite(datM, file = "Data/Wind Data/Combined files/Combined U V speed direction data final 135 degree.csv")

# mydata$Estuary <- as.factor(mydata$Estuary)
# estuaries <- levels(mydata$Estuary)
# 
# for (i in as.character(estuaries)) {
#   dat2 <- subset(datM, estuary == i)
#   fwrite(dat2, file = paste("../Data/BARRA Data/BARRA_", i, "_Monthly Modelled Wind Data Final 135 degree.csv", sep = ""))
#   
# }