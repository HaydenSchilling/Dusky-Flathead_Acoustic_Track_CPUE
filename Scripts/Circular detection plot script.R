# Script to generate circular detection plots

library(actel)

### Clyde

setwd("VPS - Hayden/Full CEFT Download/Clyde/actel/")
xx <- explore(tz = 'Australia/Sydney', report = T, max.interval = 240)

hh <- getTimes(input = xx, move.type = "array", event.type = "arrival", n.events = "all")
times <- timesToCircular(hh)
plotTimes(times)
plotTimes(times, circular.scale = "area", mean.dash = FALSE,
          file = "Clyde detection time.pdf", height = 4, width=4)


### Shoalhaven

setwd("../../actel/")

#xx <- explore(tz = 'Australia/Sydney', report = T)

xxS <- explore(tz = 'Australia/Sydney', report = T, max.interval = 240)

hhS <- getTimes(input = xxS, move.type = "array", event.type = "arrival", n.events = "all")
timesS <- timesToCircular(hhS)
plotTimes(timesS)
plotTimes(timesS, circular.scale = "area", mean.dash = FALSE,
          file = "Shoalhaven detection time.pdf", height = 4, width=4)


plotDetections(xx, tag = "A69-1303-51408")

xx$valid.detections
library(tidyverse)
detect_long <- bind_rows(xx$valid.detections, .id = "column_label")

head(detect_long)

ggplot(detect_long, aes(Timestamp, Standard.name)) + geom_point() + 
  facet_wrap(~column_label, scales = "free_x")
