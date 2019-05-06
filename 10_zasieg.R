library(tidyverse)
library(sp)

load("data/wlkp_spdf.RData")

centra <- coordinates(wlkp_spdf)

punkt <- c(16.914094, 52.401829)

odleglosc <- geosphere::distm(centra, punkt)

populacja_odl <- data.frame(pop_15_64=wlkp_spdf@data$TOT_15_64,
                            odl_km=odleglosc/1000)

populacja_odl %>%
  filter(odl_km < 5) %>%
  summarise(zasieg=sum(pop_15_64))

save(populacja_odl, file = "10_zasieg/dane.RData")
