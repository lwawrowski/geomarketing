library(tidyverse)
library(sf)
library(tmap)

woj <- read_sf("mapy/woj.shp")

st_crs(woj)

woj <- st_set_crs(woj, 2177)

st_crs(woj)

# kontury
tm_shape(woj) + tm_borders()

# powierzchnia

tm_shape(woj) + 
  tm_borders() + 
  tm_fill(col = "jpt_powier", 
          palette = "Blues", 
          n = 3, 
          title = "Powierzchnia",
          style = "fixed",
          breaks = c(0,1200000,2800000, 4000000),
          labels = c("Do 1,2 mln km2", "Od 1,2 mln km2 do 2,8 mln km2", "Od 2,8 mln km2"))

tmap_mode("view")

tm_shape(woj) + 
  tm_borders() + 
  tm_fill(col = "jpt_powier", 
          palette = "Blues", 
          n = 3, 
          title = "Powierzchnia",
          style = "fixed",
          breaks = c(0,1200000,2800000, 4000000))