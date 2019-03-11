library(tidyverse)
library(stplanr)
library(leaflet)

load("data/04_points_data.RData")

# otwarte dane nt. Poznania

leaflet(biletomaty) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~TVM)

# geokodowanie

a <- geo_code("al. niepodległości 10, poznań")

b <- geo_code("al. niepodległości 10, poznań", return_all = TRUE)

# ikea
# https://www.promoceny.pl/sklepy/szukaj/ikea/

ikea <- ikea %>%
  mutate(lon=0,
         lat=0)

for(nr in 1:11){
  
  wsp <- geo_code(ikea$adres[nr])
  
  ikea$lon[nr] <- wsp[1]
  ikea$lat[nr] <- wsp[2]
  
  Sys.sleep(1)
}

leaflet(ikea) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, label = ~miasto)

# bank

bank <- bank %>%
  mutate(adres_miasto=paste(adres, miejscowosc, sep=", "),
         lon=0,
         lat=0)

for(nr in 1:nrow(bank)){
  
  wsp <- geo_code(bank$adres_miasto[nr])
  
  bank$lon[nr] <- wsp[1]
  bank$lat[nr] <- wsp[2]
  
  Sys.sleep(1)
}

leaflet(bank) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, label = ~adres)
