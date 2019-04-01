library(tidyverse)
library(osmdata)
library(sf)
library(leaflet)
library(geosphere)
library(reshape2)

# mapa Poznania

poznan <- read_sf("mapy/Poznan.shp")

# poznan <- st_transform(poznan, 2177)

ggplot(poznan) + geom_sf()

# dane z OSM
# https://wiki.openstreetmap.org/wiki/Map_Features

atm <- opq("poznan, poland") %>%
  add_osm_feature(key = "amenity", value = "atm") %>%
  osmdata_sf()

atm

atm_points <- atm$osm_points

ggplot(atm_points) + geom_sf()

# leaflet

wsp <- as.data.frame(st_coordinates(atm_points))

leaflet(wsp) %>%
  addTiles() %>%
  addMarkers(lng = ~X, lat = ~Y)

# pomiar odległości

# google w odwrotnej kolejności podaje współrzędne
distm(c(52.406397, 16.916635), c(52.404833, 16.921216))
# trzeba zamienić, żeby uzyskać dobry wynik
distm(c(16.916635, 52.406397), c(16.921216, 52.404833))

# losowanie punktów - leaflet

los_punkty <- as.data.frame(st_coordinates(st_sample(poznan, 1000)))

leaflet(los_punkty) %>%
  addTiles() %>%
  addMarkers(lng = ~X, lat = ~Y)

# macierz odległości

macierz_odl <- distm(los_punkty, wsp)

dim(macierz_odl)

macierz_odl[1:5, 1:5]

macierz_odl_long <- melt(macierz_odl)

# ile bankomatów jest w promieniu 500 metrów?

max(macierz_odl_long$Var1)

n_atm <- macierz_odl_long %>%
  filter(value < 1000) %>%
  count(Var1)

ggplot(n_atm, aes(x=n)) + 
  geom_histogram(binwidth = 1)

# ile średnio jest do najbliższego bankomatu?

min_atm <- macierz_odl_long %>%
  group_by(Var1) %>%
  summarise(min_odl=min(value))

summary(min_atm)

ggplot(min_atm, aes(x=min_odl)) +
  geom_histogram(binwidth = 500)

#