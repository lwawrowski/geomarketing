library(tidyverse)
library(osmdata)
library(sf)
library(leaflet)
library(geosphere)
library(reshape2)

# mapa Poznania

poznan <- read_sf("mapy/Poznan.shp")

poznan <- st_transform(poznan, 2177)

ggplot(poznan)

# dane z OSM
# https://wiki.openstreetmap.org/wiki/Map_Features

atm <- opq("poznan, poland")

atm

atm_points <- atm$osm_points

ggplot(atm_points)

# leaflet

wsp <- as.data.frame(st_coordinates(atm_points))

# pomiar odległości



# losowanie punktów - leaflet

los_punkty <- as.data.frame(st_coordinates(st_sample(poznan, 1000)))



# macierz odległości



# ile bankomatów jest w promieniu 500 metrów?

# ile średnio jest do najbliższego bankomatu?
