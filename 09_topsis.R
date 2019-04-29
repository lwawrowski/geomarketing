library(leaflet)
library(tidyverse)
library(topsis)

hotels <- read.csv("data/hotels.csv", sep = ";", dec=",")

leaflet(hotels) %>%
  addTiles() %>%
  addMarkers(lng = ~x, lat = ~y)

kryteria <- hotels %>%
  select(stars, price, dist_eiffel, dist_louvre, dist_arc) %>%
  scale()

ranking1 <- topsis(decision = kryteria,
                   weights = rep(1, 5), 
                   impacts = c("+", "-", "-", "-", "-"))

hotels$rank1 <- ranking1$rank

hotels %>%
  filter(rank1 <= 15) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~x, 
             lat = ~y,
             popup = ~paste("rank: ", rank1))

# dodanie wag  
ranking2 <- topsis(decision = kryteria, 
                   weights = c(2, 3, 1, 1, 1), 
                   impacts = c("+", "-", "-", "-", "-"))

hotels$rank2 <- ranking2$rank
  
hotels %>%
  filter(rank2 <= 15) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~x, 
             lat = ~y,
             popup = ~paste("rank: ", rank2))

# kryterium budżetu
kryteria2 <- hotels %>%
  mutate(budget=abs(price-500)) %>%
  select(stars, budget, dist_eiffel, dist_louvre, dist_arc) %>%
  scale()

ranking3 <- topsis(decision = kryteria2, 
                   weights = c(2, 3, 1, 1, 1), 
                   impacts = c("+", "-", "-", "-", "-"))

hotels$rank3 <- ranking3$rank

hotels %>%
  filter(rank3 <= 15) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~x, 
             lat = ~y,
             popup = ~paste("rank: ", rank3))
  