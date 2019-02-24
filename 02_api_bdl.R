library(tidyverse)
library(jsonlite)
library(purrr)
library(sf)

woj <- read_sf("mapy/woj.shp")

plot(woj$geometry)

plot(woj["jpt_powier"], nbreaks = 3, 
     breaks = "kmeans", main = "Powierzchnia")

# tematy

tematy <- "https://bdl.stat.gov.pl/api/v1/subjects?page-size=32"

fromJSON(tematy)

# kategorie

kategorie <- "https://bdl.stat.gov.pl/api/v1/subjects?parent-id=K40"

fromJSON(kategorie)

kategorie2 <- "https://bdl.stat.gov.pl/api/v1/subjects?parent-id=G403"

fromJSON(kategorie2)

# zmienna

zmienna <- "https://bdl.stat.gov.pl/api/v1/Variables?subject-id=P2497"

fromJSON(zmienna)

# dane

wyn_url <- "https://bdl.stat.gov.pl/api/v1/data/by-variable/64428?year=2017&unit-level=2&page-size=16"

wyn_dane <- fromJSON(wyn_url)

wyn_df <- wyn_dane$results

wyn_df <- wyn_df %>%
  mutate(wynagrodzenie=map_dbl(values, "val"),
         kod_woj=substr(id,3,4)) %>%
  select(kod_woj, wynagrodzenie)

woj <- inner_join(woj, wyn_df, by=c("jpt_kod_je"="kod_woj"))

# mapa

plot(woj["wynagrodzenie"], nbreaks = 5, 
     breaks = "jenks",
     main = "Wynagrodzenia")