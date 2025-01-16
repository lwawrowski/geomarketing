library(tidyverse)
library(geosphere)
library(tidygeocoder)

# https://jessecambon.github.io/tidygeocoder/

some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "UEP", "Al. Niepodległości 10, 61-875 Poznań",
  "CDV", "Kutrzeby 10, 61-719 Poznań",     
  "PP", "plac Marii Skłodowskiej-Curie 5, 60-965 Poznań"                                  
)

lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)

distm(c(lat_longs$longitude[1], lat_longs$latitude[1]), c(lat_longs$longitude[2], lat_longs$latitude[2]))
