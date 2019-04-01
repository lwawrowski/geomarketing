library(tidyverse)
library(readxl)
library(sf)
library(janitor)

pow <- read_sf("mapy/powiaty.shp")

wybory <- read_xlsx("data/2018-sejmiki-po-powiatach-proc.xlsx")

wybory <- clean_names(wybory)

wybory_pow <- inner_join(wybory, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow) + geom_sf(aes(fill=frekwencja))

wybory_long <- wybory %>%
  select(teryt, kw_prawo_i_sprawiedliwosc:kw_ruch_narodowy_rp) %>%
  gather(kw, poparcie, -teryt)

wybory_pow2 <- left_join(wybory_long, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow2) + 
  geom_sf(aes(fill=poparcie)) +
  facet_wrap(~ kw)
