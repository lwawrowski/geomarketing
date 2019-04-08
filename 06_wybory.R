library(tidyverse)
library(readxl)
library(sf)
library(janitor)

pow <- read_sf("mapy/powiaty.shp")

wybory <- read_xlsx("data/2018-sejmiki-po-powiatach-proc.xlsx")

wybory <- clean_names(wybory)

wybory_pow <- inner_join(wybory, pow, by = c("teryt"="jpt_kod_je"))

# frekwencja

ggplot(wybory_pow) + geom_sf(aes(fill=frekwencja))

# poparcie dla 9 partii

wybory_long <- wybory %>%
  select(teryt, kw_prawo_i_sprawiedliwosc:kw_ruch_narodowy_rp) %>%
  gather(kw, poparcie, -teryt)

wybory_pow2 <- left_join(wybory_long, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow2) + 
  geom_sf(aes(fill=poparcie)) +
  facet_wrap(~ kw)

# wygrana partia w powiecie

wybory_long_all <- wybory %>%
  select(teryt, kw_prawo_i_sprawiedliwosc:kw_stronnictwa_pracy) %>%
  gather(kw, poparcie, -teryt)

wybory_pow_max <- wybory_long_all %>%
  group_by(teryt) %>%
  summarise(max_poparcie=max(poparcie, na.rm = T))

wybory_pow_kw <- left_join(wybory_pow_max, wybory_long_all, by=c("teryt", "max_poparcie"="poparcie"))

wybory_pow_kw_g <- inner_join(wybory_pow_kw, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow_kw_g) + 
  geom_sf(aes(fill=kw)) +
  scale_fill_brewer(name = "", palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave("mapa_wybory.png", width = 11, height = 11)

