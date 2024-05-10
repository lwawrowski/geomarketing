library(tidyverse)
library(readxl)
library(sf)
library(janitor)

pow <- read_sf("mapy/powiaty.shp")

st_crs(pow)

pow <- st_set_crs(pow, 2177)

wybory <- read_xlsx("data/2018-sejmiki-po-powiatach-proc.xlsx")

wybory <- clean_names(wybory)

wybory_pow <- inner_join(wybory, pow, by = c("teryt"="jpt_kod_je"))

# frekwencja

ggplot(wybory_pow) + geom_sf(aes(fill=frekwencja, geometry=geometry))

# poparcie dla 9 partii

wybory_long <- wybory %>%
  select(teryt, kw_prawo_i_sprawiedliwosc:kw_ruch_narodowy_rp) %>%
  gather(kw, poparcie, -teryt)

wybory_pow2 <- left_join(wybory_long, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow2) + 
  geom_sf(aes(fill=poparcie, geometry=geometry)) +
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
  geom_sf(aes(fill=kw, geometry = geometry)) +
  scale_fill_brewer(name = "", palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave("mapa_wybory.png", width = 11, height = 11)

library(spdep)

pow_nb <- poly2nb(pow)
pow_nb_list <- nb2listw(pow_nb)
pow_nb_matrix <- nb2mat(pow_nb)
pow_nb_matrix[1:10,1:10]

# statystyka Morana

# frekwencja

moran.test(x = wybory$frekwencja, listw = pow_nb_list)

moran.plot(x = wybory$frekwencja, listw = pow_nb_list)

wybory$frekwencja_z <- as.numeric(scale(wybory$frekwencja))

moran.plot(x = wybory$frekwencja_z, listw = pow_nb_list)

wybory$frekwencja_lag <- lag.listw(x = pow_nb_list, var = wybory$frekwencja_z)

plot(wybory$frekwencja_z, wybory$frekwencja_lag)

wybory$frekwencja_cw <- ifelse(wybory$frekwencja_z > 0 & wybory$frekwencja_lag > 0,
                               "wysokie wartości otoczone wysokimi", 
                               ifelse(wybory$frekwencja_z < 0 & wybory$frekwencja_lag > 0,
                                      "niskie wartości otoczone wysokimi",
                                      ifelse(wybory$frekwencja_z < 0 & wybory$frekwencja_lag < 0,
                                             "niskie wartości otoczone niskimi", "wysokie wartości otoczone niskimi")))

table(wybory$frekwencja_cw)

wybory_pow <- inner_join(wybory, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow) + geom_sf(aes(fill=frekwencja_cw))

# pis

moran.test(x = wybory$kw_prawo_i_sprawiedliwosc, listw = pow_nb_list)

moran.plot(x = wybory$kw_prawo_i_sprawiedliwosc, listw = pow_nb_list)

wybory$kw_prawo_i_sprawiedliwosc_z <- as.numeric(scale(wybory$kw_prawo_i_sprawiedliwosc))

moran.plot(x = wybory$kw_prawo_i_sprawiedliwosc_z, listw = pow_nb_list)

wybory$kw_prawo_i_sprawiedliwosc_lag <- lag.listw(x = pow_nb_list, var = wybory$kw_prawo_i_sprawiedliwosc_z)

plot(wybory$kw_prawo_i_sprawiedliwosc_z, wybory$kw_prawo_i_sprawiedliwosc_lag)

wybory$kw_prawo_i_sprawiedliwosc_cw <- ifelse(wybory$kw_prawo_i_sprawiedliwosc_z > 0 & wybory$kw_prawo_i_sprawiedliwosc_lag > 0,
                               "wysokie wartości otoczone wysokimi", 
                               ifelse(wybory$kw_prawo_i_sprawiedliwosc_z < 0 & wybory$kw_prawo_i_sprawiedliwosc_lag > 0,
                                      "niskie wartości otoczone wysokimi",
                                      ifelse(wybory$kw_prawo_i_sprawiedliwosc_z < 0 & wybory$kw_prawo_i_sprawiedliwosc_lag < 0,
                                             "niskie wartości otoczone niskimi", "wysokie wartości otoczone niskimi")))

table(wybory$kw_prawo_i_sprawiedliwosc_cw)

wybory_pow <- inner_join(wybory, pow, by = c("teryt"="jpt_kod_je"))

ggplot(wybory_pow) + geom_sf(aes(fill=kw_prawo_i_sprawiedliwosc_cw))

# upraszczanie mapy

pow_gugik <- read_sf("mapy/gugik/Powiaty.shp")
pow_gugik <- st_transform(pow_gugik, 2177)
pow_gugik_small <- tmaptools::simplify_shape(pow_gugik, fact = 0.1, keep.units = T)
pow_gugik_small_sp <- as_Spatial(pow_gugik_small)
rgdal::writeOGR(pow_gugik_small_sp, ".", "mapy/gugik/pow_gugik_small", driver="ESRI Shapefile")

