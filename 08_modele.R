library(tidyverse)
library(readxl)
library(sf)

pow <- read_sf("mapy/powiaty.shp")

d <- read_xlsx("data/08_dane.xlsx") %>%
  mutate(lud_500_plus=sr_500_plus/ludnosc,
         jpt_kod_je=substr(kod, 1, 4))

pow_d <- inner_join(pow, d)

lm <- lm(wynagrodzenie_100 ~ stopa_bezrobocia_100 + odleglosc +
           lud_500_plus, data = pow_d)
summary(lm)

library(spdep)

w <- poly2nb(pow_d)
w_list <- nb2listw(w)

moran.test(resid(lm), w_list)
moran.test(lm$residuals, w_list)

# sar

sar <- lagsarlm(wynagrodzenie_100 ~ stopa_bezrobocia_100 + odleglosc +
                  lud_500_plus, data = pow_d, listw = w_list)
summary(sar)

moran.test(resid(sar), w_list)

# sem

sem <- errorsarlm(wynagrodzenie_100 ~ stopa_bezrobocia_100 + odleglosc +
                    lud_500_plus, data = pow_d, listw = w_list)
summary(sem)

moran.test(resid(sem), w_list)
