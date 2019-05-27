library(tidyverse)
library(lubridate)

load("data/pogoda.RData")

pogoda <- pogoda %>%
  mutate(data_godz=paste0(data_pomiaru, " ", godzina_pomiaru, ":00"),
         data_godz=ymd_hm(data_godz)) %>%
  mutate_at(c(4:10), as.numeric)

pogoda %>%
  gather(cecha, miara, temperatura:cisnienie) %>%
  ggplot(aes(x=data_godz, y=miara)) + 
  geom_point() +
  facet_wrap(~ cecha, scales = "free_y")

load("data/prm_stacje_12-18_05_2019a.RData")

# stacja towarowa

prm_towarowa <- stacje %>%
  filter(name == "Towarowa") %>%
  mutate(day = wday(date, label = TRUE),
         hour = hour(date)) %>%
  filter(date >= "2019-05-12", date <= "2019-05-19")

ggplot(prm_towarowa, aes(x=hour, y=bikes, color=day)) +
  geom_smooth()
  
