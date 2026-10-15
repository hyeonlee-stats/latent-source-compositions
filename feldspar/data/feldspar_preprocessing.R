library(readxl)
library(dplyr)

load('feldspar/data/oxide_to_element.Rdata')

alkfp.all <- read_xls('feldspar/data/alkali.feldspars.xls', sheet = 1) %>%
  select(SiO2, TiO2, Al2O3, Fe2O3T, Fe2O3, FeOT, FeO,
         CaO, MgO, MnO, BaO, SrO, K2O, Na2O) %>%
  mutate_all(~ replace_na(.x, 0)) %>%
  combine_Fe() %>%
  mutate(CaO = CaO + SrO + BaO) %>%
  select(-c(SrO, BaO)) %>%
  mutate(total = rowSums(.))
head(alkfp.all)

alkfp.all = alkfp.all %>%
  filter(total > 98 & total < 101 & SiO2 > 62 & TiO2 < 5 & FeO < 5)

alkfp = alkfp.all %>% select(-total) %>% oxide_to_element()


plagio.all <- read_xls('feldspar/data/plagioclase.xls', sheet = 1) %>%
  select(SiO2, TiO2, Al2O3, Fe2O3T, Fe2O3, FeOT, FeO,
         CaO, MgO, MnO, K2O, Na2O) %>%
  mutate_all(~ replace_na(.x, 0)) %>%
  combine_Fe() %>%
  mutate(total = rowSums(.))
head(plagio.all)

plagio.all = plagio.all %>%
  filter(total > 98 & total < 101 & SiO2 < 75 & TiO2 < 5)

plagio = plagio.all %>% select(-total) %>% oxide_to_element()

set.seed(1)
plagio.idx = sample(1:nrow(plagio.all), nrow(alkfp))
plagio.all = plagio.all[plagio.idx,]
plagio = plagio[plagio.idx,]

fp = rbind(alkfp, plagio)
write.csv(fp, 'feldspar/data/feldspar.csv', row.names = F)
