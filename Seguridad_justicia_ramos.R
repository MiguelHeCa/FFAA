library(tidyverse)

# Gasto oficial

# Obtener datos -----------------------------------------------------------

ramos_95_18 <- read.csv("https://raw.githubusercontent.com/MiguelHeCa/cuenta_publica_mx/master/data/ramos_95_18.csv")
pib_corr_anual <- read.csv("https://raw.githubusercontent.com/MiguelHeCa/cuenta_publica_mx/master/data/pib_corr_anual.csv")
deflactor <- read.csv("https://raw.githubusercontent.com/MiguelHeCa/cuenta_publica_mx/master/data/deflactor_base2018.csv")

# Preparar datos ----------------------------------------------------------

ramos <- ramos_95_18 %>% 
  left_join(deflactor, by = c("CICLO" = "YEAR")) %>% 
  left_join(pib_corr_anual, by = c("CICLO" = "YEAR")) %>% 
  mutate(EJERCIDO_MDP = if_else(EJERCIDO_MDP == 0, NA_real_, EJERCIDO_MDP),
         EJERCIDO_REAL = EJERCIDO_MDP / FACTOR,
         PIB_REAL = PIB_CORRIENTE / FACTOR,
         RATIO_PIB = EJERCIDO_REAL / PIB_REAL * 100) %>% 
  as_tibble()

# Seguridad y justicia ----------------------------------------------------


seg_just <- ramos %>% 
  filter(ID_RAMO %in% c(3, 4, 7, 13, 17, 36)) %>% 
  arrange(CICLO, ID_RAMO)
  

seg_just %>% 
  group_by(CICLO) %>% 
  summarise(`Porcentaje del PIB` = sum(RATIO_PIB, na.rm = T)) %>% 
  print(n = Inf)

quartz()
seg_just %>% 
  group_by(CICLO) %>% 
  summarise(`Porcentaje del PIB` = sum(RATIO_PIB, na.rm = T)) %>% 
  ggplot(aes(x = CICLO, y = `Porcentaje del PIB`)) +
  geom_line()
