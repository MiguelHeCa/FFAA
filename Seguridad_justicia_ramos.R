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

seg_just %>% 
  ggplot(aes(x = CICLO, y = EJERCIDO_REAL, color = DESC_RAMO)) +
  geom_line() +
  annotate("rect", xmin = -Inf, xmax = 2000, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 2006, xmax = 2012, ymin = -Inf, ymax = Inf, alpha = .2)

seg_just %>% 
  mutate(EJERCIDO_REAL = if_else(ID_RAMO == 4 & CICLO %in% c(2002:2012),NA_real_, EJERCIDO_REAL)) %>% 
  ggplot(aes(x = CICLO, y = EJERCIDO_REAL, color = DESC_RAMO)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  annotate("rect", xmin = -Inf, xmax = 2000, ymin = -Inf, ymax = Inf, alpha = .2) +
  annotate("rect", xmin = 2006, xmax = 2012, ymin = -Inf, ymax = Inf, alpha = .2) +
  guides(color = guide_legend(title = "Ramo")) +
  labs(x = "Año",
       y = "Monto ejercido real",
       title = "Presupuesto ejercido real de los ramos dedicados a seguridad y justicia de 1995 a 2018",
       subtitle = "Millones de pesos de junio de 2018",
       caption = "Fuente: Cuenta pública federal") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


