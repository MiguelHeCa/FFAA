# Budget Percentage
library(tidyverse)

ramos <- read_csv("https://raw.githubusercontent.com/MiguelHeCa/cuenta_publica_mx/master/data/ramos.csv")

dimensiones <- ramos %>% 
  filter(EJERCIDO_MDP != 0) %>% 
  group_by(CICLO) %>% 
  summarise(TOTAL_MDP = sum(EJERCIDO_MDP),
            MIN = min(EJERCIDO_MDP),
            MAX = max(EJERCIDO_MDP))


ramos_t <- ramos %>% left_join(dimensiones)

ramos_t <- ramos_t %>% 
  filter(EJERCIDO_MDP != 0) %>% 
  group_by(CICLO) %>% 
  mutate(Porcentaje = (EJERCIDO_MDP / TOTAL_MDP) * 100,
         PMIN = min(Porcentaje),
         PMAX = max(Porcentaje),
         EJERCIDO_MMDP = EJERCIDO_MDP / 1000,
         MIN2 = MIN / 1000,
         MAX2 = MAX / 1000) %>% 
  ungroup()


  
ramos_t %>% 
  filter(ID_RAMO == 7 | ID_RAMO == 13) %>% 
  ggplot(aes(x = CICLO)) +
  geom_ribbon(aes(ymin = MIN2, ymax = MAX2), fill = "#CECECE", alpha = 0.7, show.legend = FALSE) +
  geom_line(aes(y = EJERCIDO_MMDP, color = DESC_RAMO), size = 3) +
  scale_x_continuous(breaks = ramos_t$CICLO) +
  labs(x = "Años",
       y = "Monto ejercido en miles de millones de pesos",
       title = "Presupuesto de las fuerzas armadas respecto a la Cuenta Pública de 2008 a 2017.",
       subtitle = "Precios de 2018",
       caption = "Fuente: Cuenta Pública e INEGI.") +
  theme_light()

