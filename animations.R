# Animación

# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(gganimate)
library(ggbeeswarm)

# Cargar datos ------------------------------------------------------------

mil_exp <- read_csv("data/military_expend.csv")

# Animación ---------------------------------------------------------------

countries_ols <- mil_exp %>% 
  filter((abs(expenditure - median(expenditure)) > 3*sd(expenditure))) %>% 
  select(country) %>% 
  unique()

mil_exp_without_ols <- mil_exp %>% 
  filter(!country %in% as.vector(countries_ols$country))

anim2 <- mil_exp %>%
  filter(!country == "Kuwait") %>% 
  ggplot(aes(x = region, y = expenditure, color = region)) +
  geom_beeswarm(alpha = 0.7, size = 1.5, show.legend = F) +
  geom_boxplot(alpha = 0, show.legend = F) +
  scale_color_discrete() +
  facet_grid(. ~ income, scales = "free") +
  #scale_y_log10() + 
  coord_flip() +
  theme_light() +
  theme(panel.grid = element_blank()) +
  # gganimate specs
  labs(title = "Year: {frame_time}", x = "Region", y = 'Military Expenditure (% of GDP)') +
  transition_time(year) +
  ease_aes('linear')

animate(anim2 + enter_fade() + exit_fade(), fps = 60, width = 1280, height = 720, res = 100)

anim_save("img/mil_exp_74_17_freescale.gif")