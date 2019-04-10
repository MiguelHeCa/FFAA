# Military densities


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(ggridges)
library(scales)

# Cargar datos ------------------------------------------------------------

mili_exp <- read_csv("data/military_expend.csv")

mili_exp %>%
  ggplot(aes(x = expenditure, y = year, group = year, fill = 0.5 - abs(0.5 - ..ecdf..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = 4,
    quantile_lines = TRUE,
    rel_min_height = 0.01,
    jittered_points = TRUE,
    scale = 1.25,
    alpha = 0.7,
    point_shape = ".",
    point_size = 1.25,
    size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(from = 1975, to = 2017, by = 2),
                     expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Tail probability") +
  labs(title = "Military Expenditure (% of GDP) from 1974 to 2017",
       caption = "Source: World Bank") +
  theme_light() +
  theme(panel.grid.major.x = element_blank())


# Without the maximum value -----------------------------------------------

mili_exp %>%
  filter(!country == "Kuwait") %>%
  ggplot(aes(x = expenditure, y = year, group = year, fill = 0.5 - abs(0.5 - ..ecdf..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = 4,
    quantile_lines = TRUE,
    rel_min_height = 0.01,
    jittered_points = TRUE,
    scale = 1.25,
    alpha = 0.7,
    point_shape = ".",
    point_size = 1.25,
    size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(from = 1975, to = 2017, by = 2),
                     expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Tail probability") +
  labs(title = "Military Expenditure (% of GDP) from 1974 to 2017",
       caption = "Source: World Bank") +
  theme_light() +
  theme(panel.grid.major.x = element_blank())


# Without outliers --------------------------------------------------------

countries_ols <- mili_exp %>% 
  filter((abs(expenditure - median(expenditure)) > 3*sd(expenditure))) %>% 
  select(country) %>% 
  unique()

mili_exp_without_ols <- mili_exp %>% 
  filter(!country %in% as.vector(countries_ols$country))

mili_exp_without_ols  %>% 
  ggplot(aes(x = expenditure, y = year, group = year, fill = 0.5 - abs(0.5 - ..ecdf..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = 4,
    quantile_lines = TRUE,
    rel_min_height = 0.01,
    jittered_points = TRUE,
    scale = 1.25,
    alpha = 0.7,
    point_shape = ".",
    point_size = 1,
    size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(from = 1975, to = 2017, by = 2),
                     expand = c(0.01, 0)) +
  scale_fill_viridis_c(name = "Tail probability", alpha = 0.5) +
  labs(title = "Military Expenditure (% of GDP) from 1974 to 2017",
       caption = "Source: World Bank") +
  theme_light() +
  theme(panel.grid.major.x = element_blank())
