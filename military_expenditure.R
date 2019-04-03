library(wbstats)
library(tidyverse)
library(ggbeeswarm)
library(lubridate)
library(gganimate)

# Military expenditure (as % of GDP)
mili_ind <- "MS.MIL.XPND.GD.ZS"

# PPP GDP (constant 2011 international $)
gdp_ppp_ind <- "NY.GDP.MKTP.PP.KD"

# GDP (constant 2010 US$)
gdp_2010_ind <- "NY.GDP.MKTP.KD"

raw_gdp <- wb(
  country = "countries_only",
  indicator = gdp_2010_ind,
  startdate = 1974,
  enddate = 2017,
  POSIXct = T
)

raw_mili <- wb(
  country = "countries_only",
  indicator = mili_ind,
  startdate = 1974,
  enddate = 2017,
  POSIXct = TRUE)

tidy_gdp <- raw_gdp %>% 
  select(iso3c, date_ct, country, value) %>% 
  rename(gdp = value)

tidy_mili <- raw_mili %>% 
  select(iso3c, date_ct, country, value) %>% 
  rename(expenditure = value)

countries <- wbcountries(lang = "en")

country_id <- countries %>% 
  select(iso3c, country, regionID, region, incomeID, income)

mil_exp <- tidy_mili %>%
  left_join(tidy_gdp) %>% 
  left_join(country_id, by = c("iso3c", "country")) %>%
  as_tibble() %>% 
  mutate(region = str_trim(region),
         region = factor(region,
                         levels = c("North America",
                                    "Latin America & Caribbean",
                                    "Middle East & North Africa",
                                    "Sub-Saharan Africa",
                                    "Europe & Central Asia",
                                    "East Asia & Pacific",
                                    "South Asia")),
         income = factor(income,
                         levels = c("High income",
                                    "Upper middle income",
                                    "Lower middle income",
                                    "Low income")),
         year = as.integer(year(date_ct)))

map(mil_exp, ~sum(is.na(.)))

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



