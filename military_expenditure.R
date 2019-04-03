library(wbstats)
library(tidyverse)

mili_ind <- "MS.MIL.XPND.GD.ZS"

raw_data <- wb(
  country = "countries_only",
  indicator = mili_ind,
  startdate = 1974,
  enddate = 2017,
  POSIXct = TRUE)

countries <- wbcountries(lang = "en")

country_id <- countries %>% 
  select(iso3c, country, regionID, region, incomeID, income)

tidy_data <- raw_data %>% 
  select(iso3c, date_ct, country, value)

mil_exp <- tidy_data %>% left_join(country_id, by = c("iso3c", "country")) %>% as_tibble()

map(mil_exp, ~sum(is.na(.)))

mil_exp %>% 
  ggplot()