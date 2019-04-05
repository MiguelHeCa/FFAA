library(wbstats)
library(tidyverse)
library(lubridate)


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

write_csv(mil_exp, "data/military_expend.csv")



