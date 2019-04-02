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
