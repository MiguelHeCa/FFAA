library(eurostat)
library(tidyverse)
library(knitr)

toc <- get_eurostat_toc()

kable(toc)
search_eurostat("government expenditure")

gov_exp_code <- search_eurostat("government expenditure")$code[1]

gov_exp_raw <- get_eurostat(gov_exp_code)

not_countries <- c("EA11", "EA12", "EA13", "EA14", 
                   "EA15", "EA16", "EA17", "EA18", "EA19", 
                   "EU15", "EU25", "EU27", "EU28")

gov_exp <- gov_exp %>% filter(unit == "PC_GDP",
                              !(geo %in% not_countries))

datl <- label_eurostat(gov_exp)

datl %>% distinct(na_item) %>% print(n = Inf)

datl %>% distinct(sector) %>% print(n = Inf)

datl %>% distinct(cofog99) %>% print(n = Inf)

pos <- datl %>% filter(str_detect(cofog99, "Public order and safety"))

pos %>% distinct(cofog99)

pos %>% distinct(sector)

pos %>% distinct(na_item) %>% print(n = Inf) 
