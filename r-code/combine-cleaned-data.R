library(tidyverse)
library(lubridate)

source("r-code/clean-data-20200310-through-20200322.R")
source("r-code/clean-data-20200323-through-today.R")

tb <- bind_rows(read_csv("data/cleaned-data-20200310-through-20200322.csv"), 
          read_csv("data/cleaned-data-20200323-through-today.csv")) %>% 
  arrange(Date)

states <- tb$`Province/State` %>% unique

statepop <- read_csv("data/nst-est2019-alldata.csv") %>% 
  select(NAME, POPESTIMATE2019) %>% 
  filter(NAME %in% states) %>% 
  rename(`Province/State` = NAME,
         population = POPESTIMATE2019)

out <- left_join(tb, statepop) %>% 
  mutate(`Confirmed per million residents` = 10^6 * Confirmed / population,
         `Deaths per million residents` = 10^6 * Deaths / population,
         `Recoveries per million residents` = 10^6 * Recovered / population) %>% 
  filter(`Province/State` %in% statepop$`Province/State`)

out %>% 
  write_csv("data/cleaned-data-all-series.csv")

out %>% 
  write_csv("shiny/sars-cov-2-usa-by-state/data/cleaned-data-all-series.csv")
