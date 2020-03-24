library(tidyverse)
library(janitor)
library(lubridate)


# Read in data
tb <- read_csv("../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>% 
  filter(`Country/Region` == "US",
         `Province/State` != c("Diamond Princess"),
         `Province/State` != c("Grand Princess"),) %>% 
  mutate(notstate = str_detect(`Province/State`, ",")) %>% 
  filter(!notstate) %>% 
  select(-notstate, -Lat, -Long, -`Country/Region`) %>% 
  adorn_totals() %>% 
  as_tibble() 


# Tidy and add dates
tb <- tb %>% 
  pivot_longer(cols = -`Province/State`, names_to = "Date", values_to = "Recovered") %>% 
  mutate(Date = as_date(map_dbl(Date, mdy))) %>% 
  filter(Date > mdy("March 9, 2020"))



states <- tb$`Province/State`[tb$`Province/State` != "Total"] %>% unique

statepop <- read_csv("data/nst-est2019-alldata.csv") %>% 
  select(NAME, POPESTIMATE2019) %>% 
  filter(NAME %in% states) %>% 
  rename(`Province/State` = NAME,
         population = POPESTIMATE2019)

out <- left_join(tb, statepop) %>% 
  mutate(`Recoveries per million residents` = 10^6 * Recovered / population)

write_csv(out, "shiny/sars-cov-2-usa-by-state/data/us-recovered-cleaned.csv")
write_csv(out, "data/us-recovered-cleaned.csv")
