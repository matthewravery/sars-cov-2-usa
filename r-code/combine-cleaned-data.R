library(tidyverse)
library(lubridate)

# source("r-code/clean-data-20200310-through-20200322.R")
source("r-code/clean-data-20200323-through-today.R")

out <- bind_rows(read_csv("data/cleaned-data-20200310-through-20200322.csv"), 
          read_csv("data/cleaned-data-20200323-through-today.csv")) 


out %>% 
  write_csv("data/cleaned-data-all-series.csv")

out %>% 
  write_csv("shiny/sars-cov-2-usa-by-state/data/cleaned-data-all-series.csv")
