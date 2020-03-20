library(tidyverse)
library(janitor)
library(lubridate)
library(nationalparkcolors)

tb <- read_csv("data/us-confirmed-cleaned.csv")

states <- tb$`Province/State`[tb$`Province/State` != "Total"] %>% unique

most_recent_day <- max(tb$Date)

topstates <- tb %>% 
  filter(`Province/State` %in% states) %>% 
  filter(Date == most_recent_day) %>% 
  arrange(desc(Confirmed)) %>% 
  slice(1:6) %>% 
  select(`Province/State`)

pal <- park_palette("Arches")

ppnscale <- FALSE

if(!ppnscale){
  tb %>% 
    filter(`Province/State` %in% c(topstates$`Province/State`)) %>% 
    ggplot(aes(x = Date, y = Confirmed, color = `Province/State`)) + 
    geom_line(size = 1) + 
    geom_line(data = filter(tb, `Province/State` == "Total"), color = "black", size = 2) +
    theme_bw() +
    scale_y_log10() +
    scale_color_manual(values = pal)
} else{
  tb %>% 
    filter(`Province/State` %in% c(topstates$`Province/State`)) %>% 
    ggplot(aes(x = Date, y = `Confirmed per million residents`, color = `Province/State`)) + 
    geom_line(size = 1) + 
    geom_line(data = filter(tb, `Province/State` == "Total"), color = "black", size = 2) +
    theme_bw() +
    scale_y_log10() +
    scale_color_manual(values = pal)
  }
  


