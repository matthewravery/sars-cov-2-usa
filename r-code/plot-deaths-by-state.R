library(tidyverse)
library(janitor)
library(lubridate)
library(directlabels)
library(nationalparkcolors)

tb <- read_csv("data/us-deaths-cleaned.csv")

states <- tb$`Province/State`[tb$`Province/State` != "Total"] %>% unique

most_recent_day <- max(tb$Date)

topstates <- tb %>% 
  filter(`Province/State` %in% states) %>% 
  filter(Date == most_recent_day) %>% 
  arrange(desc(Deaths)) %>% 
  slice(1:6) %>% 
  select(`Province/State`)

pal <- park_palette("Arches")

tb %>% 
  filter(`Province/State` %in% c(topstates$`Province/State`)) %>% 
  ggplot(aes(x = Date, y = Deaths, color = `Province/State`)) + 
  geom_line(size = 1) +
  geom_line(data = filter(tb, `Province/State` == "Total"), color = "black", size = 2) +
  theme_bw() +
  scale_y_log10() +
  scale_color_manual(values = pal)