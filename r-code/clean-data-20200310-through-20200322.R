# Makes a timeseries from before 23 March, when they inexplicably changed the column names

get_daily_data_old <- function(thisday){
  
  read_csv(paste0("../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", thisday)) %>% 
    filter(`Country/Region` == "US",
           `Province/State` != c("Diamond Princess"),
           `Province/State` != c("Grand Princess")) %>% 
    mutate(notstate = str_detect(`Province/State`, ",")) %>% 
    filter(!notstate) %>% 
    group_by(`Province/State`) %>% 
    summarise(Date = max(`Last Update`),
              Confirmed = sum(Confirmed),
              Deaths = sum(Deaths),
              Recovered = sum(Recovered),
              Date = as_date(Date))
  
}


thesedays <- list.files("../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")

out <- NULL
for(tday in thesedays){
  
  if(tday == "README.md") next
  if(mdy(str_split(tday, ".csv")[[1]][1]) < mdy("03-10-2020")) next
  if(mdy(str_split(tday, ".csv")[[1]][1]) > mdy("03-22-2020")) next
  
  out <- bind_rows(out, get_daily_data_old(tday))
  
}

write_csv(out, "data/cleaned-data-20200310-through-20200322.csv")