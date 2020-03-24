# Makes a timeseries from before 23 March, when they inexplicably changed the column names

get_daily_data <- function(thisday){
  
  read_csv(paste0("../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", thisday)) %>% 
    filter(`Country_Region` == "US",
           `Province_State` != c("Diamond Princess"),
           `Province_State` != c("Grand Princess")) %>% 
    mutate(notstate = str_detect(`Province_State`, ",")) %>% 
    filter(!notstate) %>% 
    group_by(`Province_State`) %>% 
    summarise(Date = max(`Last_Update`),
              Confirmed = sum(Confirmed),
              Deaths = sum(Deaths),
              Recovered = sum(Recovered),
              Date = as_date(Date))
  
}


thesedays <- list.files("../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")

out <- NULL
for(tday in thesedays){
  
  if(tday == "README.md") next
  if(mdy(str_split(tday, ".csv")[[1]][1]) < mdy("03-23-2020")) next
  
  out <- bind_rows(out, get_daily_data(tday))
  
}

out %>% 
  rename(`Province/State` = Province_State) %>% 
  write_csv("data/cleaned-data-20200323-through-today.csv")
