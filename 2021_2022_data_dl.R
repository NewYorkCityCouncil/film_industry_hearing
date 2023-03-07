library(tidyverse)
library(lubridate)
library(councildown)

# import film permits data
permits <- read_csv("https://data.cityofnewyork.us/resource/tg4x-b46p.csv?$limit=99999999") 

# write to csv for safekeeping
write_csv(permits, file = "permits_mar23.csv")

# Note: first date is 2021-01-04 00:01:00.0, last date is 2022-10-18 17:30:00.0
permits <- permits %>%
  mutate(
    month = month(startdatetime), 
    year = year(startdatetime)
  ) 

# Group by year
permits_year <- permits %>%
  group_by(year) %>%
  summarise(n = n())

# 1472 occur from Oct 18, 2021 - Dec 21, 2021; use this to extrapolate the 2022 missing data
permits %>% 
  filter(startdatetime > '2021-10-18 17:30:00.0' & startdatetime < '2021-12-31 23:59:00.0') %>%
  summarise(n = n())

# 7388 projected in 2022
(1 + (1472 / 6207)) * 5972
  