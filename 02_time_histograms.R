library(tidyverse)
library(chron)


permits <- read_csv('https://data.cityofnewyork.us/resource/tg4x-b46p.csv?$limit=9999999')

# permits <- permits %>% 
#   separate_rows(parkingheld, sep = ', ') %>% 
#   separate(., parkingheld, c('main','cross_st_1'), sep = ' between ') %>% 
#   separate(., cross_st_1, c('cross_st_1', 'cross_st_2'), sep = ' and ')

mins <- c('00:00','15:00','30:00','45:00')
hrs <- c(0:23)

ttimes <- vector()
  
time_func <- function(hrs,mins) {
  for (i in 1:length(hrs)) {
    ifelse(nchar(hrs[i]) < 2,
           hrs[i] <- paste0('0',hrs[i]),
           hrs[i] <- hrs[i])
    for (j in 1:length(mins)) {
      atime <- paste0(hrs[i],':', mins[j])
      ttimes <- c(ttimes, atime)
    }
  }
  # for (t in 1:length(ttimes)) {
  #   ifelse(nchar(ttimes[i])<4,
  #     ttimes[t] <- paste0('0',ttimes[t]),
  #     ttimes[i] <- ttimes[i])
  #   }
  return(ttimes)
}

ttimes <- chron(times=time_func(hrs, mins))



time_hist <- enframe(ttimes, name = NULL) %>% 
  rename(time = value)
