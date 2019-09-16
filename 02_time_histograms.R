library(tidyverse)
library(chron)
library(ggplot2)
library(lubridate)

permits_raw <- read_csv('https://data.cityofnewyork.us/resource/tg4x-b46p.csv?$limit=9999999')



# Create function to count occurrences of permits over time ---------------

## Using 96 for 24 hrs w 15 minute intervals

func_time <- function(start_date, end_date) {
  num_15_min <- as.numeric(floor_date(end_date, "15 minutes") - floor_date(start_date, "15 minutes"), unit = "hours")*4
  
  off_hour <- hour(floor_date(start_date, "15 minutes"))
  off_min <- minute(floor_date(start_date, "15 minutes"))
  off <- off_hour*4 + off_min/15
  
  out <- num_15_min%/%96 + c(rep(1, length.out = num_15_min%%96), rep(0, length.out = 96 - num_15_min%%96))
  
  if (off == 1) {
    out <- c(out[off:96], out[1])
    
  } else if(off != 0) {
    out <- c(out[off:96], out[1:(off-1)])
  }
  
  out
}

# Repeat over dataframe, combine results for each of 96 intervals
times <- purrr::map2(permits_raw$startdatetime, permits_raw$enddatetime, func_time)
all_times <- purrr::reduce(times, `+`, .init = 0)



# Create function to represent the 24-hr clock equivalent of 96 -----------



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
  return(ttimes)
}

ttimes <- time_func(hrs, mins)



# Combine outputs, visualize ----------------------------------------------



time_hist <- as_tibble(cbind(ttimes, all_times)) %>% 
  rename(time = ttimes, num_active_permits = all_times) %>% 
  mutate(num_active_permits = as.numeric(num_active_permits))

time_hist$time <- chron(time_hist$time)

time_hrs_only <- time_hist[grep("00:00", time_hist$time),]
time_hrs_only$hours <- 0:23

ggplot(time_hrs_only, aes(x=hours, y = num_active_permits)) + 
  geom_col(fill="tomato3") +
  xlab("Time of day") + ylab("Number of permits") +
  ggtitle("Number of Permits by Hour") +
  scale_x_continuous(breaks = c(0,6,12,18,23))









# Create date only fields for line graph analysis ------------------------------


func_date <- function(start_date, end_date) {
  duratn <- as.numeric(ceiling_date(end_date, "days") - floor_date(start_date, "days"), units =  "days")
  
  off <- yday(floor_date(start_date, "day"))
  
  ndays <- ifelse(year(floor_date(start_date))%%4 == 0, 366, 366)
  
  out <- duratn%/%ndays + c(rep(1, length.out = duratn%%ndays), rep(0, length.out = ndays - duratn%%ndays))
  
  if (off == 1) {
    out <- c(out[2:ndays], out[1])
  } else if(off != 0) {
    out <- c(out[(off):ndays], out[1:(off-1)])
  }
  
  out
}

#use only dataframe with complete years, to get accurate figures

permits <- read_csv('output_files/permits.csv')
permits_2018 <- filter(permits, startdatetime < as.POSIXct('2019-01-01 00:00:00') & startdatetime >= as.POSIXct('2018-01-01 00:00:00'))

permits_pre_19 <- filter(permits, startdatetime < as.POSIXct('2019-01-01 00:00:00'))


# Repeat over dataframe, combine results for each of 366 intervals (includes leap year)
days <- purrr::map2(permits_2018$startdatetime, permits_2018$enddatetime, func_date)
all_days <- purrr::reduce(days, `+`, .init = rep(0, 366))

#permits_raw[which(map_dbl(days, length) != 366),] %>% View()

months_long <- c('1 Jan','3 Mar', '5 May', "7 Jul", "8 Aug", '10 Oct', '12 Dec')
months_short <- c('4 Apr', '6 Jun',  "9 Sept",  "11 Nov")
days_long <- c(1:31)
days_short <- c(1:30)

mmonths <- vector()

days_func <- function(months,days) {
  for (i in 1:length(months)) {
    for (j in 1:length(days)) {
      amonth <- paste(months[i], days[j])
      mmonths <- c(mmonths, amonth)
    }
  }
  return(mmonths)
}

date_long <- days_func(months_long, days_long)
date_short <- days_func(months_short, days_short)
feb <- paste('2 Feb', 1:29)


date_month <- enframe(c(date_long,date_short,feb), name = NULL) %>% 
  select(order_date = value) %>% 
  mutate(order_date = str_replace(order_date, "\\s", "|")) %>% 
  separate(order_date, into = c("order_char", "date"), sep = "\\|") %>% 
  mutate(month = as.numeric(order_char)) %>% 
  arrange(month) %>% 
  select(-order_char)

date_month$permit_count <- all_days

date_month$char_month <- gsub("([A-Za-z]+).*", "\\1", date_month$date)

# Normalized -- divide by number of years in file
##  2018-2012 == 7

date_month$count_normalized <- date_month$permit_count/7




ggplot(date_month, aes(x=char_month, y=permit_count)) + 
  geom_col(fill="tomato3") +
  xlab("Month") + ylab("Number of Permits") +
  ggtitle("Number of Permits by Month") +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))


ggplot(date_month, aes(x=char_month, y=count_normalized)) + 
  geom_col(fill="tomato3") +
  xlab("Month") + ylab("Average Number of Permits") +
  ggtitle("Average Number of Permits by Month per Year") +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))



 plot(all_days, type = "l")
 

# Top streets -------------------------------------------------------------

permits_2018$count <- 1

top_streets <- aggregate(permits_2018$count,
                        by = list(main_street = permits_2018$main,
                                  cross_1 = permits_2018$cross_st_1,
                                  cross_2 = permits_2018$cross_st_2,
                                  borough = permits_2018$borough),
                        function(x) {sum(x)}) %>% 
  rename(num_permits = x) %>% 
  arrange(desc(num_permits)) %>% 
  mutate(street = paste(main_street, cross_1, cross_2, borough))

top_10 <- top_streets[1:10,]

write_csv(top_10, 'output_files/top_10.csv')

ggplot(top_10, aes(x = street, y = num_permits)) +
  geom_col(fill="tomato3") +
  xlab("Streets") + ylab("Number of Permits") +
  ggtitle("Busiest Streets by Number of Permits, 2018") +
  theme(axis.text.x = element_text(size = 12, angle = 45))
