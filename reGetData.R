library(Quandl)
library(lubridate)
library(dplyr)
library(purrr)

source('utilities.R')

min_date <- '1999-01-04' %>% as.Date
min_date <- '2005-04-28' %>% as.Date
max_date <- as.Date(today())
dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

price_dt_all <- NULL
for (x in dates)
{
  x <- as.Date(x)
  print(x)
  tmp <- Quandl.datatable('SHARADAR/SEP',date=x,paginate = T)
  if(is.null(price_dt_all)) price_dt_all <- tmp else price_dt_all <- rbind(price_dt_all,tmp)
}

save(price_dt_all,file='Data/New_All_Daily_Price.RData')


fundamental_dt_all <- NULL
for (x in dates)
{
  x <- as.Date(x)
  print(x)
  tmp <- Quandl.datatable('SHARADAR/SF1',calendardate=x,dimension='ARQ',paginate = T)
  if(is.null(fundamental_dt_all)) fundamental_dt_all <- tmp else fundamental_dt_all <- rbind(fundamental_dt_all,tmp)
}
save(fundamental_dt_all,file='Data/New_All_Fundamental.RData')
