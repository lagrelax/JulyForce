library(Quandl)
library(lubridate)
library(dplyr)
library(purrr)

source('utilities.R')

load('Data/All_daily_price.RData')

min_date <- adjust.next(max(price_dt_all$date)+1,'NYSE')
max_date <- as.Date('2019-01-31')
dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

# Take 1 hr to load from QUANDL
system.time(price_dt_all_new <- dates %>% map_df(~Quandl.datatable('SHARADAR/SEP',date=.x,paginate = T)))

save(price_dt_all,file=paste0('Data/All_daily_price','_',max(price_dt_all$date),'.RData'))
price_dt_all <- rbind(price_dt_all,price_dt_all_new)
save(price_dt_all,file='Data/All_daily_price.RData')

load('Data/All_daily_fundamental.RData')
min_date <- adjust.next(max(fundamental_dt_all$date)+1,'NYSE')
max_date <- as.Date('2019-01-31')
dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

system.time(fundamental_dt_all_new <- dates %>% map_df(~Quandl.datatable('SHARADAR/SF1',calendardate=.x,dimension='ARQ',paginate = T)))

save(fundamental_dt_all,file=paste0('Data/All_daily_fundamental','_',max(fundamental_dt_all$date),'.RData'))
fundamental_dt_all <- rbind(fundamental_dt_all,fundamental_dt_all_new)
save(fundamental_dt_all,file='Data/All_daily_fundamental.RData')