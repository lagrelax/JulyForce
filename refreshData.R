library(Quandl)
library(lubridate)
library(dplyr)
library(purrr)

source('utilities.R')

load('Data/All_daily_price.RData')

min_date <- adjust.next(max(price_dt_all$date)+1,'NYSE')
max_date <- as.Date(today())
dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

# Take 1 hr to load from QUANDL
system.time(price_dt_all_new <- dates %>% map_df(~Quandl.datatable('SHARADAR/SEP',date=.x,paginate = T)))
price_dt_all$dividends <- NULL
price_dt_all_new <- price_dt_all_new %>% mutate(close=closeadj,closeadj=NULL)

file.rename('Data/All_daily_price.RData',paste0('Data/All_daily_price','_',max(price_dt_all$date),'.RData'))
price_dt_all <- rbind(price_dt_all,price_dt_all_new)
save(price_dt_all,file='Data/All_daily_price.RData')

load('Data/All_qtly_fundamental.RData')
min_date <-as.Date('2000-03-31')+1
max_date <- as.Date(today())
dates <-
  seq.Date(min_date, max_date, by = 'day') 

system.time(fundamental_dt_all_new <- dates %>% map_df(~Quandl.datatable('SHARADAR/SF1',calendardate=.x,dimension='ARQ',paginate = T)))

file.rename('Data/All_qtly_fundamental.RData',paste0('Data/All_qtly_fundamental','_',max(fundamental_dt_all$date),'.RData'))
fundamental_dt_all <- rbind(filter(fundamental_dt_all,calendardate<min_date),fundamental_dt_all_new)
save(fundamental_dt_all,file='Data/All_qtly_fundamental.RData')

sp_univ_all <- read.csv('Data/SP_Univ_All.csv')
sp_univ_new <- getSP500Univ()
max_date <- max(sp_univ_all$date)
sp_univ_net_new <- sp_univ_new %>% filter(date>max_date)

file.rename('Data/SP_Univ_All.csv',paste0('Data/SP_UNIV_ALL_',max_date,'.csv'))
sp_univ_all <- rbind(sp_univ_all,sp_univ_net_new)
write.csv(sp_univ_all,'Data/SP_Univ_All.csv',row.names = F)

# stock ref 
stock_ref_all <- read.csv('Data/Stock_Ref_All.csv')
stock_ref_new <-Quandl.datatable('SHARADAR/TICKERS',table='SF1',paginate = T)
new_ticker <- stock_ref_new %>% filter(!ticker %in% stock_ref_all$ticker)
file.rename('Data/Stock_Ref_All.csv',paste0('Data/SP_UNIV_ALL_BACKUP','.csv'))
stock_ref_all <- rbind(stock_ref_all,new_ticker)
write.csv(stock_ref_all,'Data/Stock_Ref_All.csv',row.names = F)


