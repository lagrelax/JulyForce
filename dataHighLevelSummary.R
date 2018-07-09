library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)

source('utilities.R')

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

# Column description
indicators <- Quandl.datatable('SHARADAR/INDICATORS')
meta_indicators <- indicators %>% filter(table=='TICKERS')

ticker_meta <- Quandl.datatable('SHARADAR/TICKERS',paginate=T)

# Filter out Canadian tickers and other exchanges, 20k ticker left
ticker_domestic <- ticker_meta %>% filter(category=='Domestic' & exchange %in% c('NYSE','NASDAQ'))

# Use first quarter column as first date of price/fundamental for now
# Note some columns first quarter is NA, so have to do following treatment. Those tickers are NOT filtered
tmp <- ticker_domestic %>% filter(!is.na(firstquarter))
min_date <- min(tmp$firstquarter) %>% as.Date

# Most recent month end date
max_date <- as.Date('2018-06-30')

dates <- seq.Date(min_date,max_date,by='day') %>% adjust.previous(cal='NYSE') %>% unique

system.time(price_dt_all <- dates %>% map_df(~Quandl.datatable('SHARADAR/SEP',date=.x,paginate = T)))

# mthly_dates <- seq.Date(min_date+1,max_date+1,by='month')-1
# mthly_dates <- adjust.previous(mthly_dates,cal='NYSE')
# 
# # Get average volumn for each ticker during each month
# N <- length(mthly_dates)
# 
# tickers <- ticker_domestic$ticker
# tmp <- chunk(tickers,getChunkSize(tickers))
# 
# avg_volume_all <- NULL
# for (i in 2:N)
# {
#   start_date <- mthly_dates[i-1]
#   end_date <- mthly_dates[i] 
#   system.time(price_dt <- Quandl.datatable('SHARADAR/SEP',date.gte=start_date,date.lte=end_date,paginate = T))
#   
#   # Somehow still got zero data for some dates. Just skip 
#   if (nrow(price_dt)==0) next
#   
#   avg_volume <- price_dt %>% group_by(ticker) %>% summarise(avg_volume=mean(as.numeric(volume))) %>% mutate(start_date=start_date,end_date=end_date) %>% as.data.frame
#   
#   if(i>2) avg_volume_all <- rbind(avg_volume_all,avg_volume) else avg_volume_all <- avg_volume
# }
# 
# avg_volume_all$end_date <- factor(avg_volume_all$end_date)
# 
# ggplot(avg_volume_all)+geom_boxplot(aes(x=end_date,y=avg_volume)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
