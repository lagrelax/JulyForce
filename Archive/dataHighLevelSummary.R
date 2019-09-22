library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)

source('utilities.R')

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

# Column description
indicators <- Quandl.datatable('SHARADAR/INDICATORS')
meta_indicators <- indicators %>% filter(table == 'TICKERS')

ticker_meta <- Quandl.datatable('SHARADAR/TICKERS', paginate = T)

# Filter out Canadian tickers and other exchanges, 20k ticker left
ticker_domestic <-
  ticker_meta %>% filter(category == 'Domestic' &
                           exchange %in% c('NYSE', 'NASDAQ'))

# Use first quarter column as first date of price/fundamental for now
# Note some columns first quarter is NA, so have to do following treatment. Those tickers are NOT filtered
tmp <- ticker_domestic %>% filter(!is.na(firstquarter))
min_date <- min(tmp$firstquarter) %>% as.Date

# Most recent month end date
min_date <- as.Date('2018-07-01')
max_date <- as.Date('2019-01-31')
dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

# Take 1 hr to load from QUANDL
system.time(price_dt_all <- dates %>% map_df(~Quandl.datatable('SHARADAR/SEP',date=.x,paginate = T)))

load('Data/All_daily_price.RData')

price_dt_all <- price_dt_all %>% mutate(yearmonth = as.yearmon(date))

ticker_monthly_volume <-
  price_dt_all %>% group_by(ticker, yearmonth) %>% summarise(avg_vol = mean(volume))

monthly_outlier <-
  ticker_monthly_volume %>% group_by(yearmonth) %>% summarise(IQR = quantile(avg_vol, 0.75, na.rm = T) - quantile(avg_vol, 0.25, na.rm = T), N = length(ticker),lower_hinge = quantile(avg_vol,0.25,na.rm=T))

monthly_outlier <-
  monthly_outlier %>% mutate(threshold = lower_hinge - 1.58 * IQR / sqrt(N))

outliers <-
  ticker_monthly_volume %>% left_join(monthly_outlier[, c('yearmonth', 'threshold')]) %>% filter(avg_vol < threshold) %>% mutate(Filter=1)

filtered_price_dt_all <- price_dt_all %>% left_join(outliers %>% select(ticker,yearmonth,Filter))

filtered_price_dt_all[is.na(filtered_price_dt_all$Filter),'Filter'] <- 0

filtered_price_dt_all <- filtered_price_dt_all %>% filter(!Filter)

monthly_count <- filtered_price_dt_all %>% group_by(yearmonth) %>% summarise(N=length(unique(ticker)))

ggplot(monthly_count)+geom_point(aes(x=yearmonth,y=N))

save(filtered_price_dt_all,file='Data/filtered_price_dt_all.RData')
