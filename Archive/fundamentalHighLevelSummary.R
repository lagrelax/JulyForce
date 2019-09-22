library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggjoy)
library(tidyr)
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
max_date <- as.Date('2018-06-30')

dates <-
  seq.Date(min_date, max_date, by = 'day') %>% adjust.previous(cal = 'NYSE') %>% unique

# ARQ is un-adjusted quarter revision
system.time(fundamental_dt_all <- dates %>% map_df(~Quandl.datatable('SHARADAR/SF1',calendardate=.x,dimension='ARQ',paginate = T)))

save(fundamental_dt_all,file='Data/All_daily_fundamental.RData')

# Look at pb for VALUE first

pb_dt_all <- fundamental_dt_all %>% select(ticker,pb,calendardate) %>% mutate(yearmonth=as.yearmon(calendardate))

pb_montly_summary <- pb_dt_all %>% group_by(yearmonth) %>% summarise(Na=sum(is.na(pb)))
ggplot(pb_montly_summary)+geom_point(aes(x=yearmonth,y=Na))

# Z score of pb
pb_monthly_mean_sd <- pb_dt_all %>% na.omit %>% group_by(yearmonth) %>% summarise(mean=mean(pb),sd=sd(pb))

pb_z_score <- pb_dt_all %>% left_join(pb_monthly_mean_sd) %>% mutate(z_score=(pb-mean)/sd)
pb_z_score <- pb_z_score %>% group_by(yearmonth) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame

to_plot <- pb_z_score
to_plot$yearmonth <- as.factor(to_plot$yearmonth)
ggplot(to_plot)+geom_boxplot(aes(x=yearmonth,y=z_score))
ggplot(to_plot)+geom_boxplot(aes(x=yearmonth,y=z_rank))

low_value <- pb_z_score %>% filter(z_rank<=0.45)
high_value <- pb_z_score %>% filter(z_rank>=0.55)

save(low_value,high_value,file='Data/value_division.RData')
