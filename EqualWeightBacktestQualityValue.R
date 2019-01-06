library(dplyr)
library(purrr)
library(xts)
library(tidyr)
library(PerformanceAnalytics)
library(Quandl)

source('utilities.R')

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

load('Data/value_quality.RData')

value_low <- value %>% filter(z_rank<=0.05) %>% select(yearmonth,ticker,calendardate) %>% mutate(action='short')
value_high <- value %>% filter(z_rank>=0.95) %>%  select(yearmonth,ticker,calendardate) %>% mutate(action='long')

value_long_short <- value_high %>% rbind(value_low) %>% mutate(quarter=as.yearqtr(calendardate+1))

value_long_short <- value_high %>% mutate(quarter=as.yearqtr(calendardate+1))

securities <- unique(value_long_short$ticker)
load('Data/All_daily_price.RData')
all_price <- price_dt_all %>% filter(ticker %in% securities)

# Filter out penny stocks when their prices = 0
all_price <- all_price %>% filter(close>0)

price_wide <- all_price %>% select(ticker,date,close) %>% mutate(close=as.numeric(close)) %>% spread(ticker,close)
price_xts <- xts(price_wide[,-1],order.by = price_wide$date)

rtn_xts <- Return.calculate(price_xts,method='discrete')

# Fill the NA rtn with 0, when the stock close, treated as holding cash
rtn_xts[is.na(rtn_xts)] <- 0

rtn_df <- as.data.frame(rtn_xts)
rtn_df$date <- rownames(rtn_df)
rtn_df_long <- rtn_df %>% gather(ticker,return,-date) %>% mutate(date=as.Date(date)) 
qtr_date <- unique(rtn_df[,'date',F]) %>% mutate(date=as.Date(date),quarter=as.yearqtr(date))
rtn_df_long <- rtn_df_long %>% left_join(qtr_date) %>% filter(quarter!='1998 Q4')

rtn_qtly <- rtn_df_long %>% group_by(quarter,ticker) %>% summarise(return=prod(1+return))

value_long_short <- value_long_short %>% filter(ticker %in% unique(rtn_qtly$ticker))

value_sum <- value_long_short %>% group_by(quarter) %>% summarise(Nlong=sum(action=='long'),Nshort=sum(action=='short'))

value_pos <- value_long_short %>% left_join(value_sum) %>% mutate(wgt=ifelse(action=='long',1/Nlong,-1/Nshort))

# Filter out 1998
value_pos <- value_pos %>% filter(calendardate>='1998-12-01')

value_pos_rtn <- value_pos %>% left_join(rtn_qtly,by=c('ticker','quarter'))

rtn_port <- value_pos_rtn %>% group_by(quarter) %>% summarise(return=sum(wgt*return)-1)

ggplot(rtn_port)+geom_line(aes(x=quarter,y=return))