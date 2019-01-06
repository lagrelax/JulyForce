library(plyr)
library(dplyr)
library(TTR)
library(purrr)

ewma.func <- function(rets, lambda) {
  sig.p <- 0
  sig.s <- vapply(rets, function(r) sig.p <<- sig.p*lambda + (r^2)*(1 - lambda), 0)
  return(sqrt(sig.s))
}

set.seed(111)

load('data/filtered_price_dt_all.RData')

# 21 day lag
lag <- 21

# look back window 504 days
look_back <- 504

# half life 126 days, (1-decay)^(halflife-1)=1/2
half_life <- 126
decay <- 1-0.5^(1/(half_life-1))

log_price <- filtered_price_dt_all %>% select(ticker,date,closeunadj) %>% mutate(logP=log(closeunadj))

# This is the data frame used for rolling momentum calculation
log_rtn <- log_price %>% group_by(ticker) %>% arrange(desc(date)) %>% mutate(log1R=c(-diff(logP,1),NA))

log_rtn <- log_rtn %>% arrange(ticker,date)
biz_dates <- sort(unique(log_rtn$date))

start_idx <- look_back+lag            

result <- list()
for(loop_idx in start_idx:length(biz_dates))
{
  loop_date <- biz_dates[loop_idx]
  start_date <- biz_dates[loop_idx-look_back-lag+1]
  end_date <- biz_dates[loop_idx-lag]
  print(paste('Now calculating Momentum at',loop_date, 'Date range',start_date,'--', end_date))
  rtn_df <- log_rtn %>% filter(date>=start_date&date<=end_date)
  
  
  # Check for each ticker, there rtn at the last date is not NA
  rtn_check <- rtn_df %>% group_by(ticker) %>% summarise(max_date=max(date)) %>% left_join(rtn_df,by=c('ticker','max_date'='date'))
  na_ticker <- rtn_check %>% filter(is.na(log1R))
  print(paste('Filter out',nrow(na_ticker),'tickers'))
  
  filtered_tickers <- setdiff(unique(rtn_df$ticker),na_ticker$ticker)
  
  momentum_dt <- filtered_tickers %>% map_df(function(name){
    print(paste('Calcuating ticker',name))
    ticker_rtn_df <- rtn_df %>% filter(ticker==name)
    
    # Fill the NA on the first date as 0
    if(is.na(ticker_rtn_df[1,'log1R']))
      ticker_rtn_df[1,'log1R'] <- 0
    
    ema <- ewma.func(ticker_rtn_df$log1R,1-decay)
    momentum <- ema[length(ema)]
    df <- data.frame(ticker=name,date=loop_date,Momentum=momentum)
  })
  
  result <- rbind(result,momentum_dt)  
  
}
