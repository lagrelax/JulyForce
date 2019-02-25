options(stringsAsFactors = F)

library(xts)
library(PerformanceAnalytics)
library(timeDate)
library(bizdays)

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

# Get historical holiday dates for NYSE
ny_holidays <- holidayNYSE(1992:2018)

# Create a calendar exclude holidays and weekends
create.calendar('NYSE',holidays = ny_holidays,weekdays = c('saturday','sunday'))

# adjust.previous(max_date,'NYSE')

# Chunk the list of tickers
chunk <- function(x,n){
  split(x,cut(seq_along(x),n,labels=F))
}

getChunkSize <- function(funds){
  if(length(funds)<85) return(length(funds)) else return(ceiling(length(funds))/85)
}

getSP500Univ <- function()
{
  # get sp500 constituent #
  sp500 <- Quandl.datatable('SHARADAR/SP500')
  sp500.sum <- sp500 %>% group_by(date,action) %>% summarize(ticker_num = length(ticker))
  sp500.cur <- sp500 %>% filter(action=='current')
  #sp500.0102 <- sp500 %>% filter(date=='2019-01-02')
  #sp500.cur.sub <- sp500.cur %>% filter(ticker %in% sp500.0102$ticker)
  
  dates <- rev(sort(as.Date(unique(sp500$date))))
  univ <- NULL
  univ.cur <- sp500.cur
  for (i in dates)
  {
    sp500.cur <- sp500 %>% filter(date==i) 
    sp500.add <- sp500.cur %>% filter(action == 'added') 
    sp500.removed <- sp500.cur %>% filter(action == 'removed') %>% mutate(action='current')
    sp500.cur <- univ.cur %>% filter(!ticker %in% sp500.add$ticker)
    sp500.cur <- rbind(sp500.cur, sp500.removed) %>% mutate(date=i)
    #metrics.dly <- map_df(sp500.cur$ticker, ~Quandl.datatable('SHARADAR/DAILY',ticker='ZTS'))
    univ.cur <- sp500.cur
    univ <- rbind(univ,sp500.cur)
  }
  
  univ <- univ %>% mutate(date = as.Date(date))
  univ.sum <- univ %>% group_by(date,action) %>% summarize(ticker_num = length(ticker))
  return(univ)
}

getBMRtn <- function(idx='SP500',freq='dly')
{
  # TODO, change to yahoo API
  raw_file <- 'Data/SP500Daily.csv'
  if(idx=='R3000')
    raw_file <- 'Data/RUA3.csv'
  
  bm <- read.csv(raw_file)
  bm$Date <- as.Date(bm$Date)
  
  bm_xts <- as.xts(as.numeric(bm$Adj.Close),order.by = bm$Date)
  bm_rtn <- Return.calculate(bm_xts,method='discrete')
  names(bm_rtn) <- idx
  
  # Fill NA with 0
  bm_rtn[is.na(bm_rtn)] <- 0
  bm_rtn <- as.data.frame(bm_rtn)
  result <- bm_rtn
  
  if(freq=='qtly')
  {
    bm_rtn$date <- rownames(bm_rtn) %>% as.Date
    rownames(bm_rtn) <- NULL
    
    qtr_date <- unique(bm_rtn[,'date',F]) %>% mutate(date=as.Date(date),quarter=as.yearqtr(date))
    
    bm_rtn <- bm_rtn %>% left_join(qtr_date) %>% filter(!is.na(quarter))
    bm_qtly_rtn <- bm_rtn %>% group_by(quarter) %>% summarise(Qtly_Rtn=prod(1+!!sym(idx))-1)
    names(bm_qtly_rtn)[2] <- idx
    
    result <- bm_qtly_rtn
  }
  return(result)
}


getSecuritesRtn <- function(securities,freq='dly')
{
  if(!exists('price_dt_all'))
  {
    load('Data/All_daily_price.RData')
    price_dt_all <<- price_dt_all  
  }
  
  all_price <- price_dt_all %>% filter(ticker %in% securities)
  
  # Filter out penny stocks when their prices = 0
  all_price <- all_price %>% filter(close>0&volume>0)
  
  price_wide <- all_price %>% select(ticker,date,close) %>% mutate(close=as.numeric(close)) %>% spread(ticker,close)
  price_xts <- xts(price_wide[,-1],order.by = price_wide$date)
  
  rtn_xts <- Return.calculate(price_xts,method='discrete')
  
  # Fill the NA rtn with 0, when the stock close, treated as holding cash
  rtn_xts[is.na(rtn_xts)] <- 0
  
  rtn_df <- as.data.frame(rtn_xts)
  rtn_df$date <- rownames(rtn_df)
  rtn_df_long <- rtn_df %>% gather(ticker,return,-date) %>% mutate(date=as.Date(date)) 
  
  result <- rtn_df_long
  
  if(freq=='qtly')
  {
    qtr_date <- unique(rtn_df[,'date',F]) %>% mutate(date=as.Date(date),quarter=as.yearqtr(date))
    qtr_date <- qtr_date %>% filter(quarter!='1998 Q4')
    
    rtn_df_long <- rtn_df_long %>% left_join(qtr_date) 
    
    rtn_qtly <- rtn_df_long %>% group_by(quarter,ticker) %>% summarise(return=prod(1+return)-1)
    
    result <- rtn_qtly  
  }
  
  return(result)
}