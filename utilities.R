options(stringsAsFactors = F)

library(xts)
library(PerformanceAnalytics)
library(timeDate)
library(bizdays)
library(quantmod)

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

# Get historical holiday dates for NYSE
ny_holidays <- holidayNYSE(1992:2100)

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
  sp500 <- Quandl.datatable('SHARADAR/SP500',paginate=T)
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
    if('removed' %in% unique(sp500.cur$action))
      sp500.removed <- sp500.cur %>% filter(action == 'removed') %>% mutate(action='current')
    sp500.cur <- univ.cur %>% filter(!ticker %in% sp500.add$ticker)
    if(exists('sp500.removed'))
      sp500.cur <- rbind(sp500.cur, sp500.removed) 
    
    sp500.cur <- sp500.cur %>% mutate(date=i)
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
  if(idx=='SP500')
  {
    getSymbols.yahoo('^GSPC',env=globalenv(),from='1999-12-31')
    bm_xts <- GSPC[,'GSPC.Adjusted']
  }
  if(idx=='RUS3000')
  {
    getSymbols.yahoo('^RUA',env=globalenv(),from='1999-12-31')
    bm_xts <- RUA[,'RUA.Adjusted']
  }
  
  # SP500 Growth Index ETF
  if(idx=='IVW')
  {
    getSymbols.yahoo('IVW',env=globalenv(),from='1999-12-31')
    bm_xts <- IVW[,'IVW.Adjusted']
  }
  
  bm_rtn <- Return.calculate(bm_xts,method='discrete')
  names(bm_rtn) <- idx
  
  # Fill NA with 0
  bm_rtn[is.na(bm_rtn)] <- 0
  bm_rtn <- as.data.frame(bm_rtn)
  result <- bm_rtn
  
  if(freq=='mthly')
  {
    bm_rtn$date <- rownames(bm_rtn) %>% as.Date
    rownames(bm_rtn) <- NULL
    
    month_date <- unique(bm_rtn[,'date',F]) %>% mutate(date=as.Date(date),yearmon=as.yearmon(date))
    bm_rtn <- bm_rtn %>% left_join(month_date) %>% filter(!is.na(yearmon))
    bm_mthly_rtn <- bm_rtn %>% group_by(yearmon) %>% summarise(Mthly_Rtn=prod(1+!!sym(idx))-1)
    names(bm_mthly_rtn)[2] <- idx
    
    result <- bm_mthly_rtn
  }
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
  
  if(freq=='mthly')
  {
    yearmon <- unique(rtn_df[,'date',F]) %>% mutate(date=as.Date(date),yearmon=as.yearmon(date))
    #qtr_date <- qtr_date %>% filter(quarter!='1998 Q4')
    
    rtn_df_long <- rtn_df_long %>% left_join(yearmon) 
    
    rtn_mthly <- rtn_df_long %>% group_by(yearmon,ticker) %>% summarise(return=prod(1+return)-1)
    
    result <- rtn_mthly  
  }
  
  return(result)
}

#' Given a period dates, find the quarter end dates between them, adjust to previous biz dates
#'
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
getQuarterEndDates <- function(start,end)
{
  start <- as.Date(start,format='%Y%m%d') %m+% months(3)
  end <- as.Date(end,format="%Y%m%d")
  dates <- data.frame(Date=seq(start,end,by='day'))
  dates <- dates %>% mutate(QuarterEnd=as.Date(as.yearqtr(Date))-1)
  qtr_ends <- dates %>% select(QuarterEnd) %>% unique %>% mutate(BizDate=adjust.previous(QuarterEnd,'NYSE'))
  qtr_ends <- qtr_ends$BizDate
}

getMonthEndDates <- function(start,end)
{
  start <- as.Date(start,format='%Y%m%d') %m+% months(1)
  end <- as.Date(end,format="%Y%m%d")
  dates <- data.frame(Date=seq(start,end,by='day'))
  dates <- dates %>% mutate(MonthEnd=as.Date(as.yearmon(Date))-1)
  mth_ends <- dates %>% select(MonthEnd) %>% unique %>% mutate(BizDate=adjust.previous(MonthEnd,'NYSE'))
  mth_ends <- mth_ends$BizDate
}
