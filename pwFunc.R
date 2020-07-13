library(Quandl)
source('utilities.R')

library(dplyr)
library(Quandl)
library(RcppRoll)
library(ggplot2)
library(purrr)

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

ticker <- 'AAPL'


getCurrentPEPercentile <- function(ticker,value='eps')
{
  price <- Quandl.datatable('SHARADAR/SEP',ticker=ticker,paginate = T)
  fundamental <- Quandl.datatable('SHARADAR/SF1',ticker=ticker,dimension='ARQ')
  
  if(value == 'eps')
    rolling_eps <- fundamental[,c('datekey','epsusd')] %>% mutate(eps=epsusd)
  
  if(value=='ebitda')
    rolling_eps <- fundamental[,c('datekey','ebitda','shareswa')] %>% mutate(eps=ebitda/shareswa)
  
  if(value=='revenue')
    rolling_eps <- fundamental[,c('datekey','revenue','shareswa')] %>% mutate(eps=revenue/shareswa)
  
  rolling_eps <- rolling_eps %>% arrange(datekey) %>% mutate(eps4=roll_sum(eps,4,align='right',fill=NA))
  
  tmp=rolling_eps$datekey
  
  idx <- price$date %>% sapply(function(x) max(which(tmp<=x)))
  
  daily_pe <- price %>% select(date,close) 
  daily_pe$datekey <- tmp[idx]
  
  daily_pe <- daily_pe %>% filter(!is.na(datekey)) %>% left_join(rolling_eps,by='datekey')
  daily_pe <- daily_pe %>% mutate(close=as.numeric(close),pe=close/eps4) %>% na.omit
  
  daily_pe <- daily_pe %>% arrange(desc(date))
  
  if(nrow(daily_pe)==0) return(NULL)
  
  current_pe <- daily_pe[1,]
  
  pe_percentile <- ecdf(daily_pe$pe)
  
  # 0-low, 1-high
  result <- pe_percentile(current_pe$pe)
  if(value=='ebitda')
    daily_pe <- daily_pe %>% mutate(pebitda=pe) %>% select(-pe)
  if(value=='revenue')
    daily_pe <- daily_pe %>% mutate(ps=pe) %>% select(-pe)
  
  return(list(hist=daily_pe,current_percntile=result))
}

tmp <- getCurrentPEPercentile('MU')
hist <- tmp$hist
head(hist)
tmp$current_percntile

# SP_UNIV_ALL <<- getSP500Univ()
# sp_univ <- SP_UNIV_ALL %>% filter(date==max(SP_UNIV_ALL$date))
# sp_univ <- sp_univ %>% filter(ticker!='N/A') %>% filter(!is.na(ticker))
# 
# sp_current_pe_percentile <- sp_univ$ticker %>% map_df(function(x) {
#   print(x)
#   result= getCurrentPEPercentile(x)
#   if(is.null(result)) perct=NA else perct=result$current_percntile
#   data.frame(ticker=x,pe_perct=perct)})

research_dir <- 'C:/Users/lagre/Dropbox/Research'
#write.csv(hist,file=file.path(research_dir,paste0(ticker,'_hist_pe.csv')),row.names = F)
