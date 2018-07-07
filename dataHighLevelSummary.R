library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

ticker_meta <- Quandl.datatable('SHARADAR/TICKERS',paginate=T)

# Too many data to handle. Need to chunk the ticker and do multiple run. 
# For now just use the first 1000 one.
tmp_ticker <- ticker_meta$ticker[1:10]

# First date with records -- 1986-01-01
min_date <- min(ticker_meta$firstpricedate)

# Most recent quarter start date
max_date <- as.Date('2018-07-01')

qtly_dates <- seq.Date(min_date,max_date,by='quarter')

# Get average volumn for each ticker during each quarter
N <- length(qtly_dates)

avg_volume_all <- NULL
for (i in 2:N)
{
  start_date <- qtly_dates[i-1]
  end_date <- qtly_dates[i]
  
  # Will add a multiple call here later to go thorugh all tickers
  price_dt <- Quandl.datatable('SHARADAR/SEP',ticker=tmp_ticker,date.gte='2017-01-01',date.lte='2017-10-30',paginate = T)
  
  # Somehow still got zero data for some dates. Just skip 
  if (nrow(price_dt)==0) next
  
  avg_volume <- price_dt %>% group_by(ticker) %>% summarise(avg_volume=mean(as.numeric(volume))) %>% mutate(start_date=start_date,end_date=end_date) %>% as.data.frame
  
  if(i>2) avg_volume_all <- rbind(avg_volume_all,avg_volume) else avg_volume_all <- avg_volume
}

avg_volume_all$end_date <- factor(avg_volume_all$end_date)

ggplot(avg_volume_all)+geom_boxplot(aes(x=end_date,y=avg_volume)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
