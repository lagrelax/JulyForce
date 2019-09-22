options(stringsAsFactors = F)

library(Quandl)
library(dplyr)
library(purrr)
library(xts)
library(tidyr)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(plotly)

source('utilities.R')

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

load('Data/value_quality.RData')

cut_off <- 0.025
value_low <- value %>% filter(z_rank<=cut_off) %>% select(yearmonth,ticker,calendardate) %>% mutate(action='short')
value_high <- value %>% filter(z_rank>=(1-cut_off)) %>%  select(yearmonth,ticker,calendardate) %>% mutate(action='long')

value_long_short <- value_high %>% rbind(value_low) %>% mutate(quarter=as.yearqtr(calendardate+1))

value_lucky_7 <- value %>% group_by(yearmonth) %>% do(mutate(.,rank=rank(-z_rank))) %>% filter(rank<=7) %>%  select(yearmonth,ticker,calendardate) %>% as.data.frame %>% mutate(action='long',yearmonth=as.yearmon(yearmonth))

value_long_short <- value_high %>% mutate(quarter=as.yearqtr(calendardate+1))
#value_long_short <- value_lucky_7 %>% mutate(quarter=as.yearqtr(calendardate+1))

securities <- unique(value_long_short$ticker)
load('Data/All_daily_price.RData')
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

qtr_date <- unique(rtn_df[,'date',F]) %>% mutate(date=as.Date(date),quarter=as.yearqtr(date))
qtr_date <- qtr_date %>% filter(quarter!='1998 Q4')

rtn_df_long <- rtn_df_long %>% left_join(qtr_date) 

rtn_qtly <- rtn_df_long %>% group_by(quarter,ticker) %>% summarise(return=prod(1+return))

value_long_short <- value_long_short %>% filter(ticker %in% unique(rtn_qtly$ticker))

value_sum <- value_long_short %>% group_by(quarter) %>% summarise(Nlong=sum(action=='long'),Nshort=sum(action=='short'))

num_actions <- length(unique(value_long_short$action))
value_pos <- value_long_short %>% left_join(value_sum) %>% mutate(wgt=ifelse(action=='long',num_actions/Nlong,-1/Nshort))

# Filter out 1998
value_pos <- value_pos %>% filter(calendardate>='1998-12-01')

value_pos_rtn <- value_pos %>% left_join(rtn_qtly,by=c('ticker','quarter'))

rtn_port <- value_pos_rtn %>% group_by(quarter) %>% summarise(Value=sum(wgt*return)-1)

# Market BM
load('Data/R300DailyReturnXTS.RData')

# Convert daily R3000 return to qtly
r3000_df <- r3000_rtn %>% as.data.frame
r3000_df$date <- rownames(r3000_df) %>% as.Date
rownames(r3000_df) <- NULL

r3000_df <- r3000_df %>% left_join(qtr_date) %>% filter(!is.na(quarter))
r3000_qtly_rtn <- r3000_df %>% group_by(quarter) %>% summarise(R3000=prod(1+R3000)-1)

value_perf <- rtn_port %>% inner_join(r3000_qtly_rtn) 

value_perf_long <- value_perf %>% gather(Port,Ret,-quarter)

p <- ggplot(value_perf_long)+geom_line(aes(x=quarter,y=Ret,color=Port))
ggplotly(p)

tmp <- value_perf %>% mutate(date=as.Date(quarter)) 
perf_xts <- as.xts(tmp[,c('Value','R3000')],order.by = tmp$date)


# Avg Rtn (Qtly)
N=nrow(perf_xts)/4
apply(perf_xts,MARGIN = 2,mean)

# Avg Rtn Annually
port_rtn_annualized <- apply(perf_xts,MARGIN = 2,function(x)prod(1+x)^(1/N)-1)
rtn_ann <- cbind(data.frame(Statistics='Annual Return'),t(port_rtn_annualized))

# Sd Annually
apply(perf_xts,MARGIN = 2,sd)*sqrt(4)
sd_ann <- cbind(data.frame(Statistics='Annual Sd'),t(apply(perf_xts,MARGIN = 2,sd)*sqrt(4)))

#Annualzied Sharp Ratio
sharpe <- SharpeRatio.annualized(perf_xts,scale = 4)
rownames(sharpe) <- NULL
sharp_ann <- cbind(data.frame(Statistics='Annual Sharpe (Rf=0%)'),sharpe)

# Summary Table
summary <- rbind(rtn_ann,sd_ann,sharp_ann)
summary
charts.PerformanceSummary(perf_xts)
#write.csv(summary,file='Output/Port_Value_Long_Short_0.025_Summary.csv',row.names = F)
#save(value_pos_rtn,perf_xts,summary,file='Data/Port_Value_Long_Short_0.025.RData')
