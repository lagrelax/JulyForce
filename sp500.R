options(stringsAsFactors = F)

library(Quandl)
library(dplyr)
library(purrr)
library(xts)
library(tidyr)
library(PerformanceAnalytics)
library(ggplot2)
#library(plotly)

source('utilities.R')

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))
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
save(univ,file='Data/SP500.RData')

# rerun value factor using sp500 constituent #
load(file='Data/All_daily_fundamental.Rdata')


# pb ratio z score
pb_dt_all <- fundamental_dt_all %>% select(ticker,pb,calendardate) %>% mutate(yearmonth=as.yearmon(calendardate))

#pb_montly_summary <- pb_dt_all %>% group_by(yearmonth) %>% summarise(Na=sum(is.na(pb)))
#ggplot(pb_montly_summary)+geom_point(aes(x=yearmonth,y=Na))

# Z score of pb
pb_monthly_mean_sd <- pb_dt_all %>% na.omit %>% group_by(yearmonth) %>% summarise(mean=mean(pb),sd=sd(pb))

pb_z_score <- pb_dt_all %>% left_join(pb_monthly_mean_sd) %>% mutate(z_score=(pb-mean)/sd)
pb_z_score <- pb_z_score %>% group_by(yearmonth) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame

# sp500 all universe ## 
sp500.all <- unique(univ$ticker)
pb_z_score.sp500 <- pb_z_score %>% filter(ticker %in% sp500.all)
pb_z_score.sp500.sum <- pb_z_score.sp500 %>% group_by(yearmonth) %>% summarize(ticker_num=length(ticker))
