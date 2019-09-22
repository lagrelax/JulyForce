library(Quandl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggjoy)
library(tidyr)
Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

# Column description
indicators <- Quandl.datatable('SHARADAR/INDICATORS')
meta_indicators <- indicators %>% filter(table == 'SF1')

ticker_meta <- Quandl.datatable('SHARADAR/TICKERS', paginate = T)

load(file='Data/All_daily_fundamental.Rdata')

# load fundamental data 

# value factor :

# pb ratio z score
pb_dt_all <- fundamental_dt_all %>% select(ticker,pb,calendardate) %>% mutate(yearmonth=as.yearmon(calendardate))

pb_montly_summary <- pb_dt_all %>% group_by(yearmonth) %>% summarise(Na=sum(is.na(pb)))
ggplot(pb_montly_summary)+geom_point(aes(x=yearmonth,y=Na))

# Z score of pb
pb_monthly_mean_sd <- pb_dt_all %>% na.omit %>% group_by(yearmonth) %>% summarise(mean=mean(pb),sd=sd(pb))

pb_z_score <- pb_dt_all %>% left_join(pb_monthly_mean_sd) %>% mutate(z_score=(pb-mean)/sd)
pb_z_score <- pb_z_score %>% group_by(yearmonth) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame


# quality factor:

# factor 1: free cash flow = (operating cash flow - capital expenditure)/total asset
# ncfo - net cashflow from operating 
# capex - capital expenditure - why negative? below assume negative using indirect method positive using direct method 
# assets - total assets 
# factor 2: gross margin = gp/revenue
# gp - gross profit
# revenue - total sale
# factor 3: accounting accrual
# netinc - net income 
# aa = (netinc - ncfo)/assets


#

# data issues - zero assets or small assets 
quality_factor_dt_all <- fundamental_dt_all %>% select(ticker,ncfo,capex,assets,gp,netinc,revenue,calendardate) %>% mutate(yearmonth=as.yearmon(calendardate),fcf=(ncfo+capex)/assets,gm=gp/revenue,aa=(netinc-ncfo)/assets)
# filter out fcf < -60k to filter the data issues 
quality_factor_dt_all <- subset(quality_factor_dt_all, (assets!= 0)&(revenue!=0))
# filter starting from 1999
quality_factor_dt_all <- subset(quality_factor_dt_all, calendardate > '1998-12-31')
# filling the na with median 
quality_factor_montly_median <- quality_factor_dt_all %>% group_by(yearmonth) %>% summarise(fcf_median = median(fcf, na.rm=T),gm_median = median(gm, na.rm=T),aa_median=median(aa, na.rm=T))
quality_factor_dt_all <- quality_factor_dt_all %>% left_join(quality_factor_montly_median) %>% mutate(fcf=ifelse(is.na(fcf),fcf_median,fcf), gm=ifelse(is.na(gm),gm_median,gm), aa=ifelse(is.na(aa),aa_median,aa))

quality_factor_montly_summary <- quality_factor_dt_all %>% group_by(yearmonth) %>% summarise(fcf_na=sum(is.na(fcf)),gm_na=sum(is.na(gm)),aa_na=sum(is.na(aa)))
#ggplot(quality_factor_montly_summary)+geom_point(aes(x=yearmonth,y=fcf_na))

quality_factor_dt_sub <- subset(quality_factor_dt_all, select = c('ticker','yearmonth','fcf','gm','aa'))
quality_factor_dt_sub <- quality_factor_dt_sub %>% gather(Factor,Value,-ticker,-yearmonth)

# Z score of fcf 
quality_factor_monthly_mean_sd <- quality_factor_dt_sub %>% na.omit %>% group_by(yearmonth,Factor) %>% summarise(mean=mean(Value),sd=sd(Value))
quality_factor_zscore <- quality_factor_dt_sub %>% left_join(quality_factor_monthly_mean_sd) %>% mutate(z_score=(Value-mean)/sd)
#quality_factor_zscore <- quality_factor_zscore %>% group_by(yearmonth,Factor) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame
quality_factor_weighted_zscore <- quality_factor_zscore %>% group_by(ticker,yearmonth) %>% summarise(z_score=mean(z_score))
quality_factor_weighted_zscore <- quality_factor_weighted_zscore %>% group_by(yearmonth) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame

# choose one ticker 
one_ticker <- subset(quality_factor_weighted_zscore,ticker=='MSFT')
ggplot(one_ticker)+geom_point(aes(x=yearmonth,y=z_rank))

value <- pb_z_score
quality <- quality_factor_weighted_zscore

# Momentum Factor - Barra Momentum Factor # 
# get excess return first Russell 3000 index 


# ewma.func <- function(rets, lambda) {
#   sig.p <- 0
#   sig.s <- vapply(rets, function(r) sig.p <<- sig.p*lambda + (r^2)*(1 - lambda), 0)
#   return(sqrt(sig.s))
# }
# 
# ewma.loop <- function(rets, lambda) {
#   n <- length(rets)+1
#   sig.s <- rep(0, n)
#   for (i in 2:n) {
#     sig.s[i] <- sig.s[i-1]*lambda + (rets[i-1]^2)*(1 - lambda)
#   }
#   return(sqrt(tail(sig.s, n-1)))
# }
# 
# 
# lambda <- 0.94
# rets <- 0.02*rnorm(100)
# system.time( replicate(10000, ewma.loop(rets, lambda)) )
# system.time( replicate(10000, ewma.func(rets, lambda)) )

