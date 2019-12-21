options(stringsAsFactors = F)

library(Quandl)
library(dplyr)
library(purrr)
library(xts)
library(tidyr)
library(PerformanceAnalytics)
library(ggplot2)
library(plotly)
library(lubridate)
library(quantmod)

source('utilities.R')
source('FactorLibraryV2.R')
source('portUtil.R')

# Step 1, get a series of rebalance dates
start_date='19991231'
#start_date='20180101'
end_date='20190701'

backtest_period <- getMonthEndDates(start_date,end_date) 

factor <- 'pb'

fundamental_rank <- NULL
for (rebalance_date in backtest_period)
{
  rebalance_date <- as.Date(rebalance_date)
  print(paste("Process: ",rebalance_date))
  fundamental_z <- getFactorZscore(factor,rebalance_date) %>% mutate(z_rank=percent_rank(z_score))
  
  fundamental_rank <- rbind(fundamental_rank,fundamental_z)
}

rtn_all_mthly <- getSecuritesRtn(unique(fundamental_rank$ticker),'mthly')

fundamental_rank <- fundamental_rank %>% select(ticker,rebalance_date,yearmonth,z_rank)
