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
#start_date='19991231'
start_date='20000131'
end_date='20200701'

backtest_period <- getQuarterEndDates(start_date,end_date)

factor <- 'pb'

# For each rebalance date, get the Sp univserse and rank the respective fundamental column
port_wgt_rtn_ts <- NULL
for (rebalance_date in backtest_period)
{
  rebalance_date <- as.Date(rebalance_date)
  print(paste("Process: ",rebalance_date))
  fundamental_z <- getFactorZscore(factor,rebalance_date) %>% mutate(z_rank=percent_rank(z_score))
  
  # Construct the port
  # Get port weights
  cut_off <- 0.01
  fundamental_z$z_rank <- 1-fundamental_z$z_rank
  port_rtn <- constructPortQtly(fundamental_z,'z_rank',cut_off,F)
  factor_pos <- port_rtn$port
  rtn_qtly <- port_rtn$rtn
  factor_pos_rtn <- factor_pos %>% inner_join(rtn_qtly) 
  port_wgt_rtn_ts <- rbind(port_wgt_rtn_ts,factor_pos_rtn)
}


rtn_port <- port_wgt_rtn_ts %>% group_by(return_quarter) %>% summarise(Value=sum(wgt*(1+return))-1)

# Step 6. Get the market BM
# Convert daily return to qtly
#idx='SP500'
idx='RUS3000'
bm_qtly_rtn <- getBMRtn(idx,'qtly')

# Step 7. Compare with the BM
value_perf <- rtn_port %>% inner_join(bm_qtly_rtn, by=c('return_quarter'='quarter')) 
value_perf_long <- value_perf %>% gather(Port,Ret,-return_quarter)

p <- ggplot(value_perf_long)+geom_line(aes(x=return_quarter,y=Ret,color=Port))
ggplotly(p)

summarisePortPerf(value_perf)

pos_attr <- port_wgt_rtn_ts %>% mutate(attr=wgt*return)
pp <- ggplot(pos_attr)+geom_bar(aes(x=return_quarter,y=attr,fill=ticker),stat='identity')
ggplotly(pp)

