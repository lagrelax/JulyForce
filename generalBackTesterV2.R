options(stringsAsFactors = F)

library(Quandl)
library(dplyr)
library(purrr)
library(xts)
library(tidyr)
library(PerformanceAnalytics)
library(ggplot2)
library(plotly)

source('utilities.R')
source('FactorLibraryV2.R')
source('portUtil.R')

# Step 1, get a series of rebalance dates
start_date='19991231'
end_date='20190701'

backtest_period <- getQuarterEndDates(start_date,end_date)

factor <- 'pb'

# For each rebalance date, get the Sp univserse and rank the respective fundamental column
for (rebalance_date in backtest_period)
{
  fundamental_z <- getFactorZscore(factor,rebalance_date) %>% mutate(z_rank=percent_rank(z_score))
  
  # Construct the port
  # Get port weights
  cut_off <- 0.01
  fundamental_z$z_rank <- 1-fundamental_z$z_rank
  port_rtn <- constructPortQtly(fundamental_z,'z_rank',cut_off,F)
  value_pos <- port_rtn$port
  rtn_qtly <- port_rtn$rtn
  
}