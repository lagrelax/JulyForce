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
start_date='20000131'
end_date='20200701'

# get historical monthly IC
# first step: get monthly factor zscore using sp500 universe  

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

#system.time(stock_ref <- unique(fundamental_rank$ticker) %>% map_df(~Quandl.datatable('SHARADAR/TICKERS',table='SF1',ticker=.x,paginate = T)))

stock_ref <- read.csv(file='Data/Stock_Ref_All.csv')


ic_list <- calculateIC(fundamental_rank,rtn_all_mthly,forward = 3)

ic_detail <- ic_list$detail

ic <- ic_list$ic

# get stock ref industry and sector and market cap 
category_ref <- stock_ref %>% select(ticker,name,sector,industry,scalemarketcap,scalerevenue)
ic_detail <- ic_detail %>% left_join(category_ref,by='ticker')
write.csv(ic_detail,file='Output/pb_ic_details.csv')

ic_detail_corr <- ic_detail %>% group_by(ticker,name) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'))
ic_sector_rotation <- ic_detail %>% filter(!is.na(sector)) %>% group_by(rebalance_date,sector) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'),stock_cnt = length(ticker)) %>% filter(!is.na(IC))
ic_industry_rotation <- ic_detail %>% filter(!is.na(industry)) %>% group_by(rebalance_date,industry) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'),stock_cnt = length(ticker)) %>% filter(!is.na(IC))
ggplot(ic_sector_rotation)+geom_bar(aes(x=rebalance_date,y=IC,fill=sector),stat = 'identity')
ggplot(ic_sector_rotation)+geom_line(aes(x=rebalance_date,y=IC,color=sector))

ic_sector_rotation_wide <- ic_sector_rotation %>% spread(sector,IC)
ic_industry_rotation_wide <- ic_industry_rotation %>% spread(industry,IC)

write.csv(ic_sector_rotation_wide,file='Output/ic_sector_rotation.csv')
write.csv(ic_industry_rotation_wide,file='Output/ic_industry_rotation.csv')

p <- ggplot(ic)+geom_bar(aes(x=rebalance_date,y=IC),stat = 'identity')

ggplotly(p)