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
end_date='20210306'

# get historical monthly IC
# first step: get monthly factor zscore using sp500 universe  

backtest_period <- getMonthEndDates(start_date,end_date) 

factor <- 'evebitda'

fundamental_rank <- NULL
adj_fundamental_all <- NULL

# TODO: Make it incremental 
for (rebalance_date in backtest_period)
{
  rebalance_date <- as.Date(rebalance_date)
  print(paste("Process: ",rebalance_date))
  fundamental_z <- getFactorZscore(factor,rebalance_date) %>% mutate(z_rank=percent_rank(z_score))
  adj_fundamental <- getadjfundamental(rebalance_date)
  fundamental_rank <- rbind(fundamental_rank,fundamental_z)
  adj_fundamental_all <- rbind(adj_fundamental_all,adj_fundamental)
  
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
# get marketcap from fundaemtnal data 
marketcap <- adj_fundamental_all[,c('rebalance_date','ticker','marketcap')]
ic_detail <- ic_detail %>% left_join(category_ref,by='ticker')
ic_detail <- ic_detail %>% left_join(marketcap, by=c('rebalance_date','ticker'))

output_dir <- file.path('Output',factor)

if(!dir.exists(output_dir)) dir.create(output_dir)

write.csv(ic_detail,file=file.path(output_dir,paste0(factor,'_ic_details.csv')))
# calculate median, average zscore, forward_return 
ic_detail_sector_stats <- ic_detail %>% group_by(rebalance_date,sector) %>% summarise(z_rank_median = median(z_rank,na.rm=T),
                                                                                      z_rank_avg = mean(z_rank,na.rm=T),
                                                                                      z_rank_mktcap = sum(z_rank*marketcap,na.rm=T)/sum(marketcap,na.rm=T),
                                                                                      forward_return_median = median(forward_return,na.rm=T),
                                                                                      forward_return_avg = mean(forward_return,na.rm=T),
                                                                                      forward_return_mktcap = sum(forward_return*marketcap,na.rm=T)/sum(marketcap,na.rm=T))

write.csv(ic_detail_sector_stats,file=file.path(output_dir,paste0(factor,'_z_sector_stats.csv')))


ic_detail_corr <- ic_detail %>% group_by(ticker,name) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'))
ic_sector_rotation <- ic_detail %>% filter(!is.na(sector)) %>% group_by(rebalance_date,sector) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'),stock_cnt = length(ticker)) %>% filter(!is.na(IC))
ic_industry_rotation <- ic_detail %>% filter(!is.na(industry)) %>% group_by(rebalance_date,industry) %>% summarize(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'),stock_cnt = length(ticker)) %>% filter(!is.na(IC))
ggplot(ic_sector_rotation)+geom_bar(aes(x=rebalance_date,y=IC,fill=sector),stat = 'identity')
ggplot(ic_sector_rotation)+geom_line(aes(x=rebalance_date,y=IC,color=sector))

ic_sector_rotation_wide <- ic_sector_rotation %>% select(-stock_cnt) %>% spread(sector,IC)
ic_industry_rotation_wide <- ic_industry_rotation %>% select(-stock_cnt) %>% spread(industry,IC)

ic_sector_rotation_cnt <- ic_sector_rotation %>% select(-IC) %>% spread(sector,stock_cnt)
ic_industry_rotation_cnt <- ic_industry_rotation %>% select(-IC) %>% spread(industry,stock_cnt)

write.csv(ic_sector_rotation_wide,file=file.path(output_dir,'ic_sector_rotation.csv'),row.names = F)
write.csv(ic_industry_rotation_wide,file=file.path(output_dir,'ic_industry_rotation.csv'),row.names = F)
write.csv(ic_sector_rotation_cnt,file=file.path(output_dir,'ic_sector_cnt.csv'),row.names = F)
write.csv(ic_industry_rotation_cnt,file=file.path(output_dir,'ic_industry_cnt.csv'),row.names = F)

write.csv(ic_detail,file=file.path(output_dir,'ic_detail.csv'),row.names = F)

ggplot(ic)+geom_bar(aes(x=rebalance_date,y=IC),stat = 'identity')
write.csv(ic,file=file.path(output_dir,paste0(factor,'_ic.csv')),row.names=F)
