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

# Step 1. Get universe
# sp500 all universe 
#univ <- getSP500Univ()
load('Data/SP500_Univ.RData')

# Step 2. Get z-score
# Z score of pb
load(file='Data/All_daily_fundamental.Rdata')
fundamental_dt_all <- fundamental_dt_all %>% filter(ticker!='VRSN')
pb_z_score <- getFactorZscore('pb')

# Step 3. Rank the z-score
# Rank of pb within SP
sp500.all <- unique(univ$ticker)
pb_z_score.sp500 <- pb_z_score %>% filter(ticker %in% sp500.all)

pb_z_score.sp500 <- pb_z_score.sp500 %>% group_by(yearmonth) %>% mutate(z_rank=percent_rank(z_score)) %>% as.data.frame

# Step 4. Construct the port
# Get port weights
univ <- pb_z_score.sp500
cut_off <- 0.025
port_rtn <- constructPortQtly(univ,'z_rank',cut_off,F)
value_pos <- port_rtn$port
rtn_qtly <- port_rtn$rtn

# Step 5. Calculate the returns of the portfolio
# Starting at 2000 Q2 due to data issue
value_pos <- value_pos %>% filter(calendardate>='2000-04-01')
value_pos_rtn <- value_pos %>% left_join(rtn_qtly,by=c('ticker','quarter'))
rtn_port <- value_pos_rtn %>% group_by(quarter) %>% summarise(Value=sum(wgt*(1+return))-1)


# Step 6. Get the market BM
# Convert daily return to qtly
idx='SP500'
bm_qtly_rtn <- getBMRtn(idx,'qtly')

# Step 7. Compare with the BM
value_perf <- rtn_port %>% inner_join(bm_qtly_rtn) 
value_perf_long <- value_perf %>% gather(Port,Ret,-quarter)

p <- ggplot(value_perf_long)+geom_line(aes(x=quarter,y=Ret,color=Port))
ggplotly(p)

summarisePortPerf(value_perf)

pos_attr <- value_pos_rtn %>% mutate(attr=wgt*return)
pp <- ggplot(pos_attr)+geom_bar(aes(x=quarter,y=attr,fill=ticker),stat='identity')
ggplotly(pp)
