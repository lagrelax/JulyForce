library(zoo)
library(dplyr)
library(purrr)

load('Data/All_qtly_fundamental.RData')


fundamental_derived_all <- fundamental_dt_all %>% mutate(rd_ratio=rnd/ev,cpx_ratio=(capex-depamor)/revenue)

fundamental_dt_all <- fundamental_derived_all

# Before calculate the rolling avg, fill the NA assets
tmp <- fillFundamentalNA(fundamental_dt_all,'assets')

fundamental_dt_all$assets = NULL
# Remove datekey as we remove some for multiple report in the same report date, join on it would cause NAs
tmp$datekey <- NULL
fundamental_dt_all <- fundamental_dt_all %>% left_join(tmp,by=c('ticker','calendardate','reportperiod'))

# The remaining are companies whose assets are all na, drop them
fundamental_dt_all <- fundamental_dt_all %>% filter(!is.na(assets))

# Do the same thing for revenue 
tmp <- fillFundamentalNA(fundamental_dt_all,'revenue')
fundamental_dt_all$revenue = NULL
# Remove datekey as we remove some for multiple report in the same report date, join on it would cause NAs
tmp$datekey <- NULL
fundamental_dt_all <- fundamental_dt_all %>% inner_join(tmp,by=c('ticker','calendardate','reportperiod'))

# order by the date
assets_rolling <- fundamental_dt_all %>% select(ticker,calendardate,assets) %>% group_by(ticker) %>% arrange(calendardate,.by_group=T) %>% as.data.frame

# Calculate rolling avg
tmp <- split(assets_rolling,assets_rolling$ticker) %>% map_df(
  function(x) {
    if(nrow(x)>3)
      x$assetsavg=c(x$assets[1:3],rollapply(x$assets,width=4,mean)) else x$assetsavg = x$assets
    return(x)
  })

fundamental_dt_all$assetsavg <- NULL
fundamental_dt_all <- fundamental_dt_all %>% inner_join(select(tmp,-assets),by=c('ticker','calendardate'))
fundamental_dt_all <- fundamental_dt_all %>% mutate(roa_derived = revenue/assetsavg)

save(fundamental_dt_all,file='Data/All_qtly_fundamental_derived.RData')
