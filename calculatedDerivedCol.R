library(zoo)
library(dplyr)
library(purrr)

load('Data/All_qtly_fundamental.RData')

head(fundamental_dt_all)

fundamental_derived_all <- fundamental_dt_all %>% mutate(rd_ratio=rnd/ev,cpx_ratio=(capex-depamor)/revenue)

fundamental_dt_all <- fundamental_derived_all

# order by the date
assets_rolling <- fundamental_dt_all %>% select(ticker,calendardate,assets) %>% group_by(ticker) %>% arrange(calendardate,.by_group=T) %>% as.data.frame

tmp <- split(assets_rolling,assets_rolling$ticker) %>% map_df(
  function(x) {
    if(nrow(x)>3)
      x$assetsavg=c(x$assets[1:3],rollapply(x$assets,width=4,mean)) else x$assetsavg = x$assets
    return(x)
  })

fundamental_dt_all$assetsavg <- NULL
fundamental_dt_all <- fundamental_dt_all %>% left_join(select(tmp,-assets),by=c('ticker','calendardate'))

save(fundamental_dt_all,file='Data/All_qtly_fundamental_derived.RData')
