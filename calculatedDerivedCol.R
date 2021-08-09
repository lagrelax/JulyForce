library(zoo)
library(dplyr)
library(purrr)

load('Data/All_qtly_fundamental.RData')

head(fundamental_dt_all)

fundamental_derived_all <- fundamental_dt_all %>% mutate(rd_ratio=rnd/ev,cpx_ratio=(capex-depamor)/revenue)

fundamental_dt_all <- fundamental_derived_all


# Before calculate the rolling avg, fill the NA assets

# Note for the same reportperiod/calendardate, there may be multple records for the same company due to revision etc. E.g. AIRO at 2000-03-31. To avoid look-fwd bias, we decided to use the earliest reported observation. However, when the required data is missing at the first record, using those aviable from the later datekeys in an early to late order. After than, we then interpolate any remaining missing values

assets <- fundamental_dt_all %>% select(ticker,datekey,calendardate,reportperiod,assets)

tmp <- assets %>% group_by(ticker,calendardate,reportperiod) %>% mutate(IsNa=is.na(assets)) %>% mutate(earlist=min(datekey),latest=max(datekey))

tmp <- tmp %>% filter((IsNa&(earlist==datekey)) |(!IsNa)&(datekey==latest))

to_interp <- tmp %>% select(-IsNa,-earlist,-latest)

to_interp <- to_interp %>% arrange(ticker,calendardate,reportperiod,datekey)

# Get rid of tickers with all Nas
tmp <- to_interp %>% group_by(ticker) %>% dplyr::summarise(N1=length(calendardate),N2=sum(is.na(assets))) %>% filter(N1==N2)
to_interp <- to_interp %>% filter(!(ticker %in% tmp$ticker))

tmp <- split.data.frame(to_interp,to_interp$ticker) %>% map_df(function(df){
  print(unique(df$ticker))
  df[is.na(df$assets),'assets'] = 0
  # Skip when only one record of 0
  if(nrow(df)==1 & df[1,'assets']==0) return(NULL)
  df[df$assets==0,'assets'] = NA
  x <- zoo(df$assets,df$datekey)
  y <- na.approx(x,na.rm=F)
  extrap_x = index(y[is.na(y)]) 
  # Extrapolate when needed
  if(length(extrap_x)>0)
  {
    y_val = y[!is.na(y)]
    if(length(y_val==1))
    {
      # Can't extrap when only one value available
      y[extrap_x] <- y_val
    } else
    {
      extrap_y = Hmisc::approxExtrap(as.numeric(index(y[!is.na(y)])),y_val,as.numeric(extrap_x))
      y[extrap_x] <- extrap_y$y    
    }
  }
  df$assets <- y %>% as.numeric
  return(df)
})

fundamental_dt_all$assets = NULL
fundamental_dt_all <- fundamental_dt_all %>% left_join(tmp,by=c('ticker','datekey','calendardate','reportperiod'))

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
fundamental_dt_all <- fundamental_dt_all %>% left_join(select(tmp,-assets),by=c('ticker','calendardate'))
fundamental_dt_all <- fundamental_dt_all %>% mutate(roa_derived = revenue/assetsavg)

save(fundamental_dt_all,file='Data/All_qtly_fundamental_derived.RData')
