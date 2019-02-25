getFactorZscore <- function(factor='pb')
{
  if(!exists('fundamental_dt_all'))
  {
    load(file='Data/All_daily_fundamental.Rdata')
    fundamental_dt_all <<- fundamental_dt_all
  }
  
  # pb ratio z score
  factor_dt_all <- fundamental_dt_all %>% select(ticker,!!sym(factor),calendardate) %>% mutate(yearmonth=as.yearmon(calendardate))
  
  # Z score
  factor_monthly_mean_sd <- factor_dt_all %>% na.omit %>% group_by(yearmonth) %>% summarise(mean=mean(!!sym(factor)),sd=sd(!!sym(factor)))
  
  factor_z_score <- factor_dt_all %>% left_join(factor_monthly_mean_sd) %>% mutate(z_score=(!!sym(factor)-mean)/sd)
  
}

