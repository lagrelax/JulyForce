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

#' Calculate quarterly Information Coeffiecient
#' 
#' Spearman correlation between rank of all score column and rank of realized returns
#'
#' @param df A data frame with ticker, date as keys
#' @param rtn A data frame with columns: ticker, date and rtn
#' @param foward Forward period of returns, only surpport Monthly for now. Quartely rtn has to be re-cummlated
#' 
#' @return
calculateIC <- function(df,rtn,forward='Monthly')
{
  df_long <- df %>% gather(score,value,-ticker,-date)
  rtn_fwd <- rtn
  
  if(forward=='Monthly')
    rtn_fwd$date2=rtn_fwd$date %m+% months(-1)
  
  rtn_fwd <- rtn_fwd %>% mutate(date=date2) %>% select(-date2)
  ic_df <- df_long %>% inner_join(rtn_fwd,by=c('ticker','date'))
  ic_df <- ic_df %>% group_by(date,score) %>% dplyr::summarise(IC=cor(value,return,method = 'spearman',use='pairwise.complete.obs'))
  ic_df <- ic_df %>% spread(score,IC)
}