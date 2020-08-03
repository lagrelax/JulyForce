library(rlang)

#' Get the most available list of sp500 prior to rebalance date
#'
#' @param rebalance_date 
#'
#' @return
#' @export
#'
#' @examples
getAvailSPUniv <- function(rebalance_date)
{
  if(!exists('SP_UNIV_ALL'))
  {
    print('Intializting sp500 all constitutents history')
    SP_UNIV_ALL <<- read.csv('Data/SP_Univ_All.csv')
  }
    
  
  most_recent_univ <- SP_UNIV_ALL %>% filter(date<=rebalance_date)
  
  avail_univ <- most_recent_univ %>% filter(date==max(most_recent_univ$date)) %>% mutate(date=rebalance_date)
}

#' Given a univsere with rebalance date, find their most recent fundamental data
#'
#' @param avail_univ Only has universe of one obseration date
#'
#' @return
#' @export
#'
#' @examples
getAvailFundamental <- function(avail_univ)
{
  if(!exists('fundamental_dt_all'))
  {
    print('Initializting all fundamental historical data')
    load('Data/All_qtly_fundamental.RData')
  }
    
  
  rebalance_date <- unique(avail_univ$date)
  avail_fundamental <- fundamental_dt_all %>% filter(ticker%in% avail_univ$ticker) %>% filter(datekey<=rebalance_date)  
  
  max_report <- avail_fundamental %>% group_by(ticker) %>% dplyr::summarise(max_date=max(datekey))
  
  avail_fundamental <- avail_fundamental %>% left_join(max_report,by='ticker') %>% filter(datekey==max_date) %>% select(-max_date) %>% mutate(rebalance_date=rebalance_date)
}

getFactorZscore <- function(factor='pb',rebalance_date)
{
  avail_univ <- getAvailSPUniv(rebalance_date)
  avail_fundamental <- getAvailFundamental(avail_univ)
  
  # pb ratio z score
  factor_all <- avail_fundamental %>% select(ticker,!!sym(factor),rebalance_date) %>% mutate(yearmonth=as.yearmon(rebalance_date)) %>% na.omit
  
  # Z score
  factor_mean=mean(factor_all[,factor])
  factor_std=sd(factor_all[,factor])
  
  factor_z_score <- factor_all %>% mutate(mean=factor_mean,std=factor_std)
  factor_z_score$z_score <- (factor_z_score[,factor]-factor_mean)/factor_std  
  return(factor_z_score)
  
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
calculateIC <- function(fundamental_rank,rtn_mthly,forward=3)
{
  # Calculate cumulative return
  
  
  cum_rtn_all <- split(rtn_mthly,rtn_mthly$ticker) %>% map_df(function(df){
    df <- df %>% as.data.frame %>% mutate(date=as.Date(yearmon))
    df_xts <- as.xts(df$return,order.by = df$date)
    cum_rtn <- apply.rolling(df_xts,width=forward,FUN=function(x) prod(1+x)-1)
    names(cum_rtn)='forward_return'
    cum_rtn <- as.data.frame(cum_rtn)
    # The rolling make the quarter-end as date key, convert it back to quarter start
    cum_rtn$yearmon=as.yearmon(as.Date(rownames(cum_rtn)) %m+% months(-(forward-1)))
    rownames(cum_rtn) <- NULL
    cum_rtn$ticker <- df$ticker
    cum_rtn <- na.omit(cum_rtn)
  })
  
  cum_rtn_all <- cum_rtn_all %>% mutate(yearmon=as.yearmon(yearmon))
  
  # Join with rank data, 1 mth fwd
  fundamental_rank <- fundamental_rank %>% mutate(forward_period=as.yearmon(rebalance_date %m+% months(1)))
  
  zrank_ret_df <- fundamental_rank %>% inner_join(cum_rtn_all,by=c('ticker','forward_period'='yearmon'))
  
  ic_df <- zrank_ret_df %>% group_by(rebalance_date) %>% dplyr::summarise(IC=cor(z_rank,forward_return,method = 'spearman',use='pairwise.complete.obs'))
  
  return(list(detail=zrank_ret_df,ic=ic_df))
}


