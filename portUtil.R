#' Title
#'
#' @param univ 
#' @param rank_col 
#' @param cut_off Long the ones greater than 1-cutoff, short the ones smaller than cutoff
#' @param long_short 
#'
#' @return
#' @export
#'
#' @examples
constructPortQtly <- function(univ,rank_col,cut_off=0.025,long_short=F)
{
  long_portion <- univ %>% filter(!!sym(rank_col)>=(1-cut_off)) %>%  select(yearmonth,ticker,rebalance_date) %>% mutate(action='long')
  if(long_short)
    short_portion <- univ %>% filter(!!sym(rank_col)<=(cut_off)) %>%  select(yearmonth,ticker,rebalance_date) %>% mutate(action='long')
  
  if(!long_short)
    port <- long_portion else port <- rbind(long_portion,short_portion)
    
    port <- port %>% mutate(quarter=as.yearqtr(rebalance_date+1))
    
    # Get the returns of all securities in the portfolio, remove those w/o rtns data
    securities <- unique(port$ticker)
    rtn_qtly <- getSecuritesRtn(securities,'qtly')
    # Get the next 3Month Performance for the stock if choose quarterly, TODO: dynamicly choose frequency 
    rtn_qtly <- rtn_qtly %>% filter(quarter == as.yearqtr(rebalance_date %m+% months(3))) %>% plyr::rename(c('quarter'='return_quarter'))
    
    port <- port %>% filter(ticker %in% unique(rtn_qtly$ticker))
    
    # Decide weights (equal weighted)
    port_sum <- port %>% group_by(quarter) %>% summarise(Nlong=sum(action=='long'),Nshort=sum(action=='short'))
    
    port_pos <- port %>% left_join(port_sum) %>% mutate(wgt=ifelse(action=='long',1/Nlong,-1/Nshort))
    
    return(list(port=port_pos,rtn=rtn_qtly))
    
}

summarisePortPerf <- function(port_perf)
{
  tmp <- port_perf %>% mutate(date=as.Date(return_quarter)) 
  perf_xts <- as.xts(tmp[,c('Value',idx)],order.by = tmp$date)
  
  # Avg Rtn (Qtly)
  N=nrow(perf_xts)/4
  
  # Avg Rtn Annually
  port_rtn_annualized <- apply(perf_xts,MARGIN = 2,function(x)prod(1+x)^(1/N)-1)
  rtn_ann <- cbind(data.frame(Statistics='Annual Return'),t(port_rtn_annualized))
  
  # Sd Annually
  sd_ann <- cbind(data.frame(Statistics='Annual Sd'),t(apply(perf_xts,MARGIN = 2,sd)*sqrt(4)))
  
  #Annualzied Sharp Ratio
  sharpe <- SharpeRatio.annualized(perf_xts,scale = 4)
  rownames(sharpe) <- NULL
  sharp_ann <- cbind(data.frame(Statistics='Annual Sharpe (Rf=0%)'),sharpe)
  
  # Summary Table
  summary <- rbind(rtn_ann,sd_ann,sharp_ann)
  charts.PerformanceSummary(perf_xts)
  
  return(summary)
}