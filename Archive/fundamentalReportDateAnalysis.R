library(Quandl)
library(lubridate)
library(dplyr)
library(purrr)

source('utilities.R')

load('Data/All_qtly_fundamental.RData')

report_summary <- fundamental_dt_all %>% group_by(reportperiod,datekey) %>% dplyr::summarise(N=length(ticker))





