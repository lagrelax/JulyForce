library(Quandl)

Quandl.api_key(Sys.getenv('QUANDL_API_KEY'))

#Fundamentals
tmp <- Quandl.datatable('SHARADAR/SF1', calendardate='1999-06-30', ticker='A')

#Price
system.time(tmp2 <- Quandl.datatable('SHARADAR/SEP',date='2018-07-02',ticker='AMZN',paginate=T))

