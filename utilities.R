library(timeDate)
library(bizdays)

# Get historical holiday dates for NYSE
ny_holidays <- holidayNYSE(1992:2018)

# Create a calendar exclude holidays and weekends
create.calendar('NYSE',holidays = ny_holidays,weekdays = c('saturday','sunday'))

# adjust.previous(max_date,'NYSE')

# Chunk the list of tickers
chunk <- function(x,n){
  split(x,cut(seq_along(x),n,labels=F))
}

getChunkSize <- function(funds){
  if(length(funds)<85) return(length(funds)) else return(ceiling(length(funds))/85)
}

