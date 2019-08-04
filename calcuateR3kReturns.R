options(stringsAsFactors = F)

library(xts)
library(PerformanceAnalytics)

r3000 <- read.csv('Data/RUA3.csv')

r3000$Date <- r3000$Date %>% as.Date

r3000_xts <- as.xts(as.numeric(r3000$Adj.Close),order.by = r3000$Date)
names(r3000_xts) <- 'R3000'

r3000_rtn <- Return.calculate(r3000_xts,method='discrete')                    

# Fill NA with 0
r3000_rtn[is.na(r3000_rtn$R3000)] <- 0

save(r3000_rtn,file='Data/R300DailyReturnXTS.RData')

