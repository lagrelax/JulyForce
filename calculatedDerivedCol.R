library(dplyr)

load('Data/All_qtly_fundamental.RData')

head(fundamental_dt_all)

fundamental_derived_all <- fundamental_dt_all %>% mutate(rd_ratio=rnd/ev,cpx_ratio=(capex-depamor)/revenue)

fundamental_dt_all <- fundamental_derived_all

save(fundamental_dt_all,file='Data/All_qtly_fundamental_derived.RData')
