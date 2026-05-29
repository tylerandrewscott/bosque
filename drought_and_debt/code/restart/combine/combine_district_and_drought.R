library(data.table)

drt <- readRDS('drought_and_debt/input/dsci_measures.RDS')
overs <- readRDS('drought_and_debt/input/pws_county_overlaps.RDS')
setnames(drt,c('MapDate','FIPS'),c('DroughtDate','CFIPS'))

drt$CFIPS <- as.character(drt$CFIPS)
overs$CFIPS <- as.character(overs$CFIPS)

pws_dsci = merge(drt[,.(CFIPS,DroughtDate,DSCI)],overs[,.(PWS_ID,CFIPS)],allow.cartesian = T)
pws_dsci <- pws_dsci[,mean(DSCI),by=.(PWS_ID,DroughtDate)]
setnames(pws_dsci,'V1','DSCI')
saveRDS(pws_dsci,'drought_and_debt/input/pws_drought_weekly.RDS')
