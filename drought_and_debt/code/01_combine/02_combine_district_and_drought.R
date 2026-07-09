# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)

drt <- readRDS(committed('dsci_measures.RDS'))
overs <- readRDS(committed('pws_county_overlaps.RDS'))
setnames(drt,c('MapDate','FIPS'),c('DroughtDate','CFIPS'))

drt$CFIPS <- as.character(drt$CFIPS)
overs$CFIPS <- as.character(overs$CFIPS)

pws_dsci = merge(drt[,.(CFIPS,DroughtDate,DSCI)],overs[,.(PWS_ID,CFIPS)],allow.cartesian = T)
pws_dsci <- pws_dsci[,mean(DSCI),by=.(PWS_ID,DroughtDate)]
setnames(pws_dsci,'V1','DSCI')
saveRDS(pws_dsci, committed('pws_drought_weekly.RDS'))
