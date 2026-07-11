# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)

drt <- readRDS(committed('dsci_measures.RDS'))
overs <- readRDS(committed('pws_county_overlaps.RDS'))
setnames(drt,c('MapDate','FIPS'),c('DroughtDate','CFIPS'))

drt$CFIPS <- as.character(drt$CFIPS)
overs$CFIPS <- as.character(overs$CFIPS)

# Weekly PWS-level DSCI = county DSCI averaged over the counties a system
# overlaps, weighted by the share of the system's area in each county
# (Prop_Over_County from 02_combine_pws_with_counties.R). weighted.mean()
# renormalizes, so the sub-1 totals left by sliver-dropping are fine.
pws_dsci = merge(drt[,.(CFIPS,DroughtDate,DSCI)],overs[,.(PWS_ID,CFIPS,Prop_Over_County)],allow.cartesian = T)
pws_dsci <- pws_dsci[,weighted.mean(DSCI, w = Prop_Over_County),by=.(PWS_ID,DroughtDate)]
setnames(pws_dsci,'V1','DSCI')
saveRDS(pws_dsci, committed('pws_drought_weekly.RDS'))
