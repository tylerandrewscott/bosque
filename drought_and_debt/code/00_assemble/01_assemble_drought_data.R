#https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)

# Weekly county DSCI from the U. Nebraska drought API, 2000 through today.
query     <- paste0("startdate=1/1/2000&enddate=", format(Sys.Date(), "%m/%d/%Y"),
                    "&statisticsType=1")
texas_url <- paste0("https://usdmdataservices.unl.edu/api/CountyStatistics/GetDSCI?aoi=TX&",
                    query)
tx_county_dsci <- fread(texas_url)

saveRDS(tx_county_dsci, committed("dsci_measures.RDS"))
