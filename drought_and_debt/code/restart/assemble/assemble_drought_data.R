#https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp

library(data.table)
library(jsonlite)
library(tigris)

final <- 'startdate=1/1/2000&enddate=1/1/2025&statisticsType=1'
start <- 'https://usdmdataservices.unl.edu/api/'
texas_url <- paste0(start,'CountyStatistics/GetDSCI?aoi=TX&',final)
tx_county_dsci <- fread(texas_url)

saveRDS(tx_county_dsci,'drought_and_debt/input/dsci_measures.RDS')

