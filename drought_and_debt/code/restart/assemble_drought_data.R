#https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp

library(data.table)
library(jsonlite)
library(tigris)
tigris::urban_areas()
urbs <- urban_areas(class = 'sf')


final <- 'startdate=1/1/2000&enddate=1/1/2023&statisticsType=1/json'
start <- 'https://usdmdataservices.unl.edu/api/'
texas_url <- paste0(start,'StateStatistics/GetDSCI?aoi=48&',final)


geoids <- list(houston = urbs[grepl('Houston, TX',urbs$NAME10),]$GEOID,
               dallas_fw = urbs[grepl('Dallas.*Worth',urbs$NAME10),]$GEOID,
               san_antonio = urbs[grepl('San Antonio, TX',urbs$NAME10),]$GEOID,
               el_paso = urbs[grepl('El Paso, TX',urbs$NAME10),]$GEOID,
               austin = urbs[grepl('Austin, TX',urbs$NAME10),]$GEOID)
calls <- paste0(start,'UrbanAreaStatistics/GetDSCI?aoi=',geoids,'&',final)

tx<-fromJSON(texas_url)
urb_sets <- lapply(calls,fromJSON)
urbs_dt <- rbindlist(urb_sets)
dsci_vals <- rbind(tx,urbs_dt,use.names = T, fill = T)

saveRDS(dsci_vals,'drought_and_debt/input/dsci_measures.RDS')

