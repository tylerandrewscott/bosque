library(data.table)
library(lubridate)

start_date <- mdy('1/1/2010')
end_date <- mdy('9/1/2024')

tx_restrictions <- readRDS('input/combined_restriction_records.RDS')
tx_restrictions$PWS_ID = paste0('TX',tx_restrictions$`PWS ID`)
tx_restrictions[,`PWS ID`:=NULL]
tx_restrictions = tx_restrictions[!is.na(tx_restrictions$PWS_ID),]
tx_restrictions$Event = NA
tx_restrictions$Event[tx_restrictions$Priority_Numeric %in% c(1,2,3)] <- 1
tx_restrictions$Event[is.na(tx_restrictions$Event)] <- 0
tx_restrictions$Date_Notified  = ymd(tx_restrictions$Notified)
#tx_restrictions = read_csv('bosquebox/input/combined_drought_records.csv') %>% dplyr::select(-Source,-Population,-Connections)
#tx_restrictions = tx_restrictions[!duplicated(paste(tx_restrictions$PWS_ID,tx_restrictions$Obs_Date)),]
#tx_restrictions$`Date Notified`[is.na(tx_restrictions$`Date Notified`)] <- tx_restrictions$`Last Updated`[is.na(tx_restrictions$`Date Notified`)]
#tx_restrictions$Dec_Date = decimal_date(tx_restrictions$Obs_Date) - min(decimal_date(tx_restrictions$Obs_Date))
tx_rest = tx_restrictions
tx_rest = tx_rest[!duplicated(paste(tx_rest$PWS_ID,tx_rest$Date_Notified,tx_rest$STAGE)),]
tx_rest = tx_rest[tx_rest$Event==1,]
tx_rest = tx_rest[tx_rest$Date_Notified>=mdy(start_date),]
seq(start_date,end_date,by = week(1))





