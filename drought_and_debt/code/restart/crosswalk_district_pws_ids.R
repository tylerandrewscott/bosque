dinfo_dt = fread('input/twdd_records/district_list_2019-03-08.csv',stringsAsFactors = F,na.strings = "")
dinfo_dt$PWS_ID[dinfo_dt$PWS_ID == "NA"] <- NA
dinfo_dt$District_ID = as.character(dinfo_dt$District_ID)
#dinfo_dt = dinfo_dt[!is.na(PWS_ID)]
setkey(dinfo_dt,District_ID)
dinfo_dt$PWS_ID = str_split(dinfo_dt$PWS_ID,'\\|')
dinfo_dt$District_Type = dinfo_dt$Type
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
keep = c('SUD','FWSD','MUD','WCID')
dinfo_dt$District_Type[!dinfo_dt$District_Type %in% keep] <- 'Other'
dinfo_dt$`First Reported Date` = dmy(dinfo_dt$`First Reported Date`)
#dinfo_dt <- dinfo_dt[dinfo_dt$District_Type %in% keep,]
#dinfo_dt <- dinfo_dt[mdy(dinfo_dt$Created)<mdy('01/01/2006'),]
dinfo_dt <- dinfo_dt[is.na(dinfo_dt$Ended)|mdy(dinfo_dt$Ended)>mdy('01/01/2000'),]
dinfo_dt <- dinfo_dt[!grepl('MWA',dinfo_dt$District_Name),]
dinfo_dt <- dinfo_dt[District_Type!='Other',]
id_crosswalk<-rbindlist(mapply(function(x,y) data.table(District_ID = x,PWS_ID = y),x = dinfo_dt$District_ID,y = dinfo_dt$PWS_ID,SIMPLIFY = F))
saveRDS(id_crosswalk,'drought_and_debt/input/id_crosswalk.RDS')
