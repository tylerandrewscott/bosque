district_files <- list.files('input/twdd_records/',pattern = 'district_list',full.names = T)
dinfo_dt <- fread(district_files[which.max(file.info(district_files)$mtime)])

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

dinfo_dt$Created = mdy(dinfo_dt$Created)
#dinfo_dt <- dinfo_dt[dinfo_dt$District_Type %in% keep,]
#dinfo_dt <- dinfo_dt[mdy(dinfo_dt$Created)<mdy('01/01/2006'),]
dinfo_dt$Ended[dinfo_dt$Ended=='']<-NA
dinfo_dt <- dinfo_dt[is.na(dinfo_dt$Ended)|mdy(dinfo_dt$Ended)>mdy('01/01/2000'),]
dinfo_dt <- dinfo_dt[!grepl('MWA',dinfo_dt$District_Name),]
dinfo_dt <- dinfo_dt[District_Type!='Other',]

id_crosswalk<-rbindlist(mapply(function(x,y) data.table(District_ID = x,PWS_ID = y),x = dinfo_dt$District_ID,y = dinfo_dt$PWS_ID,SIMPLIFY = F))
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX2490016'] <- '8492000'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX0430053'] <- '5952250'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX0420034'] <- '2312250'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX1900009'] <- '7585150'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX2040033'] <- '7492500'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX2290037'] <- '8070000'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX2360010'] <- '7634575'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX1290010'] <- '995951'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX0940015'] <- '2412188'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX1650133'] <- '5846750'
id_crosswalk$District_ID[id_crosswalk$PWS_ID=='TX0200706'] <- '1636654'

saveRDS(id_crosswalk,'drought_and_debt/input/id_crosswalk.RDS')
