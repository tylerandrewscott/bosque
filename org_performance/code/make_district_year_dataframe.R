### 
packs = c('rgeos','rgdal','sp','maptools','readxl','hhi','spdep','lubridate','stringr','neatRanges',
          'rvest','R.utils','pbapply','jsonlite','tidyverse','data.table','sf','tigris','lwgeom','tidyquant','readr')
need = packs[!packs %in% installed.packages()[,'Package']]
if(!identical(need,character(0))){sapply(need,install.packages)}
sapply(packs,require,character.only = T)
#if(!require(lucr)){remotes::install_github('Ironholds/lucr');require(lucr)}
year_spread <- c(2000,2022)

if(!file.exists('input/SDWA_latest_downloads/SDWA_VIOLATIONS_ENFORCEMENT.csv')){
  zfile <- 'SDWA_latest_downloads.zip'
  download.file('https://echo.epa.gov/files/echodownloads/SDWA_latest_downloads.zip',destfile = 'input/SDWA_latest_downloads.zip')
  unzip(zfile)
}

update_storm <- F

systems <- fread('input/SDWA_latest_downloads/SDWA_PUB_WATER_SYSTEMS.csv',na.strings = c('NA',""))
systems <- systems[grepl('^TX',systems$PWSID),]
systems <- systems[systems$PWS_ACTIVITY_CODE=='A'|mdy(systems$PWS_DEACTIVATION_DATE)>mdy('01/01/2000'),]
systems <- systems[PWS_TYPE_CODE=='CWS',]
cws_geog <- fread('input/SDWA_latest_downloads/SDWA_GEOGRAPHIC_AREAS.csv',na.strings = '-')
systems$COUNTY_SERVED <- cws_geog$COUNTY_SERVED[match(systems$PWSID,cws_geog$PWSID)]


######### collect PWS data ######
tx_counties <- tigris::counties(state = 'TX',class = 'sf',year= 2017)
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
saveRDS(id_crosswalk,'org_performance/output/id_crosswalk.RDS')

systems$District_ID <- id_crosswalk$District_ID[match(systems$PWSID,id_crosswalk$PWS_ID)]
systems$MASTER_ID <- ifelse(!is.na(systems$District_ID),systems$District_ID,systems$PWSID)

system_keep_vars <- c("PWSID","PWS_NAME","District_ID","MASTER_ID",            
"PWS_DEACTIVATION_DATE","PRIMARY_SOURCE_CODE",         
"IS_WHOLESALER_IND","SERVICE_CONNECTIONS_COUNT",                       
"FIRST_REPORTED_DATE", "COUNTY_SERVED")
systems$IS_WHOLESALER_IND <- (systems$IS_WHOLESALER_IND == 'Y') + 0
systems$PRIMARY_SOURCE_CODE <- grepl("GW",systems$PRIMARY_SOURCE_CODE) + 0
systems$PRIMARY_SOURCE_CODE[is.na(systems$PRIMARY_SOURCE_CODE)]<-1
systems <- systems[,system_keep_vars,with = F]
systems <- Reduce(merge,list(
systems[,min(FIRST_REPORTED_DATE),by=.(MASTER_ID)]%>% rename(FIRST_REPORTED_DATE = V1),
systems[,max(PWS_DEACTIVATION_DATE,na.rm = T),by=.(MASTER_ID)]%>% rename(PWS_DEACTIVATION_DATE = V1),
systems[,sum(SERVICE_CONNECTIONS_COUNT,na.rm = T),by=.(MASTER_ID)]%>% rename(SERVICE_CONNECTIONS_COUNT = V1),
systems[,max(IS_WHOLESALER_IND,na.rm = T),by=.(MASTER_ID)]%>% rename(IS_WHOLESALER_IND = V1),
systems[,max(PRIMARY_SOURCE_CODE,na.rm = T),by=.(MASTER_ID)] %>% rename(PRIMARY_SOURCE_CODE = V1),
systems[order(-SERVICE_CONNECTIONS_COUNT),.(MASTER_ID,COUNTY_SERVED,SERVICE_CONNECTIONS_COUNT)] %>% 
  filter(!duplicated(MASTER_ID)) %>% dplyr::select(MASTER_ID,COUNTY_SERVED)))

ids <- unique(unlist(dinfo_dt$PWS_ID))
#generate query using this tool: https://echo.epa.gov/tools/web-services/facility-search-drinking-water
#system_find <- 'https://echodata.epa.gov/echo/sdw_rest_services.get_systems?p_st=TX&p_act=A'
# res <- fromJSON(system_find)
# qid <- res$Results$QueryID
# return_systems <- fread(paste0('https://echodata.epa.gov/echo/sdw_rest_services.get_download?qid=',qid,'&qcolumns=6,9,10,11,14'))
# setnames(return_systems,'PWSId','PWS_ID')
# setnames(return_systems,'PWSName','PWS_NAME')

viols <- fread('input/SDWA_latest_downloads/SDWA_VIOLATIONS_ENFORCEMENT.csv',na.strings = c('NA',""))
viols <- viols[grepl('^TX',PWSID),]
viols$District_ID <- id_crosswalk$District_ID[match(viols$PWSID,id_crosswalk$PWS_ID)]

### identify unique viols where duplicated row is about different enforcement actions
viols$CALCULATED_RTC_DATE[is.na(viols$CALCULATED_RTC_DATE)]<-Sys.Date()
viols$start <- mdy(viols$NON_COMPL_PER_BEGIN_DATE)
viols$end <- mdy(viols$CALCULATED_RTC_DATE)
viols$MASTER_ID <- ifelse(!is.na(viols$District_ID),viols$District_ID,viols$PWSID)
viols$UQID <- paste(viols$PWSID,viols$VIOLATION_ID,sep = '_')
vmins <- viols[,list(min(start),max(end)),by=.(MASTER_ID,VIOLATION_ID,IS_HEALTH_BASED_IND)]
setnames(vmins,c('V1','V2'),c('start','end'))
weirds <- vmins[start>end,]
vmins2 <- vmins[start<end,]
##### COME BACK TO THIS DATAFRAME BELOW TO MAKE PLOT
vmins2$YEAR <- year(vmins2$start)
saveRDS(vmins2,'org_performance/output/individual_violation_durations.RDS')

temp_comb <- neatRanges::collapse_ranges(vmins2,start_var='start',end_var='end',max_gap = 1,
                                     groups = c('MASTER_ID','IS_HEALTH_BASED_IND'))

temp <- neatRanges::partition_ranges(temp_comb,start_var='start',end_var='end',partition_by = 'year',
                                     vars_to_keep = c('MASTER_ID','IS_HEALTH_BASED_IND'))
temp$YEAR <- year(temp$end)
temp$day_length <- temp$end-temp$start
violdays_by_year <- temp[,sum(day_length),by=.(MASTER_ID,IS_HEALTH_BASED_IND,YEAR)]
violdays_by_year$prop_of_year <- as.numeric(violdays_by_year$V1/365)

temp_comb2 <- neatRanges::collapse_ranges(vmins2,start_var='start',end_var='end',max_gap = 1,
                                         groups = c('MASTER_ID'))
temp_allviols <- neatRanges::partition_ranges(temp_comb2,start_var='start',end_var='end',partition_by = 'year',
                                              vars_to_keep = c('MASTER_ID'))
temp_allviols$YEAR <- year(temp_allviols$end)
temp_allviols$day_length <- temp_allviols$end-temp_allviols$start
allvioldays_by_year <- temp_allviols[,sum(day_length),by=.(MASTER_ID,YEAR)]
allvioldays_by_year$total_prop_of_year <- as.numeric(allvioldays_by_year$V1/365)

grd <- dcast(violdays_by_year,MASTER_ID+YEAR ~ IS_HEALTH_BASED_IND,value.var = 'prop_of_year')
setnames(grd,c('N','Y'),c('nonhealth_prop','health_prop'))
av <- allvioldays_by_year[,.(MASTER_ID,YEAR,total_prop_of_year)]
setnames(av,'total_prop_of_year','total_prop')
setkey(grd,MASTER_ID,YEAR)
setkey(av,MASTER_ID,YEAR)
grd <- av[grd,]


system_grid <- merge(systems,data.table(expand.grid(MASTER_ID = unique(systems$MASTER_ID),YEAR = year_spread[1]:year_spread[2])))
setkey(system_grid,MASTER_ID,YEAR)
setkey(grd,MASTER_ID,YEAR)   

system_grid <- grd[system_grid,]

#system_grid$District_ID<-id_crosswalk$District_ID[match(system_grid$PWSID,id_crosswalk$PWS_ID)]

pws_dt <- system_grid
#system_grid[duplicated(paste(system_grid$MASTER_ID,system_grid$YEAR)),]
#### pws_dt still has non-districts in it, so make sure to filter those out
fig2 <- ggplot(vmins2[!grepl("TX",MASTER_ID)&YEAR>=2000,] %>% 
                 mutate(IS_HEALTH_BASED_IND = ifelse(IS_HEALTH_BASED_IND=='N','Management violations','Health violations')),aes(x = as.factor(YEAR))) + 
  facet_wrap(~IS_HEALTH_BASED_IND,scales = 'fixed') + geom_bar(aes(fill = YEAR %in% 2009:2021)) + theme_bw() + 
  scale_y_continuous(name = '# new violations') + 
  scale_x_discrete(name = 'Reporting year',
                   breaks = seq(2001,2021,5)) + 
  scale_fill_manual(name = 'test',values = c('grey70','grey20'),
                    labels = c('autoregressive model only',
                               'autoregressive + district context models'))+
  ggtitle('Total water district health and management violations') + 
  theme(text = element_text(family ='Times'),
        legend.position = c(0.25,0.6),
        legend.background = element_rect(fill = alpha('white',0.5)),
        legend.title = element_blank())

ggsave(plot = fig2,filename = 'org_performance/output/figure2.png',dpi = 400,width = 6.5,height = 3.75,units = 'in')
ggsave(plot = fig2,filename = 'org_performance/output/figure2.tiff',dpi = 400,width = 6.5,height = 3.75,units = 'in')

cfips = tigris::fips_codes
cfips$CFIPS = paste0(cfips$state_code,cfips$county_code)
cfips$county = gsub(' County','',cfips$county)
cfips = cfips[cfips$state=='TX',]
pws_dt$CFIPS <- paste0('48',cfips$county_code[match(pws_dt$COUNTY_SERVED,cfips$county)])

###### for districts, need to combine service connections and remove district duplicates

pws_dt <- pws_dt[,.(MASTER_ID,YEAR,total_prop,nonhealth_prop,health_prop,FIRST_REPORTED_DATE,PWS_DEACTIVATION_DATE,PRIMARY_SOURCE_CODE,IS_WHOLESALER_IND,SERVICE_CONNECTIONS_COUNT,CFIPS)]

cbsa_ref = read_excel('input/cbsa_delineations.xls',skip = 1) %>% 
  mutate(CFIPS = paste0(formatC(`FIPS State Code`,width = 2,flag = 0),formatC(`FIPS County Code`,width=3,flag=0)))
pws_dt$CBSA_FIPS <- cbsa_ref$`CBSA Code`[match(pws_dt$CFIPS,cbsa_ref$CFIPS)]

cbsa_totals<-pws_dt[,list(.N,sum(SERVICE_CONNECTIONS_COUNT,na.rm = T)),by=.(CBSA_FIPS,YEAR)]
setnames(cbsa_totals,c('N','V2'),c('SYSTEMS_IN_CBSA','CONNECTIONS_IN_CBSA'))
pws_dt <- merge(pws_dt,cbsa_totals,by = c("CBSA_FIPS",'YEAR'))
pws_dt$PROP_CONNECTIONS_OF_CBSA <- (pws_dt$SERVICE_CONNECTIONS_COUNT/pws_dt$CONNECTIONS_IN_CBSA)

hhi_by_cbsa <- pws_dt[,sum({100*PROP_CONNECTIONS_OF_CBSA}^2,na.rm = T),by=.(YEAR,CBSA_FIPS)]
setnames(hhi_by_cbsa ,'V1','CBSA_HHI')
pws_dt <- merge(pws_dt,hhi_by_cbsa)
master_dt<-pws_dt

audits = fread('input/tceq_audits/district_audits.csv')
money = as.vector(which(apply(audits,2,function(x) any(grepl('\\$',x)))))
audits = cbind(audits[,-money,with=F],audits[,lapply(.SD,parse_number),.SDcols = money])
audits$Year = year(mdy(audits$`FISCAL YEAR ENDED`))
audits$`FISCAL YEAR ENDED` <- mdy(audits$`FISCAL YEAR ENDED`)
setnames(audits,"DISTRICT_ID", "District_ID")
setnames(audits,"TOTAL TAX RATE", "Total_Tax_Rate")
audits = audits[, !"YEAR", with=FALSE]  
audits = audits[, !"DISTRICT_NAME", with=FALSE]  
audits$common = paste(audits$District_ID,audits$`FISCAL YEAR ENDED`,sep='_')
audits$zeros = rowSums(audits == 0,na.rm = T)
audits = audits[order(common, -zeros),]
audits = audits[!duplicated(audits, incomparables=FALSE, fromLast=FALSE, by='common'),]
audits$Date = decimal_date(audits$`FISCAL YEAR ENDED`)
audits$Retail_Wastewater = (audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`>0)+0
audits$y = 1
audits$District_ID = as.character(audits$District_ID)
#audits$YEAR <- floor(audits$Date)
#audits = audits[audits$YEAR %in% c(2007:2022),]
#audits$PERIOD= ifelse(floor(audits$Date) %in% c(2009:2011),'2009-2011',ifelse(floor(audits$Date) %in% c(2012:2014),'2012-2014','2015-2017'))
audits$Fund_Balance <- audits$`GENERAL FUND - FUND BALANCE`
audits$Total_Revenue <- audits$`ENTERPRISE FUND - OPERATING REVENUES` + audits$`GENERAL FUND - TOTAL REVENUES`
audits$Total_Expenditure<- audits$`GENERAL FUND - TOTAL EXPENDITURES`+ audits$`ENTERPRISE FUND - OPERATING EXPENSES`
audits$Water_SFU <- audits$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`
audits$Wastewater_SFU <- replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`,0) + 
  replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`,0)

master_dt$join_time <- master_dt$YEAR
setnames(audits,'Date','Audit_Date')
audits$join_time <- audits$Audit_Date

audits$MASTER_ID <- audits$District_ID
setkey(master_dt,MASTER_ID,join_time)
setkey(audits,MASTER_ID,join_time)
master_dt <- audits[master_dt, roll = T]

debt = fread('input/tbrb/Debt_Outstanding_By_Local_Government_UPDATE.csv') 
debt = debt[debt$GovernmentType == 'WD',]
debt$GovernmentName <- str_remove(toupper(debt$GovernmentName),"(\\s|-)DEFINED AREA.*")
debt = debt[,sum(TotalDebtServiceOutstanding),by=.(GovernmentName,FiscalYear)]
setnames(debt,'V1','TotalDebtServiceOutstanding')
iss = fread('input/tbrb/Local_Issuance_UPDATE.csv')
iss = iss[iss$GovernmentType=='WD'&!is.na(iss$NewMoneyPar),]
iss$GovernmentName <- str_remove(toupper(iss$GovernmentName),"(\\s|-)DEFINED AREA.*")
iss = iss[,.(GovernmentName,NewMoneyPar,FiscalYearIssuance )]
iss <- iss[,sum(NewMoneyPar),by=.(FiscalYearIssuance,GovernmentName)]
setnames(iss,c('FiscalYearIssuance','V1'),c('FiscalYear','NewMoney'))
setkey(iss,GovernmentName,FiscalYear)
setkey(debt,GovernmentName,FiscalYear)
debt = iss[debt,]

#debt$GovernmentName = toupper(debt$GovernmentName)
debt$District_Name = debt$GovernmentName
debt$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',debt$District_Name,perl = T)

debt$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',debt$District_Name,perl = T)
debt$District_Name = gsub('\\sNO(\\s[0-9]{1,})','\\1',debt$District_Name,perl = T)


debt$District_Name = gsub(' UD',' UTILITY DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub('SPECIAL UTILITY DISTRICT','SUD',debt$District_Name,perl = T)
debt$District_Name = gsub(' ID$',' IRRIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' ID ',' IRRIGATION DISTRICT ',debt$District_Name,perl = T)
debt$District_Name = gsub('IRRIG DISTRICT','IRRIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' RA$',' RIVER AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' AUTH$',' AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' WA$',' WATER AUTHORITY',debt$District_Name,perl = T)
debt$District_Name = gsub(' DD$',' DRAINAGE DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' DD ',' DRAINAGE DISTRICT ',debt$District_Name,perl = T)
debt$District_Name = gsub(' ND$',' NAVIGATION DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub(' WD$',' WATER DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub('HARRIS-FORT BEND','HARRIS FORT BEND',debt$District_Name,perl = T)
debt$District_Name[grepl('MUD 1$',debt$District_Name)] = ifelse(debt$District_Name[grepl('MUD 1$',debt$District_Name)] %in% dinfo_dt$District_Name,debt$District_Name[grepl('MUD 1$',debt$District_Name)],
                                                                gsub(" 1$",'',debt$District_Name[grepl('MUD 1$',debt$District_Name)]))
debt$District_Name = gsub("ARANSAS COUNTY ND 1","ARANSAS COUNTY NAVIGATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BEEVILLE WSD","BEEVILLE WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BELL COUNTY WCID 2-LITTLE RIVER","BELL COUNTY WCID 2",debt$District_Name,perl = T)
debt$District_Name = gsub("BELMONT FWSD 1","BELMONT FWSD 1 OF DENTON COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("BISTONE MWSD","BISTONE MUNICIPAL WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BRIGHT STAR-SALEM","BRIGHT STAR SALEM",debt$District_Name,perl=T)
debt$District_Name = gsub("BARKER-CYPRESS MUD","BARKER CYPRESS MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("BRAZORIA-FORT BEND COUNTIES MUD","BRAZORIA-FORT BEND COUNTY MUD 1",debt$District_Name,perl = T)
debt$District_Name = gsub("BROOKSHIRE MWD","BROOKSHIRE MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BRUSHY CREEK MUD-DEFINED AREA","BRUSHY CREEK MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("CANADIAN RIVER MWA","CANADIAN RIVER MUNICIPAL WATER AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("CARDINAL MEADOWS WCID","CARDINAL MEADOWS IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CENTRAL WCID","CENTRAL WCID OF ANGELINA COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("CHAMPIONS MUD","CHAMPIONS MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("COMMODORE COVE IRRIGATION DISTRICT","COMMODORE COVE IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CONROE MMD 1"," CONROE MUNICIPAL MANAGEMENT DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("CORYELL CITY WSD","CORYELL CITY WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("CY-CHAMP","CY CHAMP",debt$District_Name,perl = T)
debt$District_Name = gsub("CYPRESS SPINGS SUD","CYPRESS SPRINGS SUD",debt$District_Name,perl = T)
debt$District_Name = gsub("DALLAS COUNTY U&RD","DALLAS COUNTY UTILITY & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8A","DENTON COUNTY FWSD 8-A",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8B","DENTON COUNTY FWSD 8-B",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY FWSD 8C","DENTON COUNTY FWSD 8-C",debt$District_Name,perl = T)
debt$District_Name = gsub("DENTON COUNTY RECL & RD$","DENTON COUNTY RECLAMATION & ROAD DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("DUVAL COUNTY C&RD","DUVAL COUNTY CONSERVATION & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("EASTLAND COUNTY WSD","EASTLAND COUNTY WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("EL PASO COUNTY WID-TORNILLO","EL PASO COUNTY TORNILLO WID",debt$District_Name,perl = T)
debt$District_Name = gsub("FORT HANCOCK WCID 1","FORT HANCOCK WCID",debt$District_Name,perl = T)
debt$District_Name = gsub("GRAND PRAIRIE METROPOLITAN U&RD","GRAND PRAIRIE METROPOLITAN UTILITY & RECLAMATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("GREATER TEXOMA UA","GREATER TEXOMA UTILITY AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("GREEN VALLEY SUD","GREEN VALLEY SPECIAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("HARRIS-FORT BEND","HARRIS FORT BEND",debt$District_Name,perl = T)
debt$District_Name = gsub("HUDSPETH COUNTY C&RD 1","HUDSPETH COUNTY CONSERVATION & RECLAMATION DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("IRVING FCD SECTION 1","IRVING FLOOD CONTROL DISTRICT SECTION 1",debt$District_Name,perl = T)
debt$District_Name = gsub("IRVING FCD SECTION 3","IRVING FLOOD CONTROL DISTRICT SECTION 3",debt$District_Name,perl = T)
debt$District_Name = gsub("JACKRABBIT ROAD PUD","JACKRABBIT ROAD PUBLIC UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("JACKSON COUNTY WCID 2-VANDERBILT","JACKSON COUNTY WCID 2",debt$District_Name,perl = T)
debt$District_Name = gsub("KELLY LANE WCID 1","KELLY LANE WCID 1 OF TRAVIS COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("KELLY LANE WCID 2","KELLY LANE WCID 2 OF TRAVIS COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("LAKE CITIES MUA","LAKE CITIES MUNICIPAL UTILITY AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("LAKE VIEW MANAGEMENT & DEVELOPMENT DISTRICT","LAKE VIEW MANAGEMENT AND DEVELOPMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("LIVE OAK CREEK MUD","LIVE OAK CREEK MUD 1 OF TARRANT COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",debt$District_Name,perl = T)
debt$District_Name = gsub("LONE STAR REGIONAL WATER AUTHORITY","LONE STAR RWA",debt$District_Name,perl = T)
debt$District_Name = gsub("MACKENZIE MWA","MACKENZIE MUNICIPAL WATER AUTHORITY",debt$District_Name,perl = T)
debt$District_Name = gsub("MATAGORDA COUNTY ND 1","MATAGORDA COUNTY NAVIGATION DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("MEEKER MWD","MEEKER MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("MORNINGSTAR RANCH MUD","MORNINGSTAR RANCH MUD 1 OF PARKER COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("NORTHAMPTON MUD","NORTHAMPTON MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("NORTHEAST TEXAS MWD","NORTHEAST TEXAS MUNICIPAL WATER DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("OAKMONT PUD","OAKMONT PUBLIC UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("PALO PINTO COUNTY MWD 1","PALO PINTO COUNTY MUNICIPAL WATER DISTRICT 1",debt$District_Name,perl = T)
debt$District_Name = gsub("POLK COUNTY FWSD 2","POLK COUNTY FRESH WATER SUPPLY DISTRICT 2",debt$District_Name,perl = T)
debt$District_Name = gsub("PROVIDENCE VILLAGE WCID","PROVIDENCE VILLAGE WCID OF DENTON COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("RED RIVER AUTHORITY","RED RIVER AUTHORITY OF TEXAS",debt$District_Name,perl = T)
debt$District_Name = gsub("SEDONA LAKES MUD","SEDONA LAKES MUD 1 OF BRAZORIA COUNTY",debt$District_Name,perl = T)
debt$District_Name = gsub("SPINGS SUD$"," SPRINGS SUD",debt$District_Name,perl = T)
debt$District_Name = gsub("TARRANT REGIONAL WATER DISTRICT","TARRANT REGIONAL WATER DISTRICT A WCID",debt$District_Name,perl = T)
debt$District_Name = gsub("TERRANOVA WEST MUD","TERRANOVA WEST MUNICIPAL UTILITY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("TIMBERLAKE IRRIGATION DISTRICT","TIMBERLAKE IRRIGATION DIST",debt$District_Name,perl = T)
debt$District_Name = gsub("TRINITY BAY CD","TRINITY BAY CONSERVATION DISTRICT",debt$District_Name)
debt$District_Name = gsub("TRINITY RIVER AUTHORITY","TRINITY RIVER AUTHORITY OF TEXAS",debt$District_Name)
debt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 1","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 1",debt$District_Name)
debt$District_Name = gsub("WALLER COUNTY ROAD IRRIGATION DISTRICT 2","WALLER COUNTY ROAD IMPROVEMENT DISTRICT 2",debt$District_Name)
debt$District_Name = gsub("WEST HARRIS COUNTY REGIONAL WATER AUTHORITY","WEST HARRIS COUNTY RWA",debt$District_Name)
debt$District_Name = gsub("WEST JEFFERSON COUNTY MWD","WEST JEFFERSON COUNTY MUNICIPAL WATER DISTRICT",debt$District_Name)
debt$District_Name = gsub("WEST KEEGANS BAYOU IRRIGATION DISTRICT","WEST KEEGANS BAYOU IMPROVEMENT DISTRICT",debt$District_Name)
debt$District_Name = gsub("WILLIAMSON COUNTY WATER SEWER IRRIG & DRAINAGE DISTRICT 3","WILLIAMSON COUNTY WATER SEWER IRRIGATION AND DRAINAGE DIST 3",debt$District_Name)
debt$District_Name = gsub("WOODLANDS METRO CENTER MUD THE","THE WOODLANDS METRO CENTER MUD",debt$District_Name)
debt$District_Name = gsub("WOODLANDS MUD 2","THE WOODLANDS MUD 2",debt$District_Name)
debt$District_Name = gsub("CANEY CREEK MUD","CANEY CREEK MUD OF MATAGORDA COUNTY",debt$District_Name)
debt$District_Name = gsub("D'ARC","DARC",debt$District_Name,perl = T)
debt$District_Name = gsub("HUNTER'S GLEN MUD","HUNTERS GLEN MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("\\sCONS\\s"," CONSOLIDATED ",debt$District_Name,perl = T)
debt$District_Name = gsub("MOORE'S CROSSING MUD","MOORES CROSSING MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("TRAVIS COUNTY WCID 17.*","TRAVIS COUNTY WCID 17",debt$District_Name,perl = T)
debt$District_Name = gsub("SPORTSMAN'S WORLD MUD","SPORTSMANS WORLD MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("PORT O'CONNOR IRRIGATION DISTRICT","PORT OCONNOR IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub('FLYING "L" RANCH PUD','FLYING L PUD',debt$District_Name,perl = T)
debt$District_Name = gsub('TATTOR ROAD MUD','TATTOR ROAD MUNICIPAL DISTRICT',debt$District_Name,perl = T)
debt$District_Name = gsub('POLK COUNTY FRESH WATER SUPPLY DISTRICT 2',"POLK COUNTY FWSD 2",debt$District_Name,perl = T)
debt$District_Name = gsub('(LAKESIDE WCID 2)([A-D])','\\1-\\2',debt$District_Name,perl = T)
debt = debt[debt$FiscalYear%in%year_spread[1]:year_spread[2],]
debt$NewMoney = replace_na(debt$NewMoney,0)
debt$MASTER_ID <- dinfo_dt$District_ID[match(debt$District_Name,dinfo_dt$District_Name)]

debt = debt[!is.na(debt$MASTER_ID),]
debt$join_time <- debt$FiscalYear
setkey(debt,MASTER_ID,join_time)
setkey(master_dt,MASTER_ID,join_time)
master_dt <- debt[,-c('District_Name')][master_dt,]

tceq_geojson='https://opendata.arcgis.com/datasets/e7f6dd0a88c046fba1f54d440941a061_0.geojson'
wd = st_read(tceq_geojson)
wd = st_make_valid(wd)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
wd = st_transform(wd,crs = 3083)

wd <- wd[wd$DISTRICT_I %in% master_dt$District_ID,]
wd$District_ID <- as.character(wd$DISTRICT_I)

#pws_shape_zip_url = 'https://services.twdb.texas.gov/arcgis/rest/directories/arcgisjobs/pws/pwsexport_gpserver/jc914ceef55b44622934c2cbf48763faa/scratch/PWS_Export.zip'
#td = tempdir()
#tf = tempfile(tmpdir=td, fileext=".zip")
#download.file(pws_shape_zip_url, tf)
#fname = unzip(tf, list=TRUE)
#unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
#fpath = file.path(td, grep('shp$',fname$Name,value=T))
fpath <- 'spatial_inputs/Service_Area_Boundaries'
sb <- st_read(fpath)
sb = st_transform(sb,crs = 3083)
sb = st_make_valid(sb)
sb$District_ID  <- id_crosswalk$District_ID[match(sb$PWSId,id_crosswalk$PWS_ID)]
sb <- sb[!is.na(sb$District_ID),]
sb = st_union(sb,by_feature = T)

sb_combo <- sb  %>% 
  st_set_precision(10000) %>% 
  group_by(District_ID) %>% 
  summarise()

wd_simple <- wd[!wd$DISTRICT_ID %in% sb_combo$District_ID,c('DISTRICT_ID')]
wd_simple <- wd_simple[!duplicated(wd_simple$DISTRICT_ID),]
setnames(wd_simple,'DISTRICT_ID','District_ID')

district_sf = rbind(sb_combo,wd_simple)
district_sf$total_area <- st_area(district_sf)

demos = fread('org_performance/input/census_tract_data.csv',stringsAsFactors = F)
demos$GEOID = gsub('.*US','',demos$GEOID)
tracts2010 = tigris::tracts(state = 'TX',class='sf',year = '2010')
tracts2020 = tigris::tracts(state = 'TX',class='sf',year = '2020')
tracts2010$YEAR <- 2010
tracts2020$YEAR <- 2020
colnames(tracts2010) <- str_remove(colnames(tracts2010),'10$')
samecols <- intersect(colnames(tracts2020),colnames(tracts2010))
tracts <- rbind(tracts2010[,samecols],tracts2020[,samecols])
tracts = st_transform(tracts,crs = 3083)
tracts = st_make_valid(tracts)
pws_both_tracts <- lapply(c(2010,2020),function(year){
  print(year)
  pws_tract_inters = st_intersects(district_sf,tracts[tracts$YEAR==year,])
  pws_over_tracts = pblapply(seq_along(pws_tract_inters),
                             function(x) {st_intersection(st_make_valid(district_sf[x,]),tracts[tracts$YEAR==year,][pws_tract_inters[[x]],])})
  pws_tracts = do.call(rbind,pws_over_tracts)
  pws_tracts$Total_Area = district_sf$total_area[match(pws_tracts$District_ID,district_sf$District_ID)]
  pws_tracts$prop_weight = st_area(pws_tracts)/pws_tracts$Total_Area
  pws_tracts$YEAR <- year
  pws_tracts
})

pws_tracts <- do.call(rbind,pws_both_tracts)
total_vars = c('Total_Population','White_Population','Household_Pop','Household_Owner_Occupied','Pop_Over_25','Pop_Bach')
summary_vars = c('Median_Income','Median_Home_Value','Median Year Structure Built')


fill_grid = data.table(expand.grid(District_ID = as.character(unique(master_dt$MASTER_ID)),Year = year_spread[1]:year_spread[2]))

demo_list = pblapply(1:nrow(fill_grid),function(i){
  if(fill_grid$Year[i]<2020){
    temp_weights = pws_tracts[pws_tracts$YEAR==2010&pws_tracts$District_ID== fill_grid$District_ID[i],]}else{
      temp_weights = pws_tracts[pws_tracts$YEAR==2020&pws_tracts$District_ID== fill_grid$District_ID[i],]}
  
  temp_acs = demos[demos$GEOID %in% temp_weights$GEOID & demos$Year == fill_grid$Year[i],]
  temp_acs$Multiplier = as.numeric(temp_weights$prop_weight[match(temp_acs$GEOID,temp_weights$GEOID)])
  if(nrow(temp_acs)==0){tdt = NULL}
  if(nrow(temp_acs)==1){tdt = data.table(t(temp_acs[,sapply(.SD,"*",Multiplier),.SDcols=total_vars]),
                                         temp_acs[,lapply(.SD,weighted.mean,w=Multiplier,na.rm=T), .SDcols = summary_vars],
                                         District_ID = fill_grid$District_ID[i],Year = fill_grid$Year[i])}
  if(nrow(temp_acs)>1){tdt = data.table(t(colSums(data.table(temp_acs[,sapply(.SD,"*",Multiplier),.SDcols=total_vars]))),
                                        temp_acs[,lapply(.SD,weighted.mean,w=Multiplier,na.rm=T), .SDcols = summary_vars],
                                        District_ID = fill_grid$District_ID[i],Year = fill_grid$Year[i])}
  tdt},cl = 4)
demo_dt = rbindlist(demo_list)
names(demo_dt) <- str_replace_all(names(demo_dt),'\\s','_')
setnames(demo_dt,c('Year','District_ID'),c("YEAR",'MASTER_ID'))
setkey(demo_dt,MASTER_ID,YEAR)
setkey(master_dt,MASTER_ID,YEAR)

master_with_demos <- demo_dt[master_dt,]

#keep_ids <- dinfo_dt$District_ID[dinfo_dt$Status=='ACTIVE'&mdy(dinfo_dt$Created)<mdy('01/01/2009')]
#master_with_debts = master_with_debts[master_with_debts$District_ID %in% keep_ids,]
#master_dt = master_dt[master_dt$District_ID %in% pws_dt$District_ID,]

#dsci <- fread('https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-20230306')
dsci <- fread('input/dm_export_20000101_20230316.csv')
dm_df <- dsci %>% rename(CFIPS = FIPS) %>% mutate(CFIPS = as.character(CFIPS))
dm_dt <- as.data.table(dm_df)
dm_dt$Date <- ymd(dm_dt$MapDate)
dm_dt$YEAR <- year(dm_dt$Date)
dm_county_year <- dm_dt[,mean(DSCI),by=.(CFIPS,YEAR)]
# 
# dm_dt$PERIOD_ID <- NA
# dm_dt$Start_ddate <- decimal_date(ymd(dm_dt$ValidStart))
# dm_dt$PERIOD_ID[dm_dt$Start_ddate>2009&dm_dt$Start_ddate<2012] <- 1
# dm_dt$PERIOD_ID[dm_dt$Start_ddate>2012&dm_dt$Start_ddate<2015] <- 2
# dm_dt$PERIOD_ID[dm_dt$Start_ddate>2015&dm_dt$Start_ddate<2018] <- 3
# dm_dt <- dm_dt[!is.na(dm_dt$PERIOD_ID),]
# dm_avgs <- dm_dt[,mean(DSCI),by=.(CFIPS,PERIOD_ID)]

master_with_demos$DSCI_1yr_Weekly_Avg<-dm_county_year$V1[match(paste(master_with_demos$CFIPS,master_with_demos$YEAR),paste(dm_county_year$CFIPS,dm_county_year$YEAR))]

if(update_storm){
noaa_url = 'https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/'
location_nodes <- read_html(noaa_url) %>% html_nodes('a:contains("locations")')
location_hrefs <- location_nodes %>% html_attr('href') %>% paste0(noaa_url,.)
location_hrefs <- grep('200[5-9]|201[0-9]|202[0-2]',location_hrefs,value = T)
detail_nodes <- read_html(noaa_url) %>% html_nodes('a:contains("details")')
detail_hrefs <- detail_nodes %>% html_attr('href') %>% paste0(noaa_url,.)
detail_hrefs <- grep('200[5-9]|201[0-9]|202[0-2]',detail_hrefs,value = T)
storm_dets <- rbindlist(lapply(detail_hrefs,fread),use.names = T,fill = T)
storm_dets <- storm_dets[storm_dets$STATE=='TEXAS',]

storms <-  rbindlist(lapply(location_hrefs,fread),use.names = T,fill = T)
storms$Year <- str_extract(storms$YEARMONTH,'^[0-9]{4}')
storms <- storms[storms$EPISODE_ID %in% storm_dets$EPISODE_ID,]
storms <- storms[!is.na(LATITUDE),]

storms <- storms[EPISODE_ID %in% storm_dets$EPISODE_ID,]

storm_sf <- st_as_sf(storms,coords = c('LONGITUDE','LATITUDE'),crs = 4269)
storm_sf = st_transform(storm_sf ,crs = 3083)
tx_counties <- st_transform(tx_counties,crs = 3083)

storm_inters  = st_covered_by(storm_sf ,tx_counties)
storm_inters[sapply(storm_inters,length)==0]<- NA

storm_sf$CFIPS <- lapply(storm_inters,function(x) tx_counties$GEOID[x])

storm_sf$Year <- as.numeric(str_extract(storm_dets$BEGIN_YEARMONTH[match(storm_sf$EPISODE_ID,storm_dets$EPISODE_ID)],'[0-9]{4}'))
storm_sf <- storm_sf[storm_sf$Year %in% 2005:2023,]
storm_sf$YEAR <- storm_sf$Year
#storm_sf$PERIOD_ID <- ifelse(storm_sf$Year %in% 2009:2011,1,ifelse(storm_sf$Year %in% 2012:2014,2,3))
county_obs = mapply(function(x,y) data.table(CFIPS = x,YEAR = y), x= storm_sf$CFIPS,y = storm_sf$YEAR,SIMPLIFY = F)
county_events_by_period = rbindlist(county_obs)[,.N,by=.(CFIPS,YEAR)]
saveRDS(county_events_by_period,file = 'org_performance/input/county_weather_events.RDS')
}else{county_events_by_period <- readRDS('org_performance/input/county_weather_events.RDS')}

master_with_demos$Severe_Weather_Events <-county_events_by_period$N[match(paste(master_with_demos$CFIPS,master_with_demos$YEAR),paste(county_events_by_period$CFIPS,county_events_by_period$YEAR))]
master_with_demos$Severe_Weather_Events[is.na(master_with_demos$Severe_Weather_Events)]<-0

modeldata_dt <- master_with_demos[!grepl("TX",master_dt$MASTER_ID),]
modeldata_dt$PWS_DEACTIVATION_DATE <- mdy(modeldata_dt$PWS_DEACTIVATION_DATE)
modeldata_dt <- modeldata_dt[is.na(PWS_DEACTIVATION_DATE)|(year(PWS_DEACTIVATION_DATE)>=YEAR),]
modeldata_dt$FIRST_REPORTED_DATE <- mdy(modeldata_dt$FIRST_REPORTED_DATE)
modeldata_dt <- modeldata_dt[is.na(FIRST_REPORTED_DATE)|(year(FIRST_REPORTED_DATE)<=YEAR),]

modeldata_dt$DROP_FROM_FULL_MODEL <- is.na(modeldata_dt$Audit_Date)
modeldata_dt$DROP_FROM_FULL_MODEL <- modeldata_dt$DROP_FROM_FULL_MODEL | floor(modeldata_dt$Audit_Date)==modeldata_dt$YEAR

modeldata_dt[,NUM_PWS_CBSA:=.N,by=.(CBSA_FIPS,YEAR)]
modeldata_dt$TotalDebtServiceOutstanding[is.na(modeldata_dt$TotalDebtServiceOutstanding)]<-0
modeldata_dt$NewMoney[is.na(modeldata_dt$NewMoney)]<-0
modeldata_dt$Prop_Nonwhite = 1 - modeldata_dt$White_Population/modeldata_dt$Total_Population
modeldata_dt$Prop_Bach = modeldata_dt$Pop_Bach/modeldata_dt$Pop_Over_25
modeldata_dt$Debt_Issued_In_Period <- modeldata_dt$NewMoney

vs <- readRDS('org_performance/output/individual_violation_durations.RDS')
vs<-data.table(vs)
vcounts <- dcast(vs[,.N,by=.(YEAR,IS_HEALTH_BASED_IND,MASTER_ID)],MASTER_ID + YEAR ~IS_HEALTH_BASED_IND,value.var = 'N')        
setnames(vcounts,c('N','Y'),c('management_violations','health_violations'))
modeldata_dt <- merge(modeldata_dt,vcounts,all.x = T)
modeldata_dt$health_violations <- replace_na(modeldata_dt$health_violations,0)
modeldata_dt$management_violations <- replace_na(modeldata_dt$management_violations,0)

repls <- c('health_prop','nonhealth_prop','health_violations','management_violations')
modeldata_dt[,(repls):=lapply(.SD,replace_na,0),.SDcols = repls]
vars_to_lag=c("Debt_Issued_In_Period","Total_Revenue"  ,"Fund_Balance" ,
              "TotalDebtServiceOutstanding",'health_prop','nonhealth_prop',
              'health_violations','management_violations')

lag_var_names = paste(vars_to_lag,'L1',sep='_')
modeldata_dt[order(MASTER_ID,YEAR), (lag_var_names ) := data.table::shift(.SD, n = 1,fill= NA,type="lag"), 
             .SDcols=vars_to_lag,by = MASTER_ID]

modeldata_dt$REV_PER_CONNECTION_L1 = modeldata_dt$Total_Revenue_L1/as.numeric(modeldata_dt$SERVICE_CONNECTIONS_COUNT)
modeldata_dt$EXP_PER_CONNECTION_L1 = modeldata_dt$Total_Expenditure_L1/as.numeric(modeldata_dt$SERVICE_CONNECTIONS_COUNT)
modeldata_dt$TotalDebtServiceOutstanding_L1_Per_Connection = as.numeric(modeldata_dt$TotalDebtServiceOutstanding_L1)/as.numeric(modeldata_dt$SERVICE_CONNECTIONS_COUNT)

modeldata_dt$Groundwater<-modeldata_dt$PRIMARY_SOURCE_CODE
modeldata_dt$Wholesaler <- modeldata_dt$IS_WHOLESALER_IND
saveRDS(object = modeldata_dt,file = 'org_performance/input/df_for_model_V2.RDS')

temp <- vmins2[order(start-end),]
#temp <- temp[PWSID %in% pws_dt$PWS_ID,]
temp$order <- nrow(temp):1
start_plot <- mdy('01/01/2000')
temp$start[temp$start<start_plot&temp$end>start_plot]<-start_plot
temp$IS_HEALTH_BASED_IND <- ifelse(temp$IS_HEALTH_BASED_IND=='N','Management violation','Health violation')
#retain_pws <- unlist(dinfo_dt$PWS_ID[dinfo_dt$District_ID %in% modeldata_dt$MASTER_ID])
temp<-temp[end>start_plot,]
temp<-temp[!grepl('TX',MASTER_ID),]

figure3 <- ggplot(temp) + 
  geom_segment(aes(x = start,xend = end,y = order,yend = order)) + 
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),text = element_text(family = 'Times')) + 
  scale_x_date(limits = c(start_plot-days(1),
                          mdy('12/31/2022')+days(1)),expand = c(0,0),
               labels = c(2001,2006,2011,2016,2021),
               breaks=start_plot + c(months(18),months(18+12*5),months(18+12*10),months(18+12*15),months(18+12*20))) + 
  scale_y_continuous(expand=c(0,0),name = 'Violation periods by system') + 
  ggtitle('Water district health and management violations')+
  facet_wrap(~IS_HEALTH_BASED_IND) 


ggsave(plot = figure3,filename = 'org_performance/output/figure3.png',width=6.5,height = 3.75,units = 'in',dpi=400)
ggsave(plot = figure3,filename = 'org_performance/output/figure3.tiff',width=6.5,height = 3.75,units = 'in',dpi=400)

# 
# library(ggthemes)

cbsa_gis <- tigris::core_based_statistical_areas(class = 'sf',cb = T)
texas_core <- cbsa_gis[grepl('(dallas|antonio|houston|austin).*tx',tolower(cbsa_gis$NAME)),]
texas_core <- st_transform(texas_core,st_crs(sb))
which_cbsa <- st_intersects(texas_core,sb)

cbsa_district <- data.table(CBSAFP = rep(texas_core$CBSAFP,sapply(which_cbsa,length)),District_ID = sb$District_ID[unlist(which_cbsa)])
sb_keep <- sb[sb$District_ID %in% cbsa_district$District_ID,]
sb_keep$CBSAFP <- cbsa_district$CBSAFP[match(sb_keep$District_ID,cbsa_district$District_ID)]

urbs <- tigris::urban_areas(cb = F,class = 'sf',year = 2020)
texurbs <- urbs[grepl('(dallas|antonio|houston|austin|worth).*tx',tolower(urbs$NAME10)),]
texurbs <- st_transform(texurbs,st_crs(sb))
texurbs$CBSAFP<-texas_core$CBSAFP[match(str_extract(texurbs$NAME10,'^\\w+'),str_extract(texas_core$NAME,'^\\w+'))]

tx_plc <- tigris::places(state = 'TX',class = 'sf',cb = T)
tx_plc <- tx_plc[grepl('(^dallas|^san antonio|^houston|^austin|^fort worth)',tolower(tx_plc$NAME)),]
tx_plc$nm <- ifelse(tx_plc$NAME %in% c('Fort Worth','Dallas'),c('Dallas-Fort Worth'),tx_plc$NAME)
tx_plc <- tx_plc %>% group_by(nm) %>% summarize()
tx_plc <- st_transform(tx_plc,st_crs(sb))

tx_plc$CBSAFP<-texas_core$CBSAFP[match(str_extract(tx_plc$nm,'^\\w+'),str_extract(texas_core$NAME,'^\\w+'))]

library(ggthemes)
core_geoms<-lapply(1:nrow(tx_plc),function(x){
ggplot() + geom_sf(data = tx_plc[x,],aes(fill = 'Core city limits'),col = NA,lty = 1) + 
    geom_sf(data = sb_keep[sb_keep$CBSAFP %in% tx_plc$CBSAFP[x],],
            aes(fill = 'District service areas'),lty = .8,col = 'black') + 
  theme_map() +  ggtitle(tx_plc[x,]$nm)  + 
    scale_fill_manual(values = c('grey80',colorblind_pal()(8)[3])) + 
        theme(plot.margin=unit(rep(0,4), "cm"))
    })

library(gridExtra)
library(grid)




figure1 <- grid.arrange(core_geoms[[1]]+guides(fill = 'none'),
             core_geoms[[2]]+guides(fill = 'none'),
             core_geoms[[3]] + theme(legend.title = element_blank(),
                                     legend.position = c(0.05,0.05)),
             core_geoms[[4]] +guides(fill = 'none'),
             top=textGrob("Water districts in TX metro areas",hjust = 1))
ggsave(plot = figure1,
       filename = 'org_performance/output/figure1.png',dpi = 450,units = 'in',height = 7,width = 7)
ggsave(plot = figure1,
       filename = 'org_performance/output/figure1.tiff',dpi = 450,units = 'in',height = 7,width = 7)




# ggplot() + geom_sf(data = sb,aes(fill = tot_health_noncompliance*100),colour = NA,lwd = 0.05) + 
#   geom_sf(data = tx_counties,fill = NA,col = 'grey70',lwd=0.1)+
#   theme_map() + 
#   #geom_sf(data = sb,fill = NA,aes(color = 'black'),lwd = 0.05) +
#   scale_fill_viridis_c(option = 'D',direction = 1,name = '% time non-compliant',limits = c(0,50)) +
#   theme(legend.position = c(0.2,0.1),title = element_text(size = 14),
#         legend.text = element_text(size = 12),legend.title = element_text(size = 12),
#         legend.background = element_rect(fill = NA,colour=NA)) + 
#   scale_colour_manual(name = '',values=c('black'),labels = c('District in sample'))+
#   guides(color = guide_legend(override.aes = list(lwd = 1)))+
#   ggtitle('Water district health violations, 2007 to 2022s')
# 
# 

