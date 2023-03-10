
packs = c('rgeos','rgdal','sp','maptools','readxl','hhi','spdep','lubridate',
          'rvest','R.utils','pbapply','jsonlite','tidyverse','data.table','sf','tigris','lwgeom','tidyquant')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)
sapply(packs,require,character.only = T)
#if(!require(lucr)){remotes::install_github('Ironholds/lucr');require(lucr)}

fl <- list.files('input/',recursive = T,full = T)
grep('district_list',fl,value = T)

dinfo_dt = fread('input/twdd_records/district_list_2019-03-08.csv',stringsAsFactors = F,na.strings = "")
dinfo_dt$PWS_ID[dinfo_dt$PWS_ID == "NA"] <- NA
dinfo_dt$District_ID = as.character(dinfo_dt$District_ID)

install.packages('sf')
install.packages('units', configure.args = c('--with-udunits2-include=~/homebrew/bin/udunits2'))

dinfo_dt = dinfo_dt[!is.na(PWS_ID)]
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
dinfo_dt <- dinfo_dt[dinfo_dt$District_Type %in% keep,]
dinfo_dt <- dinfo_dt[mdy(dinfo_dt$Created)<mdy('01/01/2010'),]
dinfo_dt <- dinfo_dt[is.na(dinfo_dt$Ended),]
dinfo_dt <- dinfo_dt[!grepl('MWA',dinfo_dt$District_Name),]

master_dt <- data.table(expand.grid(District_ID = dinfo_dt$District_ID,PERIOD = c('2009-2011','2012-2014','2015-2017')))
master_dt$District_Name <- dinfo_dt$District_Name[match(master_dt$District_ID,dinfo_dt$District_ID)]
master_dt$PERIOD_START <- gsub('-[0-9]{4}','',master_dt$PERIOD)
master_dt$PERIOD_END <- gsub('[0-9]{4}-','',master_dt$PERIOD)


######### collect PWS data ######
tx_counties <- tigris::counties(state = 'TX',class = 'sf',year= 2017)
#generate query using this tool: https://echo.epa.gov/tools/web-services/facility-search-drinking-water
system_find <- 'https://echodata.epa.gov/echo/sdw_rest_services.get_systems?p_st=TX&p_act=A'
res <- fromJSON(system_find)
qid <- res$Results$QueryID
return_systems <- fread(paste0('https://echodata.epa.gov/echo/sdw_rest_services.get_download?qid=',qid,'&qcolumns=6,9,10,11,14'))
setnames(return_systems,'PWSId','PWS_ID')
setnames(return_systems,'PWSName','PWS_NAME')

tx_info = as.data.table(readRDS('org_performance/input/pws_details_2019-02-05.RDS') %>% dplyr::select(-PWS_ID_EX))
tx_info$Counties_Served <- return_systems$CountiesServed[match(tx_info$PWS_ID,return_systems$PWS_ID)]
tx_info$Counties_Served <- str_split(tx_info$Counties_Served,', ')

tx_info$Residential_Service_Connections = as.numeric(tx_info$PopulationType_ServiceConnections_Residential)
tx_info$Residential_Service_Connections[is.na(tx_info$Residential_Service_Connections)] <- 0
#tx_info$Ln_Residential_Service_Connections = log(tx_info$Residential_Service_Connections)
tx_info$Wholesale_Service_Connections = ifelse(!is.na(as.numeric(tx_info$PopulationType_ServiceConnections_Wholesale)),as.numeric(tx_info$PopulationType_ServiceConnections_Wholesale),0)
tx_info$Wholesale_Service_Connections[is.na(tx_info$Wholesale_Service_Connections)] <- 0
#tx_info$Ln_Wholesale_Service_Connections = log(tx_info$Wholesale_Service_Connections+1)
tx_info$Nonresidential_Service_Connections = ifelse(!is.na(as.numeric(tx_info$PopulationType_ServiceConnections_NonResidential)),as.numeric(tx_info$PopulationType_ServiceConnections_NonResidential),0)
tx_info$Nonresidential_Service_Connections[is.na(tx_info$Nonresidential_Service_Connections)] <- 0
tx_info$Total_Service_Connections = replace_na(tx_info$Wholesale_Service_Connections,0) + replace_na(tx_info$Nonresidential_Service_Connections,0) + replace_na(tx_info$Residential_Service_Connections,0)
consump_units = str_extract(tx_info$AverageDailyConsump.,'[A-Z]{1,}')
tx_info$Avg_Consumption_Per_Connection_G = (tx_info$Average_Daily_Consump_MGD/tx_info$Total_Service_Connections) * 1e6
tx_info$District_ID = unlist(sapply(tx_info$PWS_ID,function(p) ifelse(any(grepl(p,dinfo_dt$PWS_ID)),dinfo_dt$District_ID[grepl(p,dinfo_dt$PWS_ID)],NA),simplify = T))
#tx_info = tx_info[tx_info$`Activity Status` != "Inactive",]
tx_info <- tx_info[,-'PWS_NAME']

setkey(tx_info,PWS_ID)
setkey(return_systems,PWS_ID)
pws_dt <- return_systems[tx_info,]
pws_dt <- pws_dt[pws_dt$PWSActivityCode=='A',]

# connections <- fread('input/misc/physical_connections_sales.csv')
# connections <- connections[,!"V1",with=F]
# connections = connections[connections$connection=='P',]
# connections <- connections[!duplicated(connections),]
# connections$District_Seller <- pws_dt$District_ID[match(connections$seller,pws_dt$PWS_ID)]
# connections$District_Buyer <- pws_dt$District_ID[match(connections$buyer,pws_dt$PWS_ID)]
# mat_connection = as.matrix(table(connections$District_Seller,connections$District_Buyer))
# 

cfips = tigris::fips_codes
cfips$CFIPS = paste0(cfips$state_code,cfips$county_code)
cfips$county = gsub(' County','',cfips$county)
cfips = cfips[cfips$state=='TX',]

pws_dt$CFIPS_Served <- sapply(pws_dt$Counties_Served,function(x) cfips$CFIPS[match(x,cfips$county)])

cbsa_ref = read_excel('org_performance/input/cbsa_delineations.xls',skip = 1) %>% 
  mutate(CFIPS = paste0(formatC(`FIPS State Code`,width = 2,flag = 0),formatC(`FIPS County Code`,width=3,flag=0)))
pws_dt$CBSA_FIPS <- sapply(pws_dt$CFIPS_Served,function(x) cbsa_ref$`CBSA Code`[match(x,cbsa_ref$CFIPS)])
pws_dt = pws_dt[!sapply(pws_dt$CBSA_FIPS,function(x) any(is.na(x))),]
pws_dt$CBSA_FIPS <- sapply(pws_dt$CBSA_FIPS,function(x) x[[1]])

systems_in_cbsafips <- pws_dt[,.N,by=.(CBSA_FIPS)]
setnames(systems_in_cbsafips,'N','PWS_IN_CBSA')

systems_in_cbsafips[order(PWS_IN_CBSA),]
cbsa_total <- pws_dt[,sum(Total_Service_Connections),by=.(CBSA_FIPS)]
setnames(cbsa_total,'V1','CBSA_Total_Connections')
pws_dt$Prop_CBSA_Connection_Total <- 100 * pws_dt$Total_Service_Connections/cbsa_total$CBSA_Total_Connections[match(pws_dt$CBSA_FIPS,cbsa_total$CBSA_FIPS)]


cbsa_hhi <- do.call(rbind,lapply(split(pws_dt,by = 'CBSA_FIPS'),function(x) hhi(x,'Prop_CBSA_Connection_Total')))
cbsas <-  rownames(cbsa_hhi)
cbsa_hhi <- data.table(cbsa_hhi)
setnames(cbsa_hhi,'V1','CBSA_HHI_Index')
cbsa_hhi$CBSA_FIPS <- cbsas
setkey(cbsa_hhi,CBSA_FIPS)
setkey(systems_in_cbsafips,CBSA_FIPS)
cbsa_hhi <- systems_in_cbsafips[cbsa_hhi,]
setkey(pws_dt,CBSA_FIPS)

pws_dt <- pws_dt[cbsa_hhi,]
pws_dt <- pws_dt[pws_dt$Owner_Type == 'District',]
pws_dt <- pws_dt[sapply(pws_dt$PWS_ID,function(x) any(grepl(x,dinfo_dt$PWS_ID))),]
pws_dt$District_ID <- dinfo_dt$District_ID[unlist(sapply(pws_dt$PWS_ID,function(x) which(grepl(x,dinfo_dt$PWS_ID))))]
pws_time_dt <- data.table(expand.grid(PWS_ID = pws_dt$PWS_ID,PERIOD = c('2009-2011','2012-2014','2015-2017')))
pws_time_dt$PERIOD_START <- gsub('-[0-9]{4}','',pws_time_dt$PERIOD)
pws_time_dt$PERIOD_END <- gsub('[0-9]{4}-','',pws_time_dt$PERIOD)


vfiles = list.files('org_performance/input/violation_records/',full.names = T)
viol_dt = rbindlist(lapply(vfiles,fread),use.names = T,fill = T)
viol_dt[viol_dt=='-'] <- NA
names(viol_dt) <- str_replace_all(names(viol_dt),'\\s','_')

viol_dt$COMP_BEGIN_DATE <- dmy(viol_dt$COMPL_PER_BEGIN_DATE)
viol_dt$COMP_YEAR <- year(viol_dt$COMP_BEGIN_DATE)
viol_counts_dt <- viol_dt[,.N,by=.(PWS_ID,COMP_YEAR,IS_HEALTH_BASED_IND)]
viol_counts_dt <- viol_counts_dt[viol_counts_dt$COMP_YEAR>=2010,]
viol_counts_dt$PERIOD= ifelse(viol_counts_dt$COMP_YEAR %in% c(2009:2011),'2009-2011',ifelse(viol_counts_dt$COMP_YEAR %in% c(2012:2014),'2012-2014','2015-2017'))
viol_count_period <- dcast(viol_counts_dt[,sum(N),by=.(PERIOD,PWS_ID,IS_HEALTH_BASED_IND)],PWS_ID + PERIOD ~IS_HEALTH_BASED_IND)
setnames(viol_count_period,c('N','Y'),c('NonHealth','Health'))
setkey(pws_time_dt,PWS_ID,PERIOD)
setkey(viol_count_period,PWS_ID,PERIOD)
pws_time_dt <- viol_count_period[pws_time_dt,]
pws_time_dt$NonHealth = replace_na(pws_time_dt$NonHealth ,0)
pws_time_dt$Health = replace_na(pws_time_dt$Health,0)
setkey(pws_dt,PWS_ID)
setkey(pws_time_dt,PWS_ID)
pws_dt <- pws_time_dt[pws_dt,]


combo_vars = c('Total_Storage_MG','Interconnections','NonHealth','Health',
               'Average_Daily_Consump_MGD','Total_Product_MGD','Prop_CBSA_Connection_Total',
               'Residential_Service_Connections','Wholesale_Service_Connections','Nonresidential_Service_Connections',
               'Total_Service_Connections')
pws_to_district <- pws_dt[,lapply(.SD,sum,na.rm=T),.SDcols = combo_vars,by = .(District_ID,PERIOD,PERIOD_START,PERIOD_END)]
pws_count = pws_dt[,.N,by=District_ID]
setnames(pws_count,old = 'N',new = 'Num_PWS_Operated')
setkey(pws_to_district,District_ID)
setkey(pws_count,District_ID)
pws_to_district<- pws_count[pws_to_district,]
pws_to_district$CBSA_HHI_Index <- pws_dt$CBSA_HHI_Index[match(pws_to_district$District_ID,pws_dt$District_ID)]


audits = fread('org_performance/input/district_audits.csv')
money = as.vector(which(apply(audits,2,function(x) any(grepl('\\$',x)))))
audits = cbind(audits[,-money,with=F],audits[,lapply(.SD,from_currency),.SDcols = money])

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

audits$YEAR <- floor(audits$Date)
audits = audits[audits$YEAR %in% c(2009:2017),]
audits$PERIOD= ifelse(floor(audits$Date) %in% c(2009:2011),'2009-2011',ifelse(floor(audits$Date) %in% c(2012:2014),'2012-2014','2015-2017'))
audits$Fund_Balance <- audits$`GENERAL FUND - FUND BALANCE`
audits$Total_Revenue <- audits$`ENTERPRISE FUND - OPERATING REVENUES` + audits$`GENERAL FUND - TOTAL REVENUES`
audits$Total_Expenditure<- audits$`GENERAL FUND - TOTAL EXPENDITURES`+ audits$`ENTERPRISE FUND - OPERATING EXPENSES`
audits$Water_SFU <- audits$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`
audits$Wastewater_SFU <- replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`,0) + 
  replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`,0)

vals_to_avg <- c("Fund_Balance",'Total_Revenue','Total_Expenditure','Water_SFU','Wastewater_SFU')
three_year_avg <- paste0(vals_to_avg,'_3yrAvg')

audit_avgs <- audits[,lapply(.SD,mean,na.rm=T),.SDcols = vals_to_avg,by = .(District_ID,PERIOD)]
setkey(audit_avgs,District_ID,PERIOD)
setkey(master_dt,District_ID,PERIOD)
master_dt <- audit_avgs[master_dt,]


#dinfo = read_csv('input/twdd_records/district_list_2019-01-03.csv')
debt = fread('org_performance/input/Debt_Outstanding_By_Local_Government.csv') 
debt = debt[debt$GovernmentType == 'WD',]
debt = debt[,sum(TotalDebtServiceOutstanding),by=.(GovernmentName,FiscalYear)]
setnames(debt,'V1','TotalDebtServiceOutstanding')
iss = fread('org_performance/input/Local_Issuance.csv')
iss = iss[iss$GovernmentType=='WD'&!is.na(iss$NewMoneyPar),]
iss = iss[,.(GovernmentName,NewMoneyPar,FiscalYearIssuance )]
setnames(iss,c('NewMoneyPar','FiscalYearIssuance'),c('NewMoney','FiscalYear'))
setkey(iss,GovernmentName,FiscalYear)
setkey(debt,GovernmentName,FiscalYear)
debt = iss[debt,]
debt$GovernmentName = toupper(debt$GovernmentName)
debt$District_Name = debt$GovernmentName
debt$District_Name = gsub('(\\s)0(?=[0-9])','\\1\\2',debt$District_Name,perl = T)
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
debt$District_Name[grepl('MUD 1$',debt$District_Name)] = ifelse(debt$District_Name[grepl('MUD 1$',debt$District_Name)] %in% dinfo$District_Name,debt$District_Name[grepl('MUD 1$',debt$District_Name)],
                                                                gsub(" 1$",'',debt$District_Name[grepl('MUD 1$',debt$District_Name)]))
debt$District_Name = gsub("ARANSAS COUNTY ND 1","ARANSAS COUNTY NAVIGATION DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BEEVILLE WSD","BEEVILLE WATER SUPPLY DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub("BELL COUNTY WCID 2-LITTLE RIVER","BELL COUNTY WCID NO 2",debt$District_Name,perl = T)
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
debt$District_Name = gsub("D'ARC","DARC",debt$District_Name,perl = T)
debt$District_Name = gsub("HUNTER'S GLEN MUD","HUNTERS GLEN MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("MOORE'S CROSSING MUD","MOORES CROSSING MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("SPORTSMAN'S WORLD MUD","SPORTSMANS WORLD MUD",debt$District_Name,perl = T)
debt$District_Name = gsub("PORT O'CONNOR IRRIGATION DISTRICT","PORT OCONNOR IMPROVEMENT DISTRICT",debt$District_Name,perl = T)
debt$District_Name = gsub('FLYING "L" RANCH PUD','FLYING L PUD',debt$District_Name,perl = T)
debt = debt[debt$FiscalYear%in%2009:2017,]
debt$NewMoney = replace_na(debt$NewMoney,0)
debt$PERIOD= ifelse(debt$FiscalYear %in% 2009:2011,'2009-2011',ifelse(debt$FiscalYear %in% 2012:2014,'2012-2014','2015-2017'))

issued = debt[,sum(NewMoney),by=.(District_Name,PERIOD)]
owed = debt[,mean(TotalDebtServiceOutstanding,na.rm=T),by=.(District_Name,PERIOD)]
setnames(issued,'V1','Debt_Issued_In_Period')
setnames(owed,'V1','TotalDebtServiceOutstanding_3yrAvg')
setkey(issued,District_Name,PERIOD)
setkey(owed,District_Name,PERIOD)

debt_periods = issued[owed,]
debt_periods$District_ID <- master_dt$District_ID[match(debt_periods$District_Name,master_dt$District_Name)]
debt_periods = debt_periods[!is.na(debt_periods$District_ID),]
setkey(debt_periods,District_ID,PERIOD)
setkey(master_dt,District_ID,PERIOD)
master_dt <- debt_periods[,-c('District_Name')][master_dt,]

setkey(pws_to_district,District_ID,PERIOD)
setkey(master_dt,District_ID,PERIOD)
master_dt <- pws_to_district[master_dt,]
master_dt$Num_PWS_Operated = master_dt$Num_PWS_Operated/3


tceq_geojson='https://opendata.arcgis.com/datasets/e7f6dd0a88c046fba1f54d440941a061_0.geojson'
wd = st_read(tceq_geojson)
wd = st_make_valid(wd)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
wd = st_transform(wd,crs = 3083)

wd <- wd[wd$DISTRICT_I %in% master_dt$District_ID,]
wd$District_ID <- as.character(wd$DISTRICT_I)

pws_shape_zip_url = 'https://services.twdb.texas.gov/arcgis/rest/directories/arcgisjobs/pws/pwsexport_gpserver/jc914ceef55b44622934c2cbf48763faa/scratch/PWS_Export.zip'
td = tempdir()
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(pws_shape_zip_url, tf)
fname = unzip(tf, list=TRUE)
unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
fpath = file.path(td, grep('shp$',fname$Name,value=T))
sb <- st_read(fpath)

sb = st_transform(sb,crs = 3083)
sb = st_make_valid(sb)
sb$District_ID  <- pws_dt$District_ID[match(sb$PWSId,pws_dt$PWS_ID)]

sb <- sb[!is.na(sb$District_ID),]
sb = st_union(sb,by_feature = T)
sb_combo <- sb %>% group_by(District_ID) %>% summarize()
wd_simple <- wd[!wd$DISTRICT_ID %in% sb_combo$District_ID,c('DISTRICT_ID')]
wd_simple <- wd_simple[!duplicated(wd_simple$DISTRICT_ID),]
setnames(wd_simple,'DISTRICT_ID','District_ID')

district_sf = rbind(sb_combo,wd_simple)

urban = tigris::urban_areas(class = 'sf')
urban = st_transform(urban,crs = 3083)
urban = urban[grepl('TX',urban$NAME10),]
#urban_tx = st_union(urban)
#urban_tx = st_make_valid(urban_tx)
inters <- st_intersects(district_sf,urban)
district_sf <- st_make_valid(district_sf)
urban <- st_make_valid(urban)

urban_props <- pbsapply(seq_along(inters),function(x) sum(as.numeric(st_area(st_intersection(district_sf[x,],urban[inters[[x]],]))))/ as.numeric(st_area(district_sf[x,])),cl = 8)
district_sf$Prop_Urban <- urban_props 
district_sf$Area = st_area(district_sf)

demos = fread('org_performance/input/tract_data_2008_2019.csv',stringsAsFactors = F)
demos$GEOID = gsub('.*US','',demos$GEOID)
tracts = tigris::tracts(state = 'TX',class='sf',year = '2015')
tracts = st_transform(tracts,crs = 3083)
tracts = st_make_valid(tracts)
pws_tract_inters = st_intersects(district_sf,tracts)
pws_over_tracts = pblapply(seq_along(pws_tract_inters),
                           function(x) {st_intersection(st_make_valid(district_sf[x,]),tracts[pws_tract_inters[[x]],])})

pws_tracts = do.call(rbind,pws_over_tracts)
pws_tracts$Total_Area = district_sf$Area[match(pws_tracts$District_ID,district_sf$District_ID)]
pws_tracts$prop_weight = st_area(pws_tracts)/pws_tracts$Total_Area

total_vars = c('Total_Population','White_Population','Household_Pop','Household_Owner_Occupied','Pop_Over_25','Pop_Bach')
summary_vars = c('Median_Income','Median_Home_Value','Median Year Structure Built')

fill_grid = data.table(expand.grid(District_ID = as.character(unique(dinfo_dt$District_ID)),Year = 2000:2020))

demo_list = pblapply(1:nrow(fill_grid),function(i){
  temp_weights = pws_tracts[pws_tracts$District_ID== fill_grid$District_ID[i],]
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

setnames(demo_dt,old = 'Median Year Structure Built',new = 'Median_Year_Structure_Built')
demo_dt$PERIOD= ifelse(demo_dt$Year %in% 2009:2011,'2009-2011',ifelse(demo_dt$Year %in% 2012:2014,'2012-2014','2015-2017'))
avg3yr = c('Total_Population','White_Population','Household_Pop','Household_Owner_Occupied','Pop_Over_25','Pop_Bach',
           'Median_Income','Median_Home_Value','Median_Year_Structure_Built')
demo_values <- demo_dt[,lapply(.SD,mean),by = .(District_ID,PERIOD),.SDcols = avg3yr]


setkey(demo_values,District_ID,PERIOD)
setkey(master_dt,District_ID,PERIOD)
master_dt <- demo_values[master_dt,]
keep_ids <- dinfo_dt$District_ID[dinfo_dt$Status=='ACTIVE'&mdy(dinfo_dt$Created)<mdy('01/01/2009')]
master_dt = master_dt[master_dt$District_ID %in% keep_ids,]
master_dt = master_dt[master_dt$District_ID %in% pws_dt$District_ID,]
master_dt$CFIPS_Served <- pws_dt$CFIPS_Served[match(master_dt$District_ID,pws_dt$District_ID)]
master_dt$PERIOD_ID = as.numeric(as.factor(master_dt$PERIOD))

flist = list.files('org_performance/input/drought_index_records/','dm_export',full.names = T)
dm_list = lapply(flist,function(x){ 
  temp = fread(x)
  temp$DSCI = as.numeric(temp$D0) + as.numeric(temp$D1) * 2 + as.numeric(temp$D2) * 3 + as.numeric(temp$D3) * 4 + as.numeric(temp$D4) * 5
  temp[,.(FIPS,ValidStart,ValidEnd,DSCI,None,D0,D1,D2,D3,D4)]
})

dm_df <- rbindlist(dm_list[sapply(dm_list,nrow)>0],use.names = T,fill = T)
dm_df <- dm_df[!duplicated(dm_df),]
dm_temp <- dm_df
dm_df <- dm_df %>% rename(CFIPS = FIPS) %>% mutate(CFIPS = as.character(CFIPS))
dm_dt <- as.data.table(dm_df)
dm_dt$PERIOD_ID <- NA
dm_dt$Start_ddate <- decimal_date(ymd(dm_dt$ValidStart))
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2009&dm_dt$Start_ddate<2012] <- 1
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2012&dm_dt$Start_ddate<2015] <- 2
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2015&dm_dt$Start_ddate<2018] <- 3
dm_dt <- dm_dt[!is.na(dm_dt$PERIOD_ID),]
dm_avgs <- dm_dt[,mean(DSCI),by=.(CFIPS,PERIOD_ID)]

master_dt$DSCI_3yr_Weekly_Avg <- sapply(1:nrow(master_dt),function(x) 
  mean(dm_avgs$V1[dm_avgs$CFIPS %in% master_dt$CFIPS_Served[[x]] & dm_avgs$PERIOD_ID == master_dt$PERIOD_ID[x]]))


noaa_url = 'https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/'
location_nodes <- read_html(noaa_url) %>% html_nodes('a:contains("locations")')
location_hrefs <- location_nodes %>% html_attr('href') %>% paste0(noaa_url,.)
location_hrefs <- grep('200[5-9]|201[0-9]',detail_hrefs,value = T)

detail_nodes <- read_html(noaa_url) %>% html_nodes('a:contains("details")')
detail_hrefs <- detail_nodes %>% html_attr('href') %>% paste0(noaa_url,.)
detail_hrefs <- grep('200[5-9]|201[0-9]',detail_hrefs,value = T)

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
storm_sf <- storm_sf[storm_sf$Year %in% 2009:2017,]
storm_sf$PERIOD_ID <- ifelse(storm_sf$Year %in% 2009:2011,1,ifelse(storm_sf$Year %in% 2012:2014,2,3))

county_obs = mapply(function(x,y) data.table(CFIPS = x,PERIOD_ID = y), x= storm_sf$CFIPS,y = storm_sf$PERIOD_ID,SIMPLIFY = F)
county_events_by_period = rbindlist(county_obs)[,.N,by=.(CFIPS,PERIOD_ID)]

master_dt$Severe_Weather_Events <- sapply(1:nrow(master_dt),function(x) 
  sum(county_events_by_period$N[county_events_by_period$CFIPS %in% master_dt$CFIPS_Served[[x]] & 
                                  county_events_by_period$PERIOD_ID %in% master_dt$PERIOD_ID[x]]))
master_dt$NUM_PWS_IN_CBSA <- pws_dt$PWS_IN_CBSA[match(master_dt$District_ID,pws_dt$District_ID)]

modeldata_dt <- master_dt
modeldata_dt$Primary_Source <- as.character(unlist(sapply(modeldata_dt$District_ID,function(x) names(which.max(table(pws_dt$PrimarySourceCode[pws_dt$District_ID==x]))))))
modeldata_dt$Prop_Nonwholesale = 1 - modeldata_dt$Wholesale_Service_Connections/modeldata_dt$Total_Service_Connections
modeldata_dt$Prop_Bach = master_dt$Pop_Bach/master_dt$Total_Population
modeldata_dt$Wholesaler = (modeldata_dt$Wholesale_Service_Connections>0) + 0
modeldata_dt$Multiple_Systems = (modeldata_dt$Num_PWS_Operated > 1) + 0
modeldata_dt$Prop_Nonwhite = 1 - modeldata_dt$White_Population/modeldata_dt$Total_Population
modeldata_dt$Prop_Bach = modeldata_dt$Pop_Bach/modeldata_dt$Pop_Over_25
modeldata_dt$Prop_Owner_Occupied = modeldata_dt$Household_Owner_Occupied/modeldata_dt$Household_Pop
modeldata_dt$Wastewater_Service = (modeldata_dt$Wastewater_SFU>0) + 0
modeldata_dt$Urban_Prop <- district_sf$Prop_Urban[match(modeldata_dt$District_ID,district_sf$District_ID)]


vars_to_lag=c("Debt_Issued_In_Period","Total_Revenue"  ,"Fund_Balance" ,
              "TotalDebtServiceOutstanding_3yrAvg",'Health','NonHealth')
lag_var_names = paste(vars_to_lag,'L1',sep='_')
modeldata_dt[order(District_ID,PERIOD_ID), (lag_var_names ) := data.table::shift(.SD, n = 1,fill= NA,type="lag"), 
             .SDcols=vars_to_lag,by = District_ID]
modeldata_dt$REV_PER_CONNECTION_L1 = modeldata_dt$Total_Revenue_L1/modeldata_dt$Total_Service_Connections
modeldata_dt$EXP_PER_CONNECTION_L1 = modeldata_dt$Total_Expenditure_L1/modeldata_dt$Total_Service_Connections
modeldata_dt$TotalDebtServiceOutstanding_3yrAvg_L1[is.na(modeldata_dt$TotalDebtServiceOutstanding_3yrAvg_L1)&modeldata_dt$PERIOD_ID==3]<-0
modeldata_dt$Debt_Issued_In_Period_L1[is.na(modeldata_dt$Debt_Issued_In_Period_L1)&modeldata_dt$PERIOD_ID==3]<-0
modeldata_dt$TotalDebtServiceOutstanding_L1_Per_Connection = modeldata_dt$TotalDebtServiceOutstanding_3yrAvg_L1/modeldata_dt$Total_Service_Connections

#vars_to_ln = c("REV_PER_CONNECTION_L1","TotalDebtServiceOutstanding_L1_Per_Connection",'NUM_PWS_IN_CBSA',
#               "Debt_Issued_In_Period_L1","Total_Service_Connections",'Median_Home_Value','Severe_Weather_Events')
#ln_var_names = paste0('ln_',vars_to_ln)
#modeldata_dt[, (ln_var_names) := lapply(.SD,function(x) log(x+1)),.SDcols=vars_to_ln,]

vars_to_std= c("Debt_Issued_In_Period_L1", "REV_PER_CONNECTION_L1","Prop_Bach",
               "Fund_Balance_L1","Health_L1", "NonHealth_L1",'NUM_PWS_IN_CBSA',
               "TotalDebtServiceOutstanding_L1_Per_Connection",  "Severe_Weather_Events" ,"DSCI_3yr_Weekly_Avg" ,
               "Total_Service_Connections" ,"Urban_Prop","Median_Year_Structure_Built",
               "Median_Home_Value", "Prop_Nonwholesale", "CBSA_HHI_Index")

std_var_names = paste('std',vars_to_std,sep='_')
modeldata_dt[, (std_var_names) := lapply(.SD,scale), .SDcols=vars_to_std]

modeldata_dt$Primary_Source[modeldata_dt$Primary_Source %in% c('GUP')] <- 'GWP'
modeldata_dt$Primary_Source[modeldata_dt$Primary_Source %in% c('GU')] <- 'GW'
modeldata_dt$Groundwater <- (grepl('GW',modeldata_dt$Primary_Source))+0
modeldata_dt$Purchaser <- (grepl('P',modeldata_dt$Primary_Source))+0



fact_vars <- c('Groundwater','Purchaser')
bin_var_names <- paste0('bin_',fact_vars)
modeldata_dt[, (bin_var_names) := lapply(.SD,function(x) x), .SDcols=fact_vars]
modeldata_dt$CBSA_FIPS <- pws_dt$CBSA_FIPS[match(modeldata_dt$District_ID,pws_dt$District_ID)]

saveRDS(object = modeldata_dt,file = 'org_performance/scratch/df_for_model.RDS')




