
library(rgeos)
library(rgdal)
library(sp)
library(maptools)
devtools::install_github("ironholds/lucr")
require(lucr)
library(readxl)
library(hhi)
library(spdep)
library(lubridate)
library(pbapply)
library(tidyverse)
library(jsonlite)
library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(geojsonsf)
library(tigris)
library(lwgeom)

library(tidyquant)

dinfo_dt = fread('input/twdd_records/district_list_2019-01-03.csv',stringsAsFactors = F,na.strings = "")
dinfo_dt$PWS_ID[dinfo_dt$PWS_ID == "NA"] <- NA
dinfo_dt$District_ID = as.character(dinfo_dt$District_ID)
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
system_find <- 'https://ofmpub.epa.gov/echo/sdw_rest_services.get_systems?output=JSON&p_st=TX&p_act=A'
res <- fromJSON(system_find)
qid <- res$Results$QueryID
return_systems <- fread(paste0('https://ofmpub.epa.gov/echo/sdw_rest_services.get_download?qid=',qid,'&qcolumns=6,9,10,11,14'))
setnames(return_systems,'PWSId','PWS_ID')
setnames(return_systems,'PWSName','PWS_NAME')

tx_info = as.data.table(readRDS('scratch/pws_details_2019-02-05.RDS') %>% dplyr::select(-PWS_ID_EX))
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

cbsa_ref = read_excel('input/census/cbsa_delineations.xls',skip = 1) %>% 
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

viol_dt = fread('input/epa_sdwis/proj6/TX_PWS_violation_report_2-4-19.csv',na.strings = '-')
viol_dt = readRDS(paste0('scratch/viol_list_2021-03-02.rds'))
viol_dt[viol_dt=='-'] <- NA
setnames(viol_dt, "PWSID", "PWS_ID")

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


audits = fread('input/tceq_audits/district_audits.csv')
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

library(readxl)
#dinfo = read_csv('input/twdd_records/district_list_2019-01-03.csv')
debt = fread('input/tbrb/Debt_Outstanding_By_Local_Government.csv') 
debt = debt[debt$GovernmentType == 'WD',]
debt = debt[,sum(TotalDebtServiceOutstanding),by=.(GovernmentName,FiscalYear)]
setnames(debt,'V1','TotalDebtServiceOutstanding')
iss = fread('input/tbrb/Local_Issuance.csv')
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


library(sf)
wd = st_read('spatial_inputs/water_districts_shp/TCEQ_WaterDistricts.shp')
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
wd = st_transform(wd,crs = 3083)

wd <- wd[wd$DISTRICT_I %in% master_dt$District_ID,]
wd$District_ID <- as.character(wd$DISTRICT_I)
sb = st_read('spatial_inputs/Service_Area_Boundaries/PWS_Export.shp')
sb = st_transform(sb,crs = 3083)
sb = st_make_valid(sb)
sb$District_ID  <- pws_dt$District_ID[match(sb$PWSId,pws_dt$PWS_ID)]
sb <- sb[!is.na(sb$District_ID),]
sb = st_union(sb,by_feature = T)
sb_combo <- sb %>% group_by(District_ID) %>% summarize()
wd_simple <- wd[!wd$District_ID %in% sb_combo$District_ID,c('District_ID')]
wd_simple <- wd_simple[!duplicated(wd_simple$District_ID),]
district_sf = rbind(sb_combo,wd_simple)

urban = st_read('spatial_inputs/government_units/cb_2016_us_ua10_500k.shp')
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

demos = fread('input/census/ACS_tracts/proj6/tract_data_2009_2017.csv',stringsAsFactors = F)
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
  tdt},cl = 12)
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

flist = list.files('input/drought_index_data/','dm_export',full.names = T)
dm_list = lapply(flist,function(x){ 
  temp = read_csv(x)
  temp$DSCI = as.numeric(temp$D0) + as.numeric(temp$D1) * 2 + as.numeric(temp$D2) * 3 + as.numeric(temp$D3) * 4 + as.numeric(temp$D4) * 5
  temp %>% dplyr::select(FIPS,ValidStart,ValidEnd,DSCI,None,D0,D1,D2,D3,D4)
})
dm_df = do.call(rbind,dm_list)
dm_df = dm_df[!duplicated(dm_df),]
dm_temp = dm_df
dm_df = dm_df %>% rename(CFIPS = FIPS) %>% mutate(CFIPS = as.character(CFIPS))
dm_dt = as.data.table(dm_df)
dm_dt$PERIOD_ID <- NA
dm_dt$Start_ddate <- decimal_date(ymd(dm_dt$ValidStart))
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2009&dm_dt$Start_ddate<2012] <- 1
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2012&dm_dt$Start_ddate<2015] <- 2
dm_dt$PERIOD_ID[dm_dt$Start_ddate>2015&dm_dt$Start_ddate<2018] <- 3
dm_dt <- dm_dt[!is.na(dm_dt$PERIOD_ID),]
dm_avgs <- dm_dt[,mean(DSCI),by=.(CFIPS,PERIOD_ID)]

master_dt$DSCI_3yr_Weekly_Avg <- sapply(1:nrow(master_dt),function(x) 
  mean(dm_avgs$V1[dm_avgs$CFIPS %in% master_dt$CFIPS_Served[[x]] & dm_avgs$PERIOD_ID == master_dt$PERIOD_ID[x]]))


storm_dets<- rbindlist(lapply(list.files('input/ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/',pattern = 'details',full.names = T),fread))
storm_dets <- storm_dets[storm_dets$STATE=='TEXAS',]
storms <- rbindlist(lapply(list.files('input/ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/',pattern = 'locations',full.names = T),fread))
storms$Year <- str_extract(storms$YEARMONTH,'^[0-9]{4}')
storms <- storms[storms$EPISODE_ID %in% storm_dets$EPISODE_ID,]
storms <- storms[!is.na(storms$LATITUDE),]
storm_sf <- st_as_sf(storms,coords = c('LONGITUDE','LATITUDE'),crs = 4269)
storm_sf = st_transform(storm_sf,crs = 3083)
tx_counties <- st_transform(tx_counties,crs = 3083)


storm_inters  = st_intersects(storm_sf,tx_counties)
storm_inters[sapply(storm_inters,length)==0]<- NA
storm_sf$CFIPS <- tx_counties$GEOID[unlist(storm_inters)]
storm_sf <- storm_sf[storm_sf$Year %in% 2009:2017,]
storm_sf$PERIOD_ID <- ifelse(storm_sf$Year %in% 2009:2011,1,ifelse(storm_sf$Year %in% 2012:2014,2,3))
storm_sf <- storm_sf %>% group_by(PERIOD_ID,CFIPS) %>% summarise(severe_weather_events = n())

master_dt$Severe_Weather_Events <- sapply(1:nrow(master_dt),function(x) 
  sum(storm_sf$severe_weather_events[storm_sf$CFIPS %in% master_dt$CFIPS_Served[[x]] & 
                                       storm_sf$PERIOD_ID %in% master_dt$PERIOD_ID[x]]))
master_dt$NUM_PWS_IN_CBSA <- pws_dt$PWS_IN_CBSA[match(master_dt$District_ID,pws_dt$District_ID)]


library(INLA)
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

pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))

form_string <- paste(c(grep('std_',names(modeldata_dt),value=T),
# "Health_L1", "NonHealth_L1", 
"Primary_Source",
"f(District_ID,model = 'iid',hyper=pcprior)",
"f(CBSA_FIPS,model = 'iid',hyper=pcprior)"),collapse='+')

mod0 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'nbinomial',
            data=modeldata_dt[modeldata_dt$PERIOD_ID%in%2:3,],quantiles = c(0.01,0.025,0.05,0.5,0.95,0.975,0.99),
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

sumlist = do.call(rbind,apply(modeldata_dt[PERIOD_ID!=1,vars_to_std,with=F],2,summary),use.names = T,fill =T)

data_summary = apply(modeldata_dt[PERIOD_ID!=1,vars_to_std,with=F],2,summary)


sum_table = rbindlist(lapply(seq_along(data_summary),function(x) data.table(COEF = names(data_summary)[x],data.table(rbind(data_summary[[x]])))),use.names = T,fill = T)

htmlTable::htmlTable(round(sumlist[,c(1,3,4,6)],3))

dvs = c('Health','Health+NonHealth','NonHealth')


mod_list = lapply(dvs,function(d){
  inla(as.formula(paste(d,form_string,sep='~')),family = 'nbinomial',
       data=modeldata_dt[modeldata_dt$PERIOD_ID%in%2:3,],quantiles = c(0.025,0.5,0.975),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))
})


summary(mod_list[[2]])

tt = rbindlist(mapply(function(x,y) data.table(x$summary.fixed,Coef = rownames(x$summary.fixed),DV = y),x = mod_list,y = dvs,SIMPLIFY = F))

tt$Sig <- !(tt$`0.025quant`<0&tt$`0.975quant`>0)+0
tt$Coef <- as.factor(tt$Coef)
library(forcats)

tt$Coef <- fct_relevel(tt$Coef,
                       "(Intercept)"    ,
     "std_Health_L1" , "std_NonHealth_L1"  ,
     "std_NUM_PWS_IN_CBSA" , "std_CBSA_HHI_Index" ,
     "std_Severe_Weather_Events" , "std_DSCI_3yr_Weekly_Avg"    ,
     "std_Median_Home_Value" ,   "std_Prop_Bach" ,
     "std_Total_Service_Connections"  ,    "std_Prop_Nonwholesale"    ,
     "std_Median_Year_Structure_Built", "std_Urban_Prop" ,"std_Debt_Issued_In_Period_L1"   ,
     "std_TotalDebtServiceOutstanding_L1_Per_Connection" ,  "std_REV_PER_CONNECTION_L1"  ,
     "std_Fund_Balance_L1" ,"Primary_SourceSW", "Primary_SourceSWP"  , "Primary_SourceGWP"  )  
               

tt$Coef <- fct_recode(tt$Coef,'Health violations in prior 3 years'="std_Health_L1" ,
'Management violations in prior 3 years'= "std_NonHealth_L1"   ,
'# water systems in CBSA'="std_NUM_PWS_IN_CBSA"  ,
"% college degree" = "std_Prop_Bach",
'CBSA HHI (market concentration)'="std_CBSA_HHI_Index",
'Severe weather events'="std_Severe_Weather_Events" ,
'Drought index (weekly average)'="std_DSCI_3yr_Weekly_Avg"  ,
'Median home value'="std_Median_Home_Value",
'Service connections'="std_Total_Service_Connections" ,
'% retail connections'="std_Prop_Nonwholesale",
'Median structure build year'="std_Median_Year_Structure_Built",
'Debt issued during the last 3 years'= "std_Debt_Issued_In_Period_L1"   ,
'Debt per connection'="std_TotalDebtServiceOutstanding_L1_Per_Connection" ,
'Revenue per connection'= "std_REV_PER_CONNECTION_L1" ,
'Fund balance'= "std_Fund_Balance_L1"  ,
'% urban area' =  "std_Urban_Prop"  ,
'Source-surface water'="Primary_SourceSW" ,
'Source-purchased surface water' =  "Primary_SourceSWP",
'Source-purchased groundwater'="Primary_SourceGWP" )

tt$Coef <- fct_rev(tt$Coef)
tt[DV=='Health+NonHealth']
require(ggthemes)
ggplot(data=tt[{!grepl('Intercept',Coef)}&DV=="Health+NonHealth",]) + 
  geom_hline(yintercept = 0,lty = 2,col = 'grey50')+
  #geom_errorbar(aes(ymin = `0.01quant`,ymax = `0.99quant`,x = Coef,color = DV,fill = DV),width=0,lwd=0.4)+ 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`,x = Coef),width=0,lwd=1,position = position_dodge(0.5))+ 
  #geom_errorbar(aes(ymin = `0.05quant`,ymax = `0.95quant`,x = Coef,color=DV,fill = DV),width=0,lwd=1.6) +
  #facet_wrap(~DV) +
  geom_point(aes(x = Coef,y = mean,fill = as.factor(`0.975quant`>0&`0.025quant`<0)),shape=21,size = 2,position = position_dodge(0.5))+
 # scale_y_continuous(limits=c(-3,2),name = ~Delta[ln(violations)]/Delta[Beta])+
 # scale_fill_manual(values=c('white','black'),labels=c('0 in 95% interavl','0 !in 95% interval')) + 
#  scale_color_manual(values = rep('black',3))+
  scale_fill_manual(values = c('black','white'),labels=c('no','yes'),name ='95% CI includes 0')+
 # guides(color=guide_legend(override.aes = list(lwd = c(0.4,1,1.6), pch=c(NA,NA,NA))))+
  coord_flip() + geom_vline(xintercept=0) + theme_bw() +
  scale_y_continuous(name = 'Additive log-scale parameter estimate')+
  #scale_color_colorblind(labels = c('Health viols.','All viols.','Manage. viols.'))+
 # guides(fill = F)+
  theme(axis.text = element_text(size = 12),axis.title.y = element_blank(),legend.position = c(0.8,0.4))+
       ggtitle('Posterior mean and 95% credible interval estimates')
  

tt = tt[,.(Coef,mean,`0.025quant`,`0.975quant`,DV)]
tt$present = paste0(formatC(tt$mean,drop0trailing = F,digits = 2,format = 'f'),' (',formatC(tt$`0.025quant`,drop0trailing = F,digits = 2,format = 'f'),', ',formatC(tt$`0.975quant`,drop0trailing = F,digits = 2,format = 'f'),')')

htmlTable::htmlTable(dcast(tt,Coef~DV,value.var = 'present'))



library(plm)
library(pglm)

pld <- as.data.frame(modeldata_dt)
pld <- pld[pld$PERIOD_ID %in% 2:3,]
pld <- pld[,colnames(pld) %in% c('Health','std_Fund_Balance_L1','std_Prop_Nonwhite','std_Urban_Prop','std_ln_REV_PER_CONNECTION_L1',
              'std_ln_Median_Home_Value','std_ln_Total_Service_Connections','District_ID','PERIOD_ID',
              'std_NonHealth_L1','std_CBSA_HHI_Index','std_Severe_Weather_Events','std_DSCI_3yr_Weekly_Avg')]

pld <- pld[rowSums(is.na(pld))==0,]
form_test = as.formula(paste0('Health~',paste(grep('^std',colnames(pld),value=T),collapse='+')))
est = pglm(formula = form_test,
           data = pld,model = 'random',family = negbin,method = "nr",index = c('District_ID','PERIOD_ID'))
summary(est)

library(corrplot)
cvals <- cor(as.matrix(mod0$model.matrix[,-1]))
test = gather(as.data.frame(cvals) %>% mutate(v1 = rownames(.)),key,value,-v1)
ggplot(data = test) + geom_tile(aes(x = v1,y=key,fill=value)) + scale_fill_gradient2()



corrplot(cvals,type = 'upper')


library(INLAutils)
observed = master_dt[PERIOD_ID%in%2:3,Health]

ggplot_inla_residuals2(mod0, observed, se = FALSE)
INLAutils::plot_fixed_marginals(mod0)


mod1 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'poisson',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod2 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'zeroinflatedpoisson0',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod3 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'zeroinflatednbinomial0',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod0$waic$waic
mod1$waic$waic
mod2$waic$waic


inla.list.models()
summary(test)
as.formula(paste('Health',form_string,sep='~'))
#dt <- demo_dt[order(District_ID,Year),]
#dt[,Total_Population3:=lag(Total_Population, 3),by = .(District_ID)]
#dt$Total_Population[dt$Total_Population==0] <- 1
#dt$Total_Population3[!is.na(dt$Total_Population3) & dt$Total_Population3==0] <- 1
#dt[dt$Year%in%c(2012,2015),Pop_Change:=100 * {(Total_Population-{Total_Population3})/{Total_Population3}},]

master_dt[1:3,]

summary(test)


test$model.matrix
dt[,Total_Population2:=shift(Total_Population, 2)]
dt[,mpg_forward1:=shift(mpg, 1, type='lead')]

library(tidycensus)



setkey(fill_grid,District_ID,Year)
fill_grid = audits[fill_grid,]
fill_grid = fill_grid %>% group_by(District_ID) %>% arrange(Year,-Date) %>%   filter(!duplicated(Year)) %>% ungroup()
fill_grid = as.data.table(fill_grid)
setDT(fill_grid)[, fill_Date := na.locf(Date,na.rm = FALSE), by=District_ID]
setDT(fill_grid)[, fill_Date := na.locf0(fill_Date,fromLast = T), by=District_ID]


test = fread('https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/TX0700020/JSON')
https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/IL3141937/JSON
pref <- 'https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/TX0700020/JSON'
suf <- '/JSON'
viols <- pblapply(return_systems$PWS_ID,function(x) 
  fread(paste0(pref,x,suf),fill=T),cl = 10)

qnames <- gsub(' ','%20',toupper(tx_counties$NAME))
urls = paste0('https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/COUNTYSERVED/',qnames,'/STATE/TX/excel')
library(RCurl)
vlist <- pblapply(urls,function(x) fread(x,fill=T),cl = 5)
vdt <- rbindlist(vlist,fill=T)

qnames[which(sapply(vlist,nrow)>=10000)]
#Drought score
#Population served, logged
#Total storage per 1,000 people 
#Number of interconnections, logged 
# Groundwater

#% customers served retail 
#Average daily consumption per1,000 people
#% Democratic vote

#Median household income, logged, #% houses built after 1980, #% rural,#% Black, #% Hispanic, #% four-year college degree



tx_info = tx_info[!is.na(tx_info$`Activity Status`),]
tx_info = tx_info[tx_info$Owner_Type=='District',]




setkey(tx_district_info,District_ID)
setkey(dinfo,District_ID)
dinfo = tx_district_info[dinfo,]


dinfo$`First Reported Date` = dmy(dinfo$`First Reported Date`)


fill_grid = data.table(expand.grid(District_ID = as.character(unique(dinfo$District_ID)),Year = 2000:2020))


fill_grid = fill_grid[order(District_ID,Year),]
fill_grid$fill_Date = fill_grid$fill_Date - year(date_decimal(fill_grid$fill_Date)) + fill_grid$Year


library(sf)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
pws_combined = readRDS('scratch/proj5/pws_combined.RDS')
pws_combined = pws_combined[pws_combined$PWS_ID %in% unlist(dinfo$PWS_ID),]
pws_combined$District_ID = tx_info$District_ID[match(pws_combined$PWS_ID,tx_info$PWS_ID)]
district_sf = aggregate(pws_combined[,!colnames(pws_combined) %in% c('PWS_ID','District_ID','Adopt','In_Analysis','In_Houston','Prop_Urban')],
                        list(District_ID = pws_combined$District_ID),FUN = sum)
district_sf <- st_union(district_sf,by_feature = T)

# %>% group_by(a.x) %>% summarise(mean(a.y))

tceq_wd = st_read('spatial_inputs/water_districts_shp/TCEQ_WaterDistricts.shp')
tceq_wd = st_make_valid(tceq_wd)
tceq_wd = st_transform(tceq_wd,albersNA)
tceq_wd = tceq_wd[!tceq_wd$DISTRICT_I %in% district_sf$District_ID,]
tceq_wd = tceq_wd %>% rename(District_ID = DISTRICT_I)

district_sf = rbind(district_sf,tceq_wd[,'District_ID'])
district_sf = st_make_valid(district_sf)




fill_grid$Prop_Urban <- district_sf$Prop_Urban[match(fill_grid$District_ID,district_sf$District_ID)]


#saveRDS(object = demo_dt,'scratch/proj6/district_demo_data.RDS')
#demo_dt = readRDS('scratch/proj5/pws_demo_data.RDS')
setkey(demo_dt,District_ID,Year)
fill_grid = as.data.table(fill_grid)
setkey(fill_grid,District_ID,Year)
fill_grid = demo_dt[fill_grid ,]




setnames(debt_dt,"FiscalYear",'Year')
setkey(debt_dt,District_ID,Year)
setkey(fill_grid,District_ID,Year)
fill_grid = debt_dt[fill_grid,]

fill_grid$Total_Revenue = replace_na(fill_grid$`ENTERPRISE FUND - OPERATING REVENUES`,0) + replace_na(fill_grid$`GENERAL FUND - TOTAL REVENUES`,0)
fill_grid$Total_Expenditure = replace_na(fill_grid$`ENTERPRISE FUND - OPERATING EXPENSES`,0) + replace_na(fill_grid$`GENERAL FUND - TOTAL EXPENDITURES`,0)
fill_grid$Fund_Balance = replace_na(fill_grid$`GENERAL FUND - FUND BALANCE`,0)
fill_grid$Health_Violation_Count = replace_na(fill_grid$Health_Viol,0)
fill_grid$Management_Violation_Count = replace_na(fill_grid$NonHealth_Viol,0)
fill_grid$Water_Units = fill_grid$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`



vars_to_lag = c("Total_Debt_Service_Outstanding",'Total_Revenue','Total_Expenditure',
                'Fund_Balance','Total_Tax_Rate','DOC_ID','DOC_URL','Health_Violation_Count',
                'Management_Violation_Count','Water_Units')
lag_vars = paste(vars_to_lag,'L1',sep='_')
fill_grid[order(District_ID,Year), (lag_vars) := data.table::shift(.SD, n = 1,fill= NA,type="lag"), .SDcols=vars_to_lag,by = District_ID]

setkey(fill_grid,District_ID,Year)
setkey(dinfo,District_ID)
dinfo_dt = fill_grid[dinfo,]

dinfo_dt = dinfo_dt[,!c('CCN','First Reported Date','BOARD_NUMBER','i.District_Name','join_time','common','zeros','y','Date','DATE_SUBMITTED','GO_Service_Outstanding','REV_Service_Outstanding'),with=F]
dinfo_dt = dinfo_dt[,!grep('^ENTERPRISE FUND|^GENERAL FUND|^WASTEWATER CUST|^WATER CUST|TAX|FISCAL YEAR|BONDS OUTSTANDING|CURRENT ASSESSED',colnames(dinfo_dt),value=T),with=F]

# Last observation carried forward from last row of group
dinfo_dt <- dinfo_dt[, District_Name := na.locf0(District_Name, fromLast = TRUE), by = District_ID]
# Last observation carried forward for first row of group
dinfo_dt <- dinfo_dt[, District_Name := na.locf(District_Name), by = District_ID]

dinfo_dt = dinfo_dt[dinfo_dt$Year %in% 2009:2017,]
dinfo_dt = dinfo_dt[dinfo_dt$Status=='ACTIVE',]
dinfo_dt = dinfo_dt[!is.na(dinfo_dt$DOC_ID_L1),]
dinfo_dt = dinfo_dt[dinfo_dt$District_Type %in% c('FWSD','MUD','SUD','WCID'),]
dinfo_dt = dinfo_dt[!is.na(dinfo_dt$Health_Violation_Count),]

vars_to_ratio = c("Total_Debt_Service_Outstanding_L1",'Total_Revenue_L1','Total_Expenditure_L1','Fund_Balance_L1')
ratio_vars = paste(vars_to_ratio,'Per_Connection',sep='_')
dinfo_dt[ , (ratio_vars) := .SD/(Total_Service_Connections), .SDcols=vars_to_ratio,by = District_ID]


dinfo_dt$Perc_Change_Service_Connections = 100 *{(dinfo_dt$Water_Units - dinfo_dt$Water_Units_L1)/dinfo_dt$Water_Units_L1}
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(-Inf)] <- 0
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(Inf)] <- 0
dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50] = dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50]/100



dinfo_dt$Management_Violation_L1 = (dinfo_dt$Management_Violation_Count_L1>0) + 1
dinfo_dt$Health_Violation_L1 = (dinfo_dt$Health_Violation_Count_L1>0) + 1
dinfo_dt$Daily_MGD_Connection = dinfo_dt$Average_Daily_Consump_MGD/dinfo_dt$Total_Service_Connections
vars_to_binary = c('Wholesaler','Multiple_Systems','Retail_Wastewater')
vars_to_scale = c("Median_Year_Structure_Built"  ,"Median_Home_Value" ,"Prop_Bach",'Prop_Nonwhite',"Prop_Owner_Occupied",
                  "Prop_Urban","CBSA_Service_HHI","Perc_Change_Service_Connections",'DSCI_52week_Average',
                  "Total_Debt_Service_Outstanding_L1_Per_Connection","Total_Revenue_L1_Per_Connection"   ,              
                  "Total_Expenditure_L1_Per_Connection","Fund_Balance_L1_Per_Connection",
                  'Management_Violation_Count_L1',"Health_Violation_Count_L1",'Total_Service_Connections','Daily_MGD_Connection')
scale_labs = paste0('Std_',vars_to_scale)
dinfo_dt[ , (scale_labs) := lapply(.SD, scale), .SDcols=vars_to_scale]
saveRDS(file = 'scratch/proj6/df_for_model.RDS',object = dinfo_dt)

library(INLA)
dinfo_dt$Have_Health_Violation = (dinfo_dt$Health_Violation_Count>0)+0
dinfo_dt$Total_Viol_Count = dinfo_dt$Health_Violation_Count + dinfo_dt$Management_Violation_Count
dinfo_dt$Total_Viol_Count_L1 = dinfo_dt$Health_Violation_Count_L1 + dinfo_dt$Management_Violation_Count_L1

idf = apply(dinfo_dt,2,unlist)

#dvars = c('Total_Viol_Count','Health_Violation_Count','Management_Violation_Count')
dvars = c('Health_Violation_Count')

form <- "Health_Violation_Count ~
Wholesaler + Retail_Wastewater + Multiple_Systems +
Health_Violation_L1 + Management_Violation_L1 +
Std_Total_Service_Connections + Std_Daily_MGD_Connection + 
Std_Median_Home_Value+
Std_Median_Year_Structure_Built+
Std_Prop_Nonwhite+Std_Prop_Owner_Occupied +
Std_Prop_Urban+Std_CBSA_Service_HHI +
Std_Perc_Change_Service_Connections+Std_Total_Debt_Service_Outstanding_L1_Per_Connection +
Std_Total_Revenue_L1_Per_Connection+
Std_Fund_Balance_L1_Per_Connection+ 
Std_DSCI_52week_Average +
f(District_ID,model = 'iid') +
f(Year,model = 'rw1')" 

pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))

mod <- inla(as.formula(forms),family = 'nbinomial',data =idf,
            control.predictor=list(compute=TRUE),
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
            control.fixed = list(prec.intercept=1))

ggplot(data = dinfo_dt[dinfo_dt$Total_Service_Connections<15000,]) +
  geom_point(aes(y = Management_Violation_Count,x = Total_Service_Connections)) + 
  geom_vline(xintercept = c(500,3300,10000))

ggplot(data = dinfo_dt[dinfo_dt$Total_Service_Connections<15000,]) +
  geom_point(aes(y = Health_Violation_Count,x = Total_Service_Connections)) + 
  geom_vline(xintercept = c(500,3300,10000))

plot(, data = dinfo_dt[dinfo_dt$Total_Service_Connections<15000,])
vline(xintercept = 10000)


form2 <- "Health_Violation_Count ~
Wholesaler + Retail_Wastewater + Multiple_Systems +
Health_Violation_L1 + Management_Violation_L1 +
Std_Total_Service_Connections + 
Std_Daily_MGD_Connection + 
Std_Median_Home_Value+
Std_Median_Year_Structure_Built+
Std_Prop_Nonwhite+
Std_Prop_Urban+
Std_CBSA_Service_HHI +
Std_Perc_Change_Service_Connections+
Std_Total_Debt_Service_Outstanding_L1_Per_Connection +
Std_Total_Revenue_L1_Per_Connection+
Std_Fund_Balance_L1_Per_Connection +
Std_DSCI_52week_Average +
f(District_ID,model = 'iid') +
f(Year,model = 'rw1')" 


mod2 <- inla(as.formula(form2),family = 'nbinomial',data =idf,
             control.predictor=list(compute=TRUE),
             control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
             control.fixed = list(prec.intercept=1))


mod_list[[2]]$waic$waic
mod2$waic$waic

saveRDS(mod_list,file = 'scratch/proj6/full_mod_results.RDS')




fe_df = mod_list[[2]]$summary.fixed
fe_df$FE = rownames(fe_df)
(figure1 = ggplot(data = fe_df[(!grepl('Intercept',fe_df$FE)) ,]) + 
    geom_errorbar(aes(x = FE,ymax = `0.975quant`,ymin = `0.025quant` ),position=position_dodge(width = 0.5),width=0.5) + 
    geom_point(aes(x = FE,y = mean),,position=position_dodge(width = 0.5),pch=21) + 
    coord_flip() + theme_bw() + scale_fill_manual(values= c('white','black')) + #,'white','#FF7F0E')) + 
    scale_y_continuous(name = '95% credible interval and posterior mean') +# ,limits=c(-1.2,1.2)) +
    #scale_x_discrete(name = 'Standarized regression coefficients')
    #scale_colour_tableau(name='Violation type') + #,labels=expression(paste("standardized(", beta,")"))) + 
    guides(fill=FALSE) + 
    theme(axis.title = element_text(size=12),legend.position= c(0.8,0.2),axis.title.y = element_blank(),
          legend.background = element_rect(fill = alpha('white',0.25)),legend.title = element_blank(),
          legend.text = element_blank()) + 
    ggtitle('Health violation occurence by fiscal year') +
    geom_hline(yintercept=0,lty=2,col = 'grey50') +
    NULL)




summary(mod_list[[2]])

sapply(mod_list_p,function(x) x$waic$waic)
sapply(mod_list,function(x) x$waic$waic)
summary(mod_list[[2]])





figure1

cor(idf$Prop_Bach,idf$Median_Home_Value,use = 'pairwise.complete.obs')





dvs = c('HEALTH_VIOL_OCCURENCE_PERIOD')

temp = temp %>% group_by(DISTRICT_ID,PWS_ID) %>% 
  arrange(PWS_ID,DISTRICT_ID,PERIOD) %>% 
  mutate(WASTEWATER_SFU_P1 = lag(WASTEWATER_SFU),
         WASTEWATER_SFU_F1 = lead(WASTEWATER_SFU),
         WATER_SFU_P1 = lag(WATER_SFU),
         WATER_SFU_F1 = lead(WATER_SFU)) %>% 
  mutate(WASTEWATER_SFU = ifelse({is.na(WASTEWATER_SFU) | (WASTEWATER_SFU==0 & WASTEWATER_SFU_P1>0 & WASTEWATER_SFU_F1>0)},
                                 (WASTEWATER_SFU_P1+WASTEWATER_SFU_F1)/2,WASTEWATER_SFU),
         WATER_SFU = ifelse({is.na(WATER_SFU)| (WATER_SFU==0 & WATER_SFU_P1>0 & WATER_SFU_F1>0)},
                            (WATER_SFU_P1+WATER_SFU_F1)/2,WATER_SFU))

temp = temp %>% group_by(DISTRICT_ID,PWS_ID) %>%  arrange(PWS_ID,DISTRICT_ID,PERIOD) %>% 
  mutate(MVP1 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n=1),MVP2 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n=2),MVP3 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n =3),
         HVP1 = lag(HEALTH_VIOL_COUNT_PERIOD,n=1),HVP2 = lag(HEALTH_VIOL_COUNT_PERIOD,n=2),HVP3 = lag(HEALTH_VIOL_COUNT_PERIOD,n =3)) %>%
  mutate(MVP1 = ifelse(is.na(MVP1),0,MVP1),MVP2 = ifelse(is.na(MVP2),0,MVP2),MVP3 = ifelse(is.na(MVP3),0,MVP3),
         HVP1 = ifelse(is.na(HVP1),0,HVP1),HVP2 = ifelse(is.na(HVP2),0,HVP2),HVP3 = ifelse(is.na(HVP3),0,HVP3)) %>%
  mutate(MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years = MVP1 + MVP2 + MVP3,HEALTH_VIOL_COUNT_PERIOD_Prior3Years = HVP1 + HVP2 + HVP3)





temp = temp[temp$TOTAL_REVENUE_L1  <100000000,]
temp$REV_PER_CONNECTION_L1 = temp$TOTAL_REVENUE_L1/temp$Service_Connections
temp$EXP_PER_CONNECTION_L1 = temp$TOTAL_EXPENDITURE_L1/temp$Service_Connections
temp$Median_Structure_Age = temp$FISCAL_YEAR_END_YEAR - temp$Median_Year_Structure_Built 
temp$Median_Structure_Age[temp$Median_Structure_Age<0] <- 0
temp$WHOLESALE_REGULAR_POP[is.na(temp$WHOLESALE_REGULAR_POP)] <- 0
temp$Any_Debt_Issued_P5 = (temp$Debt_Issued_P5 >0) + 0
temp$log_debt = log(temp$Debt_Issued_P5+1)
temp$WHOLESALE <- (temp$WHOLESALE_REGULAR_POP>0) + 0
temp = temp %>% group_by(PWS_ID) %>% arrange(PWS_ID,FISCAL_YEAR_END_YEAR) %>% mutate(Prior_Total_Population = lag(Total_Population,1)) %>%
  mutate(Prop_Population_Change = (Total_Population - Prior_Total_Population) / Prior_Total_Population)


temp = temp[!is.na(temp$CBSA_CODE),]

temp$REV_PER_CONNECTION_L1 = temp$TOTAL_REVENUE_L1/temp$SERVICE_CONNECTIONS_L1












tract_sp = tx_tracts[!duplicated(tx_tracts@data$GEOID),]
tx_tract_df = fortify(tract_sp, region = 'GEOID')
tx_tract_df$GEOID = as.character(tx_tract_df$id)
tx_tract_df = left_join(tx_tract_df,tract_sp@data)
tx_tract_df = left_join(tx_tract_df,tract_acs_data  %>% ungroup(GEOID) %>% filter(Year == 2016) %>% mutate(GEOID = as.character(GEOID)))
water_district_df = fortify(keep_polys)
water_district_df$Shape_ID = water_district_df$id
water_district_df = left_join(water_district_df,tdf)
library(ggthemes)

brk = seq(50,200,50) * 1000
lb = paste0('$',seq(50,200,50),'k')

dallas_fips = c("085","113","121","139","213","231","257","397","221","251","367","439")
houston_fips = c("039","167","071","157","201","291","339","473")
austin_fips = c("021","055","209","453","491")
san_antonio_fips = c("029","091","187","493")
metro_fips = list(dallas_fips,houston_fips,austin_fips,san_antonio_fips)
names(metro_fips)<- c('Dallas-Fort Worth MSA','Houston-Galveston-Brazoria MSA','Austin-San Marcos MSA',"San Antonio MSA")

gg_msa_income = lapply(seq_along(metro_fips), function(m) {
  ggplot() + theme_map() + 
    geom_polygon(data = tx_tract_df[tx_tract_df$COUNTYFP %in% metro_fips[[m]] & !is.na(tx_tract_df$Median_Income),],aes(y = lat,x=long,group = group,fill = Median_Income)) +
    scale_fill_viridis_c(direction = -1,option = 'B',name = 'Tract median\n income, 2016',labels = lb,breaks=brk) +
    scale_colour_manual(values = 'black',labels = 'district',name = '')+
    geom_path(data = water_district_df[water_district_df$COUNTY_FIPS %in% paste0('48',metro_fips[[m]]),],aes(x = long,y= lat,group = group,colour = 'black'),alpha= 0.9,lwd=0.25) +
    ggtitle(names(metro_fips)[m]) + theme(legend.background = element_rect(fill = alpha('white',0.3),colour= NULL),title = element_text(size = 12),
                                          legend.text = element_text(size = 12),legend.title = element_text(size = 12)) + 
    guides(fill = guide_colorbar(order = 0))
})



library(ggpubr)
library(gridExtra)
library(cowplot)
prow <- plot_grid( gg_msa_income[[1]] + theme(legend.position="none"),
                   gg_msa_income[[2]] + theme(legend.position="none"),
                   gg_msa_income[[3]] + theme(legend.position="none"),
                   gg_msa_income[[4]] + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2)

legend <- get_legend( gg_msa_income[[1]] + theme(legend.background = element_rect(colour = NA)))
title <- ggdraw() + draw_label('Median income (2016) by census tract and water districts', fontface='bold')
prow_legend = prow + draw_grob(legend, 2.9/3.3, 0, .3/3.3, 0.4) 
# now add the title
plot_grid(title, prow_legend, ncol=1, rel_heights=c(0.1, 1)) 

water_district_df = left_join(water_district_df,viol_df[viol_df$Period_Start_Year>=2006,] %>% group_by(PWS_ID,`Is Health Based`) %>% summarise(hviol_since_2006 = n()) %>% 
                                filter(`Is Health Based` == 'Y'))
water_district_df$hviol_since_2006[is.na(water_district_df$hviol_since_2006)] <- 0

#all_polys@data <- data.frame(PWS_ID = getSpPPolygonsIDSlots(pws))
pws_df = fortify(all_polys,region = getSpPPolygonsIDSlots(all_polys))
pws_df$PWS_ID = pws_df$id
pws_df$PWS_ID[!grepl('TX',pws_df$PWS_ID)] <- tdf$PWS_ID[match(pws_df$PWS_ID[!grepl('TX',pws_df$PWS_ID)], tdf$DISTRICT_ID)]
pws_df = pws_df[!is.na(pws_df$PWS_ID),]
pws_df$DISTRICT_IN_SAMPLE = (pws_df$PWS_ID %in% tdf$PWS_ID) + 0

pws_df = left_join(pws_df,viol_df[viol_df$Period_Start_Year>2006,] %>% group_by(PWS_ID,`Is Health Based`) %>% summarise(hviol_since_2006 = n()) %>% 
                     filter(`Is Health Based` == 'Y'))
pws_df$hviol_since_2006[is.na(pws_df$hviol_since_2006)] <- 0
pws_df$hviol_since_2006[pws_df$hviol_since_2006>50]<- 50 


tx_counties = spTransform(tx_counties,CRS(proj4string(keep_polys)))
tx_counties_df = fortify(tx_counties)
geom_path(data = tx_counties_df,aes(x = long,y=lat,group = group),col = 'grey80',lwd=0.25)+
  theme_map() + 

ggplot() + 
  geom_path(data = tx_counties_df,aes(x = long,y=lat,group = group),col = 'grey80',lwd=0.25)+
  theme_map() + 
  geom_polygon(data = pws_df,aes(y = lat,x = long,fill = hviol_since_2006,group = group)) + 
  geom_path(data = pws_df[pws_df$DISTRICT_IN_SAMPLE==1,],aes(y = lat,x = long,group = group,colour = 'black'),lwd=0.25)+
  scale_fill_viridis_c(option = 'D',direction = -1,name = '# health violations',
                       breaks=seq(0,50,10),limits=c(0,50),labels=c(0,10,20,30,40,'50+')) + 
  theme(legend.position = c(0.2,0.22),title = element_text(size = 14),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.background = element_rect(fill = NA,colour=NA)) + 
  scale_colour_manual(name = '',values=c('black'),labels = c('District in sample'))+
  ggtitle('Retail water district health violations, 2007 to present')


tx_msa = c('48015','48071','48291','48201','48039','48157','48473','48167','48339')
keep_houston = tdf$Shape_ID[tdf$Primary_County %in% c('WALLER','AUSTIN','CHAMBERS','LIBERTY','GALVESTON','MONTGOMERY','HARRIS','FORT BEND','BRAZORIA')]
keep_counties = tx_counties@data$COUNTY_FIPS[toupper(tx_counties@data$NAME) %in% c('WALLER','AUSTIN','CHAMBERS','LIBERTY','GALVESTON','MONTGOMERY','HARRIS','FORT BEND','BRAZORIA')]


summary(tx_counties_df[tx_counties_df$id %in% keep_counties, ])
summary( pws_df[pws_df$DISTRICT_IN_SAMPLE==1 & pws_df$id %in% keep_houston,])
head(tx_counties_df[tx_counties_df$id %in% keep_counties, ])
ggplot() + 
  geom_path(data = tx_counties_df[tx_counties_df$id %in% keep_counties, ],aes(x = long,y=lat,group = group),col = 'grey80',lwd=1)+
  theme_map()  + 
  #geom_polygon(data = pws_df[pws_df$id %in% keep_houston,],aes(y = lat,x = long,fill = hviol_since_2006,group = group)) 
  geom_path(data = pws_df[pws_df$DISTRICT_IN_SAMPLE==1 & pws_df$id %in% keep_houston,],aes(y = lat,x = long,group = group,colour = 'black'),lwd=0.25)+
  scale_fill_viridis_c(option = 'D',direction = -1,name = '# health violations') + 
  theme(legend.position = c(0.2,0.22),title = element_text(size = 14),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.background = element_rect(fill = NA,colour=NA)) + 
  scale_colour_manual(name = '',values=c('black'),labels = c('District in sample'))+
  ggtitle('Houston retail water district health violations, 2007 to present')




ggplot() + 
  geom_path(data = tx_counties_df,aes(x = long,y=lat,group = group),col = 'grey50',lwd=0.25)+
  theme_map() + 
  geom_polygon(data = water_district_df,aes(y = lat,x = long,fill = hviol_since_2006,group = group)) + 
  scale_fill_viridis_c(option = 'D',direction = -1,name = '# health violations') + 
  theme(legend.position = c(0.2,0.15),title = element_text(size = 14),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.background = element_rect(fill = NA)) + 
  ggtitle('Retail water district health violations, 2006 to present')





vplot = viol_df[viol_df$PWS_ID %in% tdf$PWS_ID,]
vplot$COMP_BEGIN_DDATE = decimal_date(dmy(vplot$`Compliance Period Begin Date`))
vplot = vplot[vplot$COMP_BEGIN_DDATE>1993,]
vplot$VCAT = ifelse(vplot$`Is Health Based` == 'Y','Health violation','Management violation')
ggplot(vplot,aes(x = dmy(`Compliance Period Begin Date`),fill = `Is Health Based`)) + 
  geom_histogram(trim=T) + facet_wrap(~VCAT) + 
  scale_x_date(name = 'Compliance period') + 
  scale_fill_tableau(palette = 'tableau20',labels =c('Management violation','Health violation')) + 
  theme(legend.position = c(0.1,0.3),legend.title = element_blank()) +
  scale_y_continuous('Violation frequence for districts in sample') + guides(fill=FALSE)


length(unique(temp$DISTRICT_ID))
table(data.frame(table(temp$DISTRICT_ID))$Freq)
table(is.na(temp$Median_Income))
table(temp$Type[!duplicated(temp$DISTRICT_ID)])

table(temp$FISCAL_YEAR_START_YEAR)
viol_df$`Contaminant Name`[viol_df$`Contaminant Name`=='']
unique(viol_df$`Violation Type`[viol_df$`Is Health Based`=='Y'])
unique(viol_df$`Contaminant Name`[viol_df$`Is Health Based`=='Y'])
class(dmy(vplot$`Compliance Period Begin Date`))


