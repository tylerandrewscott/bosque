library(tigris)
library(rgeos)
library(rgdal)
library(sp)
library(maptools)
library(lucr)
library(tidyquant)
library(spdep)
library(lubridate)
library(pbapply)
library(tidyverse)

#Drought score
#Population served, logged
#Total storage per 1,000 people 
#Number of interconnections, logged 
# Groundwater

#% customers served retail 
#Average daily consumption per1,000 people
#% Democratic vote

#Median household income, logged, #% houses built after 1980, #% rural,#% Black, #% Hispanic, #% four-year college degree

library(tidyverse)
library(lubridate)
library(survival)
library(mstate)
library(sf)
library(geojsonsf)
library(data.table)
library(tigris)
library(lwgeom)
library(pbapply)



dinfo = fread('input/twdd_records/district_list_2019-01-03.csv',stringsAsFactors = F)
dinfo$PWS_ID[dinfo$PWS_ID == "NA"] <- NA
dinfo$District_ID = as.character(dinfo$District_ID)
dinfo = dinfo[!is.na(PWS_ID)]
setkey(dinfo,District_ID)
dinfo$PWS_ID = str_split(dinfo$PWS_ID,'\\|')

tx_info = as.data.table(readRDS('scratch/pws_details_2019-02-05.RDS') %>% dplyr::select(-PWS_ID_EX))
tx_systems = fread("input/epa_sdwis/tx_water_system_geographic_area_2-5-18.csv",stringsAsFactors = F) %>% 
  rename(PWS_ID = `PWS ID`,PWS_NAME =  `PWS Name`)
setkey(tx_systems,PWS_ID)
setkey(tx_info,PWS_ID)
tx_info = tx_systems[tx_info,]
tx_info = tx_info[!is.na(tx_info$`Activity Status`),]
tx_info = tx_info[tx_info$Owner_Type=='District',]
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
tx_info$District_ID = unlist(sapply(tx_info$PWS_ID,function(p) ifelse(any(grepl(p,dinfo$PWS_ID)),dinfo$District_ID[grepl(p,dinfo$PWS_ID)],NA),simplify = T))
tx_info = tx_info[tx_info$`Activity Status` != "Inactive",]
tx_info$GW = ifelse(tx_info$`GW or SW Code`=='GW',1,0)

combo_vars = c('Total_Storage_MG','Interconnections',"GW",
               'Average_Daily_Consump_MGD','Total_Product_MGD',
               'Residential_Service_Connections','Wholesale_Service_Connections','Nonresidential_Service_Connections',
               'Total_Service_Connections')
tx_district_info = tx_info[,lapply(.SD,sum,na.rm=T),.SDcols = combo_vars,by = District_ID]
district_count = tx_info[,.N,by = District_ID]
setkey(district_count,District_ID)
tx_district_info = district_count[tx_district_info,]
setnames(tx_district_info,old = 'N',new = 'Num_PWS_Operated')

setkey(tx_district_info,District_ID)
setkey(dinfo,District_ID)
dinfo = tx_district_info[dinfo,]
dinfo$District_Type = dinfo$Type
dinfo$District_Type[dinfo$District_Type %in% c('WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
dinfo$District_Type[dinfo$District_Type %in% c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
dinfo$District_Type[dinfo$District_Type %in% c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
dinfo$District_Type[dinfo$District_Type %in% c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
dinfo$District_Type[!dinfo$District_Type %in% c('SUD','FWSD','MUD','WCID')] <- 'Other'
dinfo$`First Reported Date` = dmy(dinfo$`First Reported Date`)

cfips = tigris::fips_codes
cfips$CFIPS = paste0(cfips$state_code,cfips$county_code)
cfips$county = gsub(' County','',cfips$county)
cfips = cfips[cfips$state=='TX',]
dinfo$CFIPS = cfips$CFIPS[match(toupper(dinfo$Primary_County),toupper(cfips$county))]
#tx_systems = tx_systems[tx_systems$Total_Storage_MG<=500,]
#tx_systems = tx_systems[!is.na(tx_systems$Total_Storage_MG),]

library(readxl)
cbsa_ref = read_excel('input/census/cbsa_delineations.xls',skip = 1) %>% 
  mutate(CFIPS = paste0(formatC(`FIPS State Code`,width = 2,flag = 0),formatC(`FIPS County Code`,width=3,flag=0)))
dinfo$CBSA_CODE = cbsa_ref$`CBSA Code`[match(dinfo$CFIPS,cbsa_ref$CFIPS)]
dinfo$Nonwholesale_Service_Connections = dinfo$Residential_Service_Connections + dinfo$Nonresidential_Service_Connections

connections_per_cbsa = dinfo %>% group_by(CBSA_CODE) %>% summarise(Total_CBSA_Connections = sum(Nonwholesale_Service_Connections,na.rm = T))
dinfo$Prop_CBSA_Nonwholesale_Connections = dinfo$Nonwholesale_Service_Connections / connections_per_cbsa$Total_CBSA_Connections[match(dinfo$CBSA_CODE,connections_per_cbsa$CBSA_CODE)]
dinfo$Prop_CBSA_Wholesale_Connections = dinfo$Wholesale_Service_Connections / connections_per_cbsa$Total_CBSA_Connections[match(dinfo$CBSA_CODE,connections_per_cbsa$CBSA_CODE)]
dinfo$Prop_CBSA_Nonwholesale_Connections[is.na(dinfo$Prop_CBSA_Nonwholesale_Connections)] <- 0
dinfo$Prop_CBSA_Wholesale_Connections[is.na(dinfo$Prop_CBSA_Wholesale_Connections)] <- 0

cbsa_service_hhi = do.call(rbind,lapply(sort(unique(dinfo$CBSA_CODE)),function(c) {
  data.frame(CBSA_CODE = c,CBSA_HHI = hhi::hhi(x =data.frame(dinfo[!is.na(dinfo$CBSA_CODE) & 
                                                                     dinfo$CBSA_CODE==c,]),s='Prop_CBSA_Nonwholesale_Connections'))}))
cbsa_wholesale_hhi = do.call(rbind,lapply(sort(unique(dinfo$CBSA_CODE)),function(c) {
  data.frame(CBSA_CODE = c,CBSA_HHI = hhi::hhi(x =data.frame(dinfo[!is.na(dinfo$CBSA_CODE) & 
                                                                     dinfo$CBSA_CODE==c,]),s='Prop_CBSA_Wholesale_Connections'))}))

dinfo$CBSA_Wholesale_HHI = cbsa_wholesale_hhi$CBSA_HHI[match(dinfo$CBSA_CODE,cbsa_wholesale_hhi$CBSA_CODE)]
dinfo$CBSA_Service_HHI = cbsa_service_hhi$CBSA_HHI[match(dinfo$CBSA_CODE,cbsa_service_hhi$CBSA_CODE)]


fill_grid = data.table(expand.grid(District_ID = as.character(unique(dinfo$District_ID)),Year = 2000:2020))
audits = fread('input/tceq_audits/district_audits.csv')
library(lucr)
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
#audits[,Period:=Year - min(Year),by=District_ID] 
setkey(audits,District_ID,Year)
setkey(fill_grid,District_ID,Year)
fill_grid = audits[fill_grid,]
fill_grid = fill_grid %>% group_by(District_ID) %>% arrange(Year,-Date) %>%   filter(!duplicated(Year)) %>% ungroup()
fill_grid = as.data.table(fill_grid)
setDT(fill_grid)[, fill_Date := na.locf(Date,na.rm = FALSE), by=District_ID]
setDT(fill_grid)[, fill_Date := na.locf0(fill_Date,fromLast = T), by=District_ID]

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

urban = st_read('spatial_inputs/government_units/cb_2016_us_ua10_500k.shp')
urban = st_transform(urban,albersNA)
urban = urban[grepl('TX',urban$NAME10),]
#urban_tx = st_union(urban)
#urban_tx = st_make_valid(urban_tx)
inters <- st_intersects(district_sf,urban)

urban_props <- pbsapply(seq_along(inters),function(x) sum(st_area(st_intersection(district_sf[x,],
                                 urban[inters[[x]],])))/ st_area(district_sf[x,]),cl = 8)
district_sf$Prop_Urban <- urban_props 
district_sf$Area = st_area(district_sf)

fill_grid$Prop_Urban <- district_sf$Prop_Urban[match(fill_grid$District_ID,district_sf$District_ID)]
demos = fread('input/census/ACS_tracts/proj6/tract_data_2009_2017.csv',stringsAsFactors = F)
demos$GEOID = gsub('.*US','',demos$GEOID)
tracts = tigris::tracts(state = 'TX',class='sf',year = '2015')
tracts = st_transform(tracts,st_crs(albersNA))
pws_tract_inters = st_intersects(district_sf,tracts)
district_sf = st_transform(district_sf,st_crs(albersNA))
pws_over_tracts = pblapply(seq_along(pws_tract_inters),function(x) st_intersection(district_sf[x,],tracts[pws_tract_inters[[x]],]))

pws_tracts = do.call(rbind,pws_over_tracts)
pws_tracts$Total_Area = district_sf$Area[match(pws_tracts$District_ID,district_sf$District_ID)]
pws_tracts$prop_weight = st_area(pws_tracts)/pws_tracts$Total_Area

total_vars = c('Total_Population','White_Population','Household_Pop','Household_Owner_Occupied','Pop_Over_25','Pop_Bach')
summary_vars = c('Median_Income','Median_Home_Value','Median Year Structure Built')


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
#saveRDS(object = demo_dt,'scratch/proj6/district_demo_data.RDS')
#demo_dt = readRDS('scratch/proj5/pws_demo_data.RDS')
setkey(demo_dt,District_ID,Year)
fill_grid = as.data.table(fill_grid)
setkey(fill_grid,District_ID,Year)
fill_grid = demo_dt[fill_grid ,]


viol_dt = fread('input/epa_sdwis/proj6/TX_PWS_violation_report_2-4-19.csv',na.strings = c('-'))
viol_dt[viol_dt=='-'] <- NA

#viol_dt[is.na(viol_dt$Points),.(`Violation Type`,`Violation Code`,`Contaminant Name`,`Violation Category Code`)]
viol_dt$Period_Start=dmy(viol_dt$`Compliance Period Begin Date`)
viol_dt$Period_End=dmy(viol_dt$`Compliance Period End Date`)
viol_dt$Period_Start_Year = year(viol_dt$Period_Start)
setnames(viol_dt, "PWS ID", "PWS_ID")
viol_dt$Period_End[is.na(viol_dt$Period_End)] <- viol_dt$Period_Start[is.na(viol_dt$Period_End)] + years(1) - days(1)
viol_dt$Period_Start_DDate = decimal_date(viol_dt$Period_Start)
viol_dt$Period_End_DDate = decimal_date(viol_dt$Period_End)
viol_dt$District_ID = tx_info$District_ID[match(viol_dt$PWS_ID,tx_info$PWS_ID)]
viol_dt = viol_dt[!is.na(viol_dt$District_ID),]

viol_dt[, join_time:=Period_Start_DDate]
fill_grid[, join_time:=fill_Date]
setkey(viol_dt, District_ID, join_time)
setkey(fill_grid, District_ID, join_time)
viol_dt = fill_grid[viol_dt, roll = -1]
viol_counts = dcast(viol_dt[,.N,by = .(District_ID,fill_Date,`Is Health Based`)],District_ID + fill_Date ~ `Is Health Based`,value.var = 'N')
setnames(viol_counts,c('N',"Y"),c('NonHealth_Viol','Health_Viol'))
viol_counts$NonHealth_Viol = replace_na(viol_counts$NonHealth_Viol,0)
viol_counts$Health_Viol = replace_na(viol_counts$Health_Viol,0)
setnames(viol_counts, c("Y","N"), c("Health_Violation_Count",'Management_Violation_Count'))
setkey(viol_counts, District_ID, fill_Date)
setkey(fill_grid,District_ID,fill_Date)
fill_grid = viol_counts[fill_grid,]


test = viol_dt[viol_dt$`Is Health Based`=='Y',]
vcount <- test[,.N,by=PWS_ID]
vcount$Service_Connections <- viol_dt$`Service Connections Count`[match(vcount$PWS_ID,viol_dt$PWS_ID)]
ggplot(data = vcount[vcount$Service_Connections<15000,]) + 
  geom_point(aes(x = Service_Connections,y = N)) +
  geom_vline(xintercept = c(500,3300,10000))



library(readxl)
#dinfo = read_csv('input/twdd_records/district_list_2019-01-03.csv')
debt = read_csv('input/tbrb/Debt_Outstanding_By_Local_Government.csv')  %>%
  filter(GovernmentType %in% c('WD')) 
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

debt = debt %>% group_by(District_Name,FiscalYear,PledgeType) %>% summarise_if(is.numeric,sum,na.rm=T)

debt_spread = Reduce(full_join,list(
  debt %>% ungroup() %>% 
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalPrincipalOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalPrincipalOutstanding = sum(TotalPrincipalOutstanding)) %>% 
    spread(PledgeType,TotalPrincipalOutstanding,fill=0) %>% rename(GO_Principal_Outstanding = GO,REV_Principal_Outstanding = REV),
  debt %>% ungroup() %>% 
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalInterestOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalInterestOutstanding = sum(TotalInterestOutstanding)) %>% 
    spread(PledgeType,TotalInterestOutstanding,fill=0) %>% rename(GO_Interest_Outstanding = GO,REV_Interest_Outstanding = REV),
  debt %>%ungroup() %>% 
    dplyr::select(District_Name,FiscalYear,PledgeType,TotalDebtServiceOutstanding) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TotalDebtServiceOutstanding= sum(TotalDebtServiceOutstanding)) %>% 
    spread(PledgeType,TotalDebtServiceOutstanding,fill=0) %>% rename(GO_Service_Outstanding = GO,REV_Service_Outstanding = REV),
  debt %>%ungroup() %>% 
    dplyr::select(District_Name,FiscalYear,PledgeType,TaxRateTotal) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TaxRateTotal = sum(TaxRateTotal)) %>% 
    spread(PledgeType,TaxRateTotal,fill=0) %>% dplyr::select(-REV) %>% rename(TaxRate = GO),
  debt %>% ungroup() %>% 
    dplyr::select(District_Name,FiscalYear,PledgeType,TaxableValue) %>% 
    group_by(District_Name,FiscalYear,PledgeType) %>%
    mutate(TaxableValue = sum(TaxableValue)) %>% 
    spread(PledgeType,TaxableValue,fill = 0) %>% dplyr::select(-REV) %>% rename(TaxableValue = GO)))

debt_df = debt_spread %>% dplyr::select(District_Name,FiscalYear,GO_Service_Outstanding,REV_Service_Outstanding) %>%
  mutate(Total_Debt_Service_Outstanding = GO_Service_Outstanding + REV_Service_Outstanding)
debt_df$District_ID = dinfo$District_ID[match(debt_df$District_Name,dinfo$District_Name)]
debt_dt = as.data.table(debt_df)

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

dinfo_dt$Prop_Nonwholesale = 1 - dinfo_dt$Wholesale_Service_Connections/dinfo_dt$Total_Service_Connections

dinfo_dt$Wholesaler = (dinfo_dt$Wholesale_Service_Connections>0) + 0
dinfo_dt$Multiple_Systems = (dinfo_dt$Num_PWS_Operated > 1) + 0
dinfo_dt$Prop_Nonwhite = 1 - dinfo_dt$White_Population/dinfo_dt$Total_Population
dinfo_dt$Prop_Bach = dinfo_dt$Pop_Bach/dinfo_dt$Pop_Over_25
dinfo_dt$Prop_Owner_Occupied = dinfo_dt$Household_Owner_Occupied/dinfo_dt$Household_Pop
dinfo_dt$Perc_Change_Service_Connections = 100 *{(dinfo_dt$Water_Units - dinfo_dt$Water_Units_L1)/dinfo_dt$Water_Units_L1}
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(-Inf)] <- 0
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(Inf)] <- 0
dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50] = dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50]/100

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
dm_dt[,join_time:=decimal_date(ValidStart)]
dm_dt[order(CFIPS,join_time),DSCI_52week_Average:=rollmean(DSCI,k = 52,align = 'left',fill = NA),by=CFIPS]


dinfo_dt[,join_time:=fill_Date]
setkey(dm_dt,CFIPS,join_time)
setkey(dinfo_dt,CFIPS,join_time)

dinfo_dt = dm_dt[,.(CFIPS,join_time,DSCI_52week_Average)][dinfo_dt,roll=1]
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
























table(full_data$FISCAL_YEAR_END_YEAR)

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


