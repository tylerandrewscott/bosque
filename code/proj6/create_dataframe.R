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


tx_counties = tigris::counties(state = 'TX')
tx_counties@data$COUNTY_FIPS = tx_counties@data$GEOID

sdwis = read_csv('../../google_drive/putah/input/sdwis_data/quarter_system_records/water_system_summary_tx_2018_q2.csv',na = "-") %>% 
  rename(PWS_ID = `PWS ID`) %>% filter(`PWS Type Code` == 'CWS',`Activity Status Code`=='A')
sdwis = sdwis %>% dplyr::select(PWS_ID,`# of Facilities`,`Primary Source Code`,`Service Connections Count`,`Population Served Count`) %>% 
  rename(Population_Served = `Population Served Count`, Service_Connections = `Service Connections Count`,`Num_Facilities` = `# of Facilities`,Primary_Source = `Primary Source Code`)
library(rvest)
tx_dww = readRDS('../../google_drive/putah/input/state_dww_database_records/TX_2018-10-17_PWS.RDS')
dwwi = tx_dww$pws_master_list %>% filter(Status == 'A') %>% rename(Primary_County = `Pri. Cnty Served`,Primary_Source = `Pri. Src. Water Type`) %>% 
  dplyr::select(-href,-Type,-poc_hrefs,-oper_hrefs,-`Water System No.`,-`Water System Name`,-Status)
sdwis = left_join(sdwis,dwwi)
sdwis = sdwis[!is.na(sdwis$Primary_County),]

sales = read_csv('input/texas_dww/sales_connections_2018-10-02.csv') %>% filter(Sale_Type%in%c('S','E','O')) %>% group_by(Seller) %>% 
  summarise(Total_Buyer_Service_Pop = sum(Buyer_Service_Pop))


dflow = tx_dww$flow
dflow = dflow[!grepl('Error',dflow$UOM),]
dflow$Quantity = as.numeric(dflow$Quantity)
dflow$Quantity[dflow$UOM=='GPD'] = dflow$Quantity[dflow$UOM=='GPD']/1000000
dflow$UOM[dflow$UOM=='GPD'] <- 'MGD'
dflow$Quantity[dflow$UOM=='GPM'] = (dflow$Quantity[dflow$UOM=='GPM']* 1440) / 1000000
dflow$UOM[dflow$UOM=='GPM'] <- 'MGD'
dflow$Quantity[dflow$UOM=='GAL'] = dflow$Quantity[dflow$UOM=='GAL']/1000000
dflow$UOM[dflow$UOM=='GAL'] <- 'MGD'
dflow$Flow_Type = str_extract(dflow$Type,'[A-Z]{1,}')
dflow$Type = gsub('^- ','',str_extract(dflow$Type,'-.+$'))
flow_df = dflow %>% dplyr::select(-Flow_Type,-UOM) %>% spread(Type,Quantity)
flow_df = flow_df[flow_df$PWS_ID %in% tdf$PWS_ID,]

sdwis$COUNTY_FIPS = tx_counties@data$COUNTY_FIPS[match(sdwis$Primary_County,toupper(tx_counties@data$NAME))]
sdwis = left_join(sdwis,sdwis %>% group_by(COUNTY_FIPS) %>% summarise(Total_County_Connections = sum(Service_Connections,na.rm = T)))
sdwis$County_Connection_Share = 100 * sdwis$Service_Connections/sdwis$Total_County_Connections
cbsa_ref = read_excel('input/census/cbsa_delineations.xls',skip = 1) %>% mutate(COUNTY_FIPS = paste0(formatC(`FIPS State Code`,width = 2,flag = 0),formatC(`FIPS County Code`,width=3,flag=0)))
sdwis$CBSA_CODE = cbsa_ref$`CBSA Code`[match(sdwis$COUNTY_FIPS,cbsa_ref$COUNTY_FIPS)]
sdwis = left_join(sdwis,sdwis %>% group_by(CBSA_CODE) %>% summarise(Total_CBSA_Connections = sum(Service_Connections,na.rm = T)))
sdwis$CBSA_Connection_Share = 100 * sdwis$Service_Connections/sdwis$Total_CBSA_Connections



cbsa_service_hhi = do.call(rbind,lapply(sort(unique(sdwis$CBSA_CODE)),function(x) {
  data.frame(CBSA_CODE = x,CBSA_HHI = hhi::hhi(x =data.frame(sdwis[!is.na(sdwis$CBSA_CODE) & sdwis$CBSA_CODE==x,]),s='CBSA_Connection_Share'))}))

sdwis = left_join(sdwis,cbsa_service_hhi)
sdwis$Primary_Source = ifelse(sdwis$Primary_Source%in% c('GU','GUP'),'GW',sdwis$Primary_Source)

dinfo = read_csv('input/twdd_records/district_list_2018-11-16.csv')
dinfo$District_Name = gsub('([1-9]{1,3})-([A-Z])','\\1\\2',dinfo$District_Name)
dinfo$District_Name = gsub('MUNICIPAL UTILITY DISTRICT','MUD',dinfo$District_Name)
dinfo$District_Name = gsub("([0-9]{0,})\\sOF\\s[A-Z]{1,}\\sCOUNTY$","\\1",dinfo$District_Name)
dinfo$District_Name = gsub('PUBLIC UTILITY DISTRICT','PUD',dinfo$District_Name)

tdf = dinfo %>% rename(DISTRICT_ID = District_ID)

wd_count = dinfo %>% filter(Status %in% c('ACTIVE')) %>% group_by(Primary_County) %>% summarise(county_district_count = n())
tdf = left_join(tdf,wd_count)
tdf$Year_Created = year(mdy(tdf$Created))
district_services = read_csv('input/twdd_records/district_services_2018-10-16.csv')
tdf$SPECIAL_LAW = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function=='SPECIAL LAW'])+0
tdf$RETAIL_WASTEWATER = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function=='RETAIL WASTEWATER'])+0
tdf$WHOLESALE_WASTEWATER = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function=='WHOLESALE WASTEWATER'])+0
tdf$WHOLESALE_WATER = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function=='SUPPLY RAW (UNTREATED) OR WHOLESALE WATER'])+0
tdf$DRAINAGE_WASTEWATER = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function%in%c('DRAINAGE','FLOOD CONTROL')])+0
tdf$TAX_BOND_AUTH = (tdf$DISTRICT_ID %in% district_services$District_ID[district_services$Function%in%('TAX BOND AUTHORITY')])+0
tdf$YEAR_CREATED=year(mdy(tdf$Created))
tdf = tdf[tdf$Status=='ACTIVE',]
tdf = tdf %>% filter(!is.na(PWS_ID)) %>% dplyr::select(-Ended,-CCN,-PWS_Page,-District_Link,-Created,-Ended)
tdf$COUNTY_FIPS = tx_counties@data$COUNTY_FIPS[match(tdf$Primary_County,toupper(tx_counties@data$NAME))]
tdf = tdf[tdf$Status == 'ACTIVE',]
tdf = tdf[tdf$PWS_ID %in% sdwis$PWS_ID,]
tdf =  left_join(tdf,sdwis %>% dplyr::select(-Primary_County,-COUNTY_FIPS))

tdf$BOARD_SELECTION[tdf$BOARD_SELECTION=='Elected by Precinct'] <- 'Elected'
tdf$Type[tdf$Type%in%c('WATER IMPROVEMENT DISTRICT','WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
tdf$Type[tdf$Type%in%c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
tdf$Type[tdf$Type%in%c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
tdf$Type[tdf$Type%in%c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
tdf$Type[tdf$Type%in%c('OTHER','NAVIGATION DISTRICT','REGIONAL DISTRICT')] <- 'OTHER'


tx_tracts = tigris::tracts(state = '48')
tx_tracts = tx_tracts[!duplicated(tx_tracts@data$GEOID),]
#tx_blocks = tigris::blocks(state = '48')

pws = readOGR('../../google_drive/putah/spatial_input/texas','PWS_Export')
pws <- gBuffer(pws, byid=TRUE, width=0)
pws <- unionSpatialPolygons(pws, pws@data$PWSId)
di = readOGR('../../google_drive/putah/spatial_input/texas/water_districts_shp','TCEQ_WaterDistricts')
di = spTransform(di,CRS(proj4string(pws)))
di <- gBuffer(di, byid=TRUE, width=0)
di <- unionSpatialPolygons(di, di@data$DISTRICT_I)
all_polys = rbind(di,pws)
tdf$Shape_ID = ifelse(tdf$PWS_ID %in% getSpPPolygonsIDSlots(all_polys),tdf$PWS_ID,tdf$DISTRICT_ID)

tdf$WHOLESALE_REGULAR_POP = sales$Total_Buyer_Service_Pop[match(tdf$PWS_ID,sales$Seller)]
tract_acs_data = read.csv('input/census/ACS_tracts/proj6/acs_tract_demo_data.csv',stringsAsFactors = F)
tract_acs_data$GEOID = as.character(tract_acs_data$GEOID)
keep_polys = all_polys[getSpPPolygonsIDSlots(all_polys) %in% tdf$Shape_ID,]

cbsa = readOGR('spatial_inputs/government_units/','tl_2017_us_cbsa')
cbsa = spTransform(cbsa,CRS(proj4string(keep_polys)))
over_cbsa = over(keep_polys,cbsa)
over_cbsa$Shape_ID = getSpPPolygonsIDSlots(keep_polys)
tdf$CBSA = as.character(over_cbsa$NAMELSAD[match(tdf$Shape_ID,over_cbsa$Shape_ID)])
tdf$CBSA[is.na(tdf$CBSA)] <- 'NONE'

#tract_overlay = over(keep_polys,spTransform(tx_tracts,CRS(proj4string(keep_polys))),returnList = T,minDimension = 2)
tx_tracts = spTransform(tx_tracts,CRS(proj4string(keep_polys)))
neighbors = poly2nb(tx_tracts, queen = FALSE,row.names = tx_tracts@data$GEOID)
tract_acs_data$GEOID <- tract_acs_data$Id2
tract_acs_data = tract_acs_data %>% dplyr::select(-Id,-Geography,-Id2)

library(jsonlite)
library(censusapi)
library(pbapply)
library(tidyverse)
library(tidycensus)
k = "b5a388cd6162590fc939335ddc45787bcc61778c"
quer = expand.grid(fips = 48,year = 2009:2015)
vn = list("NAME","B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")

base='https://api.census.gov/data/2016/acs/acs5?get='
geo = '&for=tract:*&in=state:48'
#fips = formatC(quer_2016$fips,width = 2,flag = 0)
post = '&in=county:*&key=b5a388cd6162590fc939335ddc45787bcc61778c'

df_2016 =  read.csv(paste0(base,paste(vn,collapse=','),geo,post)) %>% mutate(Year = 2016)
df_2016$tract. = gsub('\\]','',df_2016$tract.)
df_2016$X..NAME = gsub('\\]','',df_2016$X..NAME)
df_2016 = df_2016 %>% dplyr::select(-X) %>% rename(Tract = tract.,NAME = X..NAME,Total_Population = B02001_001E,White_Population = B02001_002E,Median_Income=B06011_001E,Median_Year_Structure_Built = B25035_001E,MEDIAN_HOME_PRICE = B25077_001E,
                                                   Owner_Occupied_Households = B07013_002E, Total_Households = B07013_001E,Bach_Degree_Or_Higher = B16010_041E,Pop_Over_25 = B16010_001E)
df_2016$MEDIAN_HOME_PRICE[df_2016$MEDIAN_HOME_PRICE<0] <- NA
df_2016$county = formatC(df_2016$county,width = 3,flag = 0)
df_2016$Median_Year_Structure_Built[df_2016$Median_Year_Structure_Built<0] <- NA
df_2016$Median_Income[df_2016$Median_Income<0] <- NA

list_pre2016 = pblapply(seq_along(quer$fips),function(x)
  tryCatch({getCensus(name = 'acs5', vintage = quer$year[x], key = k, vars = vn,region = "tract:*", 
                      regionin = paste0("state:",quer$fips[x]))}) %>% mutate(Year = quer$year[x]))
pre2016_df = do.call(rbind,list_pre2016)
pre2016_df = pre2016_df %>% rename(Tract = tract,Total_Population = B02001_001E,White_Population = B02001_002E,Median_Income=B06011_001E,Median_Year_Structure_Built = B25035_001E,MEDIAN_HOME_PRICE = B25077_001E,
                                   Owner_Occupied_Households = B07013_002E, Total_Households = B07013_001E,Bach_Degree_Or_Higher = B16010_041E,Pop_Over_25 = B16010_001E)
pre2016_df$county = formatC(pre2016_df$county,width = 3,flag = 0)

all_df = rbind(pre2016_df,df_2016)
all_df$GEOID = paste0(all_df$state,all_df$county,all_df$Tract)

all_df = all_df %>% complete(GEOID,Year = 2007:2018) %>% group_by(GEOID) %>% arrange(GEOID,Year) %>%
  fill(-Year,-GEOID,.direction = c("down")) %>%
  fill(-Year,-GEOID,.direction = c("up")) 

tx_tracts = spChFIDs(tx_tracts,tx_tracts@data$GEOID)
tract_acs_data$GEOID = as.character(tract_acs_data$GEOID)

tdf  = tdf[tdf$Shape_ID %in% getSpPPolygonsIDSlots(keep_polys),]

portion_tract_over_district = pblapply(seq_along(tdf$Shape_ID),
function(x){
overlap.geog <- gIntersection(tx_tracts, keep_polys[tdf$Shape_ID[x]],byid = TRUE,drop_lower_td =T)
over_geoid = str_extract(names(overlap.geog),'^[0-9]{1,}')
over_di = tdf$Shape_ID[x]
overlap.percentage <- gArea(overlap.geog, byid = TRUE)/gArea(tx_tracts[match(over_geoid,tx_tracts@data$GEOID),],byid = )
data.frame(Shape_ID = over_di,GEOID = over_geoid,Prop_Of_Tract = overlap.percentage,stringsAsFactors = T)})
all_portions = do.call(rbind,portion_tract_over_district)

total_vars = c('Total_Population','White_Population','Total_Households','Owner_Occupied_Households','Pop_Over_25','Bach_Degree_Or_Higher')
summary_vars = c('Median_Income','MEDIAN_HOME_PRICE','Median_Year_Structure_Built')

census_grid = expand.grid(Shape_ID = tdf$Shape_ID,Year = sort(unique(all_df$Year)))

census_weighted_results = pblapply(seq_along(census_grid$Shape_ID),function(x) {
  sub_portions = all_portions[all_portions$Shape_ID == census_grid$Shape_ID[x],]
  sub_df = all_df[all_df$GEOID %in% sub_portions$GEOID & all_df$Year==census_grid$Year[x],]
  sub_df$Multiplier = sub_portions$Prop_Of_Tract[match(sub_df$GEOID,sub_portions$GEOID)]
  data.frame(cbind(t(as.data.frame(apply(sub_df[,total_vars],2,function(x) { sum(x * sub_df$Multiplier)}))),
  t(as.data.frame(apply(sub_df[,summary_vars],2,function(x) {weighted.mean(x,w = sub_df$Multiplier)})))),
  stringsAsFactors = F,row.names = census_grid$Shape_ID[x]) %>%
    mutate(Shape_ID = census_grid$Shape_ID[x],Year =census_grid$Year[x])})

census_weighted_df = do.call(rbind,census_weighted_results)
census_weighted_df = census_weighted_df %>% mutate(Prop_Nonwhite = 1 - White_Population/Total_Population,
                                                   Prop_Owner_Occupied_Household = Owner_Occupied_Households/Total_Households,
                                                   Prop_Bach_Or_Higher = Bach_Degree_Or_Higher/Pop_Over_25)


fill_grid = expand.grid(PWS_ID = as.character(unique(tdf$PWS_ID)),PERIOD = (-5):20)
fill_grid$PWS_ID  = as.character(fill_grid$PWS_ID)
fill_grid$Shape_ID = tdf$Shape_ID[match(fill_grid$PWS_ID,tdf$PWS_ID)]
fill_grid$DISTRICT_ID= tdf$DISTRICT_ID[match(fill_grid$PWS_ID,tdf$PWS_ID)]
fill_grid$COUNTY_FIPS = tdf$COUNTY_FIPS[match(fill_grid$Shape_ID,tdf$Shape_ID)]

audits = read_csv('input/tceq_audits/district_audits.csv')
audits$`FISCAL YEAR ENDED`[audits$`FISCAL YEAR ENDED`=='08/31/0005'] <-'08/31/2005'
audits$FISCAL_YEAR_ENDED = mdy(audits$`FISCAL YEAR ENDED`)
audits = audits %>% group_by(DISTRICT_ID) %>% arrange(DISTRICT_ID,FISCAL_YEAR_ENDED) %>% 
  mutate(FISCAL_PERIOD = seq_along(FISCAL_YEAR_ENDED)-1)

audits = audits %>% ungroup() %>% mutate_if(as.vector(apply(audits,2,function(x) any(grepl('\\$',x)))),from_currency)

audits$PROP_TAX_LEVIED_FOR_DEBT_SERVICE = ifelse(audits$`DEBT SERVICE TAX LEVIED`==0,0,(audits$`DEBT SERVICE TAX LEVIED` / (audits$`DEBT SERVICE TAX LEVIED` + audits$`OPERATION & MAINTENANCE TAX LEVIED`)))
audits$PROP_TAX_LEVIED_FOR_DEBT_SERVICE = audits$PROP_TAX_LEVIED_FOR_DEBT_SERVICE * 100
audits = audits %>% rename(
  BONDS_OUTSTANDING = `BONDS OUTSTANDING`,
  CURRENT_ASSESSED_VALUATION = `CURRENT ASSESSED VALUATION`,
  ENTERPRISE_REVENUE = `ENTERPRISE FUND - OPERATING REVENUES`,
  GENERAL_REVENUE = `GENERAL FUND - TOTAL REVENUES`,
  GENERAL_FUND_BALANCE = `GENERAL FUND - FUND BALANCE`,
  GENERAL_FUND_LIABILITIES = `GENERAL FUND - LIABILITIES`,
  GENERAL_FUND_EXPENDITURES = `GENERAL FUND - TOTAL EXPENDITURES`,
  ENTERPRISE_FUND_EXPENDITURES  = `ENTERPRISE FUND - OPERATING EXPENSES`,
  ENTERPRISE_FUND_LIABILITIES = `ENTERPRISE FUND - LIABILITIES`,
  GENERAL_FUND_ASSETS = `GENERAL FUND - ASSETS`,
  ENTERPRISE_FUND_ASSETS = `ENTERPRISE FUND - ASSETS`,
  TOTAL_TAX_RATE = `TOTAL TAX RATE`,
  ENTERPRISE_FUND_BALANCE = `ENTERPRISE FUND - NET ASSETS`)
audits = audits %>% mutate(TOTAL_FUND_BALANCE = ENTERPRISE_FUND_BALANCE + GENERAL_FUND_BALANCE,
         TOTAL_REVENUE = GENERAL_REVENUE + ENTERPRISE_REVENUE,
         TOTAL_LIABILITIES =  ENTERPRISE_FUND_LIABILITIES + GENERAL_FUND_LIABILITIES,
         TOTAL_EXPENDITURE = GENERAL_FUND_EXPENDITURES + ENTERPRISE_FUND_EXPENDITURES,
         TOTAL_ASSETS = ENTERPRISE_FUND_ASSETS + GENERAL_FUND_ASSETS,
        NET_REVENUE = TOTAL_REVENUE - TOTAL_EXPENDITURE)
audits$WASTEWATER_SFU = ifelse(!is.na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`),audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`,audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`)
audits$WASTEWATER_SFU = as.numeric(audits$WASTEWATER_SFU)
audits$WATER_SFU = as.numeric(audits$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)
audits = audits %>% group_by(DISTRICT_ID) %>% arrange(DISTRICT_ID,YEAR) %>% 
  fill(WATER_SFU,WASTEWATER_SFU,TOTAL_REVENUE,TOTAL_FUND_BALANCE,BONDS_OUTSTANDING,TOTAL_TAX_RATE,PROP_TAX_LEVIED_FOR_DEBT_SERVICE,TOTAL_LIABILITIES,.direction = 'down') 
audits = audits %>% dplyr::select(-YEAR,-DISTRICT_NAME) 

fill_grid = data.frame(fill_grid,audits[match(paste(fill_grid$DISTRICT_ID,fill_grid$PERIOD,sep='_'),paste(audits$DISTRICT_ID,audits$FISCAL_PERIOD,sep='_')),],stringsAsFactors = F)

fill_grid$FISCAL_YEAR_END_YEAR = year(mdy(fill_grid$FISCAL.YEAR.ENDED))
fill_grid$FISCAL_YEAR_START_YEAR = fill_grid$FISCAL_YEAR_END_YEAR - 1
fill_grid = fill_grid %>% group_by(DISTRICT_ID) %>% arrange(DISTRICT_ID,PERIOD) %>% 
  fill(FISCAL_YEAR_ENDED,.direction = 'up') %>%
  fill(FISCAL_YEAR_ENDED,.direction = 'down') %>%
  mutate(FISCAL_YEAR_ENDED = FISCAL_YEAR_ENDED+(years({PERIOD * (PERIOD<0)}))) %>% 
  mutate(FISCAL_YEAR_ENDED = FISCAL_YEAR_ENDED + years(cumsum(duplicated(FISCAL_YEAR_ENDED)))) 
fill_grid = fill_grid[!is.na(fill_grid$FISCAL_YEAR_ENDED),]
fill_grid$FISCAL_YEAR_BEGAN = fill_grid$FISCAL_YEAR_ENDED - years(1) + days(1)
fill_grid$FISCAL_YEAR_BEGAN[is.na(fill_grid$FISCAL_YEAR_BEGAN)&day(fill_grid$FISCAL_YEAR_ENDED)==29&month(fill_grid$FISCAL_YEAR_ENDED)==2] <-fill_grid$FISCAL_YEAR_ENDED[is.na(fill_grid$FISCAL_YEAR_BEGAN)&day(fill_grid$FISCAL_YEAR_ENDED)==29&month(fill_grid$FISCAL_YEAR_ENDED)==2] - days(1) - years(1) + days(1)


viol_df = read_csv('../../google_drive/putah/input/sdwis_data/state_viol_pulls/sdwis_viol_report_TX_.csv',na = c('-'))
viol_df$Period_Start=dmy(viol_df$`Compliance Period Begin Date`)
viol_df$Period_End=dmy(viol_df$`Compliance Period End Date`)
viol_df$Period_Start_Year = year(viol_df$Period_Start)
viol_df = viol_df %>% rename(PWS_ID = `PWS ID`)
viol_df$Period_End[is.na(viol_df$Period_End)] <- viol_df$Period_Start[is.na(viol_df$Period_End)] + years(1) - days(1)

fill_grid$HEALTH_VIOL_COUNT_PERIOD = pbsapply(seq_along(fill_grid$PWS_ID),function(i){
nrow(viol_df[viol_df$`Is Health Based` == 'Y' & viol_df$PWS_ID == fill_grid$PWS_ID[i] &
viol_df$Period_Start >= fill_grid$FISCAL_YEAR_BEGAN[i] & viol_df$Period_End <= fill_grid$FISCAL_YEAR_ENDED[i],])},cl=12)

fill_grid$MANAGEMENT_VIOL_COUNT_PERIOD = pbsapply(seq_along(fill_grid$PWS_ID),function(i){
  nrow(viol_df[viol_df$`Is Health Based` == 'N' & viol_df$PWS_ID == fill_grid$PWS_ID[i] & 
                 viol_df$Period_Start >= fill_grid$FISCAL_YEAR_BEGAN[i] & viol_df$Period_End <= fill_grid$FISCAL_YEAR_ENDED[i],])},cl=12)

library(tigris)
tdf_no_filter = tdf
tdf = tdf[tdf$Type %in% c('FWSD','MUD','SUD','WCID'),]


full_data = left_join(fill_grid,census_weighted_df,
                      by = c("FISCAL_YEAR_START_YEAR" = "Year","Shape_ID"='Shape_ID'))

full_data = left_join(full_data,tdf)

full_data$DISTRICT_AGE = full_data$FISCAL_YEAR_START_YEAR - full_data$Year_Created
full_data$REVENUE_PER_CONNECTION = full_data$TOTAL_REVENUE/full_data$Service_Connections
full_data$EXP_PER_CONNECTION = full_data$TOTAL_EXPENDITURE/full_data$Service_Connections

full_data = full_data %>% group_by(DISTRICT_ID,PWS_ID) %>% arrange(PWS_ID,DISTRICT_ID,PERIOD) %>%
  mutate(TOTAL_REVENUE_L1 = lag(TOTAL_REVENUE, order_by = PERIOD),
         TOTAL_EXPENDITURE_L1 = lag(TOTAL_EXPENDITURE, order_by = PERIOD),
         TOTAL_FUND_BALANCE_L1 = lag(TOTAL_FUND_BALANCE, order_by = PERIOD),
         BONDS_OUTSTANDING_L1 = lag(BONDS_OUTSTANDING, order_by = PERIOD),
         TOTAL_LIABILITIES_L1 = lag(TOTAL_LIABILITIES, order_by = PERIOD),
         TOTAL_TAX_RATE_L1 = lag(TOTAL_TAX_RATE, order_by = PERIOD),
         SERVICE_CONNECTIONS_L1 = lag(Service_Connections, order_by = PERIOD),
         PROP_TAX_LEVIED_FOR_DEBT_SERVICE_L1 = lag(PROP_TAX_LEVIED_FOR_DEBT_SERVICE, order_by = PERIOD),
         AUDIT_DOCID_L1 = lag(DOC_ID, order_by = PERIOD),
         SERVICE_CONNECTIONS_L1 = lag(Service_Connections, order_by = PERIOD),
         EXP_PER_CONNECTION_L1 = lag(EXP_PER_CONNECTION, order_by = PERIOD),
         EXP_PER_CONNECTION_L1 = lag(EXP_PER_CONNECTION, order_by = PERIOD)) %>%
  mutate(TOTAL_REVENUE_L2 = lag(TOTAL_REVENUE, order_by = PERIOD,n = 2),
         TOTAL_EXPENDITURE_L2 = lag(TOTAL_EXPENDITURE, order_by = PERIOD,n = 2),
         TOTAL_FUND_BALANCE_L2 = lag(TOTAL_FUND_BALANCE, order_by = PERIOD,n = 2),
         BONDS_OUTSTANDING_L2 = lag(BONDS_OUTSTANDING, order_by = PERIOD,n = 2),
         TOTAL_LIABILITIES_L2 = lag(TOTAL_LIABILITIES, order_by = PERIOD,n = 2),
         TOTAL_TAX_RATE_L2 = lag(TOTAL_TAX_RATE, order_by = PERIOD,n = 2),
         PROP_TAX_LEVIED_FOR_DEBT_SERVICE_L2 = lag(PROP_TAX_LEVIED_FOR_DEBT_SERVICE, order_by = PERIOD,n = 2),
         AUDIT_DOCID_L2 = lag(DOC_ID, order_by = PERIOD,n = 2),
         SERVICE_CONNECTIONS_L2 = lag(Service_Connections, order_by = PERIOD,n=2),
         EXP_PER_CONNECTION_L2 = lag(EXP_PER_CONNECTION, order_by = PERIOD,n=2),
         EXP_PER_CONNECTION_L2 = lag(EXP_PER_CONNECTION, order_by = PERIOD,n=2))

full_data = full_data %>% ungroup()

full_data = full_data[full_data$FISCAL_YEAR_START_YEAR >= full_data$YEAR_CREATED,]

full_data = full_data[!is.na(full_data$FISCAL_YEAR_START_YEAR) & !is.na(full_data$YEAR_CREATED),]
full_data =  full_data[full_data$FISCAL_YEAR_START_YEAR >= full_data$YEAR_CREATED,]

library(readxl)

issue_list = lapply(list.files('input/fiscal_data/','debtissuance.xls'),function(x) 
  read_excel(paste0('input/fiscal_data/',x)) %>% dplyr::select(GovtID,Issuer,FYear,NewMoneySize,Purpose,IssueType))
issue_list2 = lapply(list.files('input/fiscal_data/','debtissuance.csv'),function(x) 
  read_csv(paste0('input/fiscal_data/',x)) %>% rename(FYear = Fyear) %>% 
    dplyr::select(GovtID,Issuer,FYear,NewMoneySize,Purpose,IssueType) %>% 
    mutate(NewMoneySize = lucr::from_currency(NewMoneySize))) 
issue_list[[length(issue_list)+1]] <- issue_list2[[1]]
debt_issued_df = do.call(plyr::rbind.fill,issue_list)

debt_issued_df$Purpose = toupper(debt_issued_df$Purpose)
debt_issued_df = debt_issued_df[debt_issued_df$Purpose %in% c('GENERAL PURPOSE','UTILITY SYSTEM - COMBINED','WATER RELATED'),]
debt_issued_df = debt_issued_df %>% group_by(Issuer,FYear) %>% summarise(NewMoneySize = sum(NewMoneySize,na.rm = T))
out_list = lapply(list.files('input/fiscal_data/','WDTR'),function(x) 
  read_excel(paste0('input/fiscal_data/',x)) %>% 
    dplyr::select(GovtID,Issuer,FYear,TotDebtServiceOutstanding,TotalTaxRatePriorYear,TaxAVPriorYear))
out_df = do.call(rbind,out_list)
debt_stats = full_join(out_df,debt_issued_df %>% dplyr::select(-Issuer))
debt_stats$Issuer = toupper(debt_stats$Issuer)
debt_stats$Issuer = gsub(' 0{1,}',' ',debt_stats$Issuer)
debt_stats$Issuer = gsub("([0-9]{0,})\\sOF\\s[A-Z]{1,}\\sCOUNTY$","\\1",debt_stats$Issuer)
debt_stats$Issuer = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",debt_stats$Issuer)
debt_stats$Issuer = gsub('SPECIAL UD$',"SUD",debt_stats$Issuer)
debt_stats$Issuer = gsub(' WA$'," WATER AUTHORITY",debt_stats$Issuer)
debt_stats$Issuer = gsub(' RA$'," RIVER AUTHORITY",debt_stats$Issuer)
debt_stats$Issuer = gsub(' ND$'," NAVIGATION DISTRICT",debt_stats$Issuer)
debt_stats$Issuer = gsub(' ND '," NAVIGATION DISTRICT ",debt_stats$Issuer)
debt_stats$Issuer = gsub(' DD$'," DRAINAGE DISTRICT",debt_stats$Issuer)
debt_stats$Issuer = gsub(' DD '," DRAINAGE DISTRICT ",debt_stats$Issuer)
debt_stats$Issuer = gsub(' ID$'," IRRIGATION DISTRICT",debt_stats$Issuer)
debt_stats$Issuer = gsub(' ID '," IRRIGATION DISTRICT ",debt_stats$Issuer)
debt_stats$Issuer = gsub(' METROPOLITAN WD$'," MWD",debt_stats$Issuer)
debt_stats$Issuer = gsub(' MWA$'," MUNICIPAL WATER AUTHORITY",debt_stats$Issuer)
debt_stats$Issuer = gsub("'","",debt_stats$Issuer)
debt_stats$Issuer = gsub('\\"','',debt_stats$Issuer)
debt_stats$Issuer = gsub("SPINGS","SPRINGS",debt_stats$Issuer)
debt_stats$Issuer = gsub(" WD$"," WATER DISTRICT",debt_stats$Issuer)
debt_stats$Issuer = gsub(" UD$"," UTILITY DISTRICT",debt_stats$Issuer)
debt_stats$Issuer[(!debt_stats$Issuer %in% dinfo$District_Name) & gsub(' 1$','',debt_stats$Issuer) %in% dinfo$District_Name] <- gsub(' 1$','',debt_stats$Issuer[(!debt_stats$Issuer %in% dinfo$District_Name) & gsub(' 1$','',debt_stats$Issuer) %in% dinfo$District_Name])
debt_stats = debt_stats %>% rename(District_Name = Issuer,FISCAL_YEAR_END_YEAR = FYear) 
debt_stats = debt_stats %>% 
  mutate(NewMoneySize = ifelse(!is.na(NewMoneySize),NewMoneySize,0)) %>%
  group_by(GovtID) %>% arrange(GovtID,FISCAL_YEAR_END_YEAR) %>% 
  mutate(d1 = lag(NewMoneySize,1,default = 0),
         d2 = lag(NewMoneySize,2,default = 0),
         d3 = lag(NewMoneySize,3,default = 0),
         d4 = lag(NewMoneySize,4,default = 0),
         d5 = lag(NewMoneySize,5,default = 0)) %>% mutate(Debt_Issued_P5 = d1+d2+d3+d4+d5) %>% dplyr::select(-d1,-d2,-d3,-d4,-d5)

full_data = left_join(full_data,debt_stats)
full_data$Debt_Issued_P5[is.na(full_data$Debt_Issued_P5)] <- 0
full_data = full_data %>% group_by(PWS_ID) %>% arrange(PWS_ID,FISCAL_YEAR_END_YEAR) %>%
  mutate(TotDebtServiceOutstanding_P1 = lag(TotDebtServiceOutstanding,1))

full_data$TotDebtServiceOutstanding_P1[is.na(full_data$TotDebtServiceOutstanding_P1)] <- 0
saveRDS(file = 'scratch/proj6/df_for_model.RDS',object = full_data)




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


