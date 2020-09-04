library(data.table)
library(tidyverse)
library(INLA)
library(lubridate)
library(MASS)
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
library(ggthemes)

library(rgeos)
require(spdep)
#setwd('../../')
not_district = '0000000'
tx_query = "https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=48&startdate=1/1/2000&enddate=1/1/2019&statisticsType=2/json"
library(jsonlite)
start_year = 2010
tx_dsci = fromJSON(tx_query)
tx_dsci$D0 = as.numeric(tx_dsci$D0);tx_dsci$D1 = as.numeric(tx_dsci$D1);tx_dsci$D2 = as.numeric(tx_dsci$D2)
tx_dsci$D3 = as.numeric(tx_dsci$D3);tx_dsci$D4 = as.numeric(tx_dsci$D4);
tx_dsci = tx_dsci[tx_dsci$StatisticFormatID==1,]
tx_dsci$D0 = as.numeric(tx_dsci$D0);tx_dsci$D1 = as.numeric(tx_dsci$D1);tx_dsci$D2 = as.numeric(tx_dsci$D2)
tx_dsci$D3 = as.numeric(tx_dsci$D3);tx_dsci$D4 = as.numeric(tx_dsci$D4);
tx_dsci$ValidStart = ymd(tx_dsci$ValidStart)
rib_cols = tableau_color_pal(type = 'ordered-sequential',palette = 'Classic Orange')(7)[c(1,2,3,5,6)]
figure1 =  ggplot(data = tx_dsci,aes(x = ValidStart)) +
  # geom_point(aes( y=D0),fill = 'yellow') + 
  geom_ribbon(aes(ymin=0, ymax=D0,fill =  rib_cols[1])) + 
  geom_ribbon(aes(ymin=0, ymax=D1,fill = rib_cols[2])) + 
  geom_ribbon(aes(ymin=0, ymax=D2,fill = rib_cols[3])) + 
  geom_ribbon(aes(ymin=0, ymax=D3,fill = rib_cols[4])) +
  geom_ribbon(aes(ymin=0, ymax=D4,fill = rib_cols[5])) + 
  theme_bw() + scale_y_continuous(name = '% of state land area in status',expand = c(0,0)) + 
  scale_x_date(name = 'Weekly drought status',expand = c(0,0)) + 
  theme(text = element_text(family = 'Times'),legend.position = c(0.92,0.6),axis.title = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white',0.5)))+ 
  ggtitle('Texas statewide drought conditions, 2010 to 2018') + 
  scale_fill_identity(labels = c('D4','D3-D4','D2-D4','D1-D4','D0-D4'),guide = 'legend',name = 'Category')

ggsave(figure1,filename = 'output/proj5/figure1.png',dpi = 500,width=6,height=2.8,units='in')
#Drought score
#Population served, logged
#Total storage per 1,000 people 
#Number of interconnections, logged 
# Groundwater
#% customers served retail 
#Average daily consumption per1,000 people
#% Democratic vote
#Median household income, logged, #% houses built after 1980, #% rural,#% Black, #% Hispanic, #% four-year college degree
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
twd_boundaries = st_read('spatial_inputs/Service_Area_Boundaries/PWS_Export.shp')
twd_boundaries = twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries = st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries = st_make_valid(twd_boundaries)
twd_boundaries = twd_boundaries[!duplicated(twd_boundaries$PWS_ID),]


tx_tracts = tigris::tracts(state = 'TX',class='sf',year = 2010)
tx_tracts = st_transform(tx_tracts,st_crs(albersNA))
tx_tracts = st_make_valid(tx_tracts)
#tx_county = st_read('https://opendata.arcgis.com/datasets/8b902883539a416780440ef009b3f80f_0.geojson')
tx_county = tigris::counties(state = 'TX',class = 'sf')
tx_county <- st_transform(tx_county,albersNA)
tx_county <- st_make_valid(tx_county)
pws_over_tracts = st_intersection(twd_boundaries,tx_tracts)
tract_overs = data.table(PWS_ID = pws_over_tracts$PWS_ID,GEOID10 = pws_over_tracts$GEOID10,
                         Prop_Of_Tract = as.numeric(st_area(pws_over_tracts)/st_area(tx_tracts)[match(pws_over_tracts$GEOID10,tx_tracts$GEOID10)]))

pws_over_counties = st_intersection(twd_boundaries,tx_county)
pws_over_counties = pws_over_counties %>% rename(CFIPS = GEOID)

county_overs = data.table(PWS_ID = pws_over_counties$PWS_ID,CFIPS = pws_over_counties$CFIPS,
                          Prop_Over_County = as.numeric(st_area(pws_over_counties)/st_area(twd_boundaries)[match(pws_over_counties$PWS_ID,twd_boundaries$PWS_ID)]))

county_overs$Prop_Over_County <- round(county_overs$Prop_Over_County,2)
county_overs = county_overs[county_overs$Prop_Over_County>0,]

#td = tempdir()
#climate_url = 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/CONUS_CLIMATE_DIVISIONS.shp.zip'
#tf = tempfile(tmpdir=td, fileext=".zip")
#download.file(climate_url, tf)
#fname = unzip(tf, list=TRUE)
#unzip(tf, files=fname$Name, exdir=td, overwrite=TRUE)
#fpath = file.path(td, grep('shp$',fname$Name,value=T))
#climate_div <- st_read(fpath)
#climate_div <- st_transform(climate_div,albersNA)
#climate_div <- st_make_valid(climate_div)
#climate_div = climate_div[climate_div$STATE=='Texas',]
#pws_over_climatedivs = st_intersection(twd_boundaries,climate_div)
#climatediv_overs = data.table(PWS_ID = pws_over_climatedivs$PWS_ID,FIPS_CD = pws_over_climatedivs$FIPS_CD,
#                              Climate_Div_Weight = as.numeric(st_area(pws_over_climatedivs)/
#                                                                {st_area(twd_boundaries)[match(pws_over_climatedivs$PWS_ID,twd_boundaries$PWS_ID)]}))
#climatediv_overs$Climate_Div_Weight <- round(climatediv_overs$Climate_Div_Weight,2)
#climatediv_overs = climatediv_overs[climatediv_overs$Climate_Div_Weight>0,]
library(tidycensus)
tx_systems = fread("input/epa_sdwis/tx_water_system_geographic_area_2-9-19.csv",stringsAsFactors = F)
setnames(tx_systems,c('PWS ID', 'PWS Name'),c('PWS_ID','PWS_NAME'))
tx_systems = tx_systems[!duplicated(tx_systems),]
tx_dww_info = fread('input/texas_dww/texas_master_pws.csv',stringsAsFactors = F,header=T)
tx_dww_info = tx_dww_info[,.(PWS_ID,Primary_Source_Type)]
tx_dww_info$Purchaser = grepl('P',tx_dww_info$Primary_Source_Type) + 0
tx_dww_info$Groundwater = grepl('G',tx_dww_info$Primary_Source_Type) + 0
tx_dww_info[,Primary_Source_Type:=NULL]
tx_systems <- merge(tx_systems,tx_dww_info,on = 'PWS_ID')
tx_systems$`First Reported Date` = dmy(tx_systems$`First Reported Date`)
tx_systems = tx_systems[tx_systems$`First Reported Date`< mdy(paste0('1/1/',start_year)),]
#tx_systems = tx_systems[tx_systems$PWS_ID %in% twd_boundaries$PWS_ID,]
tx_info = as.data.table(readRDS('input/texas_dww/pws_details_2019-07-02.RDS'))
#tx_info = data.table(readRDS('scratch/pws_details_2019-09-16.RDS'),stringsAsFactors = F)

tx_info$PWS_NAME = gsub('\\s\\&nbsp$','',tx_info$PWS_NAME)
setkey(tx_info,PWS_ID)
tx_info[,PWS_NAME:=NULL]
setkey(tx_systems,PWS_ID)
tx_systems = tx_systems[tx_info,]

tx_systems$Residential_Service_Connections = as.numeric(tx_systems$PopulationType_ServiceConnections_Residential)
tx_systems$Residential_Service_Connections[is.na(tx_systems$Residential_Service_Connections)] <- 0
#tx_systems$Ln_Residential_Service_Connections = log(tx_systems$Residential_Service_Connections)
tx_systems$Wholesale_Service_Connections = ifelse(!is.na(as.numeric(tx_systems$PopulationType_ServiceConnections_Wholesale)),as.numeric(tx_systems$PopulationType_ServiceConnections_Wholesale),0)
tx_systems$Wholesale_Service_Connections[is.na(tx_systems$Wholesale_Service_Connections)] <- 0
#tx_systems$Ln_Wholesale_Service_Connections = log(tx_systems$Wholesale_Service_Connections+1)
tx_systems$Nonresidential_Service_Connections = ifelse(!is.na(as.numeric(tx_systems$PopulationType_ServiceConnections_NonResidential)),as.numeric(tx_systems$PopulationType_ServiceConnections_NonResidential),0)
tx_systems$Nonresidential_Service_Connections[is.na(tx_systems$Nonresidential_Service_Connections)] <- 0
tx_systems$Total_Service_Connections = tx_systems$Wholesale_Service_Connections + tx_systems$Nonresidential_Service_Connections + tx_systems$Residential_Service_Connections
consump_units = str_extract(tx_systems$AverageDailyConsump.,'[A-Z]{1,}')
tx_systems$Avg_Consumption_Per_Connection_G = (tx_systems$Average_Daily_Consump_MGD/tx_systems$Total_Service_Connections) * 1e6
tx_systems$Owner_Type = as.character(tx_systems$Owner_Type)
tx_systems = tx_systems[!tx_systems$Owner_Type %in% c('Federal Government','State Government'),]
tx_systems$Owner_Type[tx_systems$PWS_ID%in%c('TX1700784','TX1013113','TX1700589','TX1050019','TX1260016','TX1050038','TX2090005','TX0390019','TX0430037','TX0740031')] <- 'District'
interconnects = fread('input/texas_dww/sales_connections_2018-07-03.csv')
interconnects = interconnects[!duplicated(interconnects),]
interconnects = interconnects[!duplicated(paste(Seller,Buyer)),]
interconnects = interconnects[!paste(Seller,Buyer) %in% paste(Buyer,Seller),]

emergency_interconnects = interconnects[Sale_Type=='E',]
emergency_interconnects = melt(emergency_interconnects,measure.vars = c('Seller','Buyer'))
emergency_interconnects_count <- emergency_interconnects[,.N,by=.(value)]

setnames(emergency_interconnects_count,c('value','N'),c('PWS_ID','Num_Emergency_Interconnections'))
wholesalers = interconnects[Sale_Type%in%c('S','P','O','I'),][!duplicated(Seller),][,(Seller)]
tx_systems$Wholesale <- (tx_systems$PWS_ID %in% wholesalers) + 0
tx_systems = merge(tx_systems,emergency_interconnects_count,on = 'PWS_ID',all.x=T,all.y=F)
tx_systems$Num_Interconnections[is.na(tx_systems$Num_Interconnections)]<-0



dinfo = fread('input/twdd_records/district_list_2019-03-08.csv',stringsAsFactors = F)
dinfo$PWS_ID[dinfo$PWS_ID == "NA"] <- NA
dinfdt = dinfo[!is.na(PWS_ID)]
dinfo$PWS_ID_all = dinfo$PWS_ID
dinfo$PWS_ID = str_split(dinfo$PWS_ID,'\\|')
dinfo$PWS_ID[grepl('DUVAL COUNTY CONSERVATION & RECLAMATION DISTRICT',dinfo$District_Name)][[1]] = list('TX0660001','TX0660014','TX0660015')

dinfo$District_Name[dinfo$District_Name=="WICKSON CREEK SUD"] <- "WICKSON CREEK SUD GRIMES COUNTY"
dinfo$District_Name[dinfo$District_Name=="CANEY CREEK MUD OF MATAGORDA COUNTY"] <- 'CANEY CREEK MUD'
dinfo$District_Name[dinfo$District_Name=="TATTOR ROAD MUNICIPAL DISTRICT"] <- "TATTOR ROAD MUD"
dinfo$Type[dinfo$District_Name=='TATTOR ROAD MUD'] <- 'MUNICIPAL UTILITY DISTRICT'
dinfo$Ended = mdy(dinfo$Ended)
tx_systems$District_ID <- sapply(seq_along(tx_systems$PWS_ID),function(x) {di = dinfo$District_ID[which(grepl(tx_systems$PWS_ID[x],dinfo$PWS_ID_all))]
if(length(di)==1){di}else{not_district}})

#tx_systems$District_ID[is.na(tx_systems$District_ID)] <- not_district
tx_systems$District_ID[tx_systems$PWS_ID=='TX2490016'] <- '8492000'
tx_systems$District_ID[tx_systems$PWS_ID=='TX0430053'] <- '5952250'
tx_systems$District_ID[tx_systems$PWS_ID=='TX0420034'] <- '2312250'
tx_systems$District_ID[tx_systems$PWS_ID=='TX1900009'] <- '7585150'
tx_systems$District_ID[tx_systems$PWS_ID=='TX2040033'] <- '7492500'
tx_systems$District_ID[tx_systems$PWS_ID=='TX2290037'] <- '8070000'
tx_systems$District_ID[tx_systems$PWS_ID=='TX2360010'] <- '7634575'
tx_systems$District_ID[tx_systems$PWS_ID=='TX1290010'] <- '995951'
tx_systems$District_ID[tx_systems$PWS_ID=='TX0940015'] <- '2412188'
tx_systems$District_ID[tx_systems$PWS_ID=='TX1650133'] <- '5846750'
tx_systems$District_ID[tx_systems$PWS_ID=='TX0200706'] <- '1636654'

tx_systems[,CWS_Count:=.N,by=.(District_ID)]
tx_systems$CWS_Count[tx_systems$District_ID==not_district] <- 1
#tx_systems[,District_Service_Count:=sum(Total_Service_Connections),by=.(District_ID)]
#tx_systems$District_Service_Count[is.na(tx_systems$District_ID)] <- NA
#tx_systems$District_Prop = tx_systems$Total_Service_Connections/tx_systems$District_Service_Count
tx_systems = tx_systems[tx_systems$`PWS Type`=='Community water system',]

tx_systems$IS_DISTRICT = (tx_systems$District_ID!= not_district & tx_systems$Owner_Type == 'District') + 0
tx_systems$District_Type = dinfo$Type[match(tx_systems$District_ID,dinfo$District_ID)]


tx_systems$District_Type[tx_systems$District_Type %in% c('WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
tx_systems$District_Type[tx_systems$District_Type %in% c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
tx_systems$District_Type[tx_systems$District_Type %in% c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
tx_systems$District_Type[tx_systems$District_Type %in% c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
tx_systems$District_Type[is.na(tx_systems$District_Type)] <- 'Not a district'
tx_systems$District_Type[!tx_systems$District_Type %in% c('SUD','FWSD','MUD','WCID','Not a district')] <- 'Other'

tx_systems$HAVE_SHAPEFILE = (tx_systems$PWS_ID %in% twd_boundaries$PWS_ID) + 0
tx_systems$Geog_Area = as.numeric(st_area(twd_boundaries)[match(tx_systems $PWS_ID,twd_boundaries$PWS_ID)])
library(readxl)
tx_restrictions = read_excel('input/tceq_drought_notices.xlsx') %>% rename(PWS_ID = ID)# %>% filter(!is.na(PWS_ID))
tx_restrictions = tx_restrictions[!is.na(tx_restrictions$PWS_ID),]
tx_restrictions$PWS_ID = paste0('TX',tx_restrictions$PWS_ID)
tx_restrictions = tx_restrictions[tx_restrictions$PWS_ID %in% tx_systems$PWS_ID,]
tx_restrictions$Event = NA
tx_restrictions$Event[is.na(tx_restrictions$STAGE)] <- 0
tx_restrictions$Event[tx_restrictions$STAGE %in% c(1,2,3)] <- 1
tx_restrictions$Event[is.na(tx_restrictions$Event)] <- 0
tx_restrictions$Date_Notified  = ymd(tx_restrictions$NOTIFIED)
#tx_restrictions = read_csv('input/combined_drought_records.csv') %>% dplyr::select(-Source,-Population,-Connections)
#tx_restrictions = tx_restrictions[!duplicated(paste(tx_restrictions$PWS_ID,tx_restrictions$Obs_Date)),]
#tx_restrictions$`Date Notified`[is.na(tx_restrictions$`Date Notified`)] <- tx_restrictions$`Last Updated`[is.na(tx_restrictions$`Date Notified`)]
#tx_restrictions$Dec_Date = decimal_date(tx_restrictions$Obs_Date) - min(decimal_date(tx_restrictions$Obs_Date))
tx_rest = tx_restrictions
tx_rest = tx_rest[!duplicated(paste(tx_rest$PWS_ID,tx_rest$Date_Notified,tx_rest$STAGE)),]
tx_rest = tx_rest[tx_rest$Event==1,]
tx_rest = tx_rest[tx_rest$Date_Notified>=mdy('1/1/2010'),]
tx_rest = tx_rest %>% arrange(PWS_ID,Date_Notified) %>% filter(!duplicated(PWS_ID))
tx_rest = tx_rest[!is.na(tx_rest$PWS_ID),]
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')

tx_rest = tx_rest[tx_rest$Date_Notified < end_date,]
tx_rest$Time = decimal_date(tx_rest$Date_Notified) - start_date_dec
#tx_rest = tx_rest %>% group_by(PWS_ID) %>% filter(Time == min(Time))
#tx_systems$Event = NA
#tx_systems$Time = NA
tx_systems$Time = tx_rest$Time[match(tx_systems$PWS_ID,tx_rest$PWS_ID)]
tx_systems$Event = tx_rest$Event[match(tx_systems$PWS_ID,tx_rest$PWS_ID)]
tx_systems$Event[is.na(tx_systems$Event)] <- 0
tx_systems$Time[is.na(tx_systems$Time)] <- decimal_date(end_date) - start_date_dec
tx_systems = tx_systems[tx_systems$`Activity Status`!='Inactive',]
tx_systems = tx_systems[tx_systems$Owner_Type!='County',]
tx_systems$`Deactivation Date`[tx_systems$`Deactivation Date`=='-'] <- NA
tx_systems = tx_systems[is.na(tx_systems$`Deactivation Date`)|dmy(tx_systems$`Deactivation Date`)>=2016,] 
tx_systems = tx_systems[tx_systems$`Is School Or Daycare`=='N',]
tx_systems$Owner_Type = as.character(tx_systems$Owner_Type)
tx_systems$Total_Storage_MG[tx_systems$PWS_ID%in%c('TX1110098','TX1300010')] <- NA
tx_systems = tx_systems[tx_systems$`PWS Type Code`=='CWS',]

stormod = lm(Total_Storage_MG~Total_Service_Connections,data= tx_systems)
tx_systems$Predicted_Total_Storage_MG <- as.numeric(unlist(predict.lm(stormod,newdata = data.frame(Total_Service_Connections = tx_systems$Total_Service_Connections))))
tx_systems$Total_Storage_MG_Impute = ifelse(is.na(tx_systems$Total_Storage_MG),tx_systems$Predicted_Total_Storage_MG,tx_systems$Total_Storage_MG)
# 
# uac = tigris::urban_areas(class='sf')
# uac <- st_transform(uac,albersNA)
# uac <- st_make_valid(uac)
# states = tigris::states(class='sf')
# texas = states[states$STUSPS=='TX',]
# texas <- st_transform(texas,albersNA)
# texas <- st_make_valid(texas)
# uac_tx_inter = st_intersects(uac,texas,sparse = F)
# uac_tx = uac[uac_tx_inter,]
# #uac_all_tx = st_union(uac_tx)
# wd_uac_inters = st_intersects(twd_boundaries,uac_tx)
# urban_overlap = st_intersection(twd_boundaries,uac_tx)
# urban_overlap$Area = st_area(urban_overlap)
# urban_area=urban_overlap %>% group_by(PWS_ID) %>% summarise(Urban_Area = sum(Area))
# saveRDS(urban_area,'../../../../net/tmp/tscott1/bosque_scratch/proj5/urban_area_proportions.RDS')
urban_area = readRDS('input/urban_area_proportions.RDS')
#urban_area = readRDS('scratch/proj5/urban_area_proportions.RDS')
tx_systems$Urban_Area = as.numeric(urban_area$Urban_Area[match(tx_systems$PWS_ID,urban_area$PWS_ID)])
tx_systems$Urban_Area_Prop = ifelse(is.na(tx_systems$Urban_Area),0,tx_systems$Urban_Area)/tx_systems$Geog_Area
library(tidycensus)
#k = "b5a388cd6162590fc939335ddc45787bcc61778c"
#tidycensus::census_api_key(k)
#codes = list(Total_Population = "B01003_001",Median_Income='B06011_001',Median_Home_Value='B25077_001',White_Population='B02008_001',Bachelors_Population = 'B07009_005',Median_Year_Structure_Built = 'B25035_001',Total_Homes='B25002_001',Owner_Occupied_Homes = 'B25002_002')
#acs_counties = as.character(tx_county$CNTY_NM[sapply(any_twd,length)>0])
# acs_pull = lapply(2010:2016,function(x) {print(x);tidycensus::get_acs('tract',variables =unlist(codes),
#               state = 'TX',year = x,geometry = F) %>% mutate(Year = x)})
# acs_pull_dt = rbindlist(acs_pull)
# saveRDS(acs_pull_dt,'../../../../net/tmp/tscott1/bosque_scratch/proj5/raw_acs_tract_data.RDS')
temp_congress = congressional_districts(resolution = "500k", year = 2012,class='sf')
temp_congress <- st_transform(temp_congress,albersNA)
temp_congress <- st_make_valid(temp_congress)
temp_congress=temp_congress[temp_congress$STATEFP=='48',]
temp_congress$Congressional_District = paste0(temp_congress$STATEFP,temp_congress$CD112FP)
twd_congress = st_intersection(twd_boundaries,temp_congress)

congress_overlap_dt  = data.table(PWS_ID = twd_congress$PWS_ID,Congressional_District = twd_congress$Congressional_District,
                                  Prop_Over_District = as.numeric(st_area(twd_congress)/st_area(twd_boundaries)[match(twd_congress$PWS_ID,twd_boundaries$PWS_ID)]))
congress_overlap_dt$Prop_Over_District <- round(congress_overlap_dt$Prop_Over_District,2)
congress_overlap_dt = congress_overlap_dt[Prop_Over_District>0,]

#saveRDS(congress_overlap_dt, '../../../../net/tmp/tscott1/bosque_scratch/proj5/congress_overlap_file.RDS')

plot_adopts = tx_systems[tx_systems$Event==1,] 
plot_adopts$Sample = (!is.na(plot_adopts$District_Type) & plot_adopts$District_Type %in% c('MUD')) + 0
figure2 = ggplot(data = plot_adopts) + geom_freqpoly(aes(x = Time+start_year,colour = as.factor(Sample)),binwidth = 1/12) + 
  scale_x_continuous(name = 'Month of restriction adoption',expand=c(0,0)) + 
  scale_y_continuous(name = '# of restrictions adopted',expand = c(0,0)) + 
  ggtitle('First observed mandatory water use restriction adoption') + 
  scale_color_tableau(name = 'Water systems',labels = c('Not in sample','In sample')) + 
  theme_bw() + theme(legend.position = c(0.8,0.5),axis.text = element_text(size = 12),
                     axis.title = element_text(size = 12),legend.title = element_text(size = 12),
                     legend.text = element_text(size =12),text = element_text(family = 'Times'))
ggsave(figure2,filename = 'output/proj5/figure2.png',dpi = 500,width=6,height=4,units='in')

tdf = tx_systems
dinfo$District_ID <- as.character(dinfo$District_ID)
tdf = left_join(tdf,dinfo %>% dplyr::select(-PWS_ID))
tdf = data.table(tdf)
tdf = tdf[{is.na(Created)&ymd(`First Reported Date`)<start_date} | mdy(Created)<start_date,]


library(tigris)
tx_county_sp = tigris::counties(state = 48)
tx_county_sp = spTransform(tx_county_sp,CRS(albersNA))
#Create adjacency matrix
tx.nb <- poly2nb(tx_county_sp,row.names = tx_county_sp$GEOID,queen=F)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("tx.adj", tx.nb)
#co_count = tdf %>% filter(IS_DISTRICT==1) %>% group_by(`County Served`) %>% summarise(county_count = n()) %>%
#  arrange(-county_count)
#library(forcats)
#co_count$`County Served` = fct_inorder(as.factor(co_count$`County Served`))
#cox_dt = cox_dt[cox_dt$Date<Sys.Date(),]
#cox_dt = cox_dt[cox_dt$Date<=max(phdi_tx$Date),]
cfips = tigris::fips_codes
cfips$CFIPS = paste0(cfips$state_code,cfips$county_code)
cfips$county = gsub(' County','',cfips$county)
cfips = cfips[cfips$state=='TX',]
tdf$Primary_CFIPS = cfips$CFIPS[match(toupper(tdf$`County Served`),toupper(cfips$county))]
tdf$CFIPS_ID= match(tdf$Primary_CFIPS,tx_county_sp@data$GEOID)

start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
tdf$start_date = start_date
tdf$start_date_dec = start_date_dec
tdf$Date = tdf$start_date

tx_connections = fread('input/twdb_historicalestimates/20170630_Connections.csv')
tx_connections = melt(tx_connections,id = 1:5,measure.vars = as.character(2010:2015))
tx_connections$value <- gsub('\\,','',tx_connections$value)
tx_connections$value <- as.numeric(tx_connections$value)
setnames(tx_connections,c('variable','value','PWS ID'),c('Year','Connections','PWS_ID'))
tx_connections = tx_connections[,c('PWS_ID','Year','Connections')]
tx_population = fread('input/twdb_historicalestimates/20170630_PopServed.csv')
tx_population = melt(tx_population,id = 1:5,measure.vars = as.character(2010:2015))
tx_population$value <- gsub('\\,','',tx_population$value)
tx_population$value <- as.numeric(tx_population$value)
setnames(tx_population,c('variable','value','PWS ID'),c('Year','PopServed','PWS_ID'))
tx_population = tx_population[,c('PWS_ID','Year','PopServed')]
setkey(tx_population,'PWS_ID','Year')
setkey(tx_connections,'PWS_ID','Year')
tx_usage = fread('input/twdb_historicalestimates/20170630_UseEstimate.csv')
tx_usage = melt(tx_usage,id = 1:5,measure.vars = as.character(2010:2015))
tx_usage$value <- gsub('\\,','',tx_usage$value)
tx_usage$value <- as.numeric(tx_usage$value)

setnames(tx_usage,c('variable','value','PWS ID'),c('Year','Usage','PWS_ID'))
tx_usage = tx_usage[,c('PWS_ID','Year','Usage')]
setkey(tx_usage,'PWS_ID','Year')
tx_population = tx_population[tx_connections,][tx_usage,]


tx_population$Year <- as.numeric(as.character(tx_population$Year))
#tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),PopServed:=NA]
#tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),Connections:=NA]
#tx_population[is.na(PopServed)|is.na(Connections)|is.na(Usage),Usage:=NA]
tx_population$Year = as.numeric(as.numeric(as.character(tx_population$Year)))
#tx_population$Usage_Per_Connection = tx_population$Usage/tx_population$Connections
#tx_population[order(PWS_ID,Year),Usage_Per_Connection_P1:=lag(Usage_Per_Connection,1),by=.(PWS_ID)]

#tx_population = tx_population[!is.na(PopServed),]
tx_population = tx_population[Year%in%c(2010,2015),]

tx_population[order(PWS_ID,Year),Connections_2015:=lead(Connections,1),by=.(PWS_ID)]
tx_population[order(PWS_ID,Year),PopServed_2015:=lead(PopServed,1),by=.(PWS_ID)]
tx_population = tx_population[Year==2010,]
setkey(tx_population,PWS_ID)
setkey(tdf,PWS_ID)

cox_dt = merge(tdf,tx_population,all.x=T)

acs_pull_dt = readRDS('input/raw_acs_tract_data.RDS')
#acs_pull_dt = readRDS('scratch/proj5/raw_acs_tract_data.RDS')
setnames(acs_pull_dt,'GEOID','GEOID10')
setkey(tract_overs,'GEOID10')
setkey(acs_pull_dt,'GEOID10','Year')
acs_merge = tract_overs[acs_pull_dt,allow.cartesian=TRUE]
central_vals = acs_merge[variable %in% c('Median_Year_Structure_Built','Median_Home_Value','Median_Income'),]
count_vals = acs_merge[!variable %in% c('Median_Year_Structure_Built','Median_Home_Value','Median_Income'),]
pws_sums = count_vals[,sum(Prop_Of_Tract * estimate),by = .(Year,PWS_ID,variable)]
setnames(pws_sums,'V1','estimate')
pws_sums$PWS_ID <- as.character(pws_sums$PWS_ID)
pws_sums <- pws_sums[PWS_ID %in% cox_dt$PWS_ID,]
pfill = cox_dt[,.(PWS_ID,PopServed,Year)]
pfill = pfill[!duplicated(pfill),]
setkey(pfill,PWS_ID,Year)
setkey(pws_sums,PWS_ID,Year)
pws_sums <- pws_sums[pfill,]
pws_sums <- pws_sums[!is.na(variable),]
pws_sums$Reported_District_Population = pws_sums$PopServed
pws_sums$pop_multiple <- NA
pws_sums$pop_multiple[pws_sums$variable=='Total_Population'] <- pws_sums$estimate[pws_sums$variable=='Total_Population']/pws_sums$Reported_District_Population[pws_sums$variable=='Total_Population']
pws_sums[order(PWS_ID),pop_multiple:=zoo::na.locf(pop_multiple),by=.(PWS_ID)]
pws_sums$Multiplied_Value = pws_sums$estimate*(1/pws_sums$pop_multiple)

pws_census_estimates = dcast(pws_sums,Year + PWS_ID ~ variable,value.var = 'Multiplied_Value')[!is.na(PWS_ID),]
pws_census_estimates$Prop_Bachelors_Degree = pws_census_estimates$Bachelors_Population/pws_census_estimates$Total_Population
pws_census_estimates$Prop_White_Population = pws_census_estimates$White_Population/pws_census_estimates$Total_Population
pws_census_estimates$Prop_Owner_Occupied = pws_census_estimates$Owner_Occupied_Homes/pws_census_estimates$Total_Homes
cval_avgs = central_vals[,weighted.mean(x = estimate,w = Prop_Of_Tract),by=.(Year,variable,PWS_ID)]
pws_census_avgs = dcast(cval_avgs,Year + PWS_ID ~ variable,value.var = 'V1')[!is.na(PWS_ID),]
pws_census_data = merge(pws_census_avgs,pws_census_estimates)
demo_df = pws_census_data
demo_df$PWS_ID <- as.character(demo_df$PWS_ID)
fill_vars = names(demo_df)[sapply(demo_df,is.numeric)]
fill_vars = fill_vars[fill_vars!='Year']
demo_df[order(PWS_ID,Year),(fill_vars):=lapply(.SD,zoo::na.locf,na.rm=F),by = .(PWS_ID),.SDcols = fill_vars]
demo_df = demo_df[Year==2010,]


setkey(demo_df,PWS_ID)
setkey(cox_dt,PWS_ID)
cox_dt = merge(cox_dt,demo_df,on=c('PWS_ID'),all.x = T)#demo_df[cox_dt,]


#congress_overlap_dt = readRDS('../../../../net/tmp/tscott1/bosque_scratch/proj5/congress_overlap_file.RDS')
house_recs = data.table(readRDS('input/houseAtts_5-2019.RDS'))
house_recs = house_recs[house_recs$state=='Texas',]
setnames(house_recs,'year','Year')
congress_overlap_dt$PWS_ID = as.character(congress_overlap_dt$PWS_ID)
setkey(house_recs,Congressional_District_ID,Year)
setkey(congress_overlap_dt,Congressional_District)
house_overmerge = house_recs[congress_overlap_dt,allow.cartesian=TRUE]
polvars = c('democrat','LCV_lifetime','nominate_dim1')
pol_vals_dt = house_overmerge[,lapply(.SD,weighted.mean,w=Prop_Over_District),by=.(Year,PWS_ID),.SDcols = polvars]
pol_vals_dt = pol_vals_dt[Year==2010,]
setkey(pol_vals_dt,PWS_ID,Year)
setkey(cox_dt,PWS_ID,Year)
cox_dt = merge(cox_dt,pol_vals_dt,all.x=T)



#cox_dt$Wholesale = (cox_dt$Prop_Wholesale_Connections>0) + 0
cox_dt$Urban_District = (cox_dt$Urban_Area_Prop>0.5) + 0
cox_dt$intercept = 1

cox_dt$Perc_Nonwhite <- {1-cox_dt$Prop_White_Population} * 100
cox_dt$Perc_College_Grad <- cox_dt$Prop_Bachelors_Degree * 100
cox_dt$Storage_Per_Connection_MG = cox_dt$Total_Storage_MG_Impute/cox_dt$Connections
cox_dt$Storage_Per_Connection_G = cox_dt$Storage_Per_Connection_MG*1000000
cox_dt$District_Age = decimal_date(cox_dt$Date) - decimal_date(mdy(dinfo$Created[match(cox_dt$District_ID,dinfo$District_ID)]))
cox_dt$Median_Home_Value[is.na(cox_dt$Median_Home_Value)] <- median(cox_dt$Median_Home_Value,na.rm=T)
cox_dt$Num_Emergency_Interconnections[is.na(cox_dt$Num_Emergency_Interconnections)]<-0
cox_dt$Has_Emergency_Interconnect <- (cox_dt$Num_Emergency_Interconnections>0)+0

cox_dt$Class <- NA
cox_dt$Class[cox_dt$Connections<=500]<-'Very Small'
cox_dt$Class[cox_dt$Connections>500&cox_dt$Connections<=3300]<-'Small'
cox_dt$Class[cox_dt$Connections>3300&cox_dt$Connections<=10000]<-'Medium'
cox_dt$Class[cox_dt$Connections>10000]<-'Large'

cox_dt$District_Name = as.character(cox_dt$District_Name)
cox_dt$District_ID <- as.character(cox_dt$District_ID)


fiscal_dt = readRDS('scratch/fiscal_data.RDS')
fiscal_dt[,District_Name:=NULL]
fiscal_dt$`FISCAL YEAR ENDED` = ymd(fiscal_dt$`FISCAL YEAR ENDED`)
fiscal_dt$`FISCAL YEAR ENDED`[is.na(fiscal_dt$`FISCAL YEAR ENDED`)] <- mdy(paste0('8/31/',fiscal_dt$FiscalYear[is.na(fiscal_dt$`FISCAL YEAR ENDED`)])) 

cox_dt$join_time = ymd(base::as.Date(cox_dt$start_date))
fiscal_dt$join_time = ymd(fiscal_dt$`FISCAL YEAR ENDED`)
fiscal_dt[order(DISTRICT_NAME,FiscalYear),District_ID:=zoo::na.locf0(District_ID),by = .(DISTRICT_NAME)]
fiscal_dt[order(DISTRICT_NAME,FiscalYear),District_ID:=zoo::na.locf0(District_ID,fromLast = T),by = .(DISTRICT_NAME)]

fiscal_dt = fiscal_dt[order(District_ID,`FISCAL YEAR ENDED`),]
no_audits = fiscal_dt[,all(is.na(DOC_ID)),by=.(DISTRICT_NAME)][V1==T,] 
fiscal_dt = fiscal_dt[!DISTRICT_NAME %in% no_audits$DISTRICT_NAME,]
fiscal_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[!is.na(fiscal_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`) & fiscal_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`==0] <- NA
fiscal_dt[order(District_ID,FiscalYear),`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`:=zoo::na.locf(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,na.rm=F),by = .(District_ID)]

setkey(cox_dt,District_ID,join_time)
setkey(fiscal_dt,District_ID,join_time)
cox_dt = fiscal_dt[cox_dt,roll = 730]

district_total_connections = cox_dt[,sum(Connections,na.rm=T),by = .(District_ID,FiscalYear)]
setnames(district_total_connections,'V1','District_Connections')
district_total_connections = district_total_connections[!is.na(FiscalYear)&District_ID!='0000000']
setkey(district_total_connections,'District_ID','FiscalYear')
setkey(cox_dt,'District_ID','FiscalYear')



cox_dt = district_total_connections[cox_dt,]
cox_dt$CWS_Prop = cox_dt$Connections/cox_dt$District_Connections
aud_vars = names(cox_dt)[grepl('TAX|FUND|REV|EXP|ISSUE|DEBT|PRINC|INTEREST|BONDS',toupper(names(cox_dt)))&!grepl('RATE',toupper(names(cox_dt)))]
cox_dt[,(aud_vars):=lapply(.SD,function(x) x * CWS_Prop),.SDcols = aud_vars]
cox_dt$Connections_2015[is.na(cox_dt$Connections_2015)] <- fiscal_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match(paste(cox_dt$District_ID[is.na(cox_dt$Connections_2015)],cox_dt$FiscalYear[is.na(cox_dt$Connections_2015)]),
            paste(fiscal_dt$District_ID,fiscal_dt$FiscalYear))]


# aud_vars = unique(c('BONDS OUTSTANDING',"CURRENT ASSESSED VALUATION" ,"DEBT SERVICE TAX LEVIED" , "ENTERPRISE FUND - ASSETS" ,  "ENTERPRISE FUND - LIABILITIES" ,          
#                     "ENTERPRISE FUND - NET ASSETS", "ENTERPRISE FUND - OPERATING EXPENSES", "ENTERPRISE FUND - OPERATING REVENUES" ,   
#                     "GENERAL FUND - ASSETS" , "GENERAL FUND - FUND BALANCE",             
#                     "GENERAL FUND - LIABILITIES"   , "GENERAL FUND - TOTAL EXPENDITURES","GENERAL FUND - TOTAL REVENUES",
#                     grep('Issue_',colnames(fiscal_dt),value=T),grep('Outstanding',colnames(fiscal_dt),value=T),
#                     "GENERAL FUND - LIABILITIES"   , "GENERAL FUND - TOTAL EXPENDITURES","GENERAL FUND - TOTAL REVENUES" ))



cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&{cox_dt$`BONDS OUTSTANDING`==0|(is.na(cox_dt$`BONDS OUTSTANDING`)&!is.na(cox_dt$DOC_ID))}] <- 0
cox_dt$Total_Debt_Service_Outstanding[is.na(cox_dt$Total_Debt_Service_Outstanding)&{cox_dt$`BONDS OUTSTANDING`==0|(is.na(cox_dt$`BONDS OUTSTANDING`)&!is.na(cox_dt$DOC_ID))}] <- 0

#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$`BONDS OUTSTANDING`==0] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_GO)&cox_dt$`BONDS OUTSTANDING`==0] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_GO)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0
#cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_ID%in%debt_dt$District_ID] <- 0

cox_dt$District_Type <- as.character(cox_dt$District_Type)
cox_dt$District_Type[is.na(cox_dt$District_Type)] <- 'Not a district'
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD'] <- cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD']
#cox_dt$TotalPrincipalOutstanding_GO[is.na(cox_dt$TotalPrincipalOutstanding_REV)&cox_dt$District_Type=='SUD'] <- 0
cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_Type=='SUD'] <- cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$Total_Principal_Outstanding)&cox_dt$District_Type=='SUD']
#cox_dt$TotalPrincipalOutstanding_REV[is.na(cox_dt$TotalPrincipalOutstanding_REV)&!is.na(cox_dt$TotalPrincipalOutstanding_GO)] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[!is.na(cox_dt$TotalPrincipalOutstanding_REV)&is.na(cox_dt$TotalPrincipalOutstanding_GO)] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='3398000'] <- cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='3398000']
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='3398000'] <- cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='3398000']
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='3398000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='4880000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='4880000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='4880000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='4880000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='7103000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='7103000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='7103000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='7103000'] <- 0
cox_dt$`BONDS OUTSTANDING`[cox_dt$District_ID=='7322000'] <- 0
#cox_dt$TotalPrincipalOutstanding_GO[cox_dt$District_ID=='7322000'] <- 0
#cox_dt$TotalPrincipalOutstanding_REV[cox_dt$District_ID=='7322000'] <- 0
cox_dt$Total_Principal_Outstanding[cox_dt$District_ID=='7322000'] <- 0

#cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)&!is.na(cox_dt$`BONDS OUTSTANDING`) &cox_dt$`BONDS OUTSTANDING`>0] <- cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$Total_Principal_Outstanding)&!is.na(cox_dt$`BONDS OUTSTANDING`) & cox_dt$`BONDS OUTSTANDING`>0]
cox_dt$Two_Year_Match = (decimal_date(cox_dt$Date) - decimal_date(cox_dt$`FISCAL YEAR ENDED`) > 1) + 0
cox_dt$Total_Principal_Outstanding = ifelse(cox_dt$Total_Principal_Outstanding==0,0, cox_dt$Total_Principal_Outstanding * cox_dt$CWS_Prop)
cox_dt$Total_Debt_Service_Outstanding = ifelse(cox_dt$Total_Debt_Service_Outstanding==0,0, cox_dt$Total_Debt_Service_Outstanding * cox_dt$CWS_Prop)


#cox_dt$TotalPrincipalOutstanding_REV = cox_dt$TotalPrincipalOutstanding_REV * cox_dt$CWS_Prop
#cox_dt$TotalPrincipalOutstanding_GO = cox_dt$TotalPrincipalOutstanding_GO * cox_dt$CWS_Prop
cox_dt$`GENERAL FUND - TOTAL REVENUES`[is.na(cox_dt$`GENERAL FUND - TOTAL REVENUES`)]<- 0
cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`[is.na(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`)] <- 0
cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$`BONDS OUTSTANDING`)] <- 0
#### capacity indicator 1
cox_dt$Total_Revenue = as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  +
  as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`)
cox_dt$Enterprise_Revenue = as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`)
cox_dt$General_Revenue = as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  

cox_dt$Connections[is.na(cox_dt$Connections)&!is.na(cox_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)] <-  cox_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[is.na(cox_dt$Connections)&!is.na(cox_dt$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)]
cox_dt$Total_Revenue_Per_Connection = cox_dt$Total_Revenue/cox_dt$Connections
cox_dt$Enterprise_Revenue_Per_Connection = cox_dt$Enterprise_Revenue/cox_dt$Connections
cox_dt$General_Revenue_Per_Connection = cox_dt$General_Revenue/cox_dt$Connections

cox_dt$`TOTAL TAX RATE`[!is.na(cox_dt$District_Name) & cox_dt$District_Name == 'HARRIS COUNTY MUD 86' & cox_dt$Year%in%start_year:2012] <- 0.85
cox_dt$Fee_Based_Revenue =  as.numeric(cox_dt$`GENERAL FUND - TOTAL REVENUES`)  +
  as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING REVENUES`) - as.numeric(cox_dt$`OPERATION & MAINTENANCE TAX LEVIED`)
cox_dt$Fee_Based_Revenue = cox_dt$Fee_Based_Revenue/cox_dt$Connections
##### slush fund
cox_dt$`GENERAL FUND - FUND BALANCE`[is.na(cox_dt$`GENERAL FUND - FUND BALANCE`)] <- 0
cox_dt$Fund_Balance_Per_Connection = (as.numeric(cox_dt$`GENERAL FUND - FUND BALANCE`)) / cox_dt$Connections
#### debt load
cox_dt$Total_Principal_Outstanding[is.na(cox_dt$Total_Principal_Outstanding)] <- cox_dt$`BONDS OUTSTANDING`[is.na(cox_dt$Total_Principal_Outstanding)]
cox_dt$Debt_Per_Connection = cox_dt$Total_Principal_Outstanding/cox_dt$Connections

#cox_dt$Debt_Per_Connection_REV = cox_dt$TotalPrincipalOutstanding_REV/cox_dt$Connections
#cox_dt$Debt_Per_Connection_GO = cox_dt$TotalPrincipalOutstanding_GO/cox_dt$Connections
#cox_dt$Revenue_Backed_Debt <- (cox_dt$TotalPrincipalOutstanding_REV>0) + 0
#cox_dt$TotalDebtServiceOutstanding_REV[is.na(cox_dt$TotalDebtServiceOutstanding_REV)] <- 0
#cox_dt$REV_Debt_Per_Connection = cox_dt$TotalDebtServiceOutstanding_REV/cox_dt$Connections
#cox_dt$REV_Debt_Per_Connection_1k = cox_dt$TotalDebtServiceOutstanding_REV/1000
#### operating expense ratio (OER) 
cox_dt$`GENERAL FUND - TOTAL EXPENDITURES`[is.na(cox_dt$`GENERAL FUND - TOTAL EXPENDITURES`)] <- 0
cox_dt$`ENTERPRISE FUND - OPERATING EXPENSES`[is.na(cox_dt$`ENTERPRISE FUND - OPERATING EXPENSES`)] <- 0
cox_dt$Total_Expenditure = as.numeric(cox_dt$`GENERAL FUND - TOTAL EXPENDITURES`)  + as.numeric(cox_dt$`ENTERPRISE FUND - OPERATING EXPENSES`)

#cox_dt$Prop_Rev_Deb = cox_dt$TotalPrincipalOutstanding_REV / cox_dt$Total_Principal_Outstanding
#cox_dt$Have_Ent_Rev = (cox_dt$Enterprise_Revenue_Per_Connection_1k>0)+0
#cox_dt$Debt_Service_Tax = (cox_dt$`DEBT SERVICE TAX LEVIED` > 0) + 0
#cox_dt$Any_Debt = (cox_dt$Debt_Per_Connection>0)+0
cox_dt$District_Type <- as.character(cox_dt$District_Type)
#cox_dt$FWSD <- (cox_dt$District_Type %in% 'FWSD') + 0
#cox_dt$WCID <- (cox_dt$District_Type %in% 'WCID') + 0
#cox_dt$SUD <- (cox_dt$District_Type %in% 'SUD') + 0
#cox_dt$Tax_Rate <- cox_dt$`TOTAL TAX RATE`
#cox_dt$Revenue_Backed_Debt <- {cox_dt$TotalPrincipalOutstanding_REV>0} + 0
#cox_dt$Enterprise_Revenue <- {cox_dt$Enterprise_Revenue_Per_Connection>0} + 0
#cox_dt$Yearly_Payment_Per_Connection = cox_dt$Yearly_Debt_Payment/cox_dt$Connections
#cox_dt$Yearly_Payment_To_Revenue = cox_dt$Yearly_Debt_Payment/cox_dt$Total_Revenue
cox_dt$MO_Tax <- (cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`>0) +0
cox_dt$Debt_Tax <- (cox_dt$`DEBT SERVICE TAX RATE`>0) +0
cox_dt$Median_Structure_Age = decimal_date(cox_dt$Date)-cox_dt$Median_Year_Structure_Built

cox_dt$Operating_Ratio = cox_dt$Total_Revenue/cox_dt$Total_Expenditure
cox_dt$Fund_Balance = cox_dt$`GENERAL FUND - FUND BALANCE`
cox_dt$Water_Bond_P1 = (!is.na(cox_dt$Issue_WaterRelated_P1)) + 0
cox_dt$Refund_Bond_P1 = (!is.na(cox_dt$Issue_Refund_P1)) + 0
cox_dt$Expenditure_Per_Connection = cox_dt$Total_Expenditure/cox_dt$Connections
cox_dt$OM_Tax_Over_Expenditure = cox_dt$`OPERATION & MAINTENANCE TAX LEVIED`/cox_dt$Total_Expenditure
cox_dt$Total_Debt_Service_Outstanding[is.na(cox_dt$Total_Debt_Service_Outstanding)] <- 0

cox_dt$Total_Debt_Service_Outstanding = ifelse(cox_dt$Total_Debt_Service_Outstanding==0&!is.na(cox_dt$`BONDS OUTSTANDING`)&cox_dt$`BONDS OUTSTANDING`>0,cox_dt$`BONDS OUTSTANDING`,cox_dt$Total_Debt_Service_Outstanding)
cox_dt$Total_Debt_Service_Outstanding = ifelse(is.na(cox_dt$Total_Debt_Service_Outstanding)&!is.na(cox_dt$`BONDS OUTSTANDING`),cox_dt$`BONDS OUTSTANDING`,cox_dt$Total_Debt_Service_Outstanding)
cox_dt$Growth_Over_Drought = 100 * (cox_dt$Connections_2015 - cox_dt$Connections)/cox_dt$Connections 
cox_dt$Connections_Added_During_Drought = cox_dt$Connections_2015 - cox_dt$Connections

cox_dt[order(PWS_ID,Date),Usage_Per_Connection_P1:=zoo::na.locf0(Usage_Per_Connection_P1,fromLast=T),by = .(PWS_ID)]
cox_dt$`OTHER TAX RATE(S)`[is.na(cox_dt$`OTHER TAX RATE(S)`)] <- 0
cox_dt[,Total_Tax_Rate:=`OPERATIONS & MAINTENANCE TAX RATE`+`DEBT SERVICE TAX RATE`+`OTHER TAX RATE(S)`+`OTHER TAX RATES(S)`]



weeks = length(seq.Date(from = mdy('05/04/2010'),to = mdy('07/07/2015'),by = 'week'))
months = round(weeks/4)
months2 = months/2
cox_dt$Connections[is.na(cox_dt$Connections)] <- cox_dt$Total_Service_Connections[is.na(cox_dt$Connections)]

saveRDS(cox_dt,'scratch/data_for_coxph_model.RDS')



