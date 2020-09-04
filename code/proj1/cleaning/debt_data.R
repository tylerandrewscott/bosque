library(tidyverse)
library(readxl)
library(rgdal)
library(sp)
library(rgeos)
library(lubridate)
library(stringr)
library(readxl)
library(pbapply)

houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]

tx_tracts = readOGR('spatial_inputs/government_units','cb_2015_48_tract_500k')
tx_tracts@data$GEOID = as.character(tx_tracts@data$AFFGEOID)
houston_tracts = tx_tracts[paste(tx_tracts$STATEFP,tx_tracts$COUNTYFP,sep='') %in% hout_counties$FIPS_I,]


#### Load system summary from EPA
system_df = read_csv('Input/epa_sdwis/hous_msa_sdwis_summary.csv',na = c("-"))%>%
  rename(PWS_Type = `PWS Type`,City = `City Name`) %>%
  # filter(PWS_Type == 'Community water system'&!`Owner Type` %in% c('State government','Federal government','Private')) %>%
  mutate(First_Report_Date = dmy(`First Reported Date`),Year_Start = year(First_Report_Date),
         City = str_to_title(City)) %>%
  dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) %>%
  rename(PWS_NAME = `PWS Name`,PWS_ID = `PWS ID`)

system_df = system_df %>% filter(is.na(year(dmy(system_df$`Deactivation Date`)))|year(dmy(system_df$`Deactivation Date`))>2009)


#system_df  = system_df %>% filter(!grepl('CITY|TOWN ',system_df$PWS_NAME))


system_df$PWS_NAME[grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME)] = 
  str_extract(grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME,value=T),'FORT BEND COUNTY MUD [0-9]{1,}[A-Z]{0,2}')
system_df$PWS_NAME = gsub('HCO',"HARRIS COUNTY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' NO '," ",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 24 COUNTRY COLONY","MONTGOMERY COUNTY MUD 24",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 6 CARRIAGE LANE","HARRIS COUNTY MUD 6",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 119 SPRING TRAILS","MONTGOMERY COUNTY MUD 119",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 400   WEST","HARRIS COUNTY MUD 400",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 55 HERITAGE PARK","HARRIS COUNTY MUD 55",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 148 KINGSLAKE","HARRIS COUNTY MUD 148",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 374 CYPRESS CREEK LAKE","HARRIS COUNTY MUD 374",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("WEST HARRIS COUNTY MUD 2 CHASE","WEST HARRIS COUNTY MUD 2",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('WATER SUPPLY CORPORATION','WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('REGIONAL WATER AUTHOR$|REGIONAL WATER AUTHORITY','RWA',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('HARRIS COUNTY MUD 400 - WEST','HARRIS COUNTY MUD 400',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 113 ENCHANTED VILLAGE","HARRIS COUNTY WCID 113",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THE WOODLANDS",'WOODLANDS',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('BRAZORIA COUNTY FWSD 1 DAMON','BRAZORIA COUNTY FWSD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD1",'MUD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD3",'MUD 3',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" TOWNE LAKE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GULF COAST WATER AUTHORITY TX CITY","GULF COAST WATER AUTHORITY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("LIBERTY COUNTY FWSD 1 HULL","HULL FWSD",system_df$PWS_NAME)

system_df$PWS_NAME = gsub("BROOK HOLLOW WEST S",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" FAIRFIELD VILLAGE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('TOWN OF','CITY OF',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND" ,"GALVESTON COUNTY FWSD 6" ,system_df$PWS_NAME)

system_df$PWS_NAME = gsub("CINCO SOUTHWEST MUD 3 DAYCARE","CINCO SOUTHWEST MUD 3",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 50 EL LAGO","HARRIS COUNTY WCID 50",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('DOBBIN PLANTERSVILLE WSC 1','DOBBIN PLANTERSVILLE WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 200 CRANBROOK","HARRIS COUNTY MUD 200",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('CENTRAL HARRIS COUNTY REGIONAL WATER AUT$','CENTRAL HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('NORTH HARRIS COUNTY REGIONAL WATER AUTHO$','NORTH HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 16 WHITE OAK PLANT","MONTGOMERY COUNTY MUD 16",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CITY OF WOOD BRANCH VILLAGE","CITY OF WOODBRANCH",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('-',' ',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' $','',system_df$PWS_NAME)


#table(system_df$PWS_NAME %in% debt_df$Issuer)
#grep('MUD',system_df$PWS_NAME[!system_df$PWS_NAME %in% debt_df$Issuer],value=T)

# not_in = c("HARRIS COUNTY MUD 537","HARRIS COUNTY MUD 530","HARRIS COUNTY MUD 495","HARRIS COUNTY MUD 494","HARRIS COUNTY MUD 387",
#            "MONTGOMERY COUNTY MUD 126","MONTGOMERY COUNTY MUD 127","MONTGOMERY COUNTY MUD 141","MONTGOMERY COUNTY MUD 105","FORT BEND COUNTY MUD 134D")
# 
# for (i in sort(grep('MUD',system_df$PWS_NAME[!system_df$PWS_NAME %in% debt_df$Issuer],value=T)))
# {
#   print('search');print(i)
#   print(agrep(i,unique(debt_df$Issuer),value=T,costs = 4))
# }

#temp = as.data.frame(table(system_df$PWS_NAME %in% debt_df$Issuer,system_type)) %>% spread(Var1,Freq)
#colnames(temp) = c('type','no match','match')

twd_geo = readOGR('spatial_inputs/government_units','TCEQ_WATER_DISTRICTS')
twd_geo = twd_geo[twd_geo$COUNTY %in% houston_counties,]
twd_geo$NAME = str_to_upper(twd_geo$NAME)
twd_geo$NAME = gsub('PUBLIC UTILITY DISTRICT','PUD',twd_geo$NAME)
twd_geo$NAME = gsub('UTILITY DISTRICT|UTILITY DRISTRICT','UD',twd_geo$NAME)
twd_geo$NAME = gsub('134 A','134A',twd_geo$NAME)
twd_geo$NAME = gsub('134 C','134C',twd_geo$NAME)
twd_geo$NAME = gsub('THE WOODLANDS','WOODLANDS',twd_geo$NAME)
twd_geo$NAME = gsub('POST WOOD','POSTWOOD',twd_geo$NAME)
twd_geo$NAME = gsub('CLOVERCREEK','CLOVER CREEK',twd_geo$NAME)
twd_geo$NAME = gsub('  ',' ',twd_geo$NAME)
twd_geo$NAME = gsub("'","",twd_geo$NAME)
twd_geo$NAME = gsub('OF BRAZORIA COUNTY','',twd_geo$NAME)
twd_geo$NAME = gsub('VILLIAGES','VILLAGES',twd_geo$NAME)
twd_geo$NAME = gsub('HARRIS COUNTY FWSD 1-B','HARRIS COUNTY FWSD 1B',twd_geo$NAME)
twd_geo$NAME = gsub(' $','',twd_geo$NAME)
#twd_geo$NAME = gsub('HARRIS FORT BEND COUNTIES MUD 1','HARRIS-FORT BEND COUNTIES MUD 1',twd_geo$NAME)
#twd_geo$NAME = gsub('HARRIS FORT BEND COUNTIES MUD 3','HARRIS-FORT BEND COUNTIES MUD 3',twd_geo$NAME)
twd_geo$NAME = gsub('THUNDERBIRD UD','THUNDERBIRD UD 1',twd_geo$NAME)
twd_geo$NAME = gsub('-',' ',twd_geo$NAME)
twd_geo$NAME = gsub("CYPRESS KLEIN UTILTIY DISTRICT","CYPRESS KLEIN UD WIMBLETON",twd_geo$NAME)
twd_geo$NAME = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",twd_geo$NAME)
twd_geo = spTransform(twd_geo,CRS(proj4string(tx_tracts)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)

houston_msa = readOGR('spatial_inputs/hg_gis','houston_msa_full')
houston_msa@data$id = rownames(houston_msa@data)
houston_msa = spTransform(houston_msa,CRS(proj4string(twd_geo)))
houston_msa_df = left_join(fortify(houston_msa),houston_msa@data)

tx_places = readOGR('spatial_inputs/government_units/','cb_2015_48_place_500k')
tx_places = spTransform(tx_places,CRS(proj4string(tx_tracts)))
in_hout = over(tx_places,hout_counties,returnList = FALSE)
hout_places = tx_places[!is.na(in_hout$FIPS_I),]
hout_places$NAME = paste0('CITY OF ',str_to_upper(hout_places$NAME))
hout_places$NAME = gsub("'","",hout_places$NAME)
hout_places$NAME = gsub("CITY OF MISSOURI CITY","CITY OF MISSOURI CITY MUSTANG BAYOU WATE",hout_places$NAME)

tx_utils = readOGR('spatial_inputs/government_units/PUC_CCN_WATER/','PUC_CCN_WATER')
tx_utils = spTransform(tx_utils,CRS(proj4string(tx_tracts)))
hout_utils = tx_utils[tx_utils$COUNTY %in% str_to_upper(houston_counties),]
hout_utils$UTILITY = gsub('SUGARLAND','SUGAR LAND',hout_utils$UTILITY)
hout_utils = hout_utils[!hout_utils$UTILITY %in% c(hout_places$NAME,twd_geo$NAME),]
hout_utils$UTILITY = gsub('NORTHWOODS WSC','NORTHWOOD WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('^HOE WSC','H O E WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('WOODLAND LAKES WSC','WOODLAND LAKES ESTATES WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('WALNUT COVE INC','WALNUT COVE WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub("PINE LAKE WSC INC","PINE LAKE SUBDIVISION NORTH WSC",hout_utils$UTILITY)
hout_utils$UTILITY = gsub('ENCHANTED VALLEY WSC','ENCHANTED VALLEY ESTATES WSC',hout_utils$UTILITY)

replace1 = c("COE INDUSTRIAL PARK","ALICE ACRES MOBILE HOME SUBDIVISION","2920 WEST SUBDIVISION" ,"BRANDYWINE OAKS","BRANDYWINE PINES" ,"CYPRESS CROSSING",
             "KICKAPOO FARMS SUBDIVISION","CYPRESS PASS ESTATES" ,"WILLOW OAKS MOBILE HOME SUBDIVISION" ,"VILLAGE OF NEW KENTUCKY","RED OAK TERRACE" ,
             "HOUSE CORRAL STREET WATER SYSTEM" ,"TRAILWOOD SUBDIVISION"  ,"TIMBERWILDE MH SUBDIVISION" ,"GRANT ROAD ESTATES MOBILE HOME SUB",
             "TOWERING OAKS AND ROSEWOOD HILLS SUBDIVI")
hout_utils$UTILITY[hout_utils$UTILITY=='H-M-W SUD'&hout_utils$COUNTY=='HARRIS'][1:length(replace1)] = replace1

replace2 = c("COE COUNTRY", "ALLENWOOD SUBDIVISION", "ARMADILLO WOODS SUBDIVISION" ,"RIMWICK FOREST" , "RUSTIC OAKS SUBDIVISION" , "MINK BRANCH VALLEY"  ,
             "KIPLING OAKS 1", "KIPLING OAKS AND TIMBERGREEN"  ,"ESTATES OF HOLLY LAKES","PLEASANT FOREST SUBDIVISION"  ,"DEER RIDGE SUBDIVISION", 
             "SHADY ACRES"   )
hout_utils$UTILITY[hout_utils$UTILITY=='H-M-W SUD'&hout_utils$COUNTY=='MONTGOMERY'][1:length(replace2)] = replace2



#system_df$DEBT_MATCH[grepl('CITY OF|WSC',system_df$PWS_NAME)] = 'NOT INCLUDED'
#systerm_df$DEBT_MATCH[is.na()] = 'NOT OBSERVED'
#system_df = system_df[system_df$PWS_NAME %in% debt_df$PWS_NAME,]

library(maptools)

hout_counties@data$id = hout_counties@data$COUNTYNAME
hout_counties.points = fortify(hout_counties, region="id")
hout_counties.df = plyr::join(hout_counties.points, hout_counties@data, by="id")

houston_tracts@data$id = houston_tracts@data$AFFGEOID
houston_tracts.points = fortify(houston_tracts, region="id")
houston_tracts.df = plyr::join(houston_tracts.points, houston_tracts@data, by="id")

hout_utils@data$id = hout_utils@data$UTILITY
hout_utils.points = fortify(hout_utils, region="id")
hout_utils.df = plyr::join(hout_utils.points, hout_utils@data, by='id')
hout_utils.df$PWS_NAME = hout_utils.df$UTILITY
hout_utils.df$OBS = FALSE
hout_utils.df$OBS[hout_utils.df$PWS_NAME %in% system_df$PWS_NAME] = TRUE


hout_places@data$id = hout_places@data$NAME
hout_places.points = fortify(hout_places, region="id")
hout_places.df = plyr::join(hout_places.points, hout_places@data, by="id") 
hout_places.df$PWS_NAME = hout_places.df$NAME
hout_places.df$OBS = FALSE
hout_places.df$OBS[hout_places.df$PWS_NAME %in% system_df$PWS_NAME] = TRUE

houston_wd_geo = twd_geo[twd_geo@data$COUNTY %in% houston_counties,]


######
# houston_wd_geo@data$id = houston_wd_geo@data$NAME
# hout_wd.points = fortify(houston_wd_geo,region='id')
# hout_wd.df = plyr::join(hout_wd.points,houston_wd_geo@data,by='id')
# hout_wd.df$PWS_NAME = hout_wd.df$NAME
# which_tract_wd = over(houston_wd_geo,tx_tracts)
# table(is.na(which_tract_wd$))
# houston_wd_geo@data$GEOID = as.character(which_tract_wd$GEOID)

analyze_set = system_df[grepl('MUD|WCID|FWSD',system_df$PWS_NAME),]

system_type = rep(NA,nrow(system_df))
system_type[grepl('WCID',system_df$PWS_NAME)] = 'WCID'
system_type[grepl('CITY|TOWN',system_df$PWS_NAME)] = 'CITY'
system_type[grepl('MWA',system_df$PWS_NAME)] = 'MWA'
system_type[grepl('PUD$| PUD ',system_df$PWS_NAME)] = 'PUD'
system_type[grepl('MWD',system_df$PWS_NAME)] = 'MWD'
system_type[grepl(' MUD | MUD$',system_df$PWS_NAME)] = 'MUD'
system_type[grepl('WSC',system_df$PWS_NAME)] = 'WSC'
system_type[grepl('SUD$| SUD ',system_df$PWS_NAME)] = 'SUD'
system_type[grepl('FWSD',system_df$PWS_NAME)] = 'FWSD'
system_type[grepl('MUNICIPAL WATER SYSTEM',system_df$PWS_NAME)] = 'MWS'
system_type[grepl('REGIONAL WATER',system_df$PWS_NAME)] = 'RWA'
system_type[grepl(' UD | UD$',system_df$PWS_NAME)] = 'UD'
system_type[grepl('IMPROVEMENT DISTRICT',system_df$PWS_NAME)] = 'ID'
system_type[grepl('WATER SYSTEM$',system_df$PWS_NAME)] = 'MWS'
system_type[is.na(system_type)&grepl('WATER AUTHORITY',system_df$PWS_NAME)] = 'RWA'
system_type[is.na(system_type)] = 'Other'



######
# houston_wd_geo@data$id = houston_wd_geo@data$NAME
# hout_wd.points = fortify(houston_wd_geo,region='id')
# hout_wd.df = plyr::join(hout_wd.points,houston_wd_geo@data,by='id')
# hout_wd.df$PWS_NAME = hout_wd.df$NAME
# which_tract_wd = over(houston_wd_geo,tx_tracts)
# table(is.na(which_tract_wd$))
# houston_wd_geo@data$GEOID = as.character(which_tract_wd$GEOID)

library(ggthemes)

# To use for fills, add
#hout_places.df$HOUSTON = ifelse(hout_places.df$PWS_NAME=='CITY OF HOUSTON','CITY OF HOUSTON','CITY')

service1 = readOGR('spatial_inputs/hg_gis','service_boundaries')
service1 = spTransform(service1,CRS(proj4string(hout_counties)))
service1$NAME = str_to_upper(service1$PERMIT_HOL)
service1$NAME[grepl('\\, CITY OF$|\\, CITY$',service1$NAME)] = paste0('CITY OF ',service1$NAME[grepl('\\, CITY OF$|\\, CITY$',service1$NAME)])
service1$NAME = gsub('\\, CITY OF$|\\, CITY$','',service1$NAME)
service1 = service1[!grepl('PEARLAND',service1$NAME),]
service1$NAME = gsub(' NO\\.','',service1$NAME)
service1$NAME = gsub(' 0{1,}',' ',service1$NAME)
service1$NAME = gsub('POST WOOD','POSTWOOD',service1$NAME)
service1$NAME = gsub('CLOVERCREEK','CLOVER CREEK',service1$NAME)
service1$NAME = gsub('MEADOW CREEK','MEADOWCREEK',service1$NAME)
service1$NAME = gsub('SPECIAL UD','SUD',service1$NAME)
service1$NAME = gsub('THUNDERBIRD UD$','THUNDERBIRD UD 1',service1$NAME)
service1$NAME = gsub('REGIONAL WA$|REGIONAL WATER AUTHORITY','RWA',service1$NAME)
service1$NAME = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",service1$NAME)
service1$NAME = gsub('-',' ',service1$NAME)
service1$NAME = gsub("134 C",'134C',service1$NAME)
service1$NAME = gsub("134 D",'134D',service1$NAME)
service1$NAME = gsub("134 A",'134A',service1$NAME)
service1$NAME = gsub("5\\\v",'5',service1$NAME)
service1$NAME = gsub("MUD THE$|MUD\\, THE$",'MUD',service1$NAME)
service1$NAME = gsub("OAK MANOR UD",'OAK MANOR MUD',service1$NAME)
service1$NAME = gsub("'",'',service1$NAME)
service1$NAME = gsub(" CO ",' COUNTY ',service1$NAME)

service1$NAME = gsub("FT",'FORT',service1$NAME)
service1$NAME = gsub("TX",'TEXAS',service1$NAME)
service1$NAME = gsub("MC",'MONTGOMERY COUNTY',service1$NAME)
service1$NAME = gsub("HARRIS COUNTY MUD 18" ,"HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",service1$NAME)
service1$NAME = gsub("NW HC",'NORTHWEST HARRIS COUNTY',service1$NAME)
service1$NAME = gsub("NE HC",'NORTHEAST HARRIS COUNTY',service1$NAME)
service1$NAME = gsub("HC",'HARRIS COUNTY',service1$NAME)
service1$NAME = gsub("FBC",'FORT BEND COUNTY',service1$NAME)
service1$NAME = gsub("FORT BEND MUD",'FORT BEND COUNTY MUD',service1$NAME)
service1$NAME = gsub("MUNICIPAL UTILITY DISTRICT",'MUD',service1$NAME)
service1$NAME = gsub("TAMARRON LAKES LP",'FORT BEND COUNTY MUD 182',service1$NAME)
service1$NAME = gsub("^E MONTGOMERY",'EAST MONTGOMERY',service1$NAME)

service1$NAME = gsub("FRESH WATER SUPPLY DISTRICT",'FWSD',service1$NAME)
service1$NAME = gsub("GALV COUNTY",'GALVESTON COUNTY',service1$NAME)
service1$NAME = gsub("HARRIS MONTGOMERY COUNTY",'HARRIS MONTGOMERY COUNTIES',service1$NAME)
service1$NAME = gsub("HARRIS MONTGOMERY COUNTIES MUD 386","HARRIS MONTGOMERY COUNTIES MUD 386 MAY V",service1$NAME)
service1$NAME = gsub("HARRIS COUNTY MUD 386","HARRIS MONTGOMERY COUNTIES MUD 386",service1$NAME)
service1$NAME[service1$NAME=="NORTHEAST HARRIS COUNTY MUD 1"] = "NORTHEAST HARRIS COUNTY MUD 1 SHELDON RI"
service1$NAME[service1$NAME=="SHELDON RD MUD"] = "SHELDON ROAD MUD"
service1$COMMON_NAM = str_to_upper(service1$COMMON_NAM)
service1$NAME = gsub('"','',service1$NAME)
service1$NAME = gsub('\\#','',service1$NAME)

in_region = over(service1,hout_counties)
service1 = service1[!is.na(in_region$COUNTYNAME),]
service1_over_tract = over(service1,houston_tracts)
service1$AFFGEOID = service1_over_tract$AFFGEOID
service1_over_county = over(service1,hout_counties)
service1$COUNTY = service1_over_county$COUNTYNAME
service1@data$id = service1@data$OBJECTID
service1 = raster::crop(service1,houston_msa)
service1@data$NAME[grep('SUNBELT',service1@data$NAME)] = grep('SUNBELT',system_df$PWS_NAME,value=T)

service1 = service1[service1@data$NAME %in% system_df$PWS_NAME,]
#service1 = gIntersection(houston_msa, service1)
service1.points = fortify(service1,region='id')
service1_df = plyr::join(service1.points,service1@data,by='id')

# search_set = system_df$PWS_NAME[system_df$`Owner Type`=='Local government']
# found_set = unique(c(city_df$NAME,service_df$NAME,wsc_df$UTILITY,other_df$NAME))
# sort(search_set[!search_set %in% found_set])


service2 = readOGR('spatial_inputs/hg_gis','other_districts')
service2 = spTransform(service2,CRS(proj4string(hout_counties)))
service2_over_tract = over(service2,houston_tracts)
service2$AFFGEOID = service2_over_tract$AFFGEOID
service2_over_county = over(service2,hout_counties)
service2$COUNTY = service2_over_county$COUNTYNAME
service2$NAME = str_to_upper(service2$NAME)
service2 = spChFIDs(service2,as.character((length(service1)+1):(length(service1)+length(service2))))

service2$NAME = gsub(' NO\\.|#','',service2$NAME)
service2$NAME = gsub(' 0{1,}',' ',service2$NAME)
service2$NAME = gsub('POST WOOD','POSTWOOD',service2$NAME)
service2$NAME = gsub('CLOVERCREEK','CLOVER CREEK',service2$NAME)
service2$NAME = gsub('MEADOW CREEK','MEADOWCREEK',service2$NAME)
service2$NAME = gsub('SPECIAL UD','SUD',service2$NAME)
service2$NAME = gsub("PEARLAND MMD 1", "CITY OF PEARLAND MUD 1",service2$NAME)
service2$NAME = gsub('THUNDERBIRD UD$','THUNDERBIRD UD 1',service2$NAME)
service2$NAME = gsub('REGIONAL WA$|REGIONAL WATER AUTHORITY','RWA',service2$NAME)
service2$NAME = gsub('-',' ',service2$NAME)
service2$NAME = gsub("134 C",'134C',service2$NAME)
service2$NAME = gsub("134 D",'134D',service2$NAME)
service2$NAME = gsub("134 A",'134A',service2$NAME)
service2$NAME = gsub("5\\\v",'5',service2$NAME)
service2$NAME = gsub("MUD THE$|MUD\\, THE$",'MUD',service2$NAME)
service2$NAME = gsub("OAK MANOR UD",'OAK MANOR MUD',service2$NAME)
service2$NAME = gsub("'",'',service2$NAME)
service2$NAME = gsub(" CO ",' COUNTY ',service2$NAME)
service2$NAME = gsub("^HC ",'HARRIS COUNTY ',service2$NAME)
service2$NAME = gsub("FT",'FORT',service2$NAME)
service2$NAME = gsub("TX",'TEXAS',service2$NAME)
service2$NAME = gsub("MC",'MONTGOMERY COUNTY',service2$NAME)
service2$NAME = gsub("NW HC",'NORTHWEST HARRIS COUNTY',service2$NAME)
service2$NAME = gsub("FBC",'FORT BEND COUNTY',service2$NAME)
service2$NAME = gsub("FORT BEND MUD",'FORT BEND COUNTY MUD',service2$NAME)
service2$NAME = gsub("MUNICIPAL UTILITY DISTRICT",'MUD',service2$NAME)
service2$NAME = gsub("TAMARRON LAKES LP",'FORT BEND COUNTY MUD 182',service2$NAME)
service2$NAME = gsub("^E MONTGOMERY",'EAST MONTGOMERY',service2$NAME)
service2$NAME = gsub("HC",'HARRIS COUNTY',service2$NAME)
service2$NAME = gsub("GALV COUNTY",'GALVESTON COUNTY',service2$NAME)
service2$COUNTY = as.character(service2$COUNTY)
service2$NAME = gsub("HARRIS MONTGOMERY COUNTY",'HARRIS MONTGOMERY COUNTIES',service2$NAME)
service2$COUNTY[grepl("BOLIVAR PENINSULA SUD",service2$NAME)] = 'GALVESTON'

temp = service2[service2$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% service1$NAME],]

temp@data$Type =  ifelse(grepl('Public Utility District',temp@data$Type),'PUD',ifelse(grepl('Special Utility District',temp@data$Type),'SUD',
                                                                                      ifelse(grepl('River Authority',temp@data$Type),'RA',
                                                                                             ifelse(grepl('Water Authority',temp@data$Type),'WA',  ifelse(grepl('Water Control Improvement District',temp@data$Type),'WCID',
                                                                                                                                                          ifelse(grepl('Fresh Water Supply District',temp@data$Type),'FWSD',
                                                                                                                                                                 ifelse(temp@data$Type=='Utility District','UD', temp@data$Type)))))))


temp@data$id = temp@data$NAME
library(raster)
temp = raster::crop(temp,hout_counties)
temp.points = fortify(temp,region='id')
temp_df = plyr::join(temp.points,temp@data,by='id')
service_df = full_join(service1_df,temp_df) %>% filter(NAME %in% system_df$PWS_NAME|grepl('SUNBELT',NAME))
service_df = service_df %>% mutate(TYPE = as.character(Type)) %>% dplyr::select(-Type) %>% mutate(TYPE = ifelse(TYPE=='8','MUD',TYPE))

msa = readOGR('spatial_inputs/hg_gis/','msa_coastline')
msa = spTransform(msa,CRS(proj4string(hout_counties)))

temp = houston_wd_geo[houston_wd_geo@data$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% service_df$NAME],] 
temp@data$TYPE = temp@data$TYPE
temp = raster::crop(temp,hout_counties)
temp_over_tract = over(temp,houston_tracts)
temp$AFFGEOID = temp_over_tract$AFFGEOID
temp@data$id = temp@data$OBJECTID
temp.points = fortify(temp,region='id')
temp_df = plyr::join(temp.points,temp@data,by='id')
other_df = temp_df

temp = hout_places[hout_places@data$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% (c(unique(service_df$NAME),unique(other_df$NAME)))],]
temp@data$TYPE = 'Municipal'
temp = raster::crop(temp,hout_counties)
temp_over_tract = over(temp,houston_tracts)
temp_over_county = over(temp,hout_counties)
temp$AFFGEOID = temp_over_tract$AFFGEOID
temp$COUNTY = temp_over_county$COUNTYNAME
temp@data$id = temp@data$NAME
temp.points = fortify(temp,region='id')
temp_df = plyr::join(temp.points,temp@data,by='id')
city_df = temp_df

puc_dists = readOGR('spatial_inputs/texas_puc','PUC_CCN_WATER')
puc_dists = spTransform(puc_dists,CRS(proj4string(hout_counties)))
puc_dists$UTILITY = as.character(puc_dists$UTILITY)
puc_dists$UTILITY[puc_dists$UTILITY == 'NORTHWOODS WSC'] = 'NORTHWOOD WSC'
puc_dists$UTILITY[puc_dists$UTILITY == "WOODLAND LAKES WSC"] = "WOODLAND LAKES ESTATES WSC"
puc_dists$UTILITY[puc_dists$UTILITY == "WALNUT COVE INC"] = "WALNUT COVE WSC"
puc_dists$UTILITY[puc_dists$UTILITY == "PINE LAKE WSC INC"] = "PINE LAKE SUBDIVISION NORTH WSC"


temp = puc_dists[puc_dists@data$UTILITY %in% 
                   system_df$PWS_NAME[!system_df$PWS_NAME %in% unique(c(service_df$NAME,city_df$NAME,other_df$NAME))],]
temp = raster::crop(temp,hout_counties)
temp_over_tract = over(temp,houston_tracts)
temp_over_county = over(temp,hout_counties)
temp@data$TYPE = ifelse(system_df$`Owner Type`[match(temp$UTILITY,system_df$PWS_NAME)]=='Local government','WSC',"Private")
temp$AFFGEOID = temp_over_tract$AFFGEOID
temp$COUNTY = temp_over_county$COUNTYNAME
temp@data$id = temp@data$UTILITY
temp.points = fortify(temp,region='id')
temp_df = plyr::join(temp.points,temp@data,by='id')
wsc_df = temp_df
wsc_df$MASTER_TYPE = wsc_df$TYPE


look = grep('MUD|WCID|FWSD',system_df$PWS_NAME,value=T)
found = c(service_df$NAME,wsc_df$UTILITY,city_df$NAME,other_df$NAME)


tol15rainbow= c( "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",  "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")


library(forcats) 
other_df$MASTER_TYPE = fct_collapse(other_df$TYPE,
                                    MUD = c("MUD"),
                                    WCID = "WCID",
                                    FWSD = "FWSD",
                                    WSC = 'WSC',
                                    `Other SD` = c("DD","ID" , 'PUD', "LID"  ,"MMD" ,"ND", 'PID',  "OTH" , "RA"  , "RD" ,  "SUD"  ,"SWCD" , "WID") )
city_df$MASTER_TYPE = 'Municipal'         



service_df$MASTER_TYPE = fct_collapse(service_df$TYPE,
                                      MUD = c("MUD"),
                                      WCID = "WCID",
                                      WSC = 'WSC',
                                      FWSD = "FWSD",Private = "Private",
                                      Municipal = "Municipal",
                                      `Other SD` = c("DD","ID" , "UD","WA", "LID",'PID' ,'PUD' ,"MMD" ,"ND",   "OTH" , "RA"  , "RD" ,  "SUD"  ,"SWCD" , "WID") )          

# wells = readOGR('spatial_inputs/texas_puc','TCEQ_PWS_WELLS')
# wells = spTransform(wells,CRS(proj4string(hout_counties)))
# wells = wells[paste0('TX',wells$PWS_ID) %in% system_df$PWS_ID,]
# well_coords = as.data.frame(coordinates(wells))




ggplot()+
  geom_path(aes(x=long,y=lat,group=group),data=hout_counties.df)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Other SD')),col='grey50') +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=other_df) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=city_df) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Municipal')),col='grey50') +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =wsc_df) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50') +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50') +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),legend.position = c(0.9,0.2),legend.title=element_text(size=18),legend.text=element_text(size=18)) +
  scale_fill_colorblind(name = "Water provider") 



##### add audits #####
audit = do.call(rbind,lapply(paste0('Input/tceq_audits/',grep('_audit',list.files('Input/tceq_audits/'),value=T)),read_csv))


audit_background = lapply(paste0('Input/tceq_audits/',grep('system_info',list.files('Input/tceq_audits/'),value=T)),
                          function(x) read_csv(x) %>% dplyr::select(-contains("Extens")) %>% mutate(sys_bind = paste(gsub('Input\\/tceq_audits\\/','',gsub('_system_info\\.csv','',x)),1:nrow(.),sep='_')))

audit_background = Reduce(full_join,audit_background)

audit_background = audit_background %>% rename(SYSTEM_NAME = NAME,SYSTEM_ID = WDD_ID)
audit = left_join(audit,audit_background)

audit$FYEAR = year(mdy(audit$`FISCAL YEAR ENDED`))


audit  = audit %>% mutate(PWS_NAME = SYSTEM_NAME,
                          `CURRENT ASSESSED VALUATION` = as.numeric(gsub('\\,','',gsub('\\$','',`CURRENT ASSESSED VALUATION`))),
                          `GENERAL FUND - FUND BALANCE` = as.numeric(gsub('\\,','',gsub('\\)','',
                                                                                        gsub('\\$','',gsub('\\(','-',`GENERAL FUND - FUND BALANCE`))))),
                          `GENERAL FUND - ASSETS` = as.numeric(gsub('\\,','',gsub('\\$','',`GENERAL FUND - ASSETS`))),
                          `GENERAL FUND - TOTAL REVENUES` = as.numeric(gsub('\\,','',gsub('\\$','',`GENERAL FUND - TOTAL REVENUES`))),
                          `GENERAL FUND - TOTAL EXPENDITURES` = as.numeric(gsub('\\,','',gsub('\\$','',`GENERAL FUND - TOTAL EXPENDITURES`))),
                          `ENTERPRISE FUND - OPERATING EXPENSES` = as.numeric(gsub('\\,','',gsub('\\$','',`ENTERPRISE FUND - OPERATING EXPENSES`))),
                          `CURRENT ASSESSED VALUATION` = ifelse(`CURRENT ASSESSED VALUATION` == 0,NA,`CURRENT ASSESSED VALUATION`)
)
audit$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',audit$PWS_NAME)
audit$PWS_NAME = gsub('UTILITY DISTRICT','UD',audit$PWS_NAME)
audit$PWS_NAME = gsub('-',' ',audit$PWS_NAME)
audit$PWS_NAME = gsub(' $','',audit$PWS_NAME)
audit$PWS_NAME = gsub('THE WOODLANDS','WOODLANDS',audit$PWS_NAME)
#audit$PWS_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',audit$PWS_NAME)
audit$PWS_NAME = gsub('CLOVERCREEK','CLOVER CREEK',audit$PWS_NAME)
audit$PWS_NAME = gsub('POST WOOD','POSTWOOD',audit$PWS_NAME)
audit$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",audit$PWS_NAME)
audit$PWS_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",audit$PWS_NAME)
audit$PWS_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",audit$PWS_NAME)
audit$PWS_NAME = gsub('OF BRAZORIA COUNTY$','',audit$PWS_NAME)


audit$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",audit$PWS_NAME)
audit$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",audit$PWS_NAME)
audit$PWS_NAME[audit$PWS_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
audit$FYEAR_END_DDATE  = decimal_date(mdy(audit$`FISCAL YEAR ENDED`))
audit = audit[!duplicated(audit %>% dplyr::select(-DOC_ID,-DOC_LINK,-X1)),]
audit$audit_observed = TRUE

audit = do.call(rbind,list(
  audit %>% filter(!grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)) %>% 
    mutate(PWS_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 EDGEWOOD V',PWS_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)) %>%
    mutate(PWS_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 SHELDON RI',PWS_NAME))))
audit = do.call(rbind,list(
  audit %>% filter(grepl('WOODLANDS MUD 1',PWS_NAME)) %>% 
    #dplyr::select(FYEAR,PWS_NAME,FYEAR_END_DDATE,`CURRENT ASSESSED VALUATION`)  %>%
    arrange(FYEAR,-`CURRENT ASSESSED VALUATION`) %>%
    mutate(PWS_NAME = ifelse(duplicated(FYEAR),'WOODLANDS MUD 2','MONTGOMERY COUNTY MUD 40')),
  audit %>% filter(!grepl('WOODLANDS MUD 1',PWS_NAME))))

audit = do.call(rbind,lapply(unique(audit$PWS_NAME),function(uq)
  audit %>% filter(PWS_NAME== uq) %>% 
    mutate(cur_val = `CURRENT ASSESSED VALUATION`) %>% 
    arrange(FYEAR,-cur_val) %>%
    mutate(FYEAR_END_DDATE =  ifelse(duplicated(FYEAR_END_DDATE)&!((FYEAR_END_DDATE + 1) %in% FYEAR_END_DDATE),FYEAR_END_DDATE+1,FYEAR_END_DDATE),
           FYEAR =  ifelse(duplicated(FYEAR)&!((FYEAR + 1) %in% FYEAR),FYEAR+1,FYEAR)) %>%
    arrange(FYEAR,-cur_val) %>%
    mutate(FYEAR_END_DDATE =  ifelse(duplicated(FYEAR_END_DDATE)&!((FYEAR_END_DDATE - 1) %in% FYEAR_END_DDATE),FYEAR_END_DDATE-1,FYEAR_END_DDATE),
           FYEAR =  ifelse(duplicated(FYEAR)&!((FYEAR - 1) %in% FYEAR),FYEAR-1,FYEAR))))

audit = audit[!duplicated(paste0(audit$PWS_NAME,audit$FYEAR)),]


#grep('MUD|WCID|FWSD',system_df$PWS_NAME[!system_df$PWS_NAME %in% audit$PWS_NAME],value=T)

fyear_start = data.frame(expand.grid(FYEAR = 2004:2015,PWS_ID = system_df$PWS_ID))
analyze_df  = left_join(system_df,fyear_start)
analyze_df =  left_join(analyze_df,audit)

#### load violation data
viols_df = read_csv('Input/epa_sdwis/hous_msa_violation_report_q4_2015.csv',na=c("-")) %>% filter(`PWS ID` %in% analyze_df$PWS_ID) %>% 
  filter(!duplicated(.)) %>% 
  dplyr::select(-`EPA Region`,-`Primacy Agency`,-`Primacy Type`,-`Deactivation Date`,-`Activity Status`,-`Population Served Count`,-`Primary Source`,-`PWS Type`) %>%
  mutate(Period_Begin = dmy(`Compliance Period Begin Date`),Period_End = dmy(`Compliance Period End Date`),Period_Begin_ddate = decimal_date(Period_Begin),Begin_Month = month(Period_Begin),Begin_Year = year(Period_Begin)) %>% 
  dplyr::select(-`Compliance Period Begin Date`,-`Compliance Period End Date`) %>% filter(year(Period_Begin)>=2002 & year(Period_Begin)<=2014) %>%
  mutate(Period = ifelse(Begin_Month <=3,1,ifelse(Begin_Month >3 & Begin_Month <=6, 2,ifelse(Begin_Month > 6 & Begin_Month <=9,3,4)))) %>%
  mutate(Period_Factor = paste(Begin_Year,Period,sep='_')) %>% rename(PWS_ID = `PWS ID`,PWS_NAME = `PWS Name`)

viols_df$Points = NA
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`=='Nitrate') | (viols_df$`Violation Code` %in% c('21','43','44','41','13'))] = 10
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`!='Nitrate') | 
                  (viols_df$`Violation Code` %in% c('02','11','12','22','23','24','37','40','42','46','47','57','58','59','63','64','65'))|
                  (viols_df$`Violation Code`=='03' & viols_df$`Contaminant Name`=='Nitrate')] = 5
viols_df$Points[is.na(viols_df$Points)] = 1
viols_df$RTC_Date_Dec = decimal_date(dmy(viols_df$`RTC Date`))
viols_df$RTC_Date_Dec[is.na(viols_df$RTC_Date_Dec)] = 3000  



# 
# viol_points = do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
#   viols_df %>% 
#     filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) &
#              RTC_Date_Dec > (analyze_df$FYEAR_END_DDATE[i]-1))%>%
#     summarise(ett_viol_score = sum(Points,na.rm=T),
#               ett_longest_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate,na.rm = T)),
#               ett_health_viol_score = sum(ifelse(Points>1,Points,0),na.rm=T),
#               ett_longest_health_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate[Points>1],na.rm = T)),
#               ett_management_viol_score = sum(ifelse(Points==1,Points,0),na.rm=T),
#               ett_longest_management_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate[Points==1],na.rm = T))) %>%
#     mutate(PWS_ID = analyze_df$PWS_ID[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i],
#            FYEAR = analyze_df$FYEAR[i])))
# 
# 
# viol_points$ett_longest_viol_score[viol_points$ett_longest_viol_score<0] = 0
# viol_points$ett_score_p1 = viol_points$ett_viol_score + viol_points$ett_longest_viol_score
# viol_points$ett_longest_health_viol_score[viol_points$ett_longest_health_viol_score<0] = 0
# viol_points$ett_health_score_p1 = viol_points$ett_health_viol_score + viol_points$ett_longest_health_viol_score
# viol_points$ett_longest_management_viol_score[viol_points$ett_longest_management_viol_score<0] = 0
# viol_points$ett_management_score_p1 = viol_points$ett_management_viol_score + viol_points$ett_longest_management_viol_score

viol_p5count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-6)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p5_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p5_vcount,fill = 0) %>% 
  rename(hviol_count_p5 = Y,mviol_count_p5 = N)

viol_p4count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-5)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p4_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p4_vcount,fill = 0) %>% 
  rename(hviol_count_p4 = Y,mviol_count_p4 = N)

viol_p3count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-4)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p3_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p3_vcount,fill = 0) %>% 
  rename(hviol_count_p3 = Y,mviol_count_p3 = N)

viol_p2count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-3)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p2_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p2_vcount,fill = 0) %>% 
  rename(hviol_count_p2 = Y,mviol_count_p2 = N)

viol_p1count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-2)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p1_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p1_vcount,fill = 0) %>% 
  rename(hviol_count_p1 = Y,mviol_count_p1 = N)

viol_count = plyr::join_all(lapply(grep('viol_p[1-5]count',ls(),value=T),get),type='full')


#viol_data = full_join(viol_points,viol_p3count)
#analyze_df = left_join(analyze_df,viol_points)
analyze_df = left_join(analyze_df,viol_count)

analyze_df  = analyze_df %>%
  mutate(hviol_count_p1 = ifelse(is.na(hviol_count_p1),0,hviol_count_p1),
         mviol_count_p1 = ifelse(is.na(mviol_count_p1),0,mviol_count_p1),
         hviol_count_p2 = ifelse(is.na(hviol_count_p2),0,hviol_count_p2),
         mviol_count_p2 = ifelse(is.na(mviol_count_p2),0,mviol_count_p2),
         hviol_count_p3 = ifelse(is.na(hviol_count_p3),0,hviol_count_p3),
         mviol_count_p3 = ifelse(is.na(mviol_count_p3),0,mviol_count_p3),
         hviol_count_p4 = ifelse(is.na(hviol_count_p4),0,hviol_count_p4),
         mviol_count_p4 = ifelse(is.na(mviol_count_p4),0,mviol_count_p4),
         hviol_count_p5 = ifelse(is.na(hviol_count_p5),0,hviol_count_p5),
         mviol_count_p5 = ifelse(is.na(mviol_count_p5),0,mviol_count_p5))



### load acs data ###
tract_pops = do.call(rbind,
                     lapply(paste0('Input/census/ACS_tracts/',grep('B01003_with',list.files('Input/census/ACS_tracts/'),value=T)),function(x)
                       read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
                         dplyr::select(GEO.id,HD01_VD01,YEAR) %>%    
                         rename(AFFGEOID = GEO.id,TOTAL_POP = HD01_VD01)))
tract_pops = tract_pops %>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))),
                                   TOTAL_POP = as.numeric(TOTAL_POP))

tract_income = do.call(rbind,lapply(paste0('Input/census/ACS_tracts/',grep('S1903_with',list.files('Input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
    dplyr::select(GEO.id,HC02_EST_VC02,YEAR) %>% rename(AFFGEOID = GEO.id,
                                                        HOUSEHOLD_MED_INCOME = HC02_EST_VC02)))
tract_income = tract_income %>%
  mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))


tract_ed = lapply(paste0('Input/census/ACS_tracts/',grep('S1501_with',list.files('Input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
    mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))))
tract_ed[[1]] =  tract_ed[[1]]  %>% dplyr::select(GEO.id,HC01_EST_VC15,YEAR) %>%
  mutate(PERC_BACH = as.numeric(HC01_EST_VC15)) %>%
  dplyr::select(-HC01_EST_VC15) %>%
  rename(AFFGEOID = GEO.id)
tract_ed[-1] = lapply(tract_ed[-1],function(i) i %>% dplyr::select(GEO.id,HC01_EST_VC17,YEAR) %>%
                        mutate(PERC_BACH = as.numeric(HC01_EST_VC17)) %>%
                        dplyr::select(-HC01_EST_VC17) %>%
                        rename(AFFGEOID = GEO.id))
tract_ed = do.call(rbind,tract_ed)       

tract_poverty = do.call(rbind,
                        lapply(paste0('Input/census/ACS_tracts/',grep('S1702_with',list.files('Input/census/ACS_tracts/'),value=T)),function(x)
                          read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                            mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))) %>%
                            dplyr::select(GEO.id,HC02_EST_VC01,HC03_EST_VC01,YEAR) %>%
                            mutate(PERC_HOUSEHOLDS_POVERTY = as.numeric(ifelse(YEAR==2009,HC03_EST_VC01,HC02_EST_VC01))) %>%
                            dplyr::select(-HC02_EST_VC01,-HC03_EST_VC01) %>%
                            rename(AFFGEOID = GEO.id)))


tract_home = do.call(rbind,
                     lapply(paste0('Input/census/ACS_tracts/',grep('B25077_with',list.files('Input/census/ACS_tracts/'),value=T)),function(x)
                       read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                         dplyr::select(GEO.id,HD01_VD01,YEAR) %>%
                         rename(AFFGEOID = GEO.id,MED_HOME_VALUE = HD01_VD01)))
tract_home = tract_home  %>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))



tract_acs_data = plyr::join_all(list(tract_home,tract_ed,tract_poverty,tract_pops,tract_income),type='full')

tract_acs_data$MED_HOME_VALUE  = as.numeric(gsub('\\+','',gsub('\\,','',tract_acs_data$MED_HOME_VALUE)))
tract_acs_data$HOUSEHOLD_MED_INCOME = as.numeric(tract_acs_data$HOUSEHOLD_MED_INCOME)
tract_acs_data$PERC_HOUSEHOLDS_POVERTY = as.numeric(tract_acs_data$PERC_HOUSEHOLDS_POVERTY)
tract_acs_data$PERC_BACH = as.numeric(tract_acs_data$PERC_BACH)
tract_acs_data$TOTAL_POP = as.numeric(tract_acs_data$TOTAL_POP)
tract_acs_data$FYEAR = tract_acs_data$YEAR
tract_acs_data = tract_acs_data %>% arrange(AFFGEOID,FYEAR)

tract_year_combos = tract_acs_data %>% dplyr::select(-YEAR) %>% tidyr::expand(AFFGEOID,FYEAR = 2008:2015)
tract_acs_data = full_join(tract_acs_data %>% dplyr::select(-YEAR),tract_year_combos)

tract_acs_data = lapply(unique(tract_acs_data$AFFGEOID), function(i)
  tract_acs_data %>% arrange(AFFGEOID,FYEAR) %>% filter(AFFGEOID == i) %>% 
    fill(MED_HOME_VALUE:HOUSEHOLD_MED_INCOME,.direction = c( "up")) %>%
    fill(MED_HOME_VALUE:HOUSEHOLD_MED_INCOME,.direction = c( "down"))) %>% do.call(rbind,.)

tract_acs_data$POP_DENSITY_KMSQ = tract_acs_data$TOTAL_POP/
  (tx_tracts$ALAND[match(tract_acs_data$AFFGEOID,tx_tracts$AFFGEOID)]/1000000)


analyze_df$AFFGEOID = NA
analyze_df$AFFGEOID = service_df$AFFGEOID[match(analyze_df$PWS_NAME,service_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)] = other_df$AFFGEOID[match(analyze_df$PWS_NAME[is.na(analyze_df$AFFGEOID)],other_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)] = city_df$AFFGEOID[match(analyze_df$PWS_NAME[is.na(analyze_df$AFFGEOID)],city_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('SUNBELT',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('SUNBELT',service_df$NAME)][1]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('NORTHEAST HARRIS COUNTY MUD 1 ',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('NORTHEAST HARRIS COUNTY MUD 1$',service_df$NAME)][1]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('HARRIS MONTGOMERY COUNTIES MUD 386 MAY V',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('HARRIS MONTGOMERY COUNTIES MUD 386',service_df$NAME)][1]

for (i in 1:nrow(analyze_df))
{if (grepl('SUNBELT',analyze_df$PWS_NAME[i]))
{
  analyze_df$AFFGEOID[i] = 
    service1@data$AFFGEOID[match(analyze_df$PWS_NAME[i],service1@data$NAME)]
}}

houston_tracts.df = left_join(houston_tracts.df,tract_acs_data[tract_acs_data$FYEAR==2014,])

library(viridis)
gg_income = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                                 fill = HOUSEHOLD_MED_INCOME))+
  scale_fill_viridis(direction = -1,option = 'inferno',name='Median Income',alpha =0.5,breaks=c(50,100,150,200)*1000,labels=c('$50k','$100k','$150k','$200k')) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)


gg_home = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                               fill = MED_HOME_VALUE)) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name='Median home price',alpha =0.5,breaks=c(25,50,75)*10000,labels=c('$750k','$500k','$250k')) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)




gg_pov = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                              fill = PERC_HOUSEHOLDS_POVERTY  )) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name='% < poverty line',alpha =0.5,breaks=c(25,50,75)) + 
  #scale_color_viridis(direction = -1,option = 'inferno',name='% < poverty line',alpha =0.5)+
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)

gg_bach = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                               fill = PERC_BACH)) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name="% Bachelor's",alpha =0.5) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)

library(grid)
library(gridExtra)

gridExtra::grid.arrange(gg_income,gg_home,gg_pov,gg_bach)


library(ggmap)

# geo_query_df = data.frame('PWS_ID' = master_df$`PWS ID`,Address = paste(master_df$`Address Line1`,master_df$City,'TX',sep=' ')) %>% filter(!duplicated(.)) %>%
#   mutate(Address = as.character(Address))
# geo_query_df$Address[1:2]
# geolocs = geocode(geo_query_df$Address,output = 'more')
# geo_query_df = cbind(geo_query_df,geolocs)
# 
# master_df = left_join(master_df,geo_query_df)

plot_ett_df  = analyze_df %>% filter(PWS_NAME %in% analyze_set$PWS_NAME) %>% dplyr::select(PWS_NAME,FYEAR,hviol_count_p3) %>%
  tidyr::complete(PWS_NAME,FYEAR,fill=list(hviol_count_p3 = 0)) %>% filter(FYEAR>=2008)


# 
# ggplot() + geom_dotplot(aes(x=ett_score_p1,fill = ett_score_p1>10,colour=ett_score_p1>10),data=plot_ett_df,binwidth=1) + facet_wrap(~FYEAR) + 
#   theme_tufte(ticks=F) + 
#   theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text.y=element_blank(),axis.text.x=element_text(size = 18),
#         axis.title.y=element_blank(),axis.title.x=element_text(size=18),strip.text=element_text(size=18))+
#   xlab('Enforcement targeting tool score in prior fiscal year') + 
#   scale_colour_colorblind(labels= c('Below enforcement target threshold','Above enforcement target threshold'))+ 
#   scale_fill_colorblind(labels= c('Below enforcement target threshold','Above enforcement target threshold'))
# 


ggplot(data = plot_ett_df %>% group_by(FYEAR) %>% summarise(hsum = sum(hviol_count_p3))) + 
  geom_bar(aes(x=FYEAR,y=hsum),stat='identity') + 
  theme_tufte(ticks=F) + 
  theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text.y=element_text(size=18),axis.text.x=element_text(size = 18),
        axis.title=element_text(size=18),axis.title.x=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(expand=c(0,0),name='Fiscal Year',
                     breaks=seq(2008,2014,2),labels=seq(2008,2014,2)) + 
  ylab("Health violations observed in 3 prior fiscal years")


###DEBT

#debt_df = debt_df %>% filter(PWS_NAME %in% system_df$PWS_NAME)
##### load and rectify debt data
library(readxl)
debt_df = do.call(rbind,lapply(paste0('Input/fiscal_data/',grep('WD_debt',list.files('Input/fiscal_data/'),value=T)),function(x) read_excel(x) %>% 
                                 mutate('Year' = x)%>% filter(!is.na(Issuer)) %>% 
                                 mutate(Issuer = str_to_upper(Issuer)))) %>% 
  group_by(GovtID,Issuer,FYear) %>% 
  summarise(IssueSize = sum(IssueSize,na.rm=T),
            RefundingSize = sum(RefundingSize,na.rm=T),
            NewMoneySize = sum(NewMoneySize,na.rm=T))

fiscal_df = do.call(rbind,
                    lapply(paste0('Input/fiscal_data/',grep('WDTR_clean',list.files('Input/fiscal_data/'),value=T)),
                           function(x) read_excel(x) %>% mutate('Year' = x) %>% filter(!is.na(Issuer))%>%
                             mutate(Issuer = str_to_upper(Issuer))))


debt_df = left_join(fiscal_df,debt_df)
debt_df$Issuer = str_to_upper(debt_df$Issuer)
debt_df$Issuer = gsub(' 0{1,}',' ',debt_df$Issuer)
debt_df$Issuer = gsub('POST WOOD','POSTWOOD',debt_df$Issuer)
debt_df$Issuer = gsub('CLOVERCREEK','CLOVER CREEK',debt_df$Issuer)
debt_df$Issuer = gsub('MEADOW CREEK','MEADOWCREEK',debt_df$Issuer)
debt_df$Issuer = gsub('SPECIAL UD','SUD',debt_df$Issuer)
debt_df$Issuer = gsub('THUNDERBIRD UD$','THUNDERBIRD UD 1',debt_df$Issuer)
debt_df$Issuer = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",debt_df$Issuer)
debt_df$Issuer = gsub('REGIONAL WA','RWA',debt_df$Issuer)
debt_df$Issuer = gsub('-',' ',debt_df$Issuer)
debt_df$Issuer = gsub("134 C",'134C',debt_df$Issuer)
debt_df$Issuer = gsub("134 D",'134D',debt_df$Issuer)
debt_df$Issuer = gsub("134 A",'134A',debt_df$Issuer)
debt_df$Issuer = gsub("5\\\v",'5',debt_df$Issuer)
debt_df$Issuer = gsub("MUD THE$|MUD\\, THE$",'MUD',debt_df$Issuer)
debt_df$Issuer = gsub("OAK MANOR UD",'OAK MANOR MUD',debt_df$Issuer)
debt_df$Issuer = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",debt_df$Issuer)
debt_df$Issuer = gsub("'",'',debt_df$Issuer)
debt_df = debt_df %>% rename(PWS_NAME = Issuer,FYEAR = FYear)


analyze_df =  left_join(analyze_df,debt_df)

analyze_df$NEW_DEBT = analyze_df$NewMoneySize>0
analyze_df$NEW_DEBT[is.na(analyze_df$NEW_DEBT)] = FALSE
analyze_df$DEBT_MATCH = FALSE
analyze_df$DEBT_MATCH[analyze_df$PWS_NAME %in% debt_df$PWS_NAME] = TRUE

master_df = analyze_df %>% filter(grepl('MUD|FWSD|WCID',PWS_NAME) & !is.na(FYEAR) & FYEAR>=2007) %>% left_join(.,tract_acs_data)

master_df = master_df %>% filter(PWS_NAME != 'CITY OF HOUSTON HARRIS COUNTY MUD 159')

master_df$NewMoneySize[is.na(master_df$NewMoneySize)] = 0
master_df$TotDebtServiceOutstanding[is.na(master_df$TotDebtServiceOutstanding)] = 0

master_df = master_df[!(master_df$`Is Wholesaler`=='Y'&master_df$`Population Served Count`==0),]


master_df = do.call(rbind,lapply(unique(master_df$PWS_NAME),function(uq)
  master_df %>% filter(PWS_NAME == uq) %>%
    arrange(FYEAR) %>% 
    fill(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,`CURRENT ASSESSED VALUATION`,`GENERAL FUND - TOTAL REVENUES`,`GENERAL FUND - FUND BALANCE`,.direction=c('up'))%>%
    fill(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,`CURRENT ASSESSED VALUATION`,`GENERAL FUND - TOTAL REVENUES`,`GENERAL FUND - FUND BALANCE`,.direction=c('down'))
))

master_df = master_df %>% 
  mutate(TotDebtServiceOutstanding_1m = TotDebtServiceOutstanding / 1000000,
         NewMoneySize_1m = NewMoneySize / 1000000,
         Assessed_Value_100m = `CURRENT ASSESSED VALUATION` / 100000000,
         Expenditures_1m = `GENERAL FUND - TOTAL EXPENDITURES` / 1000000,
         Revenues_1m = `GENERAL FUND - TOTAL REVENUES` / 1000000,
         FundBalance_1m = `GENERAL FUND - FUND BALANCE` / 1000000,
         Family_Units_100 = `WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,
         Pop_Served_1k = `Population Served Count`/1000)

master_df$Revenues_1m_p1 = master_df$Revenues_1m[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]
master_df$FundBalance_1m_p1 = master_df$FundBalance_1m[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]

master_df$SINGLE_FAMILY_UNITS_p1 = master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]

master_df$SERVICE_POP_CHANGE_P1 = 100*((master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS` - master_df$SINGLE_FAMILY_UNITS_p1) / master_df$SINGLE_FAMILY_UNITS_p1)

master_df$SERVICE_POP_CHANGE_P1[master_df$SERVICE_POP_CHANGE_P1==-Inf] = 0
master_df$SERVICE_POP_CHANGE_P1[master_df$SERVICE_POP_CHANGE_P1==Inf] = 0


master_df = master_df %>% filter(is.na(`Deactivation Date`)|(decimal_date(dmy(master_df$`Deactivation Date`))>FYEAR_END_DDATE)) 

#master_df = master_df %>% filter(!is.na(audit_observed)) 
uq_ids = data.frame(PWS_ID = unique(master_df$PWS_ID),
                    uq_sysid = 1:length(unique(master_df$PWS_ID)))

master_df = left_join(master_df,uq_ids)

master_df$GROUNDWATER = ifelse(grepl('Ground',master_df$`Primary Source`),1,0)
master_df$PURCHASED = ifelse(grepl('purch',master_df$`Primary Source`),1,0)
master_df$WHOLESALER = ifelse(master_df$`Is Wholesaler`=='Y',1,0)


master_df = master_df %>% filter(FYEAR>=2008)

year_tract_medians = master_df %>% dplyr::select(FYEAR,MED_HOME_VALUE,PERC_BACH,PERC_HOUSEHOLDS_POVERTY,
                                                 HOUSEHOLD_MED_INCOME) %>% group_by(FYEAR) %>%
  summarise_all( median,na.rm=T)

master_df$MED_HOME_VALUE[is.na(master_df$MED_HOME_VALUE)] = year_tract_medians$MED_HOME_VALUE[match(master_df$FYEAR[is.na(master_df$MED_HOME_VALUE)],year_tract_medians$FYEAR)]
master_df$PERC_BACH[is.na(master_df$PERC_BACH)] = year_tract_medians$PERC_BACH[match(master_df$FYEAR[is.na(master_df$PERC_BACH)],year_tract_medians$FYEAR)]
master_df$PERC_HOUSEHOLDS_POVERTY[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)] = year_tract_medians$PERC_HOUSEHOLDS_POVERTY[match(master_df$FYEAR[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)],year_tract_medians$FYEAR)]
master_df$HOUSEHOLD_MED_INCOME[is.na(master_df$HOUSEHOLD_MED_INCOME)] = year_tract_medians$HOUSEHOLD_MED_INCOME[match(master_df$FYEAR[is.na(master_df$HOUSEHOLD_MED_INCOME)],year_tract_medians$FYEAR)]


library(INLA)

## plot DV
ggplot(master_df %>% filter(NewMoneySize_1m>0)) + geom_histogram(aes(x=NewMoneySize_1m),binwidth=1) + facet_wrap(~FYEAR,scales = 'free_y') +
  theme_tufte(ticks=F)+
  theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text=element_text(size = 18),
        axis.title=element_text(size=18),strip.text=element_text(size=18)) +
  scale_x_continuous('$ millions of new debt issued') + scale_y_continuous('# issuing systems') +
  annotate('text',x=35,y=10,label=paste0(100 * round(tapply(master_df$NewMoneySize_1m>0,master_df$FYEAR,mean),2),'% issuing'),size=5)

master_df$IS_FWSD = ifelse(grepl('FWSD',master_df$PWS_NAME),1,0)

master_df$IS_WCID = ifelse(grepl('WCID',master_df$PWS_NAME),1,0)

#gCentroid(service1,byid = T)



#### MODEL SETUP ###

n = nrow(master_df)
u <- (master_df$NewMoneySize_1m>0) + 0
y <- ifelse(master_df$NewMoneySize_1m>0,master_df$NewMoneySize_1m,NA)

idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)

idat$u_regionid = c(master_df$uq_sysid,master_df$uq_sysid)
idat$y_regionid = c(master_df$uq_sysid,master_df$uq_sysid)

#idat$u_ett_health_score <- c(ifelse(master_df$ett_health_score_p1>10,1,0), rep(0,n))
#idat$y_ett_health_score <- c(rep(0,n), ifelse(master_df$ett_health_score_p1>10,1,0))
#idat$u_ett_management_score <- c(ifelse(master_df$ett_management_score_p1>10,1,0), rep(0,n))
#idat$y_ett_management_score <- c(rep(0,n), ifelse(master_df$ett_management_score_p1>10,1,0))
#idat$u_ett_score <- c(ifelse(master_df$ett_score_p1>10,1,0), rep(0,n))
#idat$y_ett_score <- c(rep(0,n), ifelse(master_df$ett_score_p1>10,1,0))

idat$u_hviols_p5 <- c(ifelse(master_df$hviol_count_p5>0,1,0), rep(0,n))
idat$y_hviols_p5 <- c(rep(0,n),ifelse(master_df$hviol_count_p5>0,1,0))
idat$u_mviols_p5 <- c(ifelse(master_df$mviol_count_p5>0,1,0), rep(0,n))
idat$y_mviols_p5 <- c(rep(0,n), ifelse(master_df$mviol_count_p5>0,1,0))

idat$u_hviols_p4 <- c(ifelse(master_df$hviol_count_p4>0,1,0), rep(0,n))
idat$y_hviols_p4 <- c(rep(0,n),ifelse(master_df$hviol_count_p4>0,1,0))
idat$u_mviols_p4 <- c(ifelse(master_df$mviol_count_p4>0,1,0), rep(0,n))
idat$y_mviols_p4 <- c(rep(0,n), ifelse(master_df$mviol_count_p4>0,1,0))

idat$u_hviols_p3 <- c(ifelse(master_df$hviol_count_p3>0,1,0), rep(0,n))
idat$y_hviols_p3 <- c(rep(0,n),ifelse(master_df$hviol_count_p3>0,1,0))
idat$u_mviols_p3 <- c(ifelse(master_df$mviol_count_p3>0,1,0), rep(0,n))
idat$y_mviols_p3 <- c(rep(0,n), ifelse(master_df$mviol_count_p3>0,1,0))

idat$u_hviols_p2 <- c(ifelse(master_df$hviol_count_p2>0,1,0), rep(0,n))
idat$y_hviols_p2 <- c(rep(0,n),ifelse(master_df$hviol_count_p2>0,1,0))
idat$u_mviols_p2 <- c(ifelse(master_df$mviol_count_p2>0,1,0), rep(0,n))
idat$y_mviols_p2 <- c(rep(0,n), ifelse(master_df$mviol_count_p2>0,1,0))

idat$u_hviols_p1 <- c(ifelse(master_df$hviol_count_p1>0,1,0), rep(0,n))
idat$y_hviols_p1 <- c(rep(0,n),ifelse(master_df$hviol_count_p1>0,1,0))
idat$u_mviols_p1 <- c(ifelse(master_df$mviol_count_p1>0,1,0), rep(0,n))
idat$y_mviols_p1 <- c(rep(0,n), ifelse(master_df$mviol_count_p1>0,1,0))

idat$u_fyear <- c(master_df$FYEAR, rep(0,n))
idat$y_fyear <- c(rep(0,n), master_df$FYEAR)
idat$u_fyear2 <- c(master_df$FYEAR, rep(0,n))
idat$y_fyear2 <- c(rep(0,n), master_df$FYEAR)
idat$u_debt_outstanding_1m <- c(scale(master_df$TotDebtServiceOutstanding_1m,center = T,scale=F), rep(0,n))
idat$y_debt_outstanding_1m <- c(rep(0,n), scale(master_df$TotDebtServiceOutstanding_1m,center = T,scale=F))



### To Does
# need population density
# need sevice population change
#system variables 
idat$u_service_pop_1k <- c(scale(master_df$Pop_Served_1k,center = T,scale=F), rep(0,n))
idat$y_service_pop_1k <- c(rep(0,n), scale(master_df$Pop_Served_1k,center = T,scale=F))
idat$u_num_facilities <- c(scale(master_df$`# of Facilities`,center=F,scale=F), rep(0,n))
idat$y_num_facilities <- c(rep(0,n), scale(master_df$`# of Facilities`,center=F,scale=F))
idat$u_system_age <-c(ifelse(master_df$FYEAR - master_df$Year_Start <0, 0,master_df$FYEAR - master_df$Year_Start), rep(0,n))
idat$y_system_age <-c(rep(0,n),ifelse(master_df$FYEAR - master_df$Year_Start <0, 0,master_df$FYEAR - master_df$Year_Start))
idat$u_assessed_value_100m <- c(scale(master_df$Assessed_Value_100m,center=F,scale=F), rep(0,n))
idat$y_assessed_value_100m <- c(rep(0,n), scale(master_df$Assessed_Value_100m,center=F,scale=F))
idat$u_total_revenue_1m <- c(scale(master_df$Revenues_1m_p1,center=F,scale=F), rep(0,n))
idat$y_total_revenue_1m <- c(rep(0,n), scale(master_df$Revenues_1m_p1,center=F,scale=F))
idat$u_total_revenue_1m <- c(scale(master_df$Revenues_1m_p1,center=F,scale=F), rep(0,n))
idat$y_fund_balance_1m_p1 <- c(rep(0,n), scale(master_df$FundBalance_1m_p1,center=F,scale=F))
idat$u_fund_balance_1m_p1 <- c( scale(master_df$FundBalance_1m_p1,center=F,scale=F), rep(0,n))



idat$u_expenditures_10m <- c(scale(master_df$Expenditures_1m,center=F,scale=F), rep(0,n))
idat$y_expenditures_10m <- c(rep(0,n),scale(master_df$Expenditures_1m,center=F,scale=F))
idat$u_service_change_perc <- c(master_df$SERVICE_POP_CHANGE_P1, rep(0,n))
idat$y_service_change_perc <- c(rep(0,n),master_df$SERVICE_POP_CHANGE_P1)
idat$u_primary_ground <- c(master_df$GROUNDWATER, rep(0,n))
idat$y_primary_ground <- c(rep(0,n),master_df$GROUNDWATER)
idat$u_primary_purch <- c(master_df$PURCHASED, rep(0,n))
idat$y_primary_purch <- c(rep(0,n),master_df$PURCHASED)
idat$u_wholesaler <- c(master_df$WHOLESALER, rep(0,n))
idat$y_wholesaler <- c(rep(0,n),master_df$WHOLESALER)
idat$u_fwsd <- c(master_df$IS_FWSD, rep(0,n))
idat$y_fwsd <- c(rep(0,n),master_df$IS_FWSD)
idat$u_wcid <- c(master_df$IS_WCID, rep(0,n))
idat$y_wcid <- c(rep(0,n),master_df$IS_WCID)
idat$u_PWS_ID <- c(as.character(master_df$PWS_ID), rep(NA,n))
idat$y_PWS_ID <- c(rep(NA,n), as.character(master_df$PWS_ID))

center_continuous_cov = TRUE
### census variables
idat$u_med_income_10k <- c(scale(master_df$HOUSEHOLD_MED_INCOME/10000,center=center_continuous_cov,scale=F),rep(0,n))
idat$y_med_income_10k <- c(rep(0,n), scale(master_df$HOUSEHOLD_MED_INCOME/10000,center=center_continuous_cov,scale=F))
idat$u_med_home_10k <- c(scale(master_df$MED_HOME_VALUE/10000,center=center_continuous_cov,scale=F),rep(0,n))
idat$y_med_home_10k <- c(rep(0,n), scale(master_df$MED_HOME_VALUE/10000,center=center_continuous_cov,scale=F))
idat$u_perc_bach <- c(scale(master_df$PERC_BACH,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_perc_bach <- c(rep(0,n), scale(master_df$PERC_BACH,center=center_continuous_cov,scale=F))
idat$u_perc_pov <- c(scale(master_df$PERC_HOUSEHOLDS_POVERTY,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_perc_pov <- c(rep(0,n),scale(master_df$PERC_HOUSEHOLDS_POVERTY,center=center_continuous_cov,scale=F))
idat$u_pop100_dens_sqkm <- c(scale(master_df$POP_DENSITY_KMSQ/100,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_pop100_dens_sqkm <- c(rep(0,n),scale(master_df$POP_DENSITY_KMSQ/100,center=center_continuous_cov,scale=F))
idat$y.i <- idat$u.i <- c(1:n, 1:n)


save.image('bypass_run.RData')


#########
### To Does
# need sevice population change

# ggplot() + geom_point(aes(y=master_df$NewMoneySize_1m[master_df$NewMoneySize_1m>0], 
# x=master_df$ett_score_p1[master_df$NewMoneySize_1m>0]),pch=1) + 
#   scale_y_continuous(name = 'New debt issued ($ millions) for issue > $0') +
#   scale_x_continuous(name = 'Enforcement Targeting Tool Score') +
#   theme_tufte(ticks=F) + theme(axis.text = element_text(size=16),
#                                axis.title = element_text(size=18))
# 
# ggplot() + geom_boxplot(aes(y=master_df$NewMoneySize_1m[master_df$NewMoneySize_1m>0], 
#                           x=as.factor(master_df$hviol_count_p3[master_df$NewMoneySize_1m>0])),pch=1) + 
#   scale_y_continuous(name = 'New debt issued ($ millions) for issue > $0') +
#   scale_x_discrete(name = 'Health violations in past 3 years') +
#   theme_tufte(ticks=F) + theme(axis.text = element_text(size=16),
#                                axis.title = element_text(size=18))

base_form = "Y ~ 0 + mu.u + mu.y +  u_perc_pov + y_perc_pov + 
  u_med_home_10k  + y_med_home_10k+
  u_fund_balance_1m_p1 +   y_fund_balance_1m_p1 + 
  u_total_revenue_1m + y_total_revenue_1m +
  u_service_pop_1k + y_service_pop_1k + 
  u_num_facilities + y_num_facilities + 
  u_system_age + y_system_age + 
  u_debt_outstanding_1m  + y_debt_outstanding_1m +
  u_primary_ground+u_primary_purch +
  y_primary_ground+y_primary_purch + 
  u_fwsd + y_fwsd + 
  u_wcid + y_wcid"

random_form = "f(u_fyear, model='iid') +
  f(y_fyear,model='iid')+
  f(u_fyear2, model='rw1') +
  f(y_fyear2,copy='u_fyear', fixed=FALSE)+
  f(u_PWS_ID, model='iid') +
  f(y_PWS_ID,model='iid')"


test = as.formula(gsub('pp','p3',paste(base_form,
"u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp*u_perc_pov*u_med_income_10k +  y_hviols_pp*y_perc_pov*y_med_income_10k",
random_form,sep='+')))

test_mod = inla(test,c('binomial', 'gamma'),
     data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
     control.predict=list(compute=TRUE),verbose=F)



capacity_interaction_template = c(
"u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_total_revenue_1m +  y_hviols_pp:y_total_revenue_1m +" ,
"u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_fund_balance_1m_p1 +  y_hviols_pp:y_fund_balance_1m_p1 +",
"u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_med_income_10k +  y_hviols_pp:y_med_income_10k +" ,
"u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_perc_pov +  y_hviols_pp:y_perc_pov")
  

capacity_interactions = unlist(lapply(as.list(paste0('p',1:5)),function(x) gsub('pp',x,capacity_interaction_template)))

form_temps = expand.grid(gsub('\\\n','',base_form),capacity_interactions,gsub('\\\n','',random_form))

form_list = lapply(1:nrow(form_temps),function(x) paste(form_temps[x,1],form_temps[x,2],form_temps[x,3],sep='+'))


# result_ett_gauss_2way <- inla(form_ett_2way, c('binomial', 'gaussian'),
#                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                control.predict=list(compute=TRUE),verbose=F)
# 
# result_hviols_gauss_2way <- inla(form_hviols_2way, c('binomial', 'gaussian'),
#                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                      control.predict=list(compute=TRUE),verbose=F)

all_results = lapply(form_list,function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                     data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                     control.predict=list(compute=TRUE),verbose=F))

names(all_results) = paste(str_extract(gsub('.*:','',form_temps[,2]),'revenue|balance|income|pov'),str_extract(form_temps[,2],'viols_p[0-5]'),sep='_')


coef_results = do.call(rbind,lapply(1:length(all_results),function(x) all_results[[x]]$summary.fix[,c(1,3,5)] %>% 
            mutate(COEF = rownames(.),MODEL = names(all_results)[x],
                   LIK = ifelse(grepl('^y_|\\.y$',COEF),'1.Gamma','2.Binomial')) %>%
         mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% 
           mutate(COEF = gsub('_p[1-5]$','',COEF)) %>%
           mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)))) %>%
  mutate(SIG_FILL = paste(SIG,LIK))

coef_results$COEF = fct_recode(f = coef_results$COEF,
                               `Management viol.` = 'mviols', 
                               `Health viol.` = 'hviols', 
                              `FWSD` = 'fwsd', 
                              `WCID` = 'wcid', 
                              `Fund balance ($1M)` = 'fund_balance_1m',
                              `Revenue ($1M)` = 'total_revenue_1m',
                              `Revenue ($1M):Health viol.` = 'total_revenue_1m:hviols',
                              `Fund balance ($1M):Health viol.` = 'fund_balance_1m_p1:hviols',
                              `Med. income ($10k):Health viol.` = 'med_income_10k:hviols',
                              `% Poverty:Health viol.` = 'perc_pov:hviols',
                              # `Service pop.:ETT score>10` = 'service_pop_1k:ett_score',
                              #`Service pop.:Health viol.` = 'service_pop_1k:hviols_p3',
                              #   Wholesaler = 'wholesaler',
                              `System age (y)` = 'system_age',`Service pop. (1k)` = 'service_pop_1k',
                              Purchaser = 'primary_purch',
                              Groundwater = 'primary_ground',
                              `% Poverty` = 'perc_pov',
                             # `% Bachelor's` = 'perc_bach',
                              `# facilities` = 'num_facilities',
                              `Med. income ($10k)` = 'med_income_10k',`Outstanding debt ($1M)` = 'debt_outstanding_1m')

coef_results$COEF = factor(coef_results$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty",#"% Bachelor's",
                                                      "Med. income ($10k)",
                                                      '# facilities','System age (y)','Groundwater','Purchaser',
                                                      # 'Purchaser:Groundwater',
                                                      #  'Wholesaler',
                                                      'FWSD','WCID',
                                                      "Service pop. (1k)",
                                                      'Outstanding debt ($1M)',
                                                      'Fund balance ($1M)',
                                                      #'Tax base ($100M)',
                                                      'Revenue ($1M)',
                                                      'Management viol.','Health viol.',
                                                      'Revenue ($1M):Health viol.',"Fund balance ($1M):Health viol.",
                                                      "Med. income ($10k):Health viol.","% Poverty:Health viol."))


coef_results$COEF = fct_rev(coef_results$COEF)

p3_results = coef_results %>% filter(!grepl('^mu',COEF),grepl('p3',MODEL))
p3_results$MODEL = as.factor(p3_results$MODEL) 
levels(p3_results$MODEL) = c('Model 2','Model 3','Model 4','Model 1')
p3_results$MODEL = factor(p3_results$MODEL,levels = c('Model 1','Model 2','Model 3','Model 4'))


gg_gamma = ggplot(p3_results) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=2) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=.5) + 
  coord_flip() + facet_grid(~MODEL) +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.8,0.15)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black')) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) +guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))

gg_gamma
gg_gamma_2 = ggplot(p3_results %>% filter(grepl('Rev|balance|income|Poverty|Health',COEF))) +
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=3) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=1) + 
  coord_flip() + facet_grid(~MODEL) +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.8,0.25)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black'),show) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) + 
  guides(fill='none',colour = guide_legend(override.aes = list(shape = 19))) 
gg_gamma

cor(master_df$PERC_HOUSEHOLDS_POVERTY,
    master_df$HOUSEHOLD_MED_INCOME,use = 'pairwise.complete.obs')

result_ett_gauss_2way <- inla(form_ett_2way, c('binomial', 'gaussian'),
                              data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                              control.predict=list(compute=TRUE),verbose=F)

result_hviols_gauss_2way <- inla(form_hviols_2way, c('binomial', 'gaussian'),
                                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                 control.predict=list(compute=TRUE),verbose=F)



coefs_gauss = rbind(
  result_ett_gauss_2way$summary.fix[,c(1,3,5)] %>% mutate(COEF = rownames(.),MODEL = 'ETT score > 10',LIK = ifelse(grepl('^y_',COEF),'1.Gauss','2.Binomial')) %>%
    mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)),
  result_hviols_gauss_2way$summary.fix[,c(1,3,5)] %>% mutate(COEF = rownames(.),MODEL = 'Health viol. in past 3 yrs',LIK = ifelse(grepl('^y_',COEF),'1.Gauss','2.Binomial')) %>%
    mutate(COEF = gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))) %>%
  mutate(SIG_FILL = paste(SIG,LIK),COEF = as.factor(COEF))


coefs_gauss$COEF = fct_recode(f = coefs_gauss$COEF,
                              `Management viol. in past 3 yrs` = 'mviols_p3', 
                              `Health viol. in past 3 yrs` = 'hviols_p3', 
                              `FWSD` = 'fwsd', 
                              `WCID` = 'wcid', 
                              `Fund balance ($100k)` = 'fund_balance_100K_p1',
                              `Revenue ($1M)` = 'total_revenue_1m',
                              `Revenue ($1M):ETT score>10` = 'total_revenue_1m:ett_score',
                              `Revenue ($1M):Health viol.` = 'total_revenue_1m:hviols_p3',
                              # `Service pop.:ETT score>10` = 'service_pop_1k:ett_score',
                              #`Service pop.:Health viol.` = 'service_pop_1k:hviols_p3',
                              #   Wholesaler = 'wholesaler',
                              `System age (y)` = 'system_age',`Service pop. (1k)` = 'service_pop_1k',
                              Purchaser = 'primary_purch',
                              Groundwater = 'primary_ground',
                              # `Purchaser:Groundwater` = 'primary_ground:primary_purch',
                              `% Poverty` = 'perc_pov',`% Bachelor's` = 'perc_bach',`# facilities` = 'num_facilities',
                              #`Tax base ($100M)` = 'assessed_value_100m',
                              `Med. income ($10k)` = 'med_income_10k',
                              #`Med. income ($10k)` = 'med_income_10k',
                              `ETT score>10` = 'ett_score',`Outstanding debt ($1M)` = 'debt_outstanding_1m')

coefs_gauss$COEF = factor(coefs_gauss$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty","% Bachelor's",
                                                      "Med. income ($10k)",
                                                      '# facilities','System age (y)','Groundwater','Purchaser',
                                                      # 'Purchaser:Groundwater',
                                                      #  'Wholesaler',
                                                      'FWSD','WCID',
                                                      "Service pop. (1k)",
                                                      'Outstanding debt ($1M)',
                                                      'Fund balance ($100k)',
                                                      #'Tax base ($100M)',
                                                      'Revenue ($1M)',
                                                      'Management viol. in past 3 yrs','Health viol. in past 3 yrs',
                                                      'Revenue ($1M):Health viol.',
                                                      'ETT score>10',
                                                      "Revenue ($1M):ETT score>10"))


coefs_gauss$COEF = fct_rev(coefs_gauss$COEF)


gg_gauss = ggplot(coefs_gauss %>% filter(!grepl('^mu',COEF))) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=3) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=1) + 
  coord_flip() + facet_grid(~MODEL) +
  theme_tufte(ticks=F) + theme(legend.position = c(0.6,0.1)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black'),show) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gaussian','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.position = c(0.5,0.1),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) + 
  guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))


