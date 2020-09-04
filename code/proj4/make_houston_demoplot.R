library(spdep)
library(maptools)
library(dplyr)
library(tidyverse)
library(lubridate)
library(sp)
library(rgeos)
library(rgdal)
library(stringr)
library(networktools)

system_df = read_csv('input/epa_sdwis/system_base_master.csv')

houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]
library(ggthemes)
hout_counties@data$id = hout_counties@data$COUNTYNAME
hout_counties.points = fortify(hout_counties, region="id")
hout_counties.df = plyr::join(hout_counties.points, hout_counties@data, by="id")

# tx_places <- readOGR('spatial_inputs/government_units','cb_2015_48_place_500k')
# tx_places@data$id = hout_places@data$NAME
# hout_places <- tx_places[hout_counties,]
# hout_places.points = fortify(hout_places, region="id")
# hout_places.df = plyr::join(hout_places.points, hout_places@data, by="id") 
# hout_places.df$PWS_NAME = hout_places.df$NAME
# hout_places.df$OBS = FALSE
# hout_places.df$OBS[hout_places.df$PWS_NAME %in% system_df$PWS_NAME] = TRUE
library(ggthemes)

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
twd_geo$NAME = gsub("CYPRESS KLEIN UTILTIY DISTRICT","CYPRESS KLEIN MUD",twd_geo$NAME)
twd_geo$NAME = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",twd_geo$NAME)
twd_geo$NAME[twd_geo$NAME=="THUNDERBIRD UD 1"] = "THUNDERBIRD UD"
twd_geo = spTransform(twd_geo,CRS(proj4string(tx_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)



houston_msa = readOGR('spatial_inputs/hg_gis','houston_msa_full')
houston_msa@data$id = rownames(houston_msa@data)
houston_msa = spTransform(houston_msa,CRS(proj4string(twd_geo)))
houston_msa_df = left_join(fortify(houston_msa),houston_msa@data)

tx_places = readOGR('spatial_inputs/government_units/','cb_2015_48_place_500k')
tx_places = spTransform(tx_places,CRS(proj4string(tx_counties)))
in_hout = over(tx_places,hout_counties,returnList = FALSE)
hout_places = tx_places[!is.na(in_hout$FIPS_I),]
hout_places$NAME = paste0('CITY OF ',str_to_upper(hout_places$NAME))
hout_places$NAME = gsub("'","",hout_places$NAME)
hout_places$NAME = gsub("CITY OF MISSOURI CITY","CITY OF MISSOURI CITY MUSTANG BAYOU WATE",hout_places$NAME)

tx_utils = readOGR('spatial_inputs/government_units/PUC_CCN_WATER/','PUC_CCN_WATER')
tx_utils = spTransform(tx_utils,CRS(proj4string(tx_counties)))
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

houston_wd_geo = twd_geo[twd_geo@data$COUNTY %in% houston_counties,]



houston_wd_geo = twd_geo[twd_geo@data$COUNTY %in% houston_counties,]
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

service1_over_county = over(service1,hout_counties)
service1$COUNTY = service1_over_county$COUNTYNAME
service1@data$COMMON_NAM = gsub('#','',service1@data$COMMON_NAM)
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

temp@data$id = temp@data$OBJECTID
temp.points = fortify(temp,region='id')
temp_df = plyr::join(temp.points,temp@data,by='id')
other_df = temp_df

temp = hout_places[hout_places@data$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% (c(unique(service_df$NAME),unique(other_df$NAME)))],]
temp@data$TYPE = 'Municipal'
temp = raster::crop(temp,hout_counties)

temp_over_county = over(temp,hout_counties)

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
temp_over_county = over(temp,hout_counties)
temp@data$TYPE = ifelse(system_df$`Owner Type`[match(temp$UTILITY,system_df$PWS_NAME)]=='Local government','WSC',"Private")

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
                                    FWSD = "FWSD",
                                    SUD = 'SUD',
                                    WCID = "WCID",
                                    WSC = 'WSC',
                                    `Other SD` = c("DD","ID" , 'PUD', "LID"  ,"MMD" ,"ND", 'PID',  "OTH" , "RA"  , "RD"  ,"SWCD" , "WID") )
city_df$MASTER_TYPE = 'Municipal'         



service_df$MASTER_TYPE = fct_collapse(service_df$TYPE,
                                      MUD = c("MUD"),
                                      WCID = "WCID",
                                      WSC = 'WSC',
                                      SUD = "SUD",
                                      FWSD = "FWSD",Private = "Private",
                                      Municipal = "Municipal",
                                      `Other SD` = c("DD","ID" , "UD","WA", "LID",'PID' ,'PUD' ,"MMD" ,"ND",   "OTH" , "RA"  , "RD"   ,"SWCD" , "WID") )          

# wells = readOGR('spatial_inputs/texas_puc','TCEQ_PWS_WELLS')
# wells = spTransform(wells,CRS(proj4string(hout_counties)))
# wells = wells[paste0('TX',wells$PWS_ID) %in% system_df$PWS_ID,]
# well_coords = as.data.frame(coordinates(wells))

# twd_geo@data = twd_geo@data %>% dplyr::select(OBJECTID,NAME,COUNTY,id)
# service1@data = service1@data %>% dplyr::select(OBJECTID,NAME,COUNTY,id)
# service1 = spChFIDs(service1,x = paste0('s',1:length(service1)))
# twd_geo = spChFIDs(twd_geo,x = paste0('t',1:length(twd_geo)))
# combined = spRbind(twd_geo,service1[!service1@data$NAME %in% twd_geo@data$NAME,])
# 
# combined = combined[combined@data$NAME %in% 
#                      system_df$PWS_NAME[system_df$system_type %in% c('WCID','FWSD','MUD')],]
#writeOGR(combined,dsn='input/scratch/',layer='district_shapes',driver="ESRI Shapefile")

service_df$MASTER_TYPE <- fct_relevel(service_df$MASTER_TYPE,
                                      "FWSD","MUD","SUD","WCID", "Other SD","Municipal","Private","WSC" )
other_df$MASTER_TYPE <- fct_relevel(other_df$MASTER_TYPE,
                                    "FWSD","MUD","SUD","WCID", "Other SD","Municipal","Private","WSC" )
city_df$MASTER_TYPE <- fct_relevel(city_df$MASTER_TYPE,
                                   "FWSD","MUD","SUD","WCID", "Other SD","Municipal","Private","WSC")
wsc_df$MASTER_TYPE <- fct_relevel(as.factor(wsc_df$MASTER_TYPE),
                                  "FWSD","MUD","SUD","WCID", "Other SD","Municipal","Private","WSC" )


ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group),data=hout_counties.df,fill = 'grey90')+
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Other SD')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=other_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=city_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Municipal')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =wsc_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50',lwd=0.25) +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),
        legend.position = c(0.25,0.2),legend.title=element_text(size=18),
        legend.text=element_text(size=14)) +
  scale_fill_colorblind(name = "Water provider",labels=c('Fresh Water Supply District',
                                                         'Municipal Utility District',
                                                         'Municipal system',
                                                         'Other special district',
                                                         'Private system',
                                                         'Special Utility District',
                                                         'Water Control & Improvement District',
                                                         'Water Service Corporation'))





temp = read_csv('input/texas_iwdd/infopage_wide.csv')  %>%
  rename(SYSTEM_ID = `District:`,SYSTEM_NAME = X2)%>% 
  mutate(HOW_CHOSEN =  `Number of Directors:`) %>% filter(!is.na(HOW_CHOSEN)) 

temp$ACTIVITY = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Activity Status:`, temp$`Business Phone:` )

temp$TYPE = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Type:`,  temp$`Activity Status:` )

temp$ELECTED_BOARD = ifelse(temp$HOW_CHOSEN %in% c('Elected',"Elected by Precinct"),1,0)
temp = temp %>% filter(ACTIVITY=='ACTIVE') 

functions_df = read_csv('input/tceq_audits/district_functions.csv')
temp = left_join(temp,functions_df )
temp = temp %>% mutate(TAX_AUTHORITY = ifelse(is.na(temp$TAX.BOND.AUTHORITY),0,1))
temp$WASTEWATER = ifelse(!is.na(temp$RETAIL.WASTEWATER),1,0)
temp$ROADS = ifelse(!is.na(temp$ROAD.POWERS),1,0)
temp$ROADS_PLUS_WASTEWATER = temp$WASTEWATER + temp$ROADS


off_df = read_csv('input/texas_iwdd/infopage_long.csv',col_names = FALSE) %>%
  mutate(X3 = gsub('\\,.*','',X3)) %>% rename(SYSTEM_ID = X1) %>%
  filter(SYSTEM_ID %in% temp$SYSTEM_ID)
library(statnet)

aj.mat = as.matrix(table(off_df$SYSTEM_ID,off_df$X3))
aj.socio = tcrossprod(aj.mat)
off_net = as.network(aj.socio,ignore.eval = F,names.eval = 'Overlap',directed=F)
off_graph = igraph::graph_from_adjacency_matrix(aj.socio,mode = 'undirected')
#off_pagerank = igraph::page.rank(off_graph)


bridging_scores = influenceR::bridging(off_graph)
bridging_df = data.frame(SYSTEM_ID = as.character(names(bridging_scores)),
                         BRIDGING_SCORE = c(bridging_scores))
off_net %v% 'Bridging_Score' <- influenceR::bridging(off_graph)

temp = left_join(temp %>% mutate(SYSTEM_ID = as.character(SYSTEM_ID)),bridging_df)

temp = temp %>% filter(!is.na(BRIDGING_SCORE))

houston_pws = read_csv('input/texas_dww/texas_master_pws.csv') %>% 
  filter(County %in% c('HARRIS','FORT BEND','AUSTIN','CHAMBERS','BRAZORIA','GALVESTON','MONTGOMERY','WALLER'))
houston_pws$Name =  toupper(gsub(' Fact  Sh.*','',houston_pws$Name))

temp = temp[temp$SYSTEM_NAME %in% houston_pws$Name,]

temp$SOURCE_AUTONOMY_CATEGORY = NA
temp$SOURCE_AUTONOMY_CATEGORY[temp$Primary_Source=='GW'] = 1
temp$SOURCE_AUTONOMY_CATEGORY[is.na(temp$SOURCE_AUTONOMY_CATEGORY)] = 0

temp$BRIDGE_SCALE = c(scale(temp$BRIDGING_SCORE))

saveRDS(object = temp,'output/temp_plot_df_object.RDS')


library(ggthemes)
ggplot(temp %>% filter(!is.na(TAX_PLUS_ELECTED)) %>% filter(TAX_PLUS_ELECTED>0),
       aes(x=as.factor(WASTEWATER),y=BRIDGE_SCALE)) + 
  geom_jitter(aes(colour = as.factor(SOURCE_AUTONOMY_CATEGORY)),pch=21,alpha = 0.8) +
  #geom_boxplot(aes(colour = as.factor(SOURCE_AUTONOMY_CATEGORY))) + 
  scale_x_discrete(name = 'Service interdependence',labels=c('Water only (n=86)',
                                                             'Water + wastewater (n=362)')) +
  scale_colour_colorblind(labels=c('Purchase/surface water (n=215)','Groundwater (n=233)'))+
  scale_y_continuous(limits=c(-3,3),name='Bridging score (standardized)') +
  theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.1),legend.title=element_blank(),
        axis.text= element_text(size=14),axis.title=element_text(size=14),
        legend.text = element_text(size=14)) +
  ggtitle('Network autonomy of districts with tax powers and/or elected boards')

unique()
ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group),data=hout_counties.df,fill = 'grey90')+
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Other SD')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=other_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=city_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Municipal')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =wsc_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50',lwd=0.25) +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),
        legend.position = c(0.25,0.2),legend.title=element_text(size=18),
        legend.text=element_text(size=14)) +
  scale_fill_colorblind(name = "Water provider",labels=c('Fresh Water Supply District',
                                                         'Municipal Utility District',
                                                         'Municipal system',
                                                         'Other special district',
                                                         'Private system',
                                                         'Special Utility District',
                                                         'Water Control & Improvement District',
                                                         'Water Service Corporation'))


table(temp$TAX_AUTHORITY[temp$SYSTEM_ID %in% network.vertex.names(off_net2)],temp$ELECTED_BOARD[temp$SYSTEM_ID %in% network.vertex.names(off_net2)])

require(rgdal)
twd <- readOGR('spatial_inputs/government_units/',"TCEQ_WATER_DISTRICTS")
library(geosphere)
twd@data$X <- centroid(twd)[,1]
twd@data$Y <- centroid(twd)[,2]
off_net %v% "X_loc" <- twd@data$X[match(network.vertex.names(off_net),twd@data$DISTRICT_I)]
off_net %v% "Y_loc" <- twd@data$Y[match(network.vertex.names(off_net),twd@data$DISTRICT_I)]
off_net %v% "TYPE" <- as.character(twd@data$TYPE[match(network.vertex.names(off_net),twd@data$DISTRICT_I)])
off_net %v% "COUNTY" <- as.character(twd@data$COUNTY[match(network.vertex.names(off_net),twd@data$DISTRICT_I)])
off_net %v% 'DRINKING_WATER' <- ((!is.na(temp$SUPPLY.TREATED.OR.RETAIL.WATER)) + 0)[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'WASTEWATER' <- temp$WASTEWATER[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'ELECTED_BOARD' <- temp$ELECTED_BOARD[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'TAX_BOND' <- temp$TAX_AUTHORITY[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'YEAR_CREATED' <- year(mdy(temp$`Date Created:`))[match(network.vertex.names(off_net),temp$SYSTEM_ID)]

off_net2 <- get.inducedSubgraph(off_net,which(!is.na(off_net%v% 'X_loc') &
                                                off_net %v% 'COUNTY' %in% houston_counties &
                                                (off_net %v% 'DRINKING_WATER')==1))
off_net2 <- get.inducedSubgraph(off_net2,v = which(off_net2 %v% 'TYPE' == 'MUD'))


twd_sub <- twd[twd@data$COUNTY %in% houston_counties & twd@data$TYPE=='MUD' &
                 twd@data$DISTRICT_I %in% network.vertex.names(off_net),]
district_centroids <- centroid(twd_sub)
twd_sub@data$id = twd_sub@data$DISTRICT_I

twd_sub.df = fortify(twd_sub, region="id")
twd_sub.df = left_join(twd_sub.df, twd_sub@data, by="id")
twd_sub.df$BRIDGING <- (off_net2 %v% 'Bridging_Score')[match(twd_sub.df$DISTRICT_I,network.vertex.names(off_net2))]
twd_sub.df$BRIDGING <- as.vector(scale(twd_sub.df$BRIDGING))
twd_sub.df$WASTEWATER <- temp$WASTEWATER[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)]
twd_sub.df$DRINKING_WATER <- (!is.na(temp$SUPPLY.TREATED.OR.RETAIL.WATER[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)])) + 0
twd_sub.df$ROADS <- temp$ROADS[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)]
temp <- temp[temp$SYSTEM_ID %in% network.vertex.names(off_net2),]
temp$SYSTEM_NAME <- gsub('MUNICIPAL UTILITY DISTRICT','MUD',temp$SYSTEM_NAME)




temp$SYSTEM_NAME[temp$SYSTEM_ID %in% network.vertex.names(off_net2)][!temp$SYSTEM_NAME[temp$SYSTEM_ID %in% network.vertex.names(off_net2)] %in% houston_pws$Name]


temp$Primary_Source = houston_pws$Primary_Source_Type[match(temp$SYSTEM_NAME,houston_pws$Name)]


url <- 'http://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=All&SourceWaterType=All&SampleType=null&begin_date=2%2F7%2F2016&end_date=2%2F7%2F2018&action=Search+For+Water+Systems'
#url <- 'http://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=ANDREWS&WaterSystemType=All&SourceWaterType=All&SampleType=null&begin_date=2%2F7%2F2016&end_date=2%2F7%2F2018&action=Search+For+Water+Systems'
library(rvest)
tt <- url %>% read_html() %>% html_nodes(css = '#AutoNumber7 td') %>%
  html_text(trim=TRUE)
tt <- matrix(tt,ncol=6,byrow=T)
colnames(tt) <- tt[1,]
tt <-  tt[-1,]
tt <- data.frame(tt)
tt$Water.System.Name <- gsub(' Fact.*','',tt$Water.System.Name)

temp$SOURCE <- tt$Pri..Src..Water.Type[match(temp$SYSTEM_NAME,tt$Water.System.Name)]
temp$PWS_STATUS <- tt$Status[match(temp$SYSTEM_NAME,tt$Water.System.Name)]


sales$Buyer_Name <- tt$Water.System.Name[match(sales$Buyer, tt$Water.System.No.)]
sales$Seller_Name <- tt$Water.System.Name[match(sales$Seller, tt$Water.System.No.)]
sales$Seller_DISTRICT_ID <- temp$SYSTEM_ID[match(sales$Seller_Name,temp$SYSTEM_NAME)]
sales$Buyer_DISTRICT_ID <- temp$SYSTEM_ID[match(sales$Buyer_Name,temp$SYSTEM_NAME)]
sales <- sales[sales$Buyer_DISTRICT_ID %in% network.vertex.names(off_net2) & sales$Seller_DISTRICT_ID %in% network.vertex.names(off_net2),]

twd_sub.df <- twd_sub.df[!is.na(twd_sub.df$DRINKING_WATER),]
twd_sub.df = left_join(twd_sub.df,data.frame(DISTRICT_I = twd_sub.df$DISTRICT_I,FAKE_BRIDGING = runif(nrow(twd_sub.df))) %>% group_by(DISTRICT_I) %>% 
                         summarise(FAKE_BRIDGING = sum(FAKE_BRIDGING)))

edge_df <- data.frame(
  district_centroids[match(network.vertex.names(off_net2)[as.edgelist(off_net2)[,1]],twd_sub$DISTRICT_I),],
  district_centroids[match(network.vertex.names(off_net2)[as.edgelist(off_net2)[,2]],twd_sub$DISTRICT_I),],
  twd_sub$DISTRICT_I[match(network.vertex.names(off_net2)[as.edgelist(off_net2)[,1]],twd_sub$DISTRICT_I)],
  twd_sub$DISTRICT_I[match(network.vertex.names(off_net2)[as.edgelist(off_net2)[,2]],twd_sub$DISTRICT_I)]
  )
colnames(edge_df) <- c('x','y','xend','yend','x_pws_id','y_pws_id')

edge_sub_df = edge_df[edge_df$x_pws_id %in% twd_sub$DISTRICT_I[twd_sub@data$COUNTY %in% c('Harris','Montgomery')] & 
edge_df$y_pws_id %in% twd_sub$DISTRICT_I[twd_sub@data$COUNTY %in% c('Harris','Montgomery')],]

twd_sub.df = left_join(twd_sub.df,data.frame(DISTRICT_I = twd_sub.df$DISTRICT_I,FAKE_BRIDGING = runif(nrow(twd_sub.df))) %>% group_by(DISTRICT_I) %>% 
  summarise(FAKE_BRIDGING = sum(FAKE_BRIDGING)))

library(viridis)
gg2a <- ggplot() + 
  geom_polygon(data = hout_counties.df,
               aes(x = long,y = lat, group=group),colour = 'grey90',fill = 'white') +
  geom_polygon(data = twd_sub.df,
               aes(x = long,y=lat,group=group,fill=FAKE_BRIDGING),lwd=0.05) + 
  geom_segment(data = edge_sub_df,aes(x=x,y=y,xend=xend,yend=yend),alpha = 0.1,lwd=0.05,col = 'grey50') +
  theme_map() + ggtitle('Shared personnel between MUDs in Houston MSA') + theme(title = element_text(size = 14)) +
  scale_fill_viridis_c()
gg2a


gg2b <- ggplot() + geom_polygon(data = hout_counties.df,aes(x = long,y = lat, group=group),colour = 'grey90',fill = 'grey90') +
  #geom_segment(data = edge_df,aes(x=x,y=y,xend=xend,yend=yend),alpha = 0.1,lwd=0.05,col = 'black') + 
  geom_polygon(data = twd_sub.df,aes(x = long,y=lat,group=group,fill = BRIDGING),lwd=0.05) + 
  theme_map() + scale_fill_viridis(name = 'Bridging score (SD)',limits = c(-2.5,2.5)) + 
  theme(legend.position = c(0.65,0.1),
        legend.direction = 'horizontal',legend.title = element_text(size=14),legend.text = element_text(size=12),title = element_text(size = 14)) +
  ggtitle('Water districts by standardized bridging score (for personnel network)') +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))
library(gridExtra)
grid.arrange(gg2a,gg2b,ncol=2)


library(scales)
gg3 <- ggplot() + geom_polygon(data = hout_counties.df,aes(x = long,y = lat, group=group),colour = 'grey90',fill = 'grey90') +
  geom_polygon(data = twd_sub.df,aes(x = long,y=lat,group=group,fill = paste0(DRINKING_WATER,WASTEWATER,ROADS)),lwd=0.05) + 
  theme_map() + theme(title = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size =14),
                      legend.position = c(0.45,0.05)) + 
  scale_fill_colorblind(name = 'Provision of closely related services',labels = c('Drinking water only', 'Drinking water and road building','Drinking water and wastewater',
                                                                                  'Drinking water, wastewater, and road building'))

sales_edges <- (sales_edge_df <- data.frame(district_centroids[match(sales$Seller_DISTRICT_ID,twd_sub$DISTRICT_I),],district_centroids[match(sales$Buyer_DISTRICT_ID,twd_sub$DISTRICT_I),]))
colnames(sales_edges) <- c('x','y','xend','yend')

twd_sub.df$SOURCE <- as.character(temp$SOURCE[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)])
twd_sub.df$SOURCE[is.na(twd_sub.df$SOURCE)|twd_sub.df$SOURCE == ''] <- 'Unknown'
twd_sub.df$SOURCE[twd_sub.df$SOURCE == 'GWP'|twd_sub.df$SOURCE == 'SWP'] <- 'Purchaser'
twd_sub.df$SOURCE[twd_sub.df$SOURCE == 'GW'] <- 'Self-sourced groundwater'
twd_sub.df$SOURCE[twd_sub.df$SOURCE == 'SW'] <- 'Self-sourced surface water'
 
(gg4 <- ggplot() + geom_polygon(data = hout_counties.df,aes(x = long,y = lat, group=group),colour = 'grey90',fill = 'grey90') +
  geom_polygon(data = twd_sub.df,aes(x = long,y=lat,group=group,fill = SOURCE),lwd=0.05) + 
  theme_map() + theme(title = element_text(size = 14),legend.text = element_text(size =12),legend.title = element_text(size =14),
                      legend.position = c(0.55,0.05)) +
    scale_fill_brewer(name = 'Water source',type = 'qual',palette = 3))
  

geom_segment(data = sales_edges,aes(x = x, y = y, xend = xend, yend = yend),colour = 'blue'))
  
off_net2 %v% 'SOURCE' <- twd_sub.df$SOURCE[match(network.vertex.names(off_net2),twd_sub.df$DISTRICT_I)]
off_net2 %v% 'SERVICES' <- paste(twd_sub.df$DRINKING_WATER[match(network.vertex.names(off_net2),twd_sub.df$DISTRICT_I)],
                                 twd_sub.df$WASTEWATER[match(network.vertex.names(off_net2),twd_sub.df$DISTRICT_I)],
                                 twd_sub.df$ROADS[match(network.vertex.names(off_net2),twd_sub.df$DISTRICT_I)])


paste0(DRINKING_WATER,WASTEWATER,ROADS)
table(off_net2 %v% 'SOURCE') / network.size(off_net2)

table(off_net2 %v% 'SERVICES',off_net2 %v% 'SOURCE')


par(mar=c(rep(0.25,4)))
class(hout_counties)
plot(hout_counties,col='white',border = 'grey50')

?geom_edges
fortify(hout_counties)

geom_segment(data = gg_off_net2,aes(x = x,y = y, xend = xend,yend=yend))

theme_map() 
head(hout_counties.df)
plot.network(off_net2,
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(off_net2%v%'X_loc',off_net2%v%'Y_loc'),  
             edge.col=alpha('grey40',.2),
             edge.lwd=0.10,
             # set the vertex size
             vertex.cex=0,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='white',
             # please don't jitter the points around
             jitter=FALSE)
plot(twd[twd@data$COUNTY %in% houston_counties & twd@data$TYPE=='MUD' &
           twd@data$DISTRICT_I %in% network.vertex.names(off_net),],add=FALSE)
plot(SpatialPoints(centroid(twd[twd@data$COUNTY %in% houston_counties & twd@data$TYPE=='MUD' &
                                  twd@data$DISTRICT_I %in% network.vertex.names(off_net),])),col='red',add=TRUE,new=FALSE)
plot(tt[,2]~tt[,1],col='red',add=TRUE,new=FALSE)

which(!is.na(off_net%v% 'X_loc') & ((off_net %v% 'COUNTY') %in% houston_counties))

head(houston_pws)
houston_pws$Name =  gsub(' Fact  Sh.*','',houston_pws$Name)



# plot the map for the background
map('world',fill=TRUE,col='#f2f2f2',lwd=0.08)
?plot.network
# plot the network using the geo coordinates
plot.network(coocNet,  # pass in the network
             # don't erase the map before drawing the network
             new=FALSE, 
             # get coordiantes from vertices and pass in as 2-col matrix
             coord=cbind(coocNet%v%'lon',coocNet%v%'lat'),  
             # ---- all the rest of these are optional to make it look nice ------
             # set a semi-transparent edge color
             edge.col='#AA555555',
             # specifiy an edge width scaled as fraction of total co-occurence
             edge.lwd=coocNet%e%'Tot_cooc'/500,
             # set the vertex size
             vertex.cex=0.5,
             # set a semi transparent vertex color
             vertex.col='#AA555555',
             vertex.border='white',
             # please don't jitter the points around
             jitter=FALSE)


table(network.vertex.names(off_net) %in% twd@data$DISTRICT_I)

plot(fc2[fc2@data == 'GULF_COAST',])

plot(fc2[fc2@data$AQ_NAME == 'GULF_COAST',])
plot(hout_counties,add=T)

# The input file geodatabase
fgdb <- "spatial_inputs/groundwater/Texas_Minor_Aquifers/Texas_Minor_Aquifers.gdb/"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
# Read the feature class
fc <- readOGR(dsn=fgdb,layer="Texas_Minor_Aquifers")

fgdb <- "spatial_inputs/groundwater/Texas_Major_Aquifers/Texas_Major_Aquifers.gdb/"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
# Read the feature class
fc2 <- readOGR(dsn=fgdb,layer="Texas_Major_Aquifers")


plot(fc3)
plot(hout_counties,add=T,col = 'red')
plot(hout_counties)
plot(fc3,add=T)
fc3

fc_hout <- crop(fc,hout_counties)
#fc_hout <- mask(fc_hout,hout_counties)

fc2_hout <- crop(fc2,hout_counties)
#fc2_hout <- mask(fc2_hout,hout_counties)

plot(hout_counties,add=F)

plot(fc2_hout,col = 'blue',add = T)
plot(fc_hout,col = 'red',add = T)

fc2_hout@data$AQ_NAME



# View the feature class
plot(fc)


temp$SYSTEM_NAME[!temp$SYSTEM_NAME %in%
                   unique(service_df$NAME)]

grep("530",service_df$NAME,value=T)

) unique(temp$SYSTEM_ID)
head(service_df)
ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group),data=hout_counties.df,fill = 'grey90')+
  # geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=other_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50',lwd=0.25) +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),
        legend.position = c(0.25,0.2),legend.title=element_text(size=18),
        legend.text=element_text(size=14)) +
  scale_fill_manual(values = rep('grey80',4))


scale_fill_colorblind(name = "Water provider",labels=c('Fresh Water Supply District',
                                                       'Municipal Utility District',
                                                       'Municipal system',
                                                       'Other special district',
                                                       'Private system',
                                                       'Special Utility District',
                                                       'Water Control & Improvement District',
                                                       'Water Service Corporation'))



sales <- read_csv('input/texas_dww/sales_connections.csv')





temp %>% filter(!is.na(TAX_PLUS_ELECTED)) %>% filter(TAX_PLUS_ELECTED>0) %>% group_by(as.factor(WASTEWATER)) %>%
  summarise(n())
temp %>% filter(!is.na(TAX_PLUS_ELECTED)) %>% filter(TAX_PLUS_ELECTED>0) %>% group_by(as.factor(SOURCE_AUTONOMY_CATEGORY)) %>%
  summarise(n())

table(temp$SOURCE_AUTONOMY_CATEGORY)
temp$ROADS_PLUS_WASTEWATER


table(temp$TAX_AUTHORITY,temp$ELECTED_BOARD)

temp$GROUNDWATER
temp$`Primary Source`
temp$HOW_CHOSEN
temp$TAX_AUTHORITY
temp$BRIDGING_SCORE







