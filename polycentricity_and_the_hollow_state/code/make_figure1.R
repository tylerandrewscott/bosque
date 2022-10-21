library(spdep)
library(maptools)
library(dplyr)
library(tidyverse)
library(lubridate)
library(sp)
library(rgeos)
library(rgdal)
library(stringr)
system_df = read_csv('input/epa_sdwis/system_base_master.csv')


######
# houston_wd_geo@data$id = houston_wd_geo@data$NAME
# hout_wd.points = fortify(houston_wd_geo,region='id')
# hout_wd.df = plyr::join(hout_wd.points,houston_wd_geo@data,by='id')
# hout_wd.df$PWS_NAME = hout_wd.df$NAME
# which_tract_wd = over(houston_wd_geo,tx_tracts)
# table(is.na(which_tract_wd$))
# houston_wd_geo@data$GEOID = as.character(which_tract_wd$GEOID)
houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]

library(ggthemes)

hout_counties@data$id = hout_counties@data$COUNTYNAME
hout_counties.points = fortify(hout_counties, region="id")
hout_counties.df = plyr::join(hout_counties.points, hout_counties@data, by="id")


# hout_utils@data$id = hout_utils@data$UTILITY
# hout_utils.points = fortify(hout_utils, region="id")
# hout_utils.df = plyr::join(hout_utils.points, hout_utils@data, by='id')
# hout_utils.df$PWS_NAME = hout_utils.df$UTILITY
# hout_utils.df$OBS = FALSE
# hout_utils.df$OBS[hout_utils.df$PWS_NAME %in% system_df$PWS_NAME] = TRUE

tx_places = readOGR('spatial_inputs/government_units/','cb_2015_48_place_500k')
in_hout = over(tx_places,hout_counties,returnList = FALSE)
hout_places = tx_places[!is.na(in_hout$FIPS_I),]
hout_places$NAME = paste0('CITY OF ',str_to_upper(hout_places$NAME))
hout_places$NAME = gsub("'","",hout_places$NAME)
hout_places$NAME = gsub("CITY OF MISSOURI CITY","CITY OF MISSOURI CITY MUSTANG BAYOU WATE",hout_places$NAME)
hout_places@data$id = hout_places@data$NAME
hout_places.points = fortify(hout_places, region="id")
hout_places.df = plyr::join(hout_places.points, hout_places@data, by="id") 
hout_places.df$PWS_NAME = hout_places.df$NAME
hout_places.df$OBS = FALSE
hout_places.df$OBS[hout_places.df$PWS_NAME %in% system_df$PWS_NAME] = TRUE


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

twd_geo = spTransform(twd_geo,CRS(proj4string(tx_places)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)

houston_msa = readOGR('spatial_inputs/hg_gis','houston_msa_full')
houston_msa@data$id = rownames(houston_msa@data)
houston_msa = spTransform(houston_msa,CRS(proj4string(tx_places)))
houston_msa_df = left_join(fortify(houston_msa),houston_msa@data)


tx_utils = readOGR('spatial_inputs/government_units/PUC_CCN_WATER/','PUC_CCN_WATER')
tx_utils = spTransform(tx_utils,CRS(proj4string(tx_places)))
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
service1 = spTransform(service1,CRS(proj4string(tx_places)))
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
service1$AFFGEOID = service1_over_tract$AFFGEOID
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
service2 = spTransform(service2,CRS(proj4string(tx_places)))
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
                                    SUD = 'SUD',
                                    `Other SD` = c("DD","ID" , 'PUD', "LID"  ,"MMD" ,"ND", 'PID',  "OTH" , "RA"  , "RD"   ,"SWCD" , "WID") )
city_df$MASTER_TYPE = 'Municipal'         



service_df$MASTER_TYPE = fct_collapse(service_df$TYPE,
                                      MUD = c("MUD"),
                                      WCID = "WCID",
                                      WSC = 'WSC',
                                      SUD = 'SUD',
                                      FWSD = "FWSD",Private = "Private",
                                      Municipal = "Municipal",
                                      `Other SD` = c("DD","ID" , "UD","WA", "LID",'PID' ,'PUD' ,"MMD" ,"ND",   "OTH" , "RA"  , "RD"  ,"SWCD" , "WID") )          

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


gg_hous = ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group),data=hout_counties.df,fill='grey90')+
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Other SD')),col='grey50') +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=other_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=city_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE %in% c('Municipal')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =wsc_df,lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50',lwd=0.25) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50',lwd=0.25) +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),#legend.background = element_rect(fill = alpha('white',0.5)),
        axis.title = element_blank(),title = element_text(size = 14),
        legend.position = c(0.25,0.21),legend.title=element_text(size=12),legend.text=element_text(size=11)) +
  scale_fill_colorblind(name = "Water provider",
                        labels=c('Fresh Water Supply District',
                                 'Municipal Utility District',
                                 'Municipal systems',
                                 'Other special district',
                                 'Private system',
                                 'Special Utility District',
                                 'Water Control & Improvement District',
                                 'Water Service Corporation')) +
  #ggtitle('Community water systems in Houston metropolitan statistical area (2017)') +
  NULL
gg_hous
ggsave(filename = 'output/proj3/figure1.tiff',gg_hous,width = 6,height=5,dpi = 600,units = 'in')







