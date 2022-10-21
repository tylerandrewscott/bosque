library(tidyverse)
library(ggplot2)
library(rgeos)
library(rgdal)
library(stringr)
library(sp)
library(raster)
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
#in_region = over(service1,hout_counties)
#service1 = service1[!is.na(in_region$COUNTYNAME),]
#service1_over_county = over(service1,hout_counties)
#service1$COUNTY = service1_over_county$COUNTYNAME
service1@data$COMMON_NAM = gsub('#','',service1@data$COMMON_NAM)
service1@data$id = service1@data$OBJECTID
service1 = raster::crop(service1,hout_counties)
service1@data$NAME[grep('SUNBELT',service1@data$NAME)] = grep('SUNBELT',system_df$PWS_NAME,value=T)
service1@data$NAME = ifelse(service1@data$COMMON_NAM %in% system_df$PWS_NAME,service1@data$COMMON_NAM,service1@data$NAME)

service1 <- service1[!service1@data$NAME %in% system_df$PWS_NAME,]
service1_df = fortify(service1,region='id')
service1_df = left_join(service1_df,service1@data)
service1_df$MASTER_TYPE = ifelse(service1_df$Type == 'Municipal','Municipal','SD')

service2 = readOGR('spatial_inputs/hg_gis','other_districts')
service2 = spTransform(service2,CRS(proj4string(hout_counties)))
# service2_over_tract = over(service2,houston_tracts)
# service2$AFFGEOID = service2_over_tract$AFFGEOID
# service2_over_county = over(service2,hout_counties)
# service2$COUNTY = service2_over_county$COUNTYNAME
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
service2$NAME = gsub("HARRIS MONTGOMERY COUNTY",'HARRIS MONTGOMERY COUNTIES',service2$NAME)
service2 = raster::crop(service2,houston_msa)
service2 = service2[!service2@data$NAME %in% service1@data$NAME,]
service2 <- service2[service2@data$NAME %in% system_df$PWS_NAME,]
service2$id = as.character(1:length(service2))
service2_df = fortify(service2)
service2_df = left_join(service_df,service2@data,by='id')
service2_df$MASTER_TYPE = ifelse(service2_df$TYPE == 'Municipal','Municipal','SD')


tx_places = readOGR('spatial_inputs/government_units/','cb_2015_48_place_500k')
tx_places = spTransform(tx_places,CRS(proj4string(hout_counties)))
hout_places <- tx_places
hout_places$NAME = paste0('CITY OF ',str_to_upper(hout_places$NAME))
hout_places$NAME = gsub("'","",hout_places$NAME)
hout_places$NAME = gsub("CITY OF MISSOURI CITY","CITY OF MISSOURI CITY MUSTANG BAYOU WATE",hout_places$NAME)
hout_places@data$id = hout_places@data$NAME
hout_places = hout_places[!hout_places@data$NAME %in% service1@data$NAME,]
hout_places = hout_places[!hout_places@data$NAME %in% service2@data$NAME,]
hout_places = hout_places[hout_places@data$NAME %in% system_df$PWS_NAME,]
hout_places@data$id = as.character(1:length(hout_places))
places_df = fortify(hout_places,region='id')
places_df$MASTER_TYPE = 'Municipal'


tx_utils = readOGR('spatial_inputs/government_units/PUC_CCN_WATER/','PUC_CCN_WATER')
tx_utils = spTransform(tx_utils,CRS(proj4string(hout_counties)))
hout_utils = tx_utils[tx_utils$COUNTY %in% str_to_upper(houston_counties),]
hout_utils$UTILITY = gsub('SUGARLAND','SUGAR LAND',hout_utils$UTILITY)
hout_utils = hout_utils[!hout_utils$UTILITY %in% c(hout_places$NAME,twd_geo$NAME),]
hout_utils$UTILITY = gsub('NORTHWOODS WSC','NORTHWOOD WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('^HOE WSC','H O E WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('WOODLAND LAKES WSC','WOODLAND LAKES ESTATES WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub('WALNUT COVE INC','WALNUT COVE WSC',hout_utils$UTILITY)
hout_utils$UTILITY = gsub("PINE LAKE WSC INC","PINE LAKE SUBDIVISION NORTH WSC",hout_utils$UTILITY)
hout_utils$UTILITY = gsub('ENCHANTED VALLEY WSC','ENCHANTED VALLEY ESTATES WSC',hout_utils$UTILITY)
hout_utils= hout_utils[!hout_utils@data$UTILITY %in% service1@data$NAME,]
hout_utils= hout_utils[!hout_utils@data$UTILITY %in% service2@data$NAME,]
hout_utils= hout_utils[!hout_utils@data$UTILITY %in% hout_places@data$NAME,]
#hout_utils = hout_utils[hout_utils@data$UTILITY %in% system_df$PWS_NAME,]
hout_utils@data$id = as.character(1:length(hout_utils))
utils_df = fortify(hout_utils,region='id')
utils_df = left_join(utils_df,hout_utils@data)
utils_df$MASTER_TYPE = ifelse(grepl('WSC',utils_df$UTILITY),'WSC','Private')


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
twd_geo = spTransform(twd_geo,CRS(proj4string(hout_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo = twd_geo[!twd_geo@data$NAME %in% service1@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% service2@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% hout_places@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% hout_utils@data$UTILITY,]
twd_geo <- twd_geo[twd_geo@data$NAME %in% system_df$PWS_NAME,]

twd_geo = spTransform(twd_geo,CRS(proj4string(hout_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)
twd_geo.df$MASTER_TYPE = 'SD'


gg_hous = ggplot()+
  geom_path(aes(x=long,y=lat,group=group),data=hout_counties.df) + 
  geom_polygon(aes(x=long,y=lat,group=group,fill='Municipal system'),col='grey50',data=places_df,lwd=0.2) +

 geom_polygon(aes(x=long,y=lat,group=group),col='grey50',data =service1_df,lwd=0.2) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data=service2_df,lwd=0.2) +
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =twd_geo.df,lwd=0.2)+
  geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),col='grey50',data =utils_df,lwd=0.2) +
  #geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50') +
  #geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50') +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),legend.position = c(0.9,0.15),legend.title=element_text(size=18),legend.text=element_text(size=18)) +
  scale_fill_colorblind(name = "Provider type") 
gg_hous

ggsave(filename = 'scratch/hout_district_types.png',gg_hous,width = 6,height=5,dpi = 450,units = 'in')





rm(list=ls())


load('primary_model.RData')


library(ggmcmc)
library(statnet)

library(lubridate)

mcmc.diagnostics(mod_main)
mod_mcmc = ggs(mod_main$sample)
head(mod_mcmc)
ggplot(mod_mcmc,aes(x=Iteration,colour=Chain,y=value)) + geom_path() + facet_wrap(~Parameter,scale = 'free_y')


#### MODEL SETUP ###
#master_df = read.csv('input/scratch/ready_model.csv')# %>% filter(DOC_ID != 256506)
model_df = read_csv('input/scratch/master_data.csv') %>% filter(TOTAL_REVENUE_LAG1 < 28000000) %>%
  filter(!is.na(TOTAL_REVENUE_LAG1)) %>% filter(SERVICE_CONNECTIONS_LAG1!=0) %>%
  filter(month(`FISCAL YEAR ENDED`) == MONTH) %>% filter(DISTRICT_AGE>0) %>% 
  filter(TOTAL_REVENUE_LAG1 != 0) %>%filter(FYEAR >= 2008) %>%
  mutate(LOG_SERVICE_CONNECTIONS = log(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`))
colnames(model_df)[1] = 'PWS_ID'

temp = read_csv('code/upwork/WIDE (1).csv') %>% 
  rename(SYSTEM_ID = `District:`,SYSTEM_NAME = X2)%>% filter(!is.na(HOW_CHOSEN))
temp$HOW_CHOSEN = temp$`Number of Directors:`
temp$ACTIVITY = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Activity Status:`, temp$`Business Phone:` )
temp$TYPE = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Type:`,  temp$`Activity Status:` )
temp$ELECTED_BOARD = ifelse(temp$HOW_CHOSEN%in% c('Elected',"Elected by Precinct"),1,0)
temp = temp %>% filter(ACTIVITY=='ACTIVE') 

temp = full_join(model_df %>% arrange(-YEAR) %>% filter(!duplicated(PWS_ID)),temp %>% dplyr::select(-`Activity Status:`))

functions_df = read_csv('input/tceq_audits/district_functions.csv')
temp = left_join(temp,functions_df %>% dplyr::select(-GROUNDWATER))
temp = temp %>% mutate(TAX_AUTHORITY = ifelse(is.na(temp$TAX.BOND.AUTHORITY),0,1))
temp$WASTEWATER = ifelse(!is.na(temp$RETAIL.WASTEWATER),1,0)
temp$ROADS = ifelse(!is.na(temp$ROAD.POWERS),1,0)
temp$ROADS_PLUS_WASTEWATER = temp$WASTEWATER + temp$ROADS


off_df = read_csv('code/upwork/LONG (1).csv',col_names = FALSE) %>%
  mutate(X3 = gsub('\\,.*','',X3)) %>% rename(SYSTEM_ID = X1) %>%
  filter(SYSTEM_ID %in% temp$SYSTEM_ID)
library(statnet)
aj.mat = as.matrix(table(off_df$SYSTEM_ID,off_df$X3))
aj.socio = tcrossprod(aj.mat)
off_net = as.network(aj.socio,ignore.eval = F,names.eval = 'Overlap',directed=F)
off_graph = igraph::graph_from_adjacency_matrix(aj.socio,mode = 'undirected')
off_pagerank = igraph::page.rank(off_graph)
bridging_scores = influenceR::bridging(off_graph)
bridging_df = data.frame(SYSTEM_ID = as.character(names(bridging_scores)),BRIDGING_SCORE = c(bridging_scores))

temp = left_join(temp %>% mutate(SYSTEM_ID = as.character(SYSTEM_ID)),bridging_df) %>%
  filter(!is.na(BRIDGING_SCORE))
temp$TAX_AUTHORITY
temp$ELECTED_BOARD
temp$TAX_AUTHORITY
temp$TAX_PLUS_ELECTED = (temp$TAX_AUTHORITY + temp$ELECTED_BOARD)
table(is.na(temp$ELECTED_BOARD))
table(temp$ELECTED_BOARD,temp$TAX_AUTHORITY)
table(temp$TAX_AUTHORITY)
table(is.na(temp$BRIDGING_SCORE))

temp$SOURCE_AUTONOMY_CATEGORY = NA
temp$SOURCE_AUTONOMY_CATEGORY[temp$`Primary Source`=='Ground water'] = 1
temp$SOURCE_AUTONOMY_CATEGORY[is.na(temp$SOURCE_AUTONOMY_CATEGORY)] = 0

temp$BRIDGE_SCALE = c(scale(temp$BRIDGING_SCORE))


library(ggthemes)
ggplot(temp[temp$TAX_PLUS_ELECTED!=0,],aes(x=as.factor(WASTEWATER),y=BRIDGE_SCALE)) + 
  #geom_jitter(aes(colour = as.factor(SOURCE_AUTONOMY_CATEGORY)),pch=21,alpha = 0.8) + 
  geom_boxplot(aes(colour = as.factor(SOURCE_AUTONOMY_CATEGORY))) + 
  scale_x_discrete(name = 'Service interdependence',labels=c('Water only','Water + wastewater')) +
  scale_colour_colorblind(labels=c('Purchase/surface water','Groundwater'))+
  scale_y_continuous(limits=c(-3,3.5),name='Bridging score (standardized)') +
  theme_tufte(ticks=F) + 
  theme(legend.position = c(0.8,0.1),legend.title=element_blank(),
        axis.text= element_text(size=14),axis.title=element_text(size=14),
        legend.text = element_text(size=14))


table(temp$SOURCE_AUTONOMY_CATEGORY)
temp$ROADS_PLUS_WASTEWATER


table(temp$TAX_AUTHORITY,temp$ELECTED_BOARD)

temp$GROUNDWATER
temp$`Primary Source`
temp$HOW_CHOSEN
temp$TAX_AUTHORITY
temp$BRIDGING_SCORE








