system('setenv OMP_NUM_THREADS 4')  # Tcsh/Csh Shell
system('export OMP_NUM_THREADS=4') 
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
library(data.table)

system_df = fread('../bosque/input/epa_sdwis/system_base_master.csv')

houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = st_read('../bosque/spatial_inputs/government_units/county_nrcs_a_tx.shp')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]
library(ggthemes)

twd_geo = st_read('../bosque/spatial_inputs/government_units/TCEQ_WATER_DISTRICTS.shp')
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
twd_geo = st_transform(twd_geo,crs = st_crs(tx_counties))

houston_msa = st_union(tx_counties[tx_counties$COUNTYNAME %in% houston_counties,])

tx_places = st_read('../bosque/spatial_inputs/government_units/cb_2015_48_place_500k.shp')
tx_places = st_transform(tx_places,st_crs(tx_counties))

in_hout = st_intersects(tx_places,hout_counties)

hout_places = tx_places[hout_counties,]
hout_places$NAME = paste0('CITY OF ',str_to_upper(hout_places$NAME))
hout_places$NAME = gsub("'","",hout_places$NAME)
hout_places$NAME = gsub("CITY OF MISSOURI CITY","CITY OF MISSOURI CITY MUSTANG BAYOU WATE",hout_places$NAME)

tx_utils = st_read('../bosque/spatial_inputs/government_units/PUC_CCN_WATER/PUC_CCN_WATER.shp')
tx_utils = st_transform(tx_utils,st_crs(tx_counties))
hout_utils = tx_utils[hout_counties,]
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
library(lwgeom)
twd_geo = st_make_valid(twd_geo)
houston_wd_geo = twd_geo[hout_counties,]

# To use for fills, add
#hout_places.df$HOUSTON = ifelse(hout_places.df$PWS_NAME=='CITY OF HOUSTON','CITY OF HOUSTON','CITY')

library(sf)
service1 = st_read('../bosque/spatial_inputs/Service_Area_Boundaries/Service_Area_Boundaries.gdb','Service_Area_Boundaries')
service1 = st_transform(service1,st_crs(hout_counties))
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
service1 = service1[hout_counties,]
service1$NAME[grep('SUNBELT',service1$NAME)] = grep('SUNBELT',system_df$PWS_NAME,value=T)
service1$COMMON_NAM = gsub('#','',service1$COMMON_NAM)
service1 = service1[service1$NAME %in% system_df$PWS_NAME,]


service2 = st_read('../bosque/spatial_inputs/Service_Area_Boundaries/Other_Districts.gdb','Other_Districts')
service2 = st_transform(service2,st_crs(hout_counties)) 
service2$NAME = str_to_upper(service2$NAME)
service2$ID = as.character((length(service1)+1):(length(service1)+length(service2)))

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

temp = service2[service2$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% service1$NAME],]
temp$TYPE=  ifelse(grepl('Public Utility District',temp$Type),'PUD',ifelse(grepl('Special Utility District',temp$Type),'SUD',
     ifelse(grepl('River Authority',temp$Type),'RA',
      ifelse(grepl('Water Authority',temp$Type),'WA',  ifelse(grepl('Water Control Improvement District',temp$Type),'WCID',
      ifelse(grepl('Fresh Water Supply District',temp$Type),'FWSD',
      ifelse(temp$Type=='Utility District','UD', temp$Type)))))))
temp$TYPE = ifelse(temp$TYPE=='8','MUD',temp$TYPE)

service1$TYPE = service1$Type
service1$PWS_ID = service1$EPA_PERMIT_NUMBER
temp$PWS_ID = NA
service1$DISTRICT_ID = NA
temp$DISTRICT_ID = temp$DISTRICT

service_dt = rbind(temp[,c('NAME','TYPE','DISTRICT_ID','PWS_ID')],service1[,c('NAME','TYPE','DISTRICT_ID','PWS_ID')])
service_dt = service_dt %>% rename(geometry = Shape)
temp = houston_wd_geo[houston_wd_geo$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% service_dt$NAME],]
temp$PWS_ID = NA
temp$DISTRICT_ID = temp$DISTRICT
service_dt = rbind(service_dt,temp[,c('NAME','TYPE','DISTRICT_ID','PWS_ID')])

temp = hout_places[hout_places$NAME %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% (c(unique(service_dt$NAME)))],]
temp$TYPE = 'Municipal'
temp$PWS_ID = NA
temp$DISTRICT_ID = NA
service_dt = rbind(service_dt,temp[,c('NAME','TYPE','DISTRICT_ID','PWS_ID')])



puc_dists = st_read('../bosque/spatial_inputs/texas_puc/PUC_CCN_WATER.shp')
puc_dists = st_transform(puc_dists,st_crs(hout_counties))
puc_dists$UTILITY = as.character(puc_dists$UTILITY)
puc_dists$UTILITY[puc_dists$UTILITY == 'NORTHWOODS WSC'] = 'NORTHWOOD WSC'
puc_dists$UTILITY[puc_dists$UTILITY == "WOODLAND LAKES WSC"] = "WOODLAND LAKES ESTATES WSC"
puc_dists$UTILITY[puc_dists$UTILITY == "WALNUT COVE INC"] = "WALNUT COVE WSC"
puc_dists$UTILITY[puc_dists$UTILITY == "PINE LAKE WSC INC"] = "PINE LAKE SUBDIVISION NORTH WSC"
temp = puc_dists[puc_dists$UTILITY %in% system_df$PWS_NAME[!system_df$PWS_NAME %in% unique(c(service_dt$NAME))],]
temp$TYPE = ifelse(system_df$`Owner Type`[match(temp$UTILITY,system_df$PWS_NAME)]=='Local government','WSC',"Private")
temp$PWS_ID = NA
temp$DISTRICT_ID = NA
temp$NAME = temp$UTILITY
service_dt = rbind(service_dt,temp[,c('NAME','TYPE','DISTRICT_ID','PWS_ID')])


tol15rainbow= c( "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",  "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

service_dt$TYPE = fct_other(service_dt$TYPE,keep = c('MUD','WSC','Private','Municipal'))


ggplot() + geom_sf(data = service_dt[service_dt$TYPE=='Other'&grepl("VALLEY AUTHORITY",service_dt$NAME),])
library(scales)

test = st_read('spatial_data/PWS_Export.shp')
test = st_transform(test,st_crs(hout_counties))
test = test[hout_counties,]
test$TYPE = NA
test$TYPE[grepl("WSC|WATER SUPPLY CORPORATION",test$pwsName)] <- "WSC"
test$TYPE[grepl("WCID|FWSD|DISTRICT| SUD | PUD |TBCD| UD | UD$| PUD$| SUD$",test$pwsName)] <- "Other District"
test$TYPE[grepl("MUD|MUNICIPAL UTILITY DISTRICT",test$pwsName)] <- "MUD"
test$TYPE[grepl("CITY OF|TOWN OF|VILLAGE OF|CITY WATER AUTHORITY",test$pwsName)] <- 'Municipal'
test$TYPE[grepl("SUBDIVISION|HOME PARK|HOME COMMUNITY|WATER SYSTEM|APARTMENTS|RV PARK|MHP|ESTATES",test$pwsName)] <- 'Private'
test$TYPE[is.na(test$TYPE)] <- "Private"

figure2 = ggplot()+
  geom_sf(data=hout_counties,fill = 'grey90',col = 'grey90') + 
 # geom_sf(aes(fill = TYPE,col = TYPE),data=st_crop(service_dt[service_dt$NAME!='LOWER NECHES VALLEY AUTHORITY',],hout_counties))+
  geom_sf(aes(fill = TYPE,col = TYPE),data=st_crop(test,hout_counties))+
ggtitle('Public water system operators in Houston, TX MSA') + theme_void() + 
  theme(axis.text = element_blank(),axis.title = element_blank(),panel.grid = element_line(colour = 'transparent'),
        legend.position = c(0.15,0.2),legend.title=element_text(size=12),text = element_text(family = 'Times'),
        legend.text=element_text(size=12),title = element_text(size = 14)) +
scale_fill_manual(name = "Water provider",values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),
                  labels=c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_colour_manual(name = "Water provider",values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),
                      labels=c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation'))

ggsave(figure2,width = 7.5,height=7.5,units = 'in',dpi = 450,filename = 'Output/figure2.tiff')


temp = read_csv('../bosque/input/texas_iwdd/infopage_wide.csv')  %>%
  rename(SYSTEM_ID = `District:`,SYSTEM_NAME = X2)%>% 
  mutate(HOW_CHOSEN =  `Number of Directors:`) %>% filter(!is.na(HOW_CHOSEN)) 

temp$ACTIVITY = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Activity Status:`, temp$`Business Phone:` )

temp$TYPE = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Type:`,  temp$`Activity Status:` )

temp$ELECTED_BOARD = ifelse(temp$HOW_CHOSEN %in% c('Elected',"Elected by Precinct"),1,0)
temp = temp %>% filter(ACTIVITY=='ACTIVE') 

functions_df = read_csv('../bosque/input/tceq_audits/district_functions.csv')
temp = left_join(temp,functions_df )
temp = temp %>% mutate(TAX_AUTHORITY = ifelse(is.na(temp$TAX.BOND.AUTHORITY),0,1))
temp$WASTEWATER = ifelse(!is.na(temp$RETAIL.WASTEWATER),1,0)
temp$ROADS = ifelse(!is.na(temp$ROAD.POWERS),1,0)
temp$ROADS_PLUS_WASTEWATER = temp$WASTEWATER + temp$ROADS


off_df = read_csv('../bosque/input/texas_iwdd/infopage_long.csv',col_names = FALSE) %>%
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
off_net %v% 'Bridging_Score' <- bridging_scores

temp = left_join(temp %>% mutate(SYSTEM_ID = as.character(SYSTEM_ID)),bridging_df)
temp = temp %>% filter(!is.na(BRIDGING_SCORE))
houston_pws = read_csv('../bosque/input/texas_dww/texas_master_pws.csv') %>% 
  filter(County %in% c('HARRIS','FORT BEND','AUSTIN','CHAMBERS','BRAZORIA','GALVESTON','MONTGOMERY','WALLER'))
houston_pws$Name =  toupper(gsub(' Fact  Sh.*','',houston_pws$Name))
temp = temp[temp$SYSTEM_NAME %in% houston_pws$Name,]

temp$SOURCE_AUTONOMY_CATEGORY = NA
temp$SOURCE_AUTONOMY_CATEGORY[temp$Primary_Source=='GW'] = 1
temp$SOURCE_AUTONOMY_CATEGORY[is.na(temp$SOURCE_AUTONOMY_CATEGORY)] = 0

temp$BRIDGE_SCALE = c(scale(temp$BRIDGING_SCORE))

#saveRDS(object = temp,'Output/temp_plot_df_object.RDS')

####
#temp = readRDS('output/temp_plot_df_object.RDS')
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


require(rgdal)
twd <- st_read("../bosque/spatial_inputs/government_units/TCEQ_WATER_DISTRICTS.shp")
twd <- st_make_valid(twd)
library(geosphere)

twd[,c('X','Y')] <- as.matrix(st_coordinates(st_centroid(twd)))

off_net %v% "X_loc" <- twd$X[match(network.vertex.names(off_net),twd$DISTRICT_I)]
off_net %v% "Y_loc" <- twd$Y[match(network.vertex.names(off_net),twd$DISTRICT_I)]
off_net %v% "TYPE" <- as.character(twd$TYPE[match(network.vertex.names(off_net),twd$DISTRICT_I)])
off_net %v% "COUNTY" <- as.character(twd$COUNTY[match(network.vertex.names(off_net),twd$DISTRICT_I)])
off_net %v% 'DRINKING_WATER' <- ((!is.na(temp$SUPPLY.TREATED.OR.RETAIL.WATER)) + 0)[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'WASTEWATER' <- temp$WASTEWATER[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'ELECTED_BOARD' <- temp$ELECTED_BOARD[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'TAX_BOND' <- temp$TAX_AUTHORITY[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net %v% 'YEAR_CREATED' <- year(mdy(temp$`Date Created:`))[match(network.vertex.names(off_net),temp$SYSTEM_ID)]

off_net2 <- get.inducedSubgraph(off_net,which(!is.na(off_net%v% 'X_loc') &
                                                off_net %v% 'COUNTY' %in% houston_counties &
                                                (off_net %v% 'DRINKING_WATER')==1))
off_net2 <- get.inducedSubgraph(off_net2,v = which(off_net2 %v% 'TYPE' == 'MUD'))


twd_sub <- twd[twd$COUNTY %in% houston_counties & twd$TYPE=='MUD' &
                 twd$DISTRICT_I %in% network.vertex.names(off_net),]
district_centroids <- st_centroid(twd_sub)
twd_sub$id = twd_sub$DISTRICT_I


twd_sub.df = twd_sub
twd_sub.df$BRIDGING <- (off_net2 %v% 'Bridging_Score')[match(twd_sub.df$DISTRICT_I,network.vertex.names(off_net2))]
twd_sub.df$BRIDGING <- as.vector(scale(twd_sub.df$BRIDGING))
twd_sub.df$WASTEWATER <- temp$WASTEWATER[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)]
twd_sub.df$DRINKING_WATER <- (!is.na(temp$SUPPLY.TREATED.OR.RETAIL.WATER[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)])) + 0
twd_sub.df$ROADS <- temp$ROADS[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)]
temp <- temp[temp$SYSTEM_ID %in% network.vertex.names(off_net2),]
temp$SYSTEM_NAME <- gsub('MUNICIPAL UTILITY DISTRICT','MUD',temp$SYSTEM_NAME)

twd_sub.df$BRIDGING_SCORE = temp$BRIDGING_SCORE[match(twd_sub.df$DISTRICT_I,temp$SYSTEM_ID)]

temp$SYSTEM_NAME[temp$SYSTEM_ID %in% network.vertex.names(off_net2)][!temp$SYSTEM_NAME[temp$SYSTEM_ID %in% network.vertex.names(off_net2)] %in% houston_pws$Name]

temp$Primary_Source = houston_pws$Primary_Source_Type[match(temp$SYSTEM_NAME,houston_pws$Name)]

#pwd_references = read_csv('input/twdd_records/district_list_2018-10-14.csv')
tt = fread('../bosque/input/twdd_records/district_list_2018-10-14.csv')
tt = tt[tt$Status=='ACTIVE',]
tt = tt[tt$Type == 'MUNICIPAL UTILITY DISTRICT',]
tt$Water.System.Name <- gsub(' Fact.*','',tt$Water.System.Name)
#temp$SOURCE <- tt$Pri..Src..Water.Type[match(temp$SYSTEM_NAME,tt$Water.System.Name)]
#temp$PWS_STATUS <- tt$Status[match(temp$SYSTEM_NAME,tt$Water.System.Name)]

sales = fread('../bosque/input/texas_dww/sales_connections_2018-11-06.csv')
sales$Buyer_Name <- tt$Water.System.Name[match(sales$Buyer, tt$Water.System.No.)]
sales$Seller_Name <- tt$Water.System.Name[match(sales$Seller, tt$Water.System.No.)]
sales$Seller_DISTRICT_ID <- temp$SYSTEM_ID[match(sales$Seller_Name,temp$SYSTEM_NAME)]
sales$Buyer_DISTRICT_ID <- temp$SYSTEM_ID[match(sales$Buyer_Name,temp$SYSTEM_NAME)]
sales <- sales[sales$Buyer_DISTRICT_ID %in% network.vertex.names(off_net2) & sales$Seller_DISTRICT_ID %in% network.vertex.names(off_net2),]

twd_sub.df <- twd_sub.df[!is.na(twd_sub.df$DRINKING_WATER),]


twd_sub.df
library(viridis)
gg2a <- ggplot() + 
  geom_sf(data = hout_counties,colour = 'grey90',fill = 'white') +
  geom_segment(data = edge_sub_df,aes(x=x,y=y,xend=xend,yend=yend),alpha = 0.1,lwd=0.05,col = 'grey15') +
  geom_polygon(data = twd_sub.df[!is.na(twd_sub.df$BRIDGING_SCORE),],
               aes(x = long,y=lat,group=group,fill=BRIDGING_SCORE),lwd=0.05) + 
  theme_map() + ggtitle('Shared personnel and bridging scores for MUDs in Houston MSA') + 
  theme(title = element_text(size = 14),legend.position = c(0.1,0.1)) +
  scale_fill_viridis_c(direction = -1,name = 'Bridging score')

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
  scale_fill_colorblind(name = 'Provision of closely related services',
                        labels = c('Drinking water only', 'Drinking water and road building','Drinking water and wastewater',
                            'Drinking water, wastewater, and road building'))

gg3
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


tt = read_csv('input/twdd_records/district_list_2018-10-14.csv')
tt = tt[tt$Status=='ACTIVE',]
tt = tt[tt$Type == 'MUNICIPAL UTILITY DISTRICT',]
geo_wd = readOGR('../../google_drive/putah/spatial_input/texas/water_districts_shp','TCEQ_WaterDistricts')





###########################







houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = st_read('../bosque/spatial_inputs/government_units/county_nrcs_a_tx.shp')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]
library(ggthemes)

texas_pws_geo = st_read('spatial_data/PWS_Export.shp')
texas_pws_geo  = st_transform(texas_pws_geo,st_crs(hout_counties))

houston_pws_geo = texas_pws_geo[hout_counties,]
houston_pws_geo_cropped = st_crop(houston_pws_geo,hout_counties)

district_centroids <- st_centroid(houston_pws_geo_cropped)

houston_pws_geo_cropped$id = houston_pws_geo_cropped$PWSId
houston_pws_df = houston_pws_geo_cropped
houston_pws_df  = houston_pws_df[!grepl('TDCJ',houston_pws_df$pwsName),]
houston_pws_df$TYPE = NA
houston_pws_df$TYPE[grepl("^CITY OF|^TOWN OF|^VILLAGE OF",houston_pws_df$pwsName)] <- 'Municipal'
houston_pws_df$TYPE[grepl("WSC",houston_pws_df$pwsName)] <- 'WSC'
houston_pws_df$TYPE[grepl("MUD",houston_pws_df$pwsName)] <- 'MUD'
houston_pws_df$TYPE[grepl("SUBDIVISION|CLUB&|ESTATES$",houston_pws_df$pwsName)] <- 'Private/Investor owned'
houston_pws_df$TYPE[grepl(" PUD | PUD$| UD | UD$|MWD|FWSD|UTILITY DISTRICT|WCID|IMPROVEMENT DISTRICT|TBCD| SUD |SUD$|SIENNA PLANTATION|WATER AUTHORITY",houston_pws_df$pwsName)] <- 'Other Water District'
houston_pws_df$TYPE[is.na(houston_pws_df$TYPE)] <- 'Private/Investor owned'

library(scales)
#show_col(colorblind_pal()(8))

figure1 = ggplot() + geom_sf(data = hout_counties,fill = 'grey90') + 
  geom_sf(data = houston_pws_df,aes(fill=TYPE),colour = 'grey50',lwd=0.05) + theme_map() + 
  scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
                    labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  theme(legend.position = c(.05,.05),panel.grid = element_line(colour = 'transparent'),
        legend.text = element_text(size = 10),legend.title = element_text(size = 12),title = element_text(size = 12)) + ggtitle('Public water system operators in Houston, TX MSA')


houston_mud_df =  houston_pws_df[ houston_pws_df$TYPE=='MUD',]
wdd_reference = fread('../bosque/input/twdd_records/district_list_2018-10-14.csv')
texas_water_sdwis = read_csv('../../google_drive/putah/input/sdwis_data/water_system_detail.csv') %>% filter(grepl('^TX',`PWS ID`),`Activity Status`!='Inactive')

houston_mud_df$District_ID = wdd_reference$District_ID[match(houston_mud_df$PWSId,wdd_reference$PWS_ID)]
houston_mud_df$Status_2018 = wdd_reference$Status[match(houston_mud_df$District_ID,wdd_reference$District_ID)]
houston_mud_df = houston_mud_df[houston_mud_df$District_ID %in% wdd_reference$District_ID,]
houston_mud_df$Water_Source = texas_water_sdwis$`Primary Source Code`[match(houston_mud_df$PWSId,texas_water_sdwis$`PWS ID`)]
houston_mud_df$BOARD_SELECTION = wdd_reference$BOARD_SELECTION[match(houston_mud_df$PWSId,wdd_reference$PWS_ID)]
houston_mud_df = houston_mud_df[houston_mud_df$Status_2018 != 'DELETED/DISSOLVED',]

district_services = read_csv('input/twdd_records/district_services_2018-10-16.csv')
houston_mud_df$RETAIL_WATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='SUPPLY TREATED OR RETAIL WATER'])+0
houston_mud_df$WHOLESALE_WATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='SUPPLY RAW (UNTREATED) OR WHOLESALE WATER'])+0
houston_mud_df$ROAD_BUILDING = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='ROAD POWERS'])+0
houston_mud_df$RETAIL_WASTEWATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='RETAIL WASTEWATER'])+0

houston_mud_df$SERVICE_OVERLAP = NA
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==0 & houston_mud_df$RETAIL_WASTEWATER==0] <- 'Drinking water only'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==0] <- 'Drinking water and road powers'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Drinking water, wastewater, and road powers'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==0 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Drinking water and wastewater'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==0 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Wastewater and road powers'
houston_mud_df$SERVICE_OVERLAP[is.na(houston_mud_df$SERVICE_OVERLAP)] <- 'Drinking water only'
houston_mud_df = houston_mud_df[houston_mud_df$SERVICE_OVERLAP!='Wastewater and road powers',]

houston_mud_df$SERVICE_OVERLAP = as.factor(houston_mud_df$SERVICE_OVERLAP)
library(forcats)
houston_mud_df$SERVICE_OVERLAP = fct_relevel(houston_mud_df$SERVICE_OVERLAP,'Drinking water only')

figure3 = ggplot() + geom_polygon(data = hout_counties.points[!hout_counties.points$COUNTYNAME%in%c('Austin','Waller','Liberty','Chambers'),],aes(x = long,y=lat,group=group),fill = 'grey90',col = 'grey30') + 
  geom_polygon(data = houston_mud_df,aes(y = lat,x=long,group =group,fill=SERVICE_OVERLAP),colour = 'grey50',lwd=0.05) + theme_map() + 
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_d(name = 'Combination of services') + 
  theme(legend.position = c(.55,.7),legend.text = element_text(size = 10),legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('MUDs acting as providers of drinking water and related services\n in Brazoria, Fort Bend, Galveston, Harris, and Montgomery counties')
figure3
library(statnet)
affils = read_csv('input/twdd_records/district_affiliates_2018-10-15.csv')
affils = affils[affils$District_ID %in% houston_mud_df$District_ID & !is.na(affils$Name),]
affil_mat = crossprod(as.matrix(table(affils$Name,affils$District_ID)) )
off_net = as.network(affil_mat,ignore.eval = F,names.eval = 'Overlap',directed=F)
off_graph = igraph::graph_from_adjacency_matrix(aj.socio,mode = 'undirected')
off_pagerank = igraph::page.rank(off_graph)

pagerank_df = data.frame(DISTRICT_ID = names(off_pagerank$vector),Pagerank_scaled = scale(off_pagerank$vector),stringsAsFactors = F)
houston_mud_df$Pagerank_scaled = pagerank_df$Pagerank_scaled[match(houston_mud_df$District_ID,pagerank_df$DISTRICT_ID)]

between_df = data.frame(Betweennes = scale(sna::betweenness(off_net)),District_ID  = network.vertex.names(off_net))
houston_mud_df$Betweenness = between_df$Betweennes[match(houston_mud_df$District_ID,between_df$District_ID)]

houston_pws_geo_cropped$District_ID = wdd_reference$District_ID[match(houston_pws_geo_cropped@data$PWSId,wdd_reference$PWS_ID)]

edge_df <- data.frame(
  district_centroids[match(network.vertex.names(off_net)[as.edgelist(off_net)[,1]],houston_pws_geo_cropped$District_ID),],
  district_centroids[match(network.vertex.names(off_net)[as.edgelist(off_net)[,2]],houston_pws_geo_cropped$District_ID),],
  houston_pws_geo_cropped$District_ID[match(network.vertex.names(off_net)[as.edgelist(off_net)[,1]],houston_pws_geo_cropped$District_ID)],
  houston_pws_geo_cropped$District_ID[match(network.vertex.names(off_net)[as.edgelist(off_net)[,2]],houston_pws_geo_cropped$District_ID)]
)
colnames(edge_df) <- c('x','y','xend','yend','x_pws_id','y_pws_id')

#bridging_scores = influenceR::bridging(off_graph)
#bridging_df = data.frame(SYSTEM_ID = as.character(names(bridging_scores)),
#                         BRIDGING_SCORE = c(bridging_scores))
#off_net %v% 'Bridging_Score' <- influenceR::bridging(off_graph)

figure2 = ggplot() + geom_polygon(data = hout_counties.points[!hout_counties.points$COUNTYNAME%in%c('Austin','Waller','Liberty','Chambers'),],aes(x = long,y=lat,group=group),fill = 'grey95',col = 'grey30') + 
  geom_segment(data = edge_df,aes(x = x,y = y,xend=xend,yend=yend),col = 'grey50',alpha = 0.25,lwd=0.025) +
  geom_polygon(data = houston_mud_df,aes(y = lat,x=long,group =group,fill=Betweenness),lwd=0.05) + theme_map() + 
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_c(name = 'Betweenness centrality',option = 'B',labels=c('3 (standard deviations)','2','1','0','-1 (standard deviations'),breaks = c(-1:3)) + 
  theme(legend.position = c(.7,.6),legend.text = element_text(size = 10),legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('Betweenness centrality in managerial network for\n MUDs in Brazoria, Fort Bend, Galveston, Harris, and Montgomery counties')
figure2



houston_mud_df$Water_Source[is.na(houston_mud_df$Water_Source)] <- 'GW'
figure4 = ggplot() + geom_polygon(data = hout_counties.points[!hout_counties.points$COUNTYNAME%in%c('Austin','Waller','Liberty','Chambers'),],aes(x = long,y=lat,group=group),fill = 'grey95',col = 'grey30') + 
  geom_polygon(data = houston_mud_df,aes(y = lat,x=long,group =group,fill=Water_Source),colour = 'grey40',lwd=0.05) + theme_map() + 
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_d(name = 'Betweenness centrality',option = 'C',labels = c('Self-sourced groundwater','Purchased groundwater','Self-sourced surface water','Purchased surface water')) + 
  theme(legend.position = c(.65,.6),legend.text = element_text(size = 10),legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('Water source type for MUDs\n in Brazoria, Fort Bend, Galveston, Harris, and Montgomery counties')

?element_text




houston_mud_df$

grep('CITY OF HOUSTON',texas_pws_geo@data$pwsName,value=T)

grep('CARVERDALE',texas_ccn_geo$UTILITY,value=T)

table(texas_water$`PWS ID` %in% test$PWSId)
tt = texas_water[!texas_water$`PWS ID` %in% test$PWSId,]

tt$`PWS Name`[!tt$`PWS Name` %in% texas_ccn_geo$UTILITY]
test@data$PWSId
plot(test[test@data$PWSId=='TX0790573',],col='red')


"td.iwud , th"
  