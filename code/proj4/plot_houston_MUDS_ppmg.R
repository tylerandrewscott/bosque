library(sf)
library(data.table)
library(tidyverse)
library(lwgeom)
library(ggthemes)
###########################

houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = st_read('../bosque/spatial_inputs/government_units/county_nrcs_a_tx.shp')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]


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

figure2 = ggplot() + geom_sf(data = hout_counties,fill = 'grey90') + 
  geom_sf(data = houston_pws_df,aes(fill=TYPE),colour = 'grey50',lwd=0.05) + 
  theme_map() + 
  scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
                    labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  theme(legend.position = c(.05,.05),panel.grid = element_line(colour = 'transparent'),
        text = element_text(family = 'Times'),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),title = element_text(size = 12)) + ggtitle('Public water system operators in Houston, TX MSA')
ggsave(figure2,width = 7.5,height=7.5,units = 'in',dpi = 450,filename = 'Output/figure2.tiff')


houston_mud_df =  houston_pws_df[ houston_pws_df$TYPE=='MUD'|grepl('MUD|MUNICIPAL UTILITY DISTRICT',houston_pws_df$pwsName),]
wdd_reference = fread('../bosque/input/twdd_records/district_list_2018-10-14.csv')
texas_water_sdwis = read_csv('../../google_drive/putah/input/sdwis_data/water_system_detail.csv') %>% filter(grepl('^TX',`PWS ID`),`Activity Status`!='Inactive')

houston_mud_df$District_ID = wdd_reference$District_ID[match(houston_mud_df$PWSId,wdd_reference$PWS_ID)]
houston_mud_df$Status_2018 = wdd_reference$Status[match(houston_mud_df$District_ID,wdd_reference$District_ID)]
houston_mud_df = houston_mud_df[houston_mud_df$District_ID %in% wdd_reference$District_ID,]
houston_mud_df$Water_Source = texas_water_sdwis$`Primary Source Code`[match(houston_mud_df$PWSId,texas_water_sdwis$`PWS ID`)]
houston_mud_df$BOARD_SELECTION = wdd_reference$BOARD_SELECTION[match(houston_mud_df$PWSId,wdd_reference$PWS_ID)]
houston_mud_df = houston_mud_df[houston_mud_df$Status_2018 != 'DELETED/DISSOLVED',]

district_services = read_csv('../bosque/input/twdd_records/district_services_2018-10-16.csv')
houston_mud_df$RETAIL_WATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='SUPPLY TREATED OR RETAIL WATER'])+0
houston_mud_df$WHOLESALE_WATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='SUPPLY RAW (UNTREATED) OR WHOLESALE WATER'])+0
houston_mud_df$ROAD_BUILDING = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='ROAD POWERS'])+0
houston_mud_df$RETAIL_WASTEWATER = (houston_mud_df$District_ID %in% district_services$District_ID[district_services$Function=='RETAIL WASTEWATER'])+0

houston_mud_df$SERVICE_OVERLAP = NA
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==0 & houston_mud_df$RETAIL_WASTEWATER==0] <- 'Drinking water'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==0] <- 'Drinking water+road powers'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Drinking water+wastewater+road powers'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==1 & houston_mud_df$ROAD_BUILDING==0 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Drinking water+wastewater'
houston_mud_df$SERVICE_OVERLAP[houston_mud_df$RETAIL_WATER==0 & houston_mud_df$ROAD_BUILDING==1 & houston_mud_df$RETAIL_WASTEWATER==1] <- 'Wastewater+road powers'
houston_mud_df$SERVICE_OVERLAP[is.na(houston_mud_df$SERVICE_OVERLAP)] <- 'Drinking water'
houston_mud_df = houston_mud_df[houston_mud_df$SERVICE_OVERLAP!='Wastewater+road powers',]

houston_mud_df$SERVICE_OVERLAP = as.factor(houston_mud_df$SERVICE_OVERLAP)
library(forcats)
houston_mud_df$SERVICE_OVERLAP = fct_relevel(houston_mud_df$SERVICE_OVERLAP,'Drinking water')
houston_mud_df$BOARD_SELECTION[is.na(houston_mud_df$BOARD_SELECTION)] <- 'Elected'



sub = hout_counties[!hout_counties$COUNTYNAME%in%c('Austin','Waller','Liberty','Chambers'),]
sub = st_crop(sub,sub)

figure4 = ggplot() + 
  geom_sf(data = sub,fill = 'grey90',col = 'grey30') + 
  geom_sf(data = st_crop(houston_mud_df,sub),aes(fill=SERVICE_OVERLAP),colour = 'grey50',lwd=0.05) + theme_map() + 
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_d(name = 'Combination of services') + 
  theme(legend.position = c(.52,0.05), panel.grid = element_line(colour = 'transparent'),
        text = element_text(family = 'Times'),axis.text = element_blank(),axis.ticks = element_blank(),
        legend.background=element_rect(fill = alpha('white',0.7),colour = 'transparent'),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('MUDs providing drinking water, related services\n in Brazoria, Fort Bend, Galveston, Harris, and Montgomery counties')
ggsave(figure4,width = 7,height=7,units = 'in',dpi = 450,filename = 'Output/figure4.tiff')


library(statnet)
affils = read_csv('../bosque/input/twdd_records/district_affiliates_2018-10-15.csv')
affils = affils[affils$District_ID %in% houston_mud_df$District_ID & !is.na(affils$Name),]
affil_mat = crossprod(as.matrix(table(affils$Name,affils$District_ID)))
off_net = as.network(affil_mat,ignore.eval = F,names.eval = 'Overlap',directed=F)
#off_graph = igraph::graph_from_adjacency_matrix(aj.socio,mode = 'undirected')
#off_pagerank = igraph::page.rank(off_graph)

#pagerank_df = data.frame(DISTRICT_ID = names(off_pagerank$vector),Pagerank_scaled = scale(off_pagerank$vector),stringsAsFactors = F)
#houston_mud_df$Pagerank_scaled = pagerank_df$Pagerank_scaled[match(houston_mud_df$District_ID,pagerank_df$DISTRICT_ID)]

between_df = data.frame(Betweennes = scale(sna::betweenness(off_net)),District_ID  = network.vertex.names(off_net))
houston_mud_df$Betweenness = between_df$Betweennes[match(houston_mud_df$District_ID,between_df$District_ID)]

houston_pws_geo_cropped$District_ID = wdd_reference$District_ID[match(houston_pws_geo_cropped$PWSId,wdd_reference$PWS_ID)]

t1 = district_centroids[match(network.vertex.names(off_net)[as.edgelist(off_net)[,1]],houston_pws_geo_cropped$District_ID),]
t2 = district_centroids[match(network.vertex.names(off_net)[as.edgelist(off_net)[,2]],houston_pws_geo_cropped$District_ID),]

library(pbapply)
line_list = pblapply(1:nrow(t2),function(x){
  st_coordinates(rbind(t1[x,],t2[x,]))},cl = 8)
service_lines = st_multilinestring(line_list,dim = "XY")
lines_sfc = st_sfc(service_lines,crs = st_crs(houston_pws_geo_cropped))

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

figure3 = ggplot() + 
  geom_sf(data = sub,fill = 'grey95',col = 'grey30') + 
  geom_sf(data = lines_sfc,alpha = 0.25,lwd=0.025,col = 'grey50')+
  geom_sf(data = st_crop(houston_mud_df,sub),aes(fill=Betweenness),lwd=0.05) +
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_c(name = 'Betweenness centrality',option = 'B',labels=c('3 (std. dev.)','2','1','0','-1 (std. dev.)'),breaks = c(-1:3)) + 
  theme_tufte(ticks=F) +
  theme(legend.position = c(.8,.8), axis.text = element_blank(),axis.ticks = element_blank(),
        text = element_text(family = 'Times'),   legend.background=element_rect(fill = alpha('white',0.7),colour = 'transparent'),
        panel.grid = element_line(colour = 'transparent'),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('Betweenness centrality, MUD managerial network\n in Brazoria, Fort Bend, Galveston, Harris, and Montgomery counties')

ggsave(figure3,width = 7,height=7,units = 'in',dpi = 450,filename = 'Output/figure3.tiff')


houston_mud_df$Water_Source[is.na(houston_mud_df$Water_Source)] <- 'GW'
100 * table(houston_mud_df$Water_Source) / nrow(houston_mud_df)
table(houston_mud_df$Water_Source)
figure5 = ggplot() + geom_sf(data = sub,fill = 'grey95',col = 'grey30') + 
  geom_sf(data = houston_mud_df,aes(fill=Water_Source),colour = 'grey40',lwd=0.05) +
  #scale_fill_manual(values = c('#E69F00','#56B4E9','#D55E00','#CC79A7','#009E73'),name = 'Provider type',
  #                  labels = c('Municipal Utility District','Municipality','Other water district','Private/investor-owned','Water Service Corporation')) + 
  scale_fill_viridis_d(name = 'Water source',option = 'C',labels = c('Self-sourced groundwater','Purchased groundwater','Self-sourced surface water','Purchased surface water')) + 
  theme_tufte(ticks=F) + 
  theme(legend.position = c(.8,0.2),panel.grid = element_line(colour = 'transparent'),
        legend.background=element_rect(fill = alpha('white',0.7),colour = 'transparent'),
        text=element_text(family = 'Times'),axis.ticks = element_blank(),axis.text= element_blank(),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),title = element_text(size = 12)) + 
  ggtitle('Water source type for MUDs in Brazoria,\n Fort Bend, Galveston, Harris, and Montgomery counties')

ggsave(figure5,width = 7,height=7,units = 'in',dpi = 450,filename = 'Output/figure5.tiff')


