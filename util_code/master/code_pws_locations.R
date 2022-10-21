library(tidyverse)
library(statnet)
library(parallel)
library(pbapply)
library(btergm)
library(ergm.terms.contrib)
library(ergm.userterms)

# system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
#   filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
#   #filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','MONTGOMERY','FORT BEND')) %>%
#   #        'WASHINGTON','SAN JACINTO','POLK','TYLER','HARDIN','JEFFERSON','ORANGE','JASPER','NEWTON','WALKER','GRIMES')) %>%
#   #filter(County %in% c('ATASCOSA','BANDERA','BEXAR','COMAL','GUADALUPE','KENDALL','MEDINA','WILSON')) %>%
#   #filter(County %in% c("BISHOP",'CALDWELL','HAYS','TRAVIS','WILLIAMSON')) %>%
#   #filter(County %in% c("WISE",'COLLIN','DALLAS','ELLIS','HOOD','HUNT','JOHNSON','KAUFMAN','PARKER','ROCKWALL','SOMERVELL','TARRANT')) %>%
#   rename(PWS_NAME = Name)

#### Load system summary from EPA
system_df =  read_csv('input/epa_sdwis/texas_cws_sdwis_summary_master.csv') %>% 
  filter(!grepl('DETENTIO|ACADEMY',`PWS Name`)) %>% 
  filter(!grepl('BOYS & GIRLS',`PWS Name`)) %>%
  rename(PWS_ID = `PWS ID`,PWS_Type = `PWS Type`,PWS_NAME = `PWS Name`,PWS_ID = `PWS ID`) %>%
  filter(`Owner Type` != 'State government') %>% 
  mutate(County = toupper(`Counties Served`)) %>% 
  #dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) %>%
  filter(`Population Served Count`!=0) %>%
  filter(!grepl('SHERIFF|ACADEMY|MONTESSORI',PWS_NAME)) %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER'))

#system_df  = system_df %>% filter(!grepl('CITY|TOWN ',system_df$PWS_NAME))
system_df$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT',"UD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('MUNICIPAL MANAGEMENT DIST ',"MMD ",system_df$PWS_NAME)
system_df$PWS_NAME[grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME)] = 
  str_extract(grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME,value=T),'FORT BEND COUNTY MUD [0-9]{1,}[A-Z]{0,2}')
system_df$PWS_NAME = gsub('HCO ',"HARRIS COUNTY ",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' NO '," ",system_df$PWS_NAME)
#system_df$PWS_NAME = gsub(' MH | MOBILE HOME '," ",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 24 COUNTRY COLONY","MONTGOMERY COUNTY MUD 24",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 6 CARRIAGE LANE","HARRIS COUNTY MUD 6",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 119 SPRING TRAILS","MONTGOMERY COUNTY MUD 119",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 400   WEST","HARRIS COUNTY MUD 400",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 55 HERITAGE PARK","HARRIS COUNTY MUD 55",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 148 KINGSLAKE","HARRIS COUNTY MUD 148",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 374 CYPRESS CREEK LAKE","HARRIS COUNTY MUD 374",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("WEST HARRIS COUNTY MUD 2 CHASE","WEST HARRIS COUNTY MUD 2",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('WATER SUPPLY CORPORATION','WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('REGIONAL WATER AUTHOR$|REGIONAL WATER AUTHORITY','RWA',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('HARRIS COUNTY MUD 400 - WEST','HARRIS COUNTY MUD 400',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 113 ENCHANTED VILLAGE","HARRIS COUNTY WCID 113",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THE WOODLANDS",'WOODLANDS',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('BRAZORIA COUNTY FWSD 1 DAMON','BRAZORIA COUNTY FWSD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD1",'MUD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD3",'MUD 3',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THUNDERBIRD UD 1","THUNDERBIRD UD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CYPRESS KLEIN UD WIMBLETON","CYPRESS KLEIN MUD",system_df$PWS_NAME)
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
system_df$PWS_NAME[system_df$PWS_NAME=='H O E WSC'] <- 'HOE WSC'

system_df$GCD_ZONE = NA
system_df$GCD_ZONE[system_df$County=='BRAZORIA'] <- 'Brazoria County GCD'
system_df$GCD_ZONE[system_df$County %in% c('HARRIS','GALVESTON')] <- 'Harris-Galveston Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('FORT BEND')] <- 'Fort Bend Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('AUSTIN','WALLER')] <- 'Bluebonnet GCD'
system_df$GCD_ZONE[system_df$County %in% c('MONTGOMERY')] <- 'Lone Star GCD'
system_df$GCD_ZONE[is.na(system_df$GCD_ZONE)]<-'NONE'
system_df$x = system_df$y = NA

system_df$`Address Line2`[system_df$`Address Line2`=='-'] <- ''
system_df = 
  system_df %>% mutate(address_query = paste(`Address Line1`,`Address Line2`,`City Name`,', TX',`Zip Code`))

library(sp)
library(maptools)
library(cleangeo)
library(rgdal)

twd = readOGR('spatial_inputs/government_units','TCEQ_WATER_DISTRICTS')
twd$NAME = toupper(twd$NAME)
twd$NAME = gsub('-',' ',twd$NAME)
twd$NAME = gsub('DRISTRICT','DISTRICT',twd$NAME)
twd$NAME = gsub('VILLIAGES','VILLAGES',twd$NAME)
twd$NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",twd$NAME)
twd$NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",twd$NAME)
twd$NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",twd$NAME)
twd$NAME = gsub('UTILITY DISTRICT',"UD",twd$NAME)
twd$NAME[twd$NAME=="POST WOOD MUD"] <- "POSTWOOD MUD"
twd$NAME[twd$NAME=="CLOVERCREEK MUD"] <- "CLOVER CREEK MUD"
twd$NAME[twd$NAME=="FORT BEND COUNTY MUD 134 A"] <- "FORT BEND COUNTY MUD 134A"
twd$NAME[twd$NAME=="FORT BEND COUNTY MUD 134 B"] <- "FORT BEND COUNTY MUD 134B"
twd$NAME[twd$NAME=="FORT BEND COUNTY MUD 134 C"] <- "FORT BEND COUNTY MUD 134C"
twd$NAME[twd$NAME=="FORT BEND COUNTY MUD 134 D"] <- "FORT BEND COUNTY MUD 134D"
twd$NAME[twd$NAME=="HARRIS COUNTY FWSD 1 B"] <- "HARRIS COUNTY FWSD 1B"
twd$NAME[twd$NAME=="CYPRESS KLEIN UTILTIY DISTRICT"] <- "CYPRESS KLEIN MUD"
twd$NAME[twd$NAME=="HARRIS COUNTY FWSD 52"] <- "CHAMPIONS MUD"
twd$NAME[twd$NAME=="HARRIS COUNTY MUD 18"] <- "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
report = cleangeo::clgeo_CollectionReport(twd)
twd = twd[report$valid,]
twd_centroid = gCentroid(twd,byid = T)


tpuc = readOGR('spatial_inputs/texas_puc','PUC_CCN_WATER')
tpuc <- spTransform(tpuc,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(tpuc)
tpuc = tpuc[report$valid,]
tpuc@data$NAME = as.character(tpuc@data$UTILITY)
tpuc_centroid <- gCentroid(tpuc,byid = T)
tpuc$NAME[tpuc$NAME=="ENCHANTED VALLEY WSC"] <- "ENCHANTED VALLEY ESTATES WSC"
tpuc$NAME[tpuc$NAME=="WALNUT COVE INC"] <- "WALNUT COVE WSC"
tpuc$NAME[tpuc$NAME=="PINE LAKE WSC INC"] <- "PINE LAKE SUBDIVISION NORTH WSC"
tpuc$NAME[tpuc$NAME=="WOODLAND LAKES WSC"] <- "WOODLAND LAKES ESTATES WSC"
tpuc$NAME[tpuc$NAME=="FRONTIER WATER COOMPANY"] <- "FRONTIER WATER CO"
tpuc$NAME[tpuc$NAME=="BATEMAN WATERWORKS"] <- "BATEMAN WATER WORKS"
tpuc$NAME[tpuc$NAME=="P & B WATER CORPORATION"] <- "P & B WATER SYSTEM"
tpuc$NAME[tpuc$NAME=="R & K WEIMAN MHP"] <- "R&K WEIMAN MHP"
tpuc$NAME[tpuc$NAME=="CC WATER WORKS" ] <- "C & C MOBILE HOME COMMUNITY"
tpuc$NAME[tpuc$NAME=="NITSCH AND SON UTILITY" ] <- "NITSCH & SON UTILITY"
tpuc$NAME = gsub(' INC$','',tpuc$NAME)

hgc = readOGR("spatial_inputs/hg_gis","service_boundaries")
hgc = spTransform(hgc,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(hgc)
hgc = hgc[report$valid,]
hgc$NAME = toupper(hgc$PERMIT_HOL)
hgc$NAME = gsub('NW HC','NORTHWEST HARRIS COUNTY',hgc$NAME)
hgc$NAME = gsub('WHC|W HC|WEST HC','WEST HARRIS COUNTY',hgc$NAME)
hgc$NAME = gsub(' HC ',' HARRIS COUNTY ',hgc$NAME)
hgc$NAME[grep('\\, CITY OF',hgc$NAME)] <- paste0('CITY OF ',hgc$NAME[grep('\\, CITY OF',hgc$NAME)])
hgc$NAME[grep('\\, CITY$',hgc$NAME)] <- paste0('CITY OF ',hgc$NAME[grep('\\, CITY$',hgc$NAME)])
hgc$NAME = gsub('\\,.*','',hgc$NAME)
hgc_centroid = gCentroid(hgc,byid=T)

hgc2 = readOGR("spatial_inputs/hg_gis","other_districts")
hgc2  = spTransform(hgc2,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(hgc2)
hgc2 = hgc2[report$valid,]
hgc2$NAME = toupper(hgc2$NAME)
hgc2_centroid = gCentroid(hgc2,byid=T)
hgc2_centroid = spTransform(hgc2_centroid,CRS(proj4string(twd)))

bz = readOGR("spatial_inputs/county_mud_gis",'brazoria')
bz = spTransform(bz,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(bz)
bz = bz[report$valid,]
bz$NAME = toupper(gsub('#','',bz$Name))
bz_centroid = gCentroid(bz,byid=T)

mg = readOGR("spatial_inputs/county_mud_gis",'montgomery')
mg = spTransform(mg,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(mg)
mg = mg[report$valid,]
mg$NAME = toupper(gsub(' 0(?=[0-9])',' ',gsub(' No ',' ',gsub(" No(?=[0-9])",' ',gsub('#','',mg$MUD_NAME),perl=T),perl=T),perl=T))
mg_centroid = gCentroid(mg,byid=T)

places = readOGR('spatial_inputs/government_units','cb_2015_48_place_500k')
places = spTransform(places,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(places)
places = places[report$valid,]
places$NAME <- toupper(places$NAME)
places$NAME <- gsub("\\'",'',places$NAME)
place_coords = coordinates(gCentroid(places,byid=T))



invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% twd@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(twd_centroid[match(system_df$PWS_NAME[i],twd@data$NAME,nomatch = 0)])}}))
invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% tpuc@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(tpuc_centroid[match(system_df$PWS_NAME[i],tpuc@data$NAME,nomatch = 0)])}}))
invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% hgc@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(hgc_centroid[match(system_df$PWS_NAME[i],hgc@data$NAME,nomatch = 0),])}}))
invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% hgc2@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(hgc_centroid[match(system_df$PWS_NAME[i],hgc2@data$NAME,nomatch = 0),])}}))
invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% bz@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(bz_centroid[match(system_df$PWS_NAME[i],bz@data$NAME,nomatch = 0),])}}))
invisible(sapply(1:nrow(system_df),function(i) {if(system_df$PWS_NAME[i] %in% mg@data$NAME & is.na(system_df$x[i])){system_df[i,c('x','y')] <<- coordinates(mg_centroid[match(system_df$PWS_NAME[i],mg@data$NAME,nomatch = 0),])}}))

system_df[is.na(system_df$x),c('x','y')] <- place_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0('CITY OF ', places$NAME),nomatch = NA),]        

system_df[grep("NORTHEAST HARRIS COUNTY MUD 1 ",system_df$PWS_NAME),c('x','y')] <- t(sapply(1:sum(grepl("NORTHEAST HARRIS COUNTY MUD 1 ",system_df$PWS_NAME)),function(x) coordinates(twd_centroid[twd$NAME=="NORTHEAST HARRIS COUNTY MUD 1"])))
system_df[grep("HARRIS MONTGOMERY COUNTIES MUD 386 ",system_df$PWS_NAME),c('x','y')] <- coordinates(twd_centroid[twd$NAME=="HARRIS MONTGOMERY COUNTIES MUD 386"])
system_df[grep("WOODLANDS MUD 1",system_df$PWS_NAME),c('x','y')] <- coordinates(gCentroid(twd_centroid[grepl("MONTGOMERY COUNTY MUD 40|THE WOODLANDS MUD 2",twd$NAME),]))
system_df[grep("CITY OF PEARLAND MUD 1",system_df$PWS_NAME),c('x','y')] <- coordinates(gCentroid(places[grepl("PEARLAND",places$NAME),]))
system_df[grep("ROSHARON TOWNSHIP",system_df$PWS_NAME),c('x','y')] <- coordinates(gCentroid(places[grepl("ROSHARON",places$NAME),]))




harris_sub = readOGR('spatial_inputs/harris','Sub_poly')
harris_sub = spTransform(harris_sub,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(harris_sub)
harris_sub = harris_sub[report$valid,]
harris_sub$NAME <- toupper(harris_sub$SUB_NAME)
harris_sub$NAME <- gsub("\\'",'',harris_sub$NAME)
harris_sub$NAME <- gsub(' U\\/R$| R\\/P$','',harris_sub$NAME)
harris_sub$NAME[harris_sub$NAME=='STABLE GATE'] <- 'STABLE GATES'
harris_sub$NAME[harris_sub$NAME=='MCGEE'] <- 'MCGEE PLACE'
harris_sub$NAME[harris_sub$NAME=="TIMBER CREEK" ] <- "TIMBER CREEK ESTATES" 
harris_sub$NAME[harris_sub$NAME=="HOLLY LAKES ESTATES" ] <- "ESTATES OF HOLLY LAKES"
harris_sub$NAME[harris_sub$NAME=="CYPRESS CROSSING MOBILE HOME PARK" ] <-"CYPRESS CROSSING"
harris_sub$NAME[harris_sub$NAME=="GRANT ROAD ESTATES U" ] <-"GRANT ROAD ESTATES MOBILE HOME SUB"
harris_sub_coords = coordinates(gCentroid(harris_sub,byid=T))
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],harris_sub$NAME,nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MH SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MOBILE HOME SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MOBILE HOME SUB'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MHP'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MOBILE HOME PARK'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MOBILE HOME COMMUNITY'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' MOBILE HOME COMM'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' HOMEOWNERS ASSOCIATION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- harris_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(harris_sub$NAME,' ESTATES'),nomatch = NA),]


montgomery_sub = readOGR('spatial_inputs/montgomery','Subdivision_Boundary')
montgomery_sub = spTransform(montgomery_sub,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(montgomery_sub)
montgomery_sub = montgomery_sub[report$valid,]
montgomery_sub$NAME <- toupper(montgomery_sub$Sub_Name)
montgomery_sub$NAME <- gsub("\\'",'',montgomery_sub$NAME)
montgomery_sub$NAME <- gsub(' U\\/R$| R\\/P$','',montgomery_sub$NAME)
montgomery_sub$NAME[montgomery_sub$NAME=='STABLE GATE'] <- 'STABLE GATES'
montgomery_sub$NAME[montgomery_sub$NAME=='MCGEE'] <- 'MCGEE PLACE'
montgomery_sub$NAME[montgomery_sub$NAME=="TIMBER CREEK" ] <- "TIMBER CREEK ESTATES" 
montgomery_sub_coords = coordinates(gCentroid(montgomery_sub,byid=T))
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],montgomery_sub$NAME,nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MH SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME SUB'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME SUB'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MHP'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME PARK'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME COMMUNITY'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' MOBILE HOME COMM'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' HOMEOWNERS ASSOCIATION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- montgomery_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(montgomery_sub$NAME,' ESTATES'),nomatch = NA),]


brazoria_sub = readOGR('spatial_inputs/brazoria','SUBDIVISIONS')
brazoria_sub = spTransform(brazoria_sub,CRS(proj4string(twd)))
report = cleangeo::clgeo_CollectionReport(brazoria_sub)
brazoria_sub = brazoria_sub[report$valid,]
brazoria_sub$NAME <- toupper(brazoria_sub$NAME)
brazoria_sub$NAME <- gsub("\\'",'',brazoria_sub$NAME)
brazoria_sub$NAME <- gsub(' U\\/R$| R\\/P$','',brazoria_sub$NAME)
brazoria_sub_coords = coordinates(gCentroid(brazoria_sub,byid=T))
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],brazoria_sub$NAME,nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MH SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME SUBDIVISION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME SUB'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME SUB'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MHP'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME PARK'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME COMMUNITY'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' MOBILE HOME COMM'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' HOMEOWNERS ASSOCIATION'),nomatch = NA),]
system_df[is.na(system_df$x),c('x','y')] <- brazoria_sub_coords[match(system_df$PWS_NAME[is.na(system_df$x)],paste0(brazoria_sub$NAME,' ESTATES'),nomatch = NA),]



system_df[is.na(system_df$x)&grepl('SUNBELT FWSD',system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl('SUNBELT FWSD',system_df$PWS_NAME)),function(x) coordinates(gCentroid(twd[twd@data$NAME=='SUNBELT FWSD',])),simplify=F))
system_df[is.na(system_df$x)&grepl('RIMWICK FOREST',system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl('RIMWICK FOREST',system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep('RIMWICK FOREST',montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl('ALLENWOOD SUBDIVISION',system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl('ALLENWOOD SUBDIVISION',system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep('ALLENWOOD 0',montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("HUNTERS RETREAT",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("HUNTERS RETREAT",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("HUNTERS RETREAT",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("KIPLING OAKS [0-9]",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("KIPLING OAKS [0-9]",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("KIPLING OAKS [0-9]",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("TIMBERGREEN",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("TIMBERGREEN",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("TIMBERGREEN",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("ARMADILLO",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("ARMADILLO",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("ARMADILLO",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("TOWERING OAKS",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("TOWERING OAKS",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("TOWERING OAKS",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("DEER RIDGE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("DEER RIDGE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("DEER RIDGE",montgomery_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("AUSTIN COUNTY WSC",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("AUSTIN COUNTY WSC",system_df$PWS_NAME)),function(x) coordinates(gCentroid(tpuc[grep("AUSTIN COUNTY WSC",tpuc@data$NAME),])),simplify=F))



system_df[is.na(system_df$x)&grepl("TBCD",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("TBCD",system_df$PWS_NAME)),function(x) coordinates(gCentroid(twd[grep("TRINITY BAY CONSERVATION DISTRICT",twd@data$NAME),])),simplify=F))
system_df[grepl("G & W WSC ",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("G & W WSC ",system_df$PWS_NAME)),function(x) coordinates(gCentroid(tpuc[grep("G & W WSC",tpuc@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("HUNTERS COVE SEC 1",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("HUNTERS COVE SEC 1",system_df$PWS_NAME)),function(x) coordinates(gCentroid(tpuc[grep("HUNTERS COV",tpuc@data$NAME),])),simplify=F))


system_df[is.na(system_df$x)&grepl("VILLAGE OF SURFSIDE BEACH",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("VILLAGE OF SURFSIDE BEACH",system_df$PWS_NAME)),function(x) coordinates(gCentroid(places[grep("SURFSIDE",places@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("CITY OF SUGAR LAND ",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("CITY OF SUGAR LAND ",system_df$PWS_NAME)),function(x) coordinates(gCentroid(places[grep("SUGAR LAND",places@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("CITY OF FREEPORT ",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("CITY OF FREEPORT ",system_df$PWS_NAME)),function(x) coordinates(gCentroid(places[grep("FREEPORT",places@data$NAME),])),simplify=F))




system_df[is.na(system_df$x)&grepl("THUNDERBIRD UD SYSTEM 2",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("THUNDERBIRD UD SYSTEM 2",system_df$PWS_NAME)),function(x) coordinates(gCentroid(twd[grep("THUNDERBIRD",twd@data$NAME),])),simplify=F))

system_df[is.na(system_df$x)&grepl("^CYPRESS VILLAGE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^CYPRESS VILLAGE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(harris_sub[grep("^CYPRESS VILLAGE",harris_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("ORANGE GROVE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("ORANGE GROVE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(tpuc[grep("ORANGE GROVE",tpuc@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("PALOMA ACRES",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("PALOMA ACRES",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("PALOMA ACRES",brazoria_sub@data$NAME),])),simplify=F))


system_df[is.na(system_df$x)&grepl("ROBIN COVE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("ROBIN COVE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("ROBIN COVE",brazoria_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("COUNTRY OAKS",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("COUNTRY OAKS",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("COUNTRY OAKS",brazoria_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("^WOOD OAKS",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^WOOD OAKS",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("^WOOD OAKS",brazoria_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("^LEE RIDGE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^LEE RIDGE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("^LEE RIDGE",brazoria_sub@data$NAME),])),simplify=F))

system_df[is.na(system_df$x)&grepl("^SNUG HARBOR",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^SNUG HARBOR",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("^SNUG HARBOR",brazoria_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("^MEADOWVIEW",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^MEADOWVIEW",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("^MEADOWVIEW",brazoria_sub@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("^ROYAL RIDGE",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("^ROYAL RIDGE",system_df$PWS_NAME)),function(x) coordinates(gCentroid(brazoria_sub[grep("^ROYAL RIDGE",brazoria_sub@data$NAME),])),simplify=F))

system_df[is.na(system_df$x)&grepl("SUN RANCH",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("SUN RANCH",system_df$PWS_NAME)),function(x) coordinates(gCentroid(tpuc[grep("SUN RANCH",tpuc@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("CITY OF MISSOURI CITY MUSTANG",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("CITY OF MISSOURI CITY MUSTANG",system_df$PWS_NAME)),function(x) coordinates(gCentroid(places[grep("MISSOURI",places@data$NAME),])),simplify=F))
system_df[is.na(system_df$x)&grepl("ARROWHEAD LAKE & FRONTIER",system_df$PWS_NAME),c('x','y')] <- do.call(rbind,sapply(1:sum(grepl("ARROWHEAD LAKE & FRONTIER",system_df$PWS_NAME)),function(x) coordinates(gCentroid(montgomery_sub[grep("ARROWHEAD LAKES |FRONTIER LAKES ",montgomery_sub@data$NAME),])),simplify=F))



library(ggmap)
library(stringr)
# geo_other= cbind(PWS_NAME = system_df$PWS_NAME[is.na(system_df$x)],
#                    geocode(paste(gsub(' WATER SYSTEM',' ',system_df$PWS_NAME[is.na(system_df$x)]),', ',
#                                  str_to_title(system_df$County[is.na(system_df$x)]),' County, TX',sep=''),source = 'google'))
# system_df$x[is.na(system_df$x)] <- geo_other$lon[match(system_df$PWS_NAME[is.na(system_df$x)],geo_other$PWS_NAME)]
# system_df$y[is.na(system_df$y)] <- geo_other$lat[match(system_df$PWS_NAME[is.na(system_df$y)],geo_other$PWS_NAME)]
# 
# locs <- cbind(PWS_ID = system_df$PWS_ID[is.na(system_df$x)],
#               geocode(system_df$address_query[is.na(system_df$x)]))
# system_df[is.na(system_df$x),c('x','y')] <- locs[,c('lon','lat')]

summary(system_df[,c('x','y')])
write.csv(system_df,'scratch/system_with_lat_lon.csv')
