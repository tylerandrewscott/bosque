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
#library(esri2sf)
library(rgeos)
require(spdep)

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

#https://www3.twdb.texas.gov/apps/waterserviceboundaries

twd_boundaries = st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile/PWS_Export.shp')
twd_boundaries = twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries = st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries = st_make_valid(twd_boundaries)
twd_boundaries$District_ID  <- id_crosswalk$District_ID[match(twd_boundaries$PWS_ID,id_crosswalk$PWS_ID)]
twd_boundaries <- twd_boundaries[!is.na(twd_boundaries$District_ID),]
twd_boundaries_combo <- twd_boundaries  %>% 
  st_set_precision(10000) %>% 
  group_by(District_ID) %>% 
  summarise()

tceq_geojson='https://opendata.arcgis.com/datasets/e7f6dd0a88c046fba1f54d440941a061_0.geojson'
wd = st_read(tceq_geojson)
wd = st_make_valid(wd)
#albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
wd = st_transform(wd,st_crs(albersNA))
wd$District_ID <- as.character(wd$DISTRICT_I)
wd_simple <- wd[!wd$DISTRICT_ID %in% twd_boundaries_combo$District_ID,c('DISTRICT_ID')]
wd_simple <- wd_simple[!duplicated(wd_simple$DISTRICT_ID),]
setnames(wd_simple,'DISTRICT_ID','District_ID')
district_sf = rbind(twd_boundaries_combo,wd_simple)
district_sf$total_area <- st_area(district_sf)
district_sf <- district_sf[st_is_valid(district_sf),]

library(lwgeom)
tx_tracts2010 = tigris::tracts(state = 'TX',class='sf',year = 2010)
tx_tracts2020 = tigris::tracts(state = 'TX',class='sf',year = 2020)
tx_tracts2010 = st_transform(tx_tracts2010,st_crs(albersNA))
tx_tracts2020 = st_transform(tx_tracts2020,st_crs(albersNA))
tx_tracts2010 = st_make_valid(tx_tracts2010)
tx_tracts2020 = st_make_valid(tx_tracts2020)
#tx_county = st_read('https://opendata.arcgis.com/datasets/8b902883539a416780440ef009b3f80f_0.geojson')
tx_county = tigris::counties(state = 'TX',class = 'sf')
tx_county <- st_transform(tx_county,albersNA)
tx_county <- st_make_valid(tx_county)
district_over_tracts_2010 = st_intersection(district_sf,tx_tracts2010)
district_over_tracts_2020 = st_intersection(district_sf,tx_tracts2020)

tract_overs2010 = data.table(District_ID = district_over_tracts_2010$District_ID,GEOID10 = district_over_tracts_2010$GEOID10,
                             Prop_Of_Tract = as.numeric(st_area(district_over_tracts_2010)/
                                                          st_area(tx_tracts2010)[match(district_over_tracts_2010$GEOID10,tx_tracts2010$GEOID10)]))
tract_overs2020 = data.table(District_ID = district_over_tracts_2020$District_ID,GEOID10 = district_over_tracts_2020$GEOID10,
                             Prop_Of_Tract = as.numeric(st_area(district_over_tracts_2020)/
                                                          st_area(tx_tracts2020)[match(district_over_tracts_2020$GEOID,tx_tracts2020$GEOID)]))

tract_overs <- list('tracts_2010' = tract_overs2010,'tracts_2020'=tract_overs2020)
saveRDS(tract_overs,'drought_and_debt/input/district_tract_overlaps.RDS')


district_over_counties = st_intersection(twd_boundaries,tx_county)
district_over_counties = district_over_counties %>% rename(CFIPS = GEOID)

county_overs = data.table(District_ID = district_over_counties$District_ID,CFIPS = district_over_counties$CFIPS,
                          Prop_Over_County = as.numeric(st_area(district_over_counties)/st_area(twd_boundaries)[match(district_over_counties$District_ID,twd_boundaries$District_ID)]))
county_overs$Prop_Over_County <- round(county_overs$Prop_Over_County,2)
county_overs = county_overs[county_overs$Prop_Over_County>0,]

saveRDS(county_overs,'drought_and_debt/input/district_county_overlaps.RDS')
