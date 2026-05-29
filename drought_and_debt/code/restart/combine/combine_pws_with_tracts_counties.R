library(MASS)
library(tidyverse)
library(lubridate)
library(survival)

library(sf)
library(geojsonsf)
library(data.table)
library(tigris)
library(lwgeom)
library(pbapply)
library(ggthemes)
#library(esri2sf)

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
id_crosswalk <- readRDS('drought_and_debt/input/id_crosswalk.RDS')

pws_boundaries = st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp')
pws_boundaries = pws_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
pws_boundaries = st_transform(pws_boundaries,st_crs(albersNA))
pws_boundaries = st_make_valid(pws_boundaries)

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


pws_over_tracts_2010 = st_intersection(pws_boundaries,tx_tracts2010)
pws_over_tracts_2020 = st_intersection(pws_boundaries,tx_tracts2020)

tract_overs2010 = data.table(PWS_ID = pws_over_tracts_2010$PWS_ID,GEOID10 = pws_over_tracts_2010$GEOID10,
                             Prop_Of_Tract = as.numeric(st_area(pws_over_tracts_2010)/
                                                          st_area(tx_tracts2010)[match(pws_over_tracts_2010$GEOID10,tx_tracts2010$GEOID10)]))
tract_overs2020 = data.table(PWS_ID = pws_over_tracts_2020$PWS_ID,GEOID10 = pws_over_tracts_2020$GEOID10,
                             Prop_Of_Tract = as.numeric(st_area(pws_over_tracts_2020)/
                                                          st_area(tx_tracts2020)[match(pws_over_tracts_2020$GEOID,tx_tracts2020$GEOID)]))

tract_overs <- list('tracts_2010' = tract_overs2010,'tracts_2020'=tract_overs2020)
saveRDS(tract_overs,'drought_and_debt/input/pws_tract_overlaps.RDS')

pws_over_counties = st_intersection(pws_boundaries,tx_county)
pws_over_counties = pws_over_counties %>% rename(CFIPS = GEOID)

county_overs = data.table(PWS_ID = pws_over_counties$PWS_ID,CFIPS = pws_over_counties$CFIPS,
                          Prop_Over_County = as.numeric(st_area(pws_over_counties)/st_area(pws_boundaries)[match(pws_over_counties$PWS_ID,pws_boundaries$PWS_ID)]))
county_overs$Prop_Over_County <- round(county_overs$Prop_Over_County,2)
county_overs = county_overs[county_overs$Prop_Over_County>0,]

saveRDS(county_overs,'drought_and_debt/input/pws_county_overlaps.RDS')
