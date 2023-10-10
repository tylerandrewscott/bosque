library(sf)
library(tidyverse)
library(tigris)
library(stringr)
library(lubridate)
library(data.table)
### from MULLIN/RUBADO
#We assigned scores to water systems by using GIS to match boundaries of drought-classified
#areas for each week of our study period to the boundaries of water system
#service areas, as indicated on spatial maps provided by the TCEQ. Water
#systems received the drought score covering the plurality of the spatial area
#of their jurisdiction so long as the majority of the spatial area fell into some
#category of drought. If majority of the area fell outside the bounds of area
#classified as being in drought, the water system received a 0 score for drought
#severity in that week.

first_url <- 'https://droughtmonitor.unl.edu/data/json/usdm_20100105.json'
start_date <- ymd(str_extract(first_url,'[0-9]{1,}'))

drought_start_date = mdy('5/4/2010')
drought_end_date = mdy('7/7/2015')
weekly <- seq(start_date,drought_end_date,by='weeks')
start_date_dec = decimal_date(start_date)
weekly_feed <- str_remove_all(weekly,'-')

albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
twd_boundaries <- st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile/PWS_Export.shp')
twd_boundaries <- twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries <- st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries <- st_make_valid(twd_boundaries)
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()

file <- 'drought_and_debt/input/MR_drought_scores.csv'
if(file.exists(file)){temp <- fread(file)}else{temp <- data.table()}
weekly_feed <- weekly_feed[!weekly_feed %in% temp$Week]

transform = T
grabbed <- list()
while(any(!weekly_feed %in% temp$Week)){
  for(i in weekly_feed){
  url <- paste0('https://droughtmonitor.unl.edu/data/json/usdm_',i,".json")
  temp_url <- st_read(url)
  if(!all(st_is_valid(temp_url))){
    temp_url <- st_make_valid(temp_url)
    temp_url <- temp_url[st_is_valid(temp_url),]
  }
  if(transform)
  {twd_boundaries <- st_transform(twd_boundaries, st_crs(temp_url))
  transform <- F}
  pws_areas_dt <- data.table(PWS_ID = twd_boundaries$PWS_ID,PWS_Area = st_area(twd_boundaries))
  bound_intersects <- suppressWarnings(sf::st_intersection(twd_boundaries,temp_url))
  bound_intersects$bound_area <- st_area(bound_intersects)
  bound_intersects$pws_area <- pws_areas_dt$PWS_Area[match(bound_intersects$PWS_ID,pws_areas_dt$PWS_ID)]
  bound_intersects$w <- bound_intersects$bound_area/bound_intersects$pws_area
  bound_intersects$w <- as.numeric(bound_intersects$w)
  bound_intersects$drought_binary <- (bound_intersects$DM>0) + 0
  any_drought <- bound_intersects %>% group_by(PWS_ID) %>%
    summarize(avg = weighted.mean(x = drought_binary,w = w)) %>%
    filter(avg>=0.5)
  
  system_dm_scores <-bound_intersects %>% group_by(PWS_ID) %>%
    filter(PWS_ID %in% any_drought$PWS_ID &
           max(w)==w) %>% arrange(-DM) %>%
    as.data.frame() %>%
    dplyr::select(PWS_ID,DM) 
  system_dm_scores <- right_join(system_dm_scores,
                     data.table(PWS_ID = twd_boundaries$PWS_ID))
  system_dm_scores$DM[is.na(system_dm_scores$DM)] <- 0
  system_dm_scores$Week <- i
  fwrite(file = file,append = T,x = system_dm_scores) 
  grabbed <- append(grabbed,i)
  weekly_feed <- weekly_feed[!weekly_feed %in% temp$Week & !weekly_feed %in% grabbed]
  }
}






