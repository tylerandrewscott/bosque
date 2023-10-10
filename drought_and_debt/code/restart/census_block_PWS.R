#from MULLIN/RUBADO

#To calculate variables measuring characteristics of the population served
#by the water system, we aggregated data collected at the 2010 Census block
#group levels up to boundaries of water system service areas, splitting block
#group population according to the proportion of the block group’s spatial area
#lying within a water system’s service boundaries.

### TYLER NOTE: some of the vars don't actually seem to be available in public block records
# so we use census tract instead for those. perhaps M/R had non-public data, or just an oversight in reporting
# % houses built after 1980 - check
#% poverty - check 
#Median household income, logged  - check
#% Black- check
#% Hispanic- check
#% four-year college degree - check
# % rural - check

library(sf)
library(data.table)
library(tidyverse)
library(tigris)

albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
twd_boundaries <- st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile/PWS_Export.shp')
twd_boundaries <- twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries <- st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries <- st_make_valid(twd_boundaries)
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()

tx_blocks <- tigris::block_groups(state = 'TX',cb = T,year = 2010)
tx_tracts <- tigris::tracts(state = 'TX',cb = T,year = 2010)
tx_blocks$GEOID <- str_remove(tx_blocks$GEO_ID,'^1500000US')
tx_tracts$GEOID <- str_remove(tx_tracts$GEO_ID,'^1400000US')
### % hispanic ####
hispanic <- tidycensus::get_decennial(geography = 'block group',
                                  state = 'TX',year = 2010,variables = c('P011001','P011002'))
hispanic  <- data.table(hispanic )
hispanic <- dcast(hispanic,GEOID ~ variable,value.var = 'value')
hispanic$Perc_Hispanic <- 100 * (hispanic$P011002 / hispanic$P011001)
tx_blocks <- left_join(tx_blocks,hispanic[,.(GEOID,Perc_Hispanic)])

### % black ####
black <- tidycensus::get_decennial(geography = 'block group',
                                      state = 'TX',year = 2010,
                                   variables = c('P010001','P010004','P010011','P010016',
                                                 'P010017',
                                                 'P010018',
                                                 'P010019'))
black  <- data.table(black)
black$is_total <- ifelse((black$variable=='P010001'),'total','sub')
black <- dcast(black[,sum(value,na.rm = T),by=.(GEOID,is_total)],GEOID ~ is_total,value.var = 'V1') 
black$Perc_Black <- 100 * (black$sub / black$total)
tx_blocks <- left_join(tx_blocks,black[,.(GEOID,Perc_Black)])

# % rural
rural <- tidycensus::get_decennial(geography = 'block group',
                                   state = 'TX',year = 2010,
                                   variables = c('H002001','H002005'))
rural  <- data.table(rural )
rural <- dcast(rural,GEOID ~ variable,value.var = 'value')
rural$Perc_Rural <- 100 * (rural$H002005 / rural$H002001)
tx_blocks <- left_join(tx_blocks,rural[,.(GEOID,Perc_Rural)])

bach <- tidycensus::get_acs(geography = 'tract',survey = 'acs5',
                            state = 'TX',year = 2010,
                            variables = c('S1501_C01_015E'))
setnames(bach,c('estimate'),c('Perc_Bachelors'))
bach <- data.table(bach)
tx_tracts <- left_join(tx_tracts,bach[,.(GEOID,Perc_Bachelors)])


income <- tidycensus::get_acs(geography = 'tract',survey = 'acs5',
                            state = 'TX',year = 2010,
                            variables = c('S1903_C02_001E'))
setnames(income,c('estimate'),c('Med_Household_Income'))
income <- data.table(income)
tx_tracts <- left_join(tx_tracts,income[,.(GEOID,Med_Household_Income)])

house <- tidycensus::get_acs(geography = 'tract',survey = 'acs5',
                              state = 'TX',year = 2010,
                              variables = c("B25034_001",
                                            "B25034_002",
                                            "B25034_003",
                                            "B25034_004",
                                            "B25034_005"))
house_tots <- house %>% mutate(cat = ifelse(variable == 'B25034_001','total','since 1980')) %>%
  group_by(GEOID,cat) %>% summarise(value = sum(estimate,na.rm = T)) 
house_dt <- dcast(data.table(house_tots),GEOID ~ cat,value.var = 'value') %>% mutate(Perc_Houses_Since1980 = 100 * `since 1980`/total)
tx_tracts<-left_join(tx_tracts,house_dt[,.(GEOID,Perc_Houses_Since1980)])


poverty <- tidycensus::get_acs(geography = 'tract',survey = 'acs5',
                             state = 'TX',year = 2010,
                             variables = c("S1702_C02_001E"))
setnames(poverty,'estimate','Perc_Under_Poverty_Line')
poverty <- data.table(poverty)
tx_tracts <- left_join(tx_tracts,poverty[,.(GEOID,Perc_Under_Poverty_Line)])


library(foreign)
#### dem vote share (not from census)
#https://dataverse.harvard.edu/dataverse/eda
#https://geodata.lib.utexas.edu/catalog/princeton-ww72bg01w
precints <- st_read('spatial_inputs/princeton-ww72bg01w-geojson.json')
precints$COUNTY_VTD <- paste0('48',precints$COUNTYFP10,'_',precints$VTDST10)
elections <- list.files('drought_and_debt/input/precinct_votes/',full.names =T)
elects <- rbindlist(lapply(elections,read.dta),fill = T,use.names = T)
elects$CFIPS <- paste0('48',formatC(elects$fips,width = 3,flag = '0'))
elects$COUNTY_VTD <- paste(elects$CFIPS,elects$vtd,sep = '_')

elect_dt <- data.table(elects %>% dplyr::select(fips,COUNTY_VTD,contains('GOV'),contains('USP')))

elect_dt2 <- melt(elect_dt,id.vars = c('fips','COUNTY_VTD')) %>% 
  filter(grepl('dv$|tv$',variable)) %>%
  mutate(YEAR = str_extract(variable,'[0-9]{4}')) %>%
  mutate(variable = str_extract(variable,'(GOV|USP)_(tv|dv)')) %>%
  filter(!is.na(value))

elect_dt2<-data.table(elect_dt2) 
dem_vote <- dcast(elect_dt2,COUNTY_VTD + fips + YEAR ~ variable,value.var = 'value') %>%
  mutate(tv = ifelse(is.na(USP_tv),GOV_tv,USP_tv),
         dv = ifelse(is.na(USP_dv),GOV_dv,USP_dv)) %>%
  mutate(Perc_Dem = 100 * dv/tv )
dem_vote_share <- dem_vote[,mean(Perc_Dem,na.rm = T),by=.(COUNTY_VTD)] 
setnames(dem_vote_share,'V1','Perc_Dem_Vote_Share')
precints <- left_join(precints,dem_vote_share)


tx_tracts <- st_transform(tx_tracts,st_crs(twd_boundaries))
pws_tracts <- st_intersection(twd_boundaries,tx_tracts)
pws_tracts$intersect_area <- st_area(pws_tracts)
pws_tracts$pws_area <- st_area(twd_boundaries)[match(pws_tracts$PWS_ID,twd_boundaries$PWS_ID)]
pws_tracts$w <- as.numeric(pws_tracts$intersect_area/pws_tracts$pws_area)
pws_tracts_dt <- data.table(pws_tracts)
sdcols <- c("Perc_Bachelors"  ,"Med_Household_Income", "Perc_Houses_Since1980", "Perc_Under_Poverty_Line")
pws_tracts_dt[,(sdcols):=lapply(.SD,weighted.mean,w = w,na.rm = T),by=.(PWS_ID),.SDcols = sdcols]
pws_tracts_dt <- pws_tracts_dt[,c("PWS_ID",sdcols),with = F]
pws_tracts_dt<-pws_tracts_dt[!duplicated(pws_tracts_dt),]


tx_blocks <- st_transform(tx_blocks,st_crs(twd_boundaries))
pws_blocks <- st_intersection(twd_boundaries,tx_blocks)
pws_blocks$intersect_area <- st_area(pws_blocks)
pws_blocks$pws_area <- st_area(twd_boundaries)[match(pws_blocks$PWS_ID,twd_boundaries$PWS_ID)]
pws_blocks$w <- as.numeric(pws_blocks$intersect_area/pws_blocks$pws_area)
pws_blocks_dt <- data.table(pws_blocks)
sdcols <- c("Perc_Hispanic",'Perc_Black','Perc_Rural')
pws_blocks_dt[,(sdcols):=lapply(.SD,weighted.mean,w = w,na.rm = T),by=.(PWS_ID),.SDcols = sdcols]
pws_blocks_dt <- pws_blocks_dt[,c("PWS_ID",sdcols),with = F]
pws_blocks_dt <- pws_blocks_dt[!duplicated(pws_blocks_dt),]


tx_precincts <- st_transform(precints,st_crs(twd_boundaries))
pws_precincts <- st_intersection(twd_boundaries,tx_precincts)
pws_precincts$intersect_area <- st_area(pws_precincts)
pws_precincts$pws_area <- st_area(twd_boundaries)[match(pws_precincts$PWS_ID,twd_boundaries$PWS_ID)]
pws_precincts$w <- as.numeric(pws_precincts$intersect_area/pws_precincts$pws_area)
pws_precincts_dt <- data.table(pws_precincts)

pws_vote_dt <- pws_precincts_dt[,weighted.mean(x = Perc_Dem_Vote_Share,w = w,na.rm = T),by=.(PWS_ID)]
setnames(pws_vote_dt ,'V1','Perc_Dem_Vote_Share')

lapply(list(pws_blocks_dt,pws_tracts_dt,pws_vote_dt),function(x) {table(duplicated(x$PWS_ID))}
)

pws_demos_dt <- Reduce('merge',list(pws_blocks_dt,pws_tracts_dt,pws_vote_dt))
saveRDS(pws_demos_dt,file = 'drought_and_debt/input/pws_demos_MR.RDS')




