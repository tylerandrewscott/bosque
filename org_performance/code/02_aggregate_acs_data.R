library(tidyverse)
library(tidycensus)
library(readr)
#vn = list("NAME","GEOID","B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn = c("B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn2 = c('GEOID',vn)
yrs = 2009:2021
source('../census_key')
tidycensus::census_api_key(k)
#rm(k)
yfips = expand.grid(Year = yrs,FIPS = as.numeric(fips_codes$county_code[fips_codes$state=='TX']))
yfips$FIPS = formatC(yfips$FIPS,width = 3,flag = '0')

quer_list = pblapply(nrow(yfips):1,function(i)
{tryCatch({get_acs(geography = "tract", variables = vn,year = yfips$Year[i],county = yfips$FIPS[i],
                   state = "TX", geometry = FALSE,survey = 'acs5',keep_geo_vars = F)},
          error = function(e) NULL)},cl = 8)

quer_list[which(sapply(quer_list,is.null))] <- lapply(which(sapply(quer_list,is.null)),function(i){
  get_acs(geography = "tract", variables = vn,year = yfips$Year[i],county = yfips$FIPS[i],
          state = "TX", geometry = FALSE,survey = 'acs5',keep_geo_vars = F)})
acs_dt = data.table::rbindlist(quer_list)
acs_dt = acs_dt[,-'moe',with=F]

acs_dt$variable = as.factor(acs_dt$variable)
acs_dt$variable = fct_recode(acs_dt$variable,"Total_Population" = "B02001_001",
                             "White_Population" = "B02001_002" ,
                             "Median_Income" = "B06011_001",
                             "Household_Pop" = "B07013_001",
                             "Household_Owner_Occupied" = "B07013_002",
                             "Pop_Over_25"= "B16010_001",
                             "Pop_Bach" = "B16010_041",
                             "Median Year Structure Built" = "B25035_001",
                             "Median_Home_Value" = "B25077_001")
acs_dt$Year = unlist(mapply(function(v,r) rep(x = v,times = r),
                            v = yfips$Year,r = unlist(sapply(quer_list,nrow))))

acs_vars_dt = acs_dt %>% spread(variable,estimate)
acs_vars_dt = acs_vars_dt %>% dplyr::select(-NAME)

fwrite(x = acs_vars_dt,file = 'org_performance/input/census_tract_data.csv')
