library(jsonlite)
library(censusapi)
library(pbapply)
library(tidyverse)
library(tidycensus)
library(data.table)
k = "b5a388cd6162590fc939335ddc45787bcc61778c"
tidycensus::census_api_key(k)
#vn = list("NAME","GEOID","B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn = c("B02001_001E","B02001_002E","B06011_001E","B07013_001E","B07013_002E","B16010_001E","B16010_041E","B25035_001E","B25077_001E")
vn2 = c('GEOID',vn)
yrs = 2009:2017

k = "b5a388cd6162590fc939335ddc45787bcc61778c"
tidycensus::census_api_key(k)

yfips = expand.grid(Year = 2010:2017,FIPS = as.numeric(fips_codes$county_code[fips_codes$state=='TX']))
yfips$FIPS = formatC(yfips$FIPS,width = 3,flag = '0')
quer_list = pblapply(1:nrow(yfips),function(i)
{tryCatch({get_acs(geography = "tract", variables = vn,year = yfips$Year[i],county = yfips$FIPS[i],
                                    state = "TX", geometry = FALSE,survey = 'acs5',keep_geo_vars = F)},
                                    error = function(e) NULL)},cl = 8)

quer_list[which(sapply(quer_list,is.null))] <- lapply(which(sapply(quer_list,is.null)),function(i){
get_acs(geography = "tract", variables = vn,year = yfips$Year[i],county = yfips$FIPS[i],
        state = "TX", geometry = FALSE,survey = 'acs5',keep_geo_vars = F)})
acs_dt = data.table::rbindlist(quer_list)
acs_dt = acs_dt[,-'moe',with=F]
library(forcats)
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
acs_vars_dt = acs_vars_dt[,-'NAME',]
ur2009 = paste0("https://api.census.gov/data/2009/acs5?key=b5a388cd6162590fc939335ddc45787bcc61778c&get=",
                paste(vn2,collapse=','),"&for=tract:*&in=county:*&in=state:48")
t2009 = fromJSON(ur2009)
tdt = as.data.table(t2009)
colnames(tdt) <- as.character(tdt[1,])
tdt = tdt[-1,]
tdt_long = gather(tdt,variable,estimate,-county,-tract,-state,-GEOID) 
tdt_long = tdt_long[,!colnames(tdt_long) %in% c('county','tract','state')]
tdt_long$variable = as.factor(gsub('E$','',tdt_long$variable))
tdt_long$variable = fct_recode(tdt_long$variable,"Total_Population" = "B02001_001",
                             "White_Population" = "B02001_002" ,
                             "Median_Income" = "B06011_001",
                             "Household_Pop" = "B07013_001",
                             "Household_Owner_Occupied" = "B07013_002",
                             "Pop_Over_25"= "B16010_001",
                             "Pop_Bach" = "B16010_041",
                             "Median Year Structure Built" = "B25035_001",
                             "Median_Home_Value" = "B25077_001")
tdt_spread = tdt_long %>% spread(variable,estimate)
tdt_spread$Year = 2009
acs_dt_2009 = as.data.table(tdt_spread)
acs_vars_dt = rbind(acs_dt_2009,acs_vars_dt)
write_csv(x = acs_vars_dt ,path = 'input/census/ACS_tracts/proj6/tract_data_2009_2017.csv')



