
library(RCurl)
library(rvest)
### download bls county unemp data
#bls_unemp_url = 'http://www.bls.gov/lau/#tables'
#read_html(bls_unemp_url)  %>% html_nodes("a") %>% html_attr("href") %>% grep('laucnty[0-9]',.,value=T) %>% grep('txt',.,value=T) %>%
#  lapply(.,function(x)  download.file(url = paste0('http://www.bls.gov',x),destfile = paste0('Input/census/unemp',gsub('lau\\/','',x))))
library(rgeos)
library(sp)
library(rgdal)

set_year = 2000:2015
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
county_grid = expand.grid(tx_counties@data$FIPS_I,set_year) %>% rename(COUNTY_FIPS = Var1,Year = Var2)


# unemployment rate by county-year
cnty_unemp_90_15 = lapply(paste0('Input/census/unemp/',list.files('Input/census/unemp/')),function(x) read_table(x,skip=4,col_names = T)) %>%
lapply(.,function(x) if(ncol(x)>9) {x %>% select(-X5,-X6)} else {x}) %>% do.call(rbind,.) %>% filter(Code == '48') %>%
  select(Code,Code_1,Year,Rate) %>% mutate(COUNTY_FIPS = paste0(Code,Code_1)) %>% select(-Code,-Code_1)
  

##### Create % bachelor degree by county-year
census_ed = lapply(paste0('Input/census/ACS_education/',grep('S1501_with',list.files('Input/census/ACS_education/'),value=T)),function(x) read_csv(x) %>% .[-1,] %>% mutate(Year = x))
bach_by_county = do.call(rbind,lapply(census_ed, function(x) x %>% mutate(NAMELSAD = gsub('\\, Texas$',"",`GEO.display-label`)) %>% rename(COUNTY_FIPS = GEO.id2,PERC_BACH_OVER25 = HC01_EST_VC05) %>% 
dplyr::select(COUNTY_FIPS,NAMELSAD,PERC_BACH_OVER25,Year) %>% mutate(Year = gsub('ACS_','20',str_extract(Year,'ACS_[0-9]{2}')),PERC_BACH_OVER25 = as.numeric(PERC_BACH_OVER25))))
year_bach_grid = expand.grid(tx_counties@data$FIPS_I,set_year) %>% rename(COUNTY_FIPS = Var1,Year = Var2) %>% mutate(Year = as.character(Year))
year_bach_grid = left_join(year_bach_grid,bach_by_county %>% arrange(NAMELSAD,Year) %>% dplyr::select(-NAMELSAD)) %>% group_by(COUNTY_FIPS)
fips_bach_mean = year_bach_grid %>% group_by(COUNTY_FIPS) %>% summarise(FIPS_MEAN = mean(PERC_BACH_OVER25,na.rm=T))
year_bach_grid$PERC_BACH_OVER25[is.na(year_bach_grid$PERC_BACH_OVER25)] = fips_bach_mean$FIPS_MEAN[match(year_bach_grid$COUNTY_FIPS[is.na(year_bach_grid$PERC_BACH_OVER25)],fips_bach_mean$COUNTY_FIPS)]


# ######
# metro_2003 = read_csv('Input/misc/ruralurbancodes2003.csv') %>% filter(State=='TX') %>%
#   mutate(nonmetro = `2003 Rural-urban Continuum Code` %in% c(4:7),
#          metro = `2003 Rural-urban Continuum Code` %in% c(1:3),
#          rural = `2003 Rural-urban Continuum Code` %in% c(8:9)) %>% 
#   rename(COUNTY_FIPS = `FIPS Code`) %>% dplyr::select(COUNTY_FIPS,nonmetro,metro,rural) %>%
#   merge(.,data.frame(Year = as.character(2003:2012)))
# 
# metro_2013 =  read_csv('Input/misc/ruralurbancodes2013.csv') %>% filter(State=='TX') %>%
#   mutate(nonmetro = RUCC_2013 %in% c(4:7),
#          metro = RUCC_2013 %in% c(1:3),
#          rural = RUCC_2013 %in% c(8:9)) %>% 
#   rename(COUNTY_FIPS = FIPS) %>% dplyr::select(COUNTY_FIPS,nonmetro,metro,rural) %>%
#   merge(.,data.frame(Year = as.character(2013:2015)))
# 
# metro = full_join(metro_2003,metro_2013)










read_table('https://www.census.gov/did/www/saipe/downloads/estmod97/est97US.dat')

