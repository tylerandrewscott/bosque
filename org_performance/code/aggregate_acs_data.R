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

# 
# acs_population = do.call(rbind,
#         lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('B01003.*(with|Data)',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x)
#           read_csv(x,na = '-',skip=1) %>% mutate(Year = readr::parse_number(str_extract(x,'Y[0-9]{4}\\.')))))
# 
# acs_population = acs_population %>% dplyr::select(Geography,`Estimate!!Total`,Year) %>% rename(Population = `Estimate!!Total`)
# 
# vals <- c("estimate!!percent!!age by educational attainment!!population 25 years and over!!bachelor's degree or higher",
#           "total!!estimate!!percent bachelor's degree or higher")
# 
# acs_ed_list = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('S1501.*(with|Data)',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x)
#                            read_csv(x,na = '-',skip=1) %>% mutate(Year = readr::parse_number(str_extract(x,'Y[0-9]{4}\\.'))) %>% 
#                        dplyr::select(Geography,Year,which(tolower(names(.)) %in% vals)) %>% mutate_if(tolower(names(.)) %in% vals,as.numeric))
# acs_ed <- rbindlist(acs_ed_list,fill =T,use.names = T)
# acs_ed$Perc_Bach_and_Higher <- rowSums(acs_ed[,tolower(names(acs_ed)) %in% vals,with = F],na.rm = T)
# acs_ed <- acs_ed[,.(Geography,Year,Perc_Bach_and_Higher)]
# 
# vals <- c()
# 
# 
# acs_inc = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('S1903_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x){
#   ingvisible(read_csv(x,na = '-',skip=0)) %>% 
#     mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>%
#     rename(Median_Household_Income = HC02_EST_VC02,Id = GEO.id,Id2 = GEO.id2) %>% 
#     dplyr::select(Id,Id2,Year,Median_Household_Income) %>% .[-1,]})
# names(acs_inc) <- grep('S1903_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)
# acs_median_houshold_income_df = do.call(rbind,acs_inc)
# acs_median_houshold_income_df$Median_Household_Income[acs_median_houshold_income_df$Median_Household_Income=='250,000+'] <- 300000
# 
# 
# acs_home = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('S1101_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x){
#   print(x); invisible(read_csv(x,na = '-',skip=1)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}'))))})
# names(acs_home) <- grep('S1101_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)
# acs_owner_occ = do.call(rbind,lapply(names(acs_home),function(x) {if(x == 'ACS_09_5YR_S1101_with_ann.csv'){
#   acs_home[[x]] %>% dplyr::select(`Total; Estimate; Total households - HOUSING TENURE - Owner-occupied housing units`,Year,Id,Id2) %>% 
#     rename(Perc_Owner_Occupied  = `Total; Estimate; Total households - HOUSING TENURE - Owner-occupied housing units`)}
#   else{acs_home[[x]] %>% dplyr::select(`Total; Estimate; HOUSING TENURE - Owner-occupied housing units`,Year,Id,Id2) %>% 
#       rename(Perc_Owner_Occupied = `Total; Estimate; HOUSING TENURE - Owner-occupied housing units`)}}))
# acs_owner_occ$Id2 = as.character(acs_owner_occ$Id2)
# 
# 
# acs_med_home_value = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('B25077_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x){
#   print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
#     rename(Median_Home_Price = HD01_VD01,Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-HD02_VD01,-`GEO.display-label`) %>% .[-1,]})
# names(acs_med_home_value) <- grep('B25077_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)
# acs_med_home_value_df = do.call(rbind,acs_med_home_value)
# acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="1,000,000+"] <- '1500000'
# acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="2,000,000+"] <- '2500000'
# acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="10,000-"] <- '5000'
# acs_med_home_value_df$Median_Home_Price = as.numeric(acs_med_home_value_df$Median_Home_Price)
# 
# acs_med_year_structure_built = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('B25035_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x){
#   print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
#     rename(Median_Year_Structure_Built = HD01_VD01,Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-HD02_VD01,-`GEO.display-label`) %>% .[-1,]})
# names(acs_med_home_value) <- grep('B25035_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)
# acs_med_year_structure_built_df = do.call(rbind,acs_med_year_structure_built)
# acs_med_year_structure_built_df$Median_Year_Structure_Built[!is.na(acs_med_year_structure_built_df$Median_Year_Structure_Built) & acs_med_year_structure_built_df$Median_Year_Structure_Built=='2005+'] <- acs_med_year_structure_built_df$Year
# acs_med_year_structure_built_df$Median_Year_Structure_Built[!is.na(acs_med_year_structure_built_df$Median_Year_Structure_Built) & acs_med_year_structure_built_df$Median_Year_Structure_Built=='1939-'] <- 1920
# 
# 
# acs_mobility = lapply(paste0('input/census/ACS_tracts/acs_2010_2021/',grep('S0701_with',list.files('input/census/ACS_tracts/acs_2010_2021'),value=T)),function(x){
#   print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
#     rename(Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-`GEO.display-label`) %>% .[-1,] %>%
#     mutate(TOTAL_PERC_MOVED_IN = as.numeric(HC02_EST_VC01) + as.numeric(HC03_EST_VC01) + 
#              as.numeric(HC04_EST_VC01) + as.numeric(HC05_EST_VC01)) %>% dplyr::select(Id,Id2,Year,TOTAL_PERC_MOVED_IN)})
# acs_mobility_df = do.call(rbind,acs_mobility)
# 
# 
# acs_tract_demos = Reduce(full_join,list(acs_population,acs_median_houshold_income_df,
#                                         acs_owner_occ,acs_unemp,acs_bach_df,
#                                         acs_med_year_structure_built_df,acs_med_home_value_df,acs_mobility_df))
# 
# write_csv(path = 'input/census/ACS_tracts/acs_2010_2021/acs_tract_demo_data.csv',x = acs_tract_demos)
#        
# 
