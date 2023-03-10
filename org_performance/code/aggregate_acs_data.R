library(tidyverse)

acs_population = do.call(rbind,
        lapply(paste0('input/census/ACS_tracts/proj6/',grep('B01003.*(with|Data)',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x)
          read_csv(x,na = '-',skip=1) %>% mutate(Year = str_extract(x,'ACS_[0-9]{2}')) %>% mutate(Year = paste0('20',gsub('ACS_','',Year)))))
acs_population = acs_population %>% dplyr::select(-`Margin of Error; Total`) %>% 
  rename(Population = `Estimate; Total`) %>% mutate(Id2 = as.character(Id2))


acs_ed = lapply(paste0('input/census/ACS_tracts/proj6/',grep('S1501.*(with|Data)',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  print(x); invisible(read_csv(x,na = '-',skip=0)) %>% 
    rename(Id = GEO.id, Id2 = GEO.id2) %>%
    mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% .[-1,]})
names(acs_ed) <- grep('S1501_with',list.files('input/census/ACS_tracts/proj6'),value=T)
ed_guide = data.frame(Year = 2017:2009,
                      file = sort(grep('S1501.*(with|Data)',list.files('input/census/ACS_tracts/proj6'),value=T),decreasing = T),
                      code = c('HC02_EST_VC18',
           'HC02_EST_VC18',
           'HC01_EST_VC17',
           'HC01_EST_VC17',
           'HC01_EST_VC17',
           'HC01_EST_VC17',
           'HC01_EST_VC17',
           'HC01_EST_VC15'),stringsAsFactors = F)

ed_guide
acs_bach_df = do.call(rbind,lapply(seq_along(acs_ed),function(x) {
acs_ed[[x]] %>% dplyr::select(Id,Id2,Year,as.character(ed_guide$code[ed_guide$file==names(acs_ed)[x]])) %>%
    rename(Perc_Bach_and_Higher  = as.character(ed_guide$code[ed_guide$file==names(acs_ed)[x]]))
}))


acs_english = do.call(rbind,
  lapply(paste0('input/census/ACS_tracts/proj6/',grep('S1601_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
read_csv(x,na = '-',skip=1) %>%  mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>%
      dplyr::select(Year,Id,Id2,`Percent of specified language speakers  - Speak English  less than "very well"; Estimate; Population 5 years and over`) %>%
      rename(Perc_English_Fluency = `Percent of specified language speakers  - Speak English  less than "very well"; Estimate; Population 5 years and over`)}))

#2009: HC02_EST_VC06
#2010: HC01_VC13
#2011-2016: HC01_VC07

acs_unemp = do.call(rbind,lapply(paste0('input/census/ACS_tracts/proj6/',grep('S2301_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x)
read_csv(x,na = '-') %>% .[-1,] %>% dplyr::select(HC04_EST_VC01,GEO.id,GEO.id2)%>%  
  mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}'))))))
acs_unemp = acs_unemp %>% rename(unemp_rate = HC04_EST_VC01) %>% mutate(unemp_rate = as.numeric(unemp_rate))
acs_unemp = acs_unemp %>% rename(Id = GEO.id,Id2 = GEO.id2)

acs_inc = lapply(paste0('input/census/ACS_tracts/proj6/',grep('S1903_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  invisible(read_csv(x,na = '-',skip=0)) %>% 
    mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>%
    rename(Median_Household_Income = HC02_EST_VC02,Id = GEO.id,Id2 = GEO.id2) %>% 
    dplyr::select(Id,Id2,Year,Median_Household_Income) %>% .[-1,]})
names(acs_inc) <- grep('S1903_with',list.files('input/census/ACS_tracts/proj6'),value=T)
acs_median_houshold_income_df = do.call(rbind,acs_inc)
acs_median_houshold_income_df$Median_Household_Income[acs_median_houshold_income_df$Median_Household_Income=='250,000+'] <- 300000


acs_home = lapply(paste0('input/census/ACS_tracts/proj6/',grep('S1101_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  print(x); invisible(read_csv(x,na = '-',skip=1)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}'))))})
names(acs_home) <- grep('S1101_with',list.files('input/census/ACS_tracts/proj6'),value=T)
acs_owner_occ = do.call(rbind,lapply(names(acs_home),function(x) {if(x == 'ACS_09_5YR_S1101_with_ann.csv'){
  acs_home[[x]] %>% dplyr::select(`Total; Estimate; Total households - HOUSING TENURE - Owner-occupied housing units`,Year,Id,Id2) %>% 
    rename(Perc_Owner_Occupied  = `Total; Estimate; Total households - HOUSING TENURE - Owner-occupied housing units`)}
  else{acs_home[[x]] %>% dplyr::select(`Total; Estimate; HOUSING TENURE - Owner-occupied housing units`,Year,Id,Id2) %>% 
      rename(Perc_Owner_Occupied = `Total; Estimate; HOUSING TENURE - Owner-occupied housing units`)}}))
acs_owner_occ$Id2 = as.character(acs_owner_occ$Id2)


acs_med_home_value = lapply(paste0('input/census/ACS_tracts/proj6/',grep('B25077_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
    rename(Median_Home_Price = HD01_VD01,Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-HD02_VD01,-`GEO.display-label`) %>% .[-1,]})
names(acs_med_home_value) <- grep('B25077_with',list.files('input/census/ACS_tracts/proj6'),value=T)
acs_med_home_value_df = do.call(rbind,acs_med_home_value)
acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="1,000,000+"] <- '1500000'
acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="2,000,000+"] <- '2500000'
acs_med_home_value_df$Median_Home_Price[!is.na(acs_med_home_value_df$Median_Home_Price) & acs_med_home_value_df$Median_Home_Price=="10,000-"] <- '5000'
acs_med_home_value_df$Median_Home_Price = as.numeric(acs_med_home_value_df$Median_Home_Price)

acs_med_year_structure_built = lapply(paste0('input/census/ACS_tracts/proj6/',grep('B25035_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
    rename(Median_Year_Structure_Built = HD01_VD01,Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-HD02_VD01,-`GEO.display-label`) %>% .[-1,]})
names(acs_med_home_value) <- grep('B25035_with',list.files('input/census/ACS_tracts/proj6'),value=T)
acs_med_year_structure_built_df = do.call(rbind,acs_med_year_structure_built)
acs_med_year_structure_built_df$Median_Year_Structure_Built[!is.na(acs_med_year_structure_built_df$Median_Year_Structure_Built) & acs_med_year_structure_built_df$Median_Year_Structure_Built=='2005+'] <- acs_med_year_structure_built_df$Year
acs_med_year_structure_built_df$Median_Year_Structure_Built[!is.na(acs_med_year_structure_built_df$Median_Year_Structure_Built) & acs_med_year_structure_built_df$Median_Year_Structure_Built=='1939-'] <- 1920


acs_mobility = lapply(paste0('input/census/ACS_tracts/proj6/',grep('S0701_with',list.files('input/census/ACS_tracts/proj6'),value=T)),function(x){
  print(x); invisible(read_csv(x,na = '-',skip=0)) %>% mutate(Year = paste0('20',gsub('ACS_','',str_extract(x,'ACS_[0-9]{2}')))) %>% 
    rename(Id2 = GEO.id2,Id = GEO.id) %>% dplyr::select(-`GEO.display-label`) %>% .[-1,] %>%
    mutate(TOTAL_PERC_MOVED_IN = as.numeric(HC02_EST_VC01) + as.numeric(HC03_EST_VC01) + 
             as.numeric(HC04_EST_VC01) + as.numeric(HC05_EST_VC01)) %>% dplyr::select(Id,Id2,Year,TOTAL_PERC_MOVED_IN)})
acs_mobility_df = do.call(rbind,acs_mobility)


acs_tract_demos = Reduce(full_join,list(acs_population,acs_median_houshold_income_df,
                                        acs_owner_occ,acs_unemp,acs_bach_df,
                                        acs_med_year_structure_built_df,acs_med_home_value_df,acs_mobility_df))

write_csv(path = 'input/census/ACS_tracts/proj6/acs_tract_demo_data.csv',x = acs_tract_demos)
       

