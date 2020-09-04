library(tidyverse)
library(lubridate)

library(readxl)
library(rgdal)
library(sp)
library(rgeos)
library(lubridate)
library(stringr)
library(readxl)
library(pbapply)



houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]
tx_tracts = readOGR('spatial_inputs/government_units','cb_2015_48_tract_500k')
tx_tracts@data$GEOID = as.character(tx_tracts@data$AFFGEOID)
houston_tracts = tx_tracts[paste(tx_tracts$STATEFP,tx_tracts$COUNTYFP,sep='') %in% hout_counties$FIPS_I,]


#### Load system summary from EPA
sdwis_df = read_csv('input/epa_sdwis/texas_cws_sdwis_summary_master.csv',na = c("-")) %>%
  rename(PWS_Type = `PWS Type`,PWS_NAME = `PWS Name`,PWS_ID = `PWS ID`) %>%
  # filter(PWS_Type == 'Community water system'&!`Owner Type` %in% c('State government','Federal government','Private')) %>%
  mutate(First_Report_Date = dmy(`First Reported Date`),Year_Start = year(First_Report_Date)) %>%
  dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) %>%
  mutate(`Deactivation Date` = dmy(`Deactivation Date`)) %>%
  filter(`Owner Type`!='Private') %>% filter(!grepl('^CITY OF|^TOWN OF|^VILLAGE OF|WSC|MUNICIPAL WATER SYSTEM',PWS_NAME))


sdwis_df$PWS_NAME[grep('^FORT BEND COUNTY MUD',sdwis_df$PWS_NAME)] = 
  str_extract(grep('^FORT BEND COUNTY MUD',sdwis_df$PWS_NAME,value=T),'FORT BEND COUNTY MUD [0-9]{1,}[A-Z]{0,2}')
sdwis_df$PWS_NAME = gsub('HCO',"HARRIS COUNTY",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub(' NO '," ",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 24 COUNTRY COLONY","MONTGOMERY COUNTY MUD 24",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 6 CARRIAGE LANE","HARRIS COUNTY MUD 6",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 119 SPRING TRAILS","MONTGOMERY COUNTY MUD 119",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 400   WEST","HARRIS COUNTY MUD 400",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 55 HERITAGE PARK","HARRIS COUNTY MUD 55",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 148 KINGSLAKE","HARRIS COUNTY MUD 148",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 374 CYPRESS CREEK LAKE","HARRIS COUNTY MUD 374",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("WEST HARRIS COUNTY MUD 2 CHASE","WEST HARRIS COUNTY MUD 2",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('WATER SUPPLY CORPORATION','WSC',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('REGIONAL WATER AUTHOR$|REGIONAL WATER AUTHORITY','RWA',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('HARRIS COUNTY MUD 400 - WEST','HARRIS COUNTY MUD 400',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY WCID 113 ENCHANTED VILLAGE","HARRIS COUNTY WCID 113",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("THE WOODLANDS",'WOODLANDS',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('BRAZORIA COUNTY FWSD 1 DAMON','BRAZORIA COUNTY FWSD 1',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("MUD1",'MUD 1',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("MUD3",'MUD 3',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub(" TOWNE LAKE",'',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("GULF COAST WATER AUTHORITY TX CITY","GULF COAST WATER AUTHORITY",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("LIBERTY COUNTY FWSD 1 HULL","HULL FWSD",sdwis_df$PWS_NAME)

sdwis_df$PWS_NAME = gsub("BROOK HOLLOW WEST S",'',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub(" FAIRFIELD VILLAGE",'',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('TOWN OF','CITY OF',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND" ,"GALVESTON COUNTY FWSD 6" ,sdwis_df$PWS_NAME)

sdwis_df$PWS_NAME = gsub("CINCO SOUTHWEST MUD 3 DAYCARE","CINCO SOUTHWEST MUD 3",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY WCID 50 EL LAGO","HARRIS COUNTY WCID 50",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('DOBBIN PLANTERSVILLE WSC 1','DOBBIN PLANTERSVILLE WSC',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY MUD 200 CRANBROOK","HARRIS COUNTY MUD 200",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('CENTRAL HARRIS COUNTY REGIONAL WATER AUT$','CENTRAL HARRIS COUNTY REGIONAL WATER AUTHORITY',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('NORTH HARRIS COUNTY REGIONAL WATER AUTHO$','NORTH HARRIS COUNTY REGIONAL WATER AUTHORITY',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 16 WHITE OAK PLANT","MONTGOMERY COUNTY MUD 16",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("CITY OF WOOD BRANCH VILLAGE","CITY OF WOODBRANCH",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub('-',' ',sdwis_df$PWS_NAME)
sdwis_df$PWS_NAME = gsub(' $','',sdwis_df$PWS_NAME)

texdwb_df = read_csv('input/tceq_audits/system_info_detail_full.csv')
texdwb_df = texdwb_df  %>% rename(PWS_NAME = NAME,SYSTEM_ID = WDD_ID)
texdwb_df$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('-',' ',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub(' $','',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('THE WOODLANDS','WOODLANDS',texdwb_df$PWS_NAME)
#texdwb_df$PWS_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('CLOVERCREEK','CLOVER CREEK',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('POST WOOD','POSTWOOD',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub('OF BRAZORIA COUNTY$','',texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",texdwb_df$PWS_NAME)
texdwb_df$PWS_NAME[texdwb_df$PWS_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
texdwb_df$texdwb_df_observed = TRUE


#texdwb_df  = texdwb_df %>% filter(!is.na(`Primary County:`),`Primary County:` %in% c('HARRIS','BRAZORIA','GALVESTON','LIBERTY','AUSTIN',
#                                                                                       'MONTGOMERY','WALLER','FORT BEND','CHAMBERS'))


#wd_functions <- read_csv('input/tceq_audits/district_functions.csv')



unique(texdwb_df$`Acre Size:`)




table(sdwis_df$PWS_NAME %in% texdwb_df$PWS_NAME)



sdwis_df$

sdwis_df$`Owner Type`

sdwis_df$PWS_NAME[!sdwis_df$PWS_NAME %in% texdwb_df$PWS_NAME]

sdwis_df$PWS_NAME[!sdwis_df$PWS_NAME %in% texdwb_df$PWS_NAME]

grep('DENTON',texdwb_df$PWS_NAME,value=T)

temp = read_csv('input/epa_sdwis/violation_report_full.csv') %>%
  mutate(Year_Reported = year(dmy(temp$`Violation First Reported Date`)))  %>%
  filter(`Violation Type` == 'Maximum Contaminant Level Violation, Average')




texdwb_df$PWS_NAME






table(system_df$`Activity Status`)
table(system_df$`Deactivation Date`)

library(networkD3)

test = do.call(rbind,lapply(grep('tx_systems',list.files('input/epa_sdwis/'),value=T),function(x)
  read_csv(paste0('input/epa_sdwis/',x)) %>% mutate(Quarter = str_extract(x,'q[0-9]'),
                                                    Year = str_extract(x,'[0-9]{4}'))))
test = test[!duplicated(test %>% select(-Quarter,-Year)),]
test = test %>% mutate(Deactived_Date = dmy(`Deactivation Date`)) %>%
  filter(!`Owner Type` %in% c('Federal government','State government','Native American'))
test = test %>% dplyr::arrange(`PWS Name`,desc(Year)) %>% 
  filter(!duplicated(paste(`PWS Name`)))  %>%
  filter(grepl('[A-Za-z]',`PWS Name`))


table(test$`Owner Type`)

test$`PWS ID`[test$`PWS Name`=='-']



table(test$`Owner Type`)
test$`PWS Name`[test$`Owner Type`=='State government']


dmy(test$)




q1_2013 = read_csv('input/epa_sdwis/tx_systems_2013q1.csv')
q4_2014 = read_csv('input/epa_sdwis/tx_systems_2014q4.csv')

q1_2013$`Activity Status`[!q1_2013$`PWS ID` %in% q4_2014$`PWS ID`]

grep('GREENWOOD',q4_2014$`PWS Name`,value=T)

q4_2014$`First Reported Date`[!q4_2014$`PWS ID` %in% q1_2013$`PWS ID`]

table(is.na(system_df$`Deactivation Date`))
table(is.na(system_df$`Deactivation Date`))




#### load violation data
viols_df = read_csv('input/epa_sdwis/hous_msa_violation_report_q4_2015.csv',na=c("-")) %>% filter(`PWS ID` %in% system_df$PWS_ID) %>% 
  filter(!duplicated(.)) %>% 
  dplyr::select(-`EPA Region`,-`Primacy Agency`,-`Primacy Type`,-`Deactivation Date`,-`Activity Status`,-`Population Served Count`,-`Primary Source`,-`PWS Type`) %>%
  mutate(Period_Begin = dmy(`Compliance Period Begin Date`),Period_End = dmy(`Compliance Period End Date`),Period_Begin_ddate = decimal_date(Period_Begin),Begin_Month = month(Period_Begin),Begin_Year = year(Period_Begin)) %>% 
  dplyr::select(-`Compliance Period Begin Date`,-`Compliance Period End Date`) %>% 
  mutate(Period = ifelse(Begin_Month <=3,1,ifelse(Begin_Month >3 & Begin_Month <=6, 2,ifelse(Begin_Month > 6 & Begin_Month <=9,3,4)))) %>%
  mutate(Period_Factor = paste(Begin_Year,Period,sep='_')) %>% rename(PWS_ID = `PWS ID`,PWS_NAME = `PWS Name`)

viols_df$Points = NA
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`=='Nitrate') | (viols_df$`Violation Code` %in% c('21','43','44','41','13'))] = 10
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`!='Nitrate') | 
                  (viols_df$`Violation Code` %in% c('02','11','12','22','23','24','37','40','42','46','47','57','58','59','63','64','65'))|
                  (viols_df$`Violation Code`=='03' & viols_df$`Contaminant Name`=='Nitrate')] = 5
viols_df$Points[is.na(viols_df$Points)] = 1
viols_df$RTC_Date_Dec = decimal_date(dmy(viols_df$`RTC Date`))
viols_df$RTC_Date_Dec[is.na(viols_df$RTC_Date_Dec)] = 3000  




##### add audits #####


        
test = system_set %>% filter(`Activity Reason:` == 'DORMANCY AFFIDAVIT')
system_set$`Activity Reason:`[!is.na(system_set$`Annexer Name:`)]
system_set$`Activity Status:`[!is.na(system_set$`Annexer Name:`)]
table(system_set$`Activity Reason:`)


system_set = system_set %>% mutate(Creation = mdy(`Creation Date:`),)

table(is.na(system_set$`Activity Reason:`))
system_set$`Dissolved Date:`


table(system_set$`Activity Status`)

table(system_df$`Activity Status:`)




system_df = left_join(system_df,system_set)






# 
# viol_points = do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
#   viols_df %>% 
#     filter(PWS_ID == analyze_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) &
#              RTC_Date_Dec > (analyze_df$FYEAR_END_DDATE[i]-1))%>%
#     summarise(ett_viol_score = sum(Points,na.rm=T),
#               ett_longest_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate,na.rm = T)),
#               ett_health_viol_score = sum(ifelse(Points>1,Points,0),na.rm=T),
#               ett_longest_health_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate[Points>1],na.rm = T)),
#               ett_management_viol_score = sum(ifelse(Points==1,Points,0),na.rm=T),
#               ett_longest_management_viol_score = trunc(max(analyze_df$FYEAR_END_DDATE[i] - Period_Begin_ddate[Points==1],na.rm = T))) %>%
#     mutate(PWS_ID = analyze_df$PWS_ID[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i],
#            FYEAR = analyze_df$FYEAR[i])))
# 
# 
# viol_points$ett_longest_viol_score[viol_points$ett_longest_viol_score<0] = 0
# viol_points$ett_score_p1 = viol_points$ett_viol_score + viol_points$ett_longest_viol_score
# viol_points$ett_longest_health_viol_score[viol_points$ett_longest_health_viol_score<0] = 0
# viol_points$ett_health_score_p1 = viol_points$ett_health_viol_score + viol_points$ett_longest_health_viol_score
# viol_points$ett_longest_management_viol_score[viol_points$ett_longest_management_viol_score<0] = 0
# viol_points$ett_management_score_p1 = viol_points$ett_management_viol_score + viol_points$ett_longest_management_viol_score






viol_p5count =   do.call(rbind,pblapply(1:nrow(analyze_df),function(i) 
  viols_df %>% 
    filter(PWS_ID == system_df$PWS_ID[i] & Period_Begin_ddate < (analyze_df$FYEAR_END_DDATE[i]-1) & 
             Period_Begin_ddate > (analyze_df$FYEAR_END_DDATE[i]-6)) %>%
    group_by(`Is Health Based`,PWS_ID) %>% summarise(p5_vcount = n()) %>% 
    mutate(FYEAR = analyze_df$FYEAR[i],FYEAR_END_DDATE = analyze_df$FYEAR_END_DDATE[i]))) %>% 
  spread(`Is Health Based`,p5_vcount,fill = 0) %>% 
  rename(hviol_count_p5 = Y,mviol_count_p5 = N)

viol_count = plyr::join_all(lapply(grep('viol_p[1-5]count',ls(),value=T),get),type='full')


test = mdy(system_df$`Dissolved Date:`)

system_df$`Merged into:`[!is.na(system_df$`Merged into:`)]
system_df$`Merged from:`[!is.na(system_df$`Merged from:`)]

table(is.na(dmy(system_df$`Deactivation Date`)))




































system_df$Dea
test = system_df %>% filter(!is.na(`Deactivation Date`))


test$`Deactivation Date`[c(12,16)]
test$`Dissolved Date:`[c(12,16)]
test$`Activity Status:`


  dplyr::select(PWS_NAME,`Deactivation_Date`,`Dissolved Date:`)



  names(system_df)
  [!is.na(system_df$`Annexer Name:`)]

table(system_df$`Activity Status:`)








#viol_data = full_join(viol_points,viol_p3count)
#analyze_df = left_join(analyze_df,viol_points)
analyze_df = left_join(analyze_df,viol_count)

analyze_df  = analyze_df %>%
  mutate(hviol_count_p1 = ifelse(is.na(hviol_count_p1),0,hviol_count_p1),
         mviol_count_p1 = ifelse(is.na(mviol_count_p1),0,mviol_count_p1),
         hviol_count_p2 = ifelse(is.na(hviol_count_p2),0,hviol_count_p2),
         mviol_count_p2 = ifelse(is.na(mviol_count_p2),0,mviol_count_p2),
         hviol_count_p3 = ifelse(is.na(hviol_count_p3),0,hviol_count_p3),
         mviol_count_p3 = ifelse(is.na(mviol_count_p3),0,mviol_count_p3),
         hviol_count_p4 = ifelse(is.na(hviol_count_p4),0,hviol_count_p4),
         mviol_count_p4 = ifelse(is.na(mviol_count_p4),0,mviol_count_p4),
         hviol_count_p5 = ifelse(is.na(hviol_count_p5),0,hviol_count_p5),
         mviol_count_p5 = ifelse(is.na(mviol_count_p5),0,mviol_count_p5))



### load acs data ###
tract_pops = do.call(rbind,
                     lapply(paste0('input/census/ACS_tracts/',grep('B01003_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
                       read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
                         dplyr::select(GEO.id,HD01_VD01,YEAR) %>%    
                         rename(AFFGEOID = GEO.id,TOTAL_POP = HD01_VD01)))
tract_pops = tract_pops %>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))),
                                   TOTAL_POP = as.numeric(TOTAL_POP))

tract_income = do.call(rbind,lapply(paste0('input/census/ACS_tracts/',grep('S1903_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
    dplyr::select(GEO.id,HC02_EST_VC02,YEAR) %>% rename(AFFGEOID = GEO.id,
                                                        HOUSEHOLD_MED_INCOME = HC02_EST_VC02)))
tract_income = tract_income %>%
  mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))


tract_ed = lapply(paste0('input/census/ACS_tracts/',grep('S1501_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
    mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))))
tract_ed[[1]] =  tract_ed[[1]]  %>% dplyr::select(GEO.id,HC01_EST_VC15,YEAR) %>%
  mutate(PERC_BACH = as.numeric(HC01_EST_VC15)) %>%
  dplyr::select(-HC01_EST_VC15) %>%
  rename(AFFGEOID = GEO.id)
tract_ed[-1] = lapply(tract_ed[-1],function(i) i %>% dplyr::select(GEO.id,HC01_EST_VC17,YEAR) %>%
                        mutate(PERC_BACH = as.numeric(HC01_EST_VC17)) %>%
                        dplyr::select(-HC01_EST_VC17) %>%
                        rename(AFFGEOID = GEO.id))
tract_ed = do.call(rbind,tract_ed)       

tract_poverty = do.call(rbind,
                        lapply(paste0('input/census/ACS_tracts/',grep('S1702_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
                          read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                            mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))) %>%
                            dplyr::select(GEO.id,HC02_EST_VC01,HC03_EST_VC01,YEAR) %>%
                            mutate(PERC_HOUSEHOLDS_POVERTY = as.numeric(ifelse(YEAR==2009,HC03_EST_VC01,HC02_EST_VC01))) %>%
                            dplyr::select(-HC02_EST_VC01,-HC03_EST_VC01) %>%
                            rename(AFFGEOID = GEO.id)))


tract_home = do.call(rbind,
                     lapply(paste0('input/census/ACS_tracts/',grep('B25077_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
                       read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                         dplyr::select(GEO.id,HD01_VD01,YEAR) %>%
                         rename(AFFGEOID = GEO.id,MED_HOME_VALUE = HD01_VD01)))
tract_home = tract_home  %>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))



tract_acs_data = plyr::join_all(list(tract_home,tract_ed,tract_poverty,tract_pops,tract_income),type='full')

tract_acs_data$MED_HOME_VALUE  = as.numeric(gsub('\\+','',gsub('\\,','',tract_acs_data$MED_HOME_VALUE)))
tract_acs_data$HOUSEHOLD_MED_INCOME = as.numeric(tract_acs_data$HOUSEHOLD_MED_INCOME)
tract_acs_data$PERC_HOUSEHOLDS_POVERTY = as.numeric(tract_acs_data$PERC_HOUSEHOLDS_POVERTY)
tract_acs_data$PERC_BACH = as.numeric(tract_acs_data$PERC_BACH)
tract_acs_data$TOTAL_POP = as.numeric(tract_acs_data$TOTAL_POP)
tract_acs_data$FYEAR = tract_acs_data$YEAR
tract_acs_data = tract_acs_data %>% arrange(AFFGEOID,FYEAR)

tract_year_combos = tract_acs_data %>% dplyr::select(-YEAR) %>% tidyr::expand(AFFGEOID,FYEAR = 2008:2015)
tract_acs_data = full_join(tract_acs_data %>% dplyr::select(-YEAR),tract_year_combos)

tract_acs_data = lapply(unique(tract_acs_data$AFFGEOID), function(i)
  tract_acs_data %>% arrange(AFFGEOID,FYEAR) %>% filter(AFFGEOID == i) %>% 
    fill(MED_HOME_VALUE:HOUSEHOLD_MED_INCOME,.direction = c( "up")) %>%
    fill(MED_HOME_VALUE:HOUSEHOLD_MED_INCOME,.direction = c( "down"))) %>% do.call(rbind,.)

tract_acs_data$POP_DENSITY_KMSQ = tract_acs_data$TOTAL_POP/
  (tx_tracts$ALAND[match(tract_acs_data$AFFGEOID,tx_tracts$AFFGEOID)]/1000000)


analyze_df$AFFGEOID = NA
analyze_df$AFFGEOID = service_df$AFFGEOID[match(analyze_df$PWS_NAME,service_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)] = other_df$AFFGEOID[match(analyze_df$PWS_NAME[is.na(analyze_df$AFFGEOID)],other_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)] = city_df$AFFGEOID[match(analyze_df$PWS_NAME[is.na(analyze_df$AFFGEOID)],city_df$NAME)]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('SUNBELT',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('SUNBELT',service_df$NAME)][1]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('NORTHEAST HARRIS COUNTY MUD 1 ',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('NORTHEAST HARRIS COUNTY MUD 1$',service_df$NAME)][1]
analyze_df$AFFGEOID[is.na(analyze_df$AFFGEOID)&grepl('HARRIS MONTGOMERY COUNTIES MUD 386 MAY V',analyze_df$PWS_NAME)] = service_df$AFFGEOID[grepl('HARRIS MONTGOMERY COUNTIES MUD 386',service_df$NAME)][1]

for (i in 1:nrow(analyze_df))
{if (grepl('SUNBELT',analyze_df$PWS_NAME[i]))
{
  analyze_df$AFFGEOID[i] = 
    service1@data$AFFGEOID[match(analyze_df$PWS_NAME[i],service1@data$NAME)]
}}

houston_tracts.df = left_join(houston_tracts.df,tract_acs_data[tract_acs_data$FYEAR==2014,])

library(viridis)
gg_income = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                                 fill = HOUSEHOLD_MED_INCOME))+
  scale_fill_viridis(direction = -1,option = 'inferno',name='Median Income',alpha =0.5,breaks=c(50,100,150,200)*1000,labels=c('$50k','$100k','$150k','$200k')) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)


gg_home = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                               fill = MED_HOME_VALUE)) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name='Median home price',alpha =0.5,breaks=c(25,50,75)*10000,labels=c('$750k','$500k','$250k')) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)




gg_pov = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                              fill = PERC_HOUSEHOLDS_POVERTY  )) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name='% < poverty line',alpha =0.5,breaks=c(25,50,75)) + 
  #scale_color_viridis(direction = -1,option = 'inferno',name='% < poverty line',alpha =0.5)+
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)

gg_bach = ggplot() + geom_polygon(data = houston_tracts.df,aes(x=long,y=lat,group=group,
                                                               fill = PERC_BACH)) + 
  scale_fill_viridis(direction = -1,option = 'inferno',name="% Bachelor's",alpha =0.5) + 
  theme_tufte(ticks=F) + 
  theme(axis.text=element_blank(),axis.title=element_blank(),
        legend.position = c(0.15,0.2),legend.title = element_text(size=16),legend.text=element_text(size=16))+
  geom_polygon(data = other_df[other_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)+
  geom_polygon(data = service_df[service_df$NAME %in% analyze_set$PWS_NAME,],aes(x=long,y=lat,group=group),fill='grey50',alpha = 0.7)

library(grid)
library(gridExtra)

gridExtra::grid.arrange(gg_income,gg_home,gg_pov,gg_bach)


library(ggmap)

# geo_query_df = data.frame('PWS_ID' = master_df$`PWS ID`,Address = paste(master_df$`Address Line1`,master_df$City,'TX',sep=' ')) %>% filter(!duplicated(.)) %>%
#   mutate(Address = as.character(Address))
# geo_query_df$Address[1:2]
# geolocs = geocode(geo_query_df$Address,output = 'more')
# geo_query_df = cbind(geo_query_df,geolocs)
# 
# master_df = left_join(master_df,geo_query_df)

plot_ett_df  = analyze_df %>% filter(PWS_NAME %in% analyze_set$PWS_NAME) %>% dplyr::select(PWS_NAME,FYEAR,hviol_count_p3) %>%
  tidyr::complete(PWS_NAME,FYEAR,fill=list(hviol_count_p3 = 0)) %>% filter(FYEAR>=2008)


# 
# ggplot() + geom_dotplot(aes(x=ett_score_p1,fill = ett_score_p1>10,colour=ett_score_p1>10),data=plot_ett_df,binwidth=1) + facet_wrap(~FYEAR) + 
#   theme_tufte(ticks=F) + 
#   theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text.y=element_blank(),axis.text.x=element_text(size = 18),
#         axis.title.y=element_blank(),axis.title.x=element_text(size=18),strip.text=element_text(size=18))+
#   xlab('Enforcement targeting tool score in prior fiscal year') + 
#   scale_colour_colorblind(labels= c('Below enforcement target threshold','Above enforcement target threshold'))+ 
#   scale_fill_colorblind(labels= c('Below enforcement target threshold','Above enforcement target threshold'))
# 


ggplot(data = plot_ett_df %>% group_by(FYEAR) %>% summarise(hsum = sum(hviol_count_p3))) + 
  geom_bar(aes(x=FYEAR,y=hsum),stat='identity') + 
  theme_tufte(ticks=F) + 
  theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text.y=element_text(size=18),axis.text.x=element_text(size = 18),
        axis.title=element_text(size=18),axis.title.x=element_text(size=18),strip.text=element_text(size=18))+
  scale_x_continuous(expand=c(0,0),name='Fiscal Year',
                     breaks=seq(2008,2014,2),labels=seq(2008,2014,2)) + 
  ylab("Health violations observed in 3 prior fiscal years")


###DEBT

#debt_df = debt_df %>% filter(PWS_NAME %in% system_df$PWS_NAME)
##### load and rectify debt data
library(readxl)
debt_df = do.call(rbind,lapply(paste0('input/fiscal_data/',grep('WD_debt',list.files('input/fiscal_data/'),value=T)),function(x) read_excel(x) %>% 
                                 mutate('Year' = x)%>% filter(!is.na(Issuer)) %>% 
                                 mutate(Issuer = str_to_upper(Issuer)))) %>% 
  group_by(GovtID,Issuer,FYear) %>% 
  summarise(IssueSize = sum(IssueSize,na.rm=T),
            RefundingSize = sum(RefundingSize,na.rm=T),
            NewMoneySize = sum(NewMoneySize,na.rm=T))

fiscal_df = do.call(rbind,
                    lapply(paste0('input/fiscal_data/',grep('WDTR_clean',list.files('input/fiscal_data/'),value=T)),
                           function(x) read_excel(x) %>% mutate('Year' = x) %>% filter(!is.na(Issuer))%>%
                             mutate(Issuer = str_to_upper(Issuer))))


debt_df = left_join(fiscal_df,debt_df)
debt_df$Issuer = str_to_upper(debt_df$Issuer)
debt_df$Issuer = gsub(' 0{1,}',' ',debt_df$Issuer)
debt_df$Issuer = gsub('POST WOOD','POSTWOOD',debt_df$Issuer)
debt_df$Issuer = gsub('CLOVERCREEK','CLOVER CREEK',debt_df$Issuer)
debt_df$Issuer = gsub('MEADOW CREEK','MEADOWCREEK',debt_df$Issuer)
debt_df$Issuer = gsub('SPECIAL UD','SUD',debt_df$Issuer)
debt_df$Issuer = gsub('THUNDERBIRD UD$','THUNDERBIRD UD 1',debt_df$Issuer)
debt_df$Issuer = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",debt_df$Issuer)
debt_df$Issuer = gsub('REGIONAL WA','RWA',debt_df$Issuer)
debt_df$Issuer = gsub('-',' ',debt_df$Issuer)
debt_df$Issuer = gsub("134 C",'134C',debt_df$Issuer)
debt_df$Issuer = gsub("134 D",'134D',debt_df$Issuer)
debt_df$Issuer = gsub("134 A",'134A',debt_df$Issuer)
debt_df$Issuer = gsub("5\\\v",'5',debt_df$Issuer)
debt_df$Issuer = gsub("MUD THE$|MUD\\, THE$",'MUD',debt_df$Issuer)
debt_df$Issuer = gsub("OAK MANOR UD",'OAK MANOR MUD',debt_df$Issuer)
debt_df$Issuer = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",debt_df$Issuer)
debt_df$Issuer = gsub("'",'',debt_df$Issuer)
debt_df = debt_df %>% rename(PWS_NAME = Issuer,FYEAR = FYear)


analyze_df =  left_join(analyze_df,debt_df)

analyze_df$NEW_DEBT = analyze_df$NewMoneySize>0
analyze_df$NEW_DEBT[is.na(analyze_df$NEW_DEBT)] = FALSE
analyze_df$DEBT_MATCH = FALSE
analyze_df$DEBT_MATCH[analyze_df$PWS_NAME %in% debt_df$PWS_NAME] = TRUE

master_df = analyze_df %>% filter(grepl('MUD|FWSD|WCID',PWS_NAME) & !is.na(FYEAR) & FYEAR>=2007) %>% left_join(.,tract_acs_data)

master_df = master_df %>% filter(PWS_NAME != 'CITY OF HOUSTON HARRIS COUNTY MUD 159')

master_df$NewMoneySize[is.na(master_df$NewMoneySize)] = 0
master_df$TotDebtServiceOutstanding[is.na(master_df$TotDebtServiceOutstanding)] = 0

master_df = master_df[!(master_df$`Is Wholesaler`=='Y'&master_df$`Population Served Count`==0),]


master_df = do.call(rbind,lapply(unique(master_df$PWS_NAME),function(uq)
  master_df %>% filter(PWS_NAME == uq) %>%
    arrange(FYEAR) %>% 
    fill(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,`CURRENT ASSESSED VALUATION`,`GENERAL FUND - TOTAL REVENUES`,`GENERAL FUND - FUND BALANCE`,.direction=c('up'))%>%
    fill(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,`CURRENT ASSESSED VALUATION`,`GENERAL FUND - TOTAL REVENUES`,`GENERAL FUND - FUND BALANCE`,.direction=c('down'))
))

master_df = master_df %>% 
  mutate(TotDebtServiceOutstanding_1m = TotDebtServiceOutstanding / 1000000,
         NewMoneySize_1m = NewMoneySize / 1000000,
         Assessed_Value_100m = `CURRENT ASSESSED VALUATION` / 100000000,
         Expenditures_1m = `GENERAL FUND - TOTAL EXPENDITURES` / 1000000,
         Revenues_1m = `GENERAL FUND - TOTAL REVENUES` / 1000000,
         FundBalance_1m = `GENERAL FUND - FUND BALANCE` / 1000000,
         Family_Units_100 = `WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,
         Pop_Served_1k = `Population Served Count`/1000)

master_df$Revenues_1m_p1 = master_df$Revenues_1m[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]
master_df$FundBalance_1m_p1 = master_df$FundBalance_1m[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]

master_df$SINGLE_FAMILY_UNITS_p1 = master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match(paste0(master_df$PWS_ID,master_df$FYEAR-1), paste0(master_df$PWS_ID,master_df$FYEAR))]

master_df$SERVICE_POP_CHANGE_P1 = 100*((master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS` - master_df$SINGLE_FAMILY_UNITS_p1) / master_df$SINGLE_FAMILY_UNITS_p1)

master_df$SERVICE_POP_CHANGE_P1[master_df$SERVICE_POP_CHANGE_P1==-Inf] = 0
master_df$SERVICE_POP_CHANGE_P1[master_df$SERVICE_POP_CHANGE_P1==Inf] = 0


master_df = master_df %>% filter(is.na(`Deactivation Date`)|(decimal_date(dmy(master_df$`Deactivation Date`))>FYEAR_END_DDATE)) 

#master_df = master_df %>% filter(!is.na(audit_observed)) 
uq_ids = data.frame(PWS_ID = unique(master_df$PWS_ID),
                    uq_sysid = 1:length(unique(master_df$PWS_ID)))

master_df = left_join(master_df,uq_ids)

master_df$GROUNDWATER = ifelse(grepl('Ground',master_df$`Primary Source`),1,0)
master_df$PURCHASED = ifelse(grepl('purch',master_df$`Primary Source`),1,0)
master_df$WHOLESALER = ifelse(master_df$`Is Wholesaler`=='Y',1,0)


master_df = master_df %>% filter(FYEAR>=2008)

year_tract_medians = master_df %>% dplyr::select(FYEAR,MED_HOME_VALUE,PERC_BACH,PERC_HOUSEHOLDS_POVERTY,
                                                 HOUSEHOLD_MED_INCOME) %>% group_by(FYEAR) %>%
  summarise_all( median,na.rm=T)

master_df$MED_HOME_VALUE[is.na(master_df$MED_HOME_VALUE)] = year_tract_medians$MED_HOME_VALUE[match(master_df$FYEAR[is.na(master_df$MED_HOME_VALUE)],year_tract_medians$FYEAR)]
master_df$PERC_BACH[is.na(master_df$PERC_BACH)] = year_tract_medians$PERC_BACH[match(master_df$FYEAR[is.na(master_df$PERC_BACH)],year_tract_medians$FYEAR)]
master_df$PERC_HOUSEHOLDS_POVERTY[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)] = year_tract_medians$PERC_HOUSEHOLDS_POVERTY[match(master_df$FYEAR[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)],year_tract_medians$FYEAR)]
master_df$HOUSEHOLD_MED_INCOME[is.na(master_df$HOUSEHOLD_MED_INCOME)] = year_tract_medians$HOUSEHOLD_MED_INCOME[match(master_df$FYEAR[is.na(master_df$HOUSEHOLD_MED_INCOME)],year_tract_medians$FYEAR)]


library(INLA)

## plot DV
ggplot(master_df %>% filter(NewMoneySize_1m>0)) + geom_histogram(aes(x=NewMoneySize_1m),binwidth=1) + facet_wrap(~FYEAR,scales = 'free_y') +
  theme_tufte(ticks=F)+
  theme(legend.position = c(0.7,0.1),legend.text=element_text(size=16),legend.title=element_blank(),axis.text=element_text(size = 18),
        axis.title=element_text(size=18),strip.text=element_text(size=18)) +
  scale_x_continuous('$ millions of new debt issued') + scale_y_continuous('# issuing systems') +
  annotate('text',x=35,y=10,label=paste0(100 * round(tapply(master_df$NewMoneySize_1m>0,master_df$FYEAR,mean),2),'% issuing'),size=5)

master_df$IS_FWSD = ifelse(grepl('FWSD',master_df$PWS_NAME),1,0)

master_df$IS_WCID = ifelse(grepl('WCID',master_df$PWS_NAME),1,0)

#gCentroid(service1,byid = T)



#### MODEL SETUP ###

n = nrow(master_df)
u <- (master_df$NewMoneySize_1m>0) + 0
y <- ifelse(master_df$NewMoneySize_1m>0,master_df$NewMoneySize_1m,NA)

idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)

idat$u_regionid = c(master_df$uq_sysid,master_df$uq_sysid)
idat$y_regionid = c(master_df$uq_sysid,master_df$uq_sysid)

#idat$u_ett_health_score <- c(ifelse(master_df$ett_health_score_p1>10,1,0), rep(0,n))
#idat$y_ett_health_score <- c(rep(0,n), ifelse(master_df$ett_health_score_p1>10,1,0))
#idat$u_ett_management_score <- c(ifelse(master_df$ett_management_score_p1>10,1,0), rep(0,n))
#idat$y_ett_management_score <- c(rep(0,n), ifelse(master_df$ett_management_score_p1>10,1,0))
#idat$u_ett_score <- c(ifelse(master_df$ett_score_p1>10,1,0), rep(0,n))
#idat$y_ett_score <- c(rep(0,n), ifelse(master_df$ett_score_p1>10,1,0))

idat$u_hviols_p5 <- c(ifelse(master_df$hviol_count_p5>0,1,0), rep(0,n))
idat$y_hviols_p5 <- c(rep(0,n),ifelse(master_df$hviol_count_p5>0,1,0))
idat$u_mviols_p5 <- c(ifelse(master_df$mviol_count_p5>0,1,0), rep(0,n))
idat$y_mviols_p5 <- c(rep(0,n), ifelse(master_df$mviol_count_p5>0,1,0))

idat$u_hviols_p4 <- c(ifelse(master_df$hviol_count_p4>0,1,0), rep(0,n))
idat$y_hviols_p4 <- c(rep(0,n),ifelse(master_df$hviol_count_p4>0,1,0))
idat$u_mviols_p4 <- c(ifelse(master_df$mviol_count_p4>0,1,0), rep(0,n))
idat$y_mviols_p4 <- c(rep(0,n), ifelse(master_df$mviol_count_p4>0,1,0))

idat$u_hviols_p3 <- c(ifelse(master_df$hviol_count_p3>0,1,0), rep(0,n))
idat$y_hviols_p3 <- c(rep(0,n),ifelse(master_df$hviol_count_p3>0,1,0))
idat$u_mviols_p3 <- c(ifelse(master_df$mviol_count_p3>0,1,0), rep(0,n))
idat$y_mviols_p3 <- c(rep(0,n), ifelse(master_df$mviol_count_p3>0,1,0))

idat$u_hviols_p2 <- c(ifelse(master_df$hviol_count_p2>0,1,0), rep(0,n))
idat$y_hviols_p2 <- c(rep(0,n),ifelse(master_df$hviol_count_p2>0,1,0))
idat$u_mviols_p2 <- c(ifelse(master_df$mviol_count_p2>0,1,0), rep(0,n))
idat$y_mviols_p2 <- c(rep(0,n), ifelse(master_df$mviol_count_p2>0,1,0))

idat$u_hviols_p1 <- c(ifelse(master_df$hviol_count_p1>0,1,0), rep(0,n))
idat$y_hviols_p1 <- c(rep(0,n),ifelse(master_df$hviol_count_p1>0,1,0))
idat$u_mviols_p1 <- c(ifelse(master_df$mviol_count_p1>0,1,0), rep(0,n))
idat$y_mviols_p1 <- c(rep(0,n), ifelse(master_df$mviol_count_p1>0,1,0))

idat$u_fyear <- c(master_df$FYEAR, rep(0,n))
idat$y_fyear <- c(rep(0,n), master_df$FYEAR)
idat$u_fyear2 <- c(master_df$FYEAR, rep(0,n))
idat$y_fyear2 <- c(rep(0,n), master_df$FYEAR)
idat$u_debt_outstanding_1m <- c(scale(master_df$TotDebtServiceOutstanding_1m,center = T,scale=F), rep(0,n))
idat$y_debt_outstanding_1m <- c(rep(0,n), scale(master_df$TotDebtServiceOutstanding_1m,center = T,scale=F))



### To Does
# need population density
# need sevice population change
#system variables 
idat$u_service_pop_1k <- c(scale(master_df$Pop_Served_1k,center = T,scale=F), rep(0,n))
idat$y_service_pop_1k <- c(rep(0,n), scale(master_df$Pop_Served_1k,center = T,scale=F))
idat$u_num_facilities <- c(scale(master_df$`# of Facilities`,center=F,scale=F), rep(0,n))
idat$y_num_facilities <- c(rep(0,n), scale(master_df$`# of Facilities`,center=F,scale=F))
idat$u_system_age <-c(ifelse(master_df$FYEAR - master_df$Year_Start <0, 0,master_df$FYEAR - master_df$Year_Start), rep(0,n))
idat$y_system_age <-c(rep(0,n),ifelse(master_df$FYEAR - master_df$Year_Start <0, 0,master_df$FYEAR - master_df$Year_Start))
idat$u_assessed_value_100m <- c(scale(master_df$Assessed_Value_100m,center=F,scale=F), rep(0,n))
idat$y_assessed_value_100m <- c(rep(0,n), scale(master_df$Assessed_Value_100m,center=F,scale=F))
idat$u_total_revenue_1m <- c(scale(master_df$Revenues_1m_p1,center=F,scale=F), rep(0,n))
idat$y_total_revenue_1m <- c(rep(0,n), scale(master_df$Revenues_1m_p1,center=F,scale=F))
idat$u_total_revenue_1m <- c(scale(master_df$Revenues_1m_p1,center=F,scale=F), rep(0,n))
idat$y_fund_balance_1m_p1 <- c(rep(0,n), scale(master_df$FundBalance_1m_p1,center=F,scale=F))
idat$u_fund_balance_1m_p1 <- c( scale(master_df$FundBalance_1m_p1,center=F,scale=F), rep(0,n))



idat$u_expenditures_10m <- c(scale(master_df$Expenditures_1m,center=F,scale=F), rep(0,n))
idat$y_expenditures_10m <- c(rep(0,n),scale(master_df$Expenditures_1m,center=F,scale=F))
idat$u_service_change_perc <- c(master_df$SERVICE_POP_CHANGE_P1, rep(0,n))
idat$y_service_change_perc <- c(rep(0,n),master_df$SERVICE_POP_CHANGE_P1)
idat$u_primary_ground <- c(master_df$GROUNDWATER, rep(0,n))
idat$y_primary_ground <- c(rep(0,n),master_df$GROUNDWATER)
idat$u_primary_purch <- c(master_df$PURCHASED, rep(0,n))
idat$y_primary_purch <- c(rep(0,n),master_df$PURCHASED)
idat$u_wholesaler <- c(master_df$WHOLESALER, rep(0,n))
idat$y_wholesaler <- c(rep(0,n),master_df$WHOLESALER)
idat$u_fwsd <- c(master_df$IS_FWSD, rep(0,n))
idat$y_fwsd <- c(rep(0,n),master_df$IS_FWSD)
idat$u_wcid <- c(master_df$IS_WCID, rep(0,n))
idat$y_wcid <- c(rep(0,n),master_df$IS_WCID)
idat$u_PWS_ID <- c(as.character(master_df$PWS_ID), rep(NA,n))
idat$y_PWS_ID <- c(rep(NA,n), as.character(master_df$PWS_ID))

center_continuous_cov = TRUE
### census variables
idat$u_med_income_10k <- c(scale(master_df$HOUSEHOLD_MED_INCOME/10000,center=center_continuous_cov,scale=F),rep(0,n))
idat$y_med_income_10k <- c(rep(0,n), scale(master_df$HOUSEHOLD_MED_INCOME/10000,center=center_continuous_cov,scale=F))
idat$u_med_home_10k <- c(scale(master_df$MED_HOME_VALUE/10000,center=center_continuous_cov,scale=F),rep(0,n))
idat$y_med_home_10k <- c(rep(0,n), scale(master_df$MED_HOME_VALUE/10000,center=center_continuous_cov,scale=F))
idat$u_perc_bach <- c(scale(master_df$PERC_BACH,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_perc_bach <- c(rep(0,n), scale(master_df$PERC_BACH,center=center_continuous_cov,scale=F))
idat$u_perc_pov <- c(scale(master_df$PERC_HOUSEHOLDS_POVERTY,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_perc_pov <- c(rep(0,n),scale(master_df$PERC_HOUSEHOLDS_POVERTY,center=center_continuous_cov,scale=F))
idat$u_pop100_dens_sqkm <- c(scale(master_df$POP_DENSITY_KMSQ/100,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_pop100_dens_sqkm <- c(rep(0,n),scale(master_df$POP_DENSITY_KMSQ/100,center=center_continuous_cov,scale=F))
idat$y.i <- idat$u.i <- c(1:n, 1:n)


save.image('bypass_run.RData')


#########
### To Does
# need sevice population change

# ggplot() + geom_point(aes(y=master_df$NewMoneySize_1m[master_df$NewMoneySize_1m>0], 
# x=master_df$ett_score_p1[master_df$NewMoneySize_1m>0]),pch=1) + 
#   scale_y_continuous(name = 'New debt issued ($ millions) for issue > $0') +
#   scale_x_continuous(name = 'Enforcement Targeting Tool Score') +
#   theme_tufte(ticks=F) + theme(axis.text = element_text(size=16),
#                                axis.title = element_text(size=18))
# 
# ggplot() + geom_boxplot(aes(y=master_df$NewMoneySize_1m[master_df$NewMoneySize_1m>0], 
#                           x=as.factor(master_df$hviol_count_p3[master_df$NewMoneySize_1m>0])),pch=1) + 
#   scale_y_continuous(name = 'New debt issued ($ millions) for issue > $0') +
#   scale_x_discrete(name = 'Health violations in past 3 years') +
#   theme_tufte(ticks=F) + theme(axis.text = element_text(size=16),
#                                axis.title = element_text(size=18))

base_form = "Y ~ 0 + mu.u + mu.y +  u_perc_pov + y_perc_pov + 
u_med_home_10k  + y_med_home_10k+
u_fund_balance_1m_p1 +   y_fund_balance_1m_p1 + 
u_total_revenue_1m + y_total_revenue_1m +
u_service_pop_1k + y_service_pop_1k + 
u_num_facilities + y_num_facilities + 
u_system_age + y_system_age + 
u_debt_outstanding_1m  + y_debt_outstanding_1m +
u_primary_ground+u_primary_purch +
y_primary_ground+y_primary_purch + 
u_fwsd + y_fwsd + 
u_wcid + y_wcid"

random_form = "f(u_fyear, model='iid') +
f(y_fyear,model='iid')+
f(u_fyear2, model='rw1') +
f(y_fyear2,copy='u_fyear', fixed=FALSE)+
f(u_PWS_ID, model='iid') +
f(y_PWS_ID,model='iid')"


test = as.formula(gsub('pp','p3',paste(base_form,
                                       "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp*u_perc_pov*u_med_income_10k +  y_hviols_pp*y_perc_pov*y_med_income_10k",
                                       random_form,sep='+')))

test_mod = inla(test,c('binomial', 'gamma'),
                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                control.predict=list(compute=TRUE),verbose=F)



capacity_interaction_template = c(
  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_total_revenue_1m +  y_hviols_pp:y_total_revenue_1m +" ,
  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_fund_balance_1m_p1 +  y_hviols_pp:y_fund_balance_1m_p1 +",
  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_med_income_10k +  y_hviols_pp:y_med_income_10k +" ,
  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_perc_pov +  y_hviols_pp:y_perc_pov")


capacity_interactions = unlist(lapply(as.list(paste0('p',1:5)),function(x) gsub('pp',x,capacity_interaction_template)))

form_temps = expand.grid(gsub('\\\n','',base_form),capacity_interactions,gsub('\\\n','',random_form))

form_list = lapply(1:nrow(form_temps),function(x) paste(form_temps[x,1],form_temps[x,2],form_temps[x,3],sep='+'))


# result_ett_gauss_2way <- inla(form_ett_2way, c('binomial', 'gaussian'),
#                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                control.predict=list(compute=TRUE),verbose=F)
# 
# result_hviols_gauss_2way <- inla(form_hviols_2way, c('binomial', 'gaussian'),
#                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                      control.predict=list(compute=TRUE),verbose=F)

all_results = lapply(form_list,function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                                   data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                   control.predict=list(compute=TRUE),verbose=F))

names(all_results) = paste(str_extract(gsub('.*:','',form_temps[,2]),'revenue|balance|income|pov'),str_extract(form_temps[,2],'viols_p[0-5]'),sep='_')


coef_results = do.call(rbind,lapply(1:length(all_results),function(x) all_results[[x]]$summary.fix[,c(1,3,5)] %>% 
                                      mutate(COEF = rownames(.),MODEL = names(all_results)[x],
                                             LIK = ifelse(grepl('^y_|\\.y$',COEF),'1.Gamma','2.Binomial')) %>%
                                      mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% 
                                      mutate(COEF = gsub('_p[1-5]$','',COEF)) %>%
                                      mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)))) %>%
  mutate(SIG_FILL = paste(SIG,LIK))

coef_results$COEF = fct_recode(f = coef_results$COEF,
                               `Management viol.` = 'mviols', 
                               `Health viol.` = 'hviols', 
                               `FWSD` = 'fwsd', 
                               `WCID` = 'wcid', 
                               `Fund balance ($1M)` = 'fund_balance_1m',
                               `Revenue ($1M)` = 'total_revenue_1m',
                               `Revenue ($1M):Health viol.` = 'total_revenue_1m:hviols',
                               `Fund balance ($1M):Health viol.` = 'fund_balance_1m_p1:hviols',
                               `Med. income ($10k):Health viol.` = 'med_income_10k:hviols',
                               `% Poverty:Health viol.` = 'perc_pov:hviols',
                               # `Service pop.:ETT score>10` = 'service_pop_1k:ett_score',
                               #`Service pop.:Health viol.` = 'service_pop_1k:hviols_p3',
                               #   Wholesaler = 'wholesaler',
                               `System age (y)` = 'system_age',`Service pop. (1k)` = 'service_pop_1k',
                               Purchaser = 'primary_purch',
                               Groundwater = 'primary_ground',
                               `% Poverty` = 'perc_pov',
                               # `% Bachelor's` = 'perc_bach',
                               `# facilities` = 'num_facilities',
                               `Med. income ($10k)` = 'med_income_10k',`Outstanding debt ($1M)` = 'debt_outstanding_1m')

coef_results$COEF = factor(coef_results$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty",#"% Bachelor's",
                                                        "Med. income ($10k)",
                                                        '# facilities','System age (y)','Groundwater','Purchaser',
                                                        # 'Purchaser:Groundwater',
                                                        #  'Wholesaler',
                                                        'FWSD','WCID',
                                                        "Service pop. (1k)",
                                                        'Outstanding debt ($1M)',
                                                        'Fund balance ($1M)',
                                                        #'Tax base ($100M)',
                                                        'Revenue ($1M)',
                                                        'Management viol.','Health viol.',
                                                        'Revenue ($1M):Health viol.',"Fund balance ($1M):Health viol.",
                                                        "Med. income ($10k):Health viol.","% Poverty:Health viol."))


coef_results$COEF = fct_rev(coef_results$COEF)

p3_results = coef_results %>% filter(!grepl('^mu',COEF),grepl('p3',MODEL))
p3_results$MODEL = as.factor(p3_results$MODEL) 
levels(p3_results$MODEL) = c('Model 2','Model 3','Model 4','Model 1')
p3_results$MODEL = factor(p3_results$MODEL,levels = c('Model 1','Model 2','Model 3','Model 4'))


gg_gamma = ggplot(p3_results) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=2) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=.5) + 
  coord_flip() + facet_grid(~MODEL) +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.8,0.15)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black')) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) +guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))

gg_gamma
gg_gamma_2 = ggplot(p3_results %>% filter(grepl('Rev|balance|income|Poverty|Health',COEF))) +
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=3) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=1) + 
  coord_flip() + facet_grid(~MODEL) +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.8,0.25)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black'),show) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) + 
  guides(fill='none',colour = guide_legend(override.aes = list(shape = 19))) 
gg_gamma

cor(master_df$PERC_HOUSEHOLDS_POVERTY,
    master_df$HOUSEHOLD_MED_INCOME,use = 'pairwise.complete.obs')

result_ett_gauss_2way <- inla(form_ett_2way, c('binomial', 'gaussian'),
                              data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                              control.predict=list(compute=TRUE),verbose=F)

result_hviols_gauss_2way <- inla(form_hviols_2way, c('binomial', 'gaussian'),
                                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                 control.predict=list(compute=TRUE),verbose=F)



coefs_gauss = rbind(
  result_ett_gauss_2way$summary.fix[,c(1,3,5)] %>% mutate(COEF = rownames(.),MODEL = 'ETT score > 10',LIK = ifelse(grepl('^y_',COEF),'1.Gauss','2.Binomial')) %>%
    mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)),
  result_hviols_gauss_2way$summary.fix[,c(1,3,5)] %>% mutate(COEF = rownames(.),MODEL = 'Health viol. in past 3 yrs',LIK = ifelse(grepl('^y_',COEF),'1.Gauss','2.Binomial')) %>%
    mutate(COEF = gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))) %>%
  mutate(SIG_FILL = paste(SIG,LIK),COEF = as.factor(COEF))


coefs_gauss$COEF = fct_recode(f = coefs_gauss$COEF,
                              `Management viol. in past 3 yrs` = 'mviols_p3', 
                              `Health viol. in past 3 yrs` = 'hviols_p3', 
                              `FWSD` = 'fwsd', 
                              `WCID` = 'wcid', 
                              `Fund balance ($100k)` = 'fund_balance_100K_p1',
                              `Revenue ($1M)` = 'total_revenue_1m',
                              `Revenue ($1M):ETT score>10` = 'total_revenue_1m:ett_score',
                              `Revenue ($1M):Health viol.` = 'total_revenue_1m:hviols_p3',
                              # `Service pop.:ETT score>10` = 'service_pop_1k:ett_score',
                              #`Service pop.:Health viol.` = 'service_pop_1k:hviols_p3',
                              #   Wholesaler = 'wholesaler',
                              `System age (y)` = 'system_age',`Service pop. (1k)` = 'service_pop_1k',
                              Purchaser = 'primary_purch',
                              Groundwater = 'primary_ground',
                              # `Purchaser:Groundwater` = 'primary_ground:primary_purch',
                              `% Poverty` = 'perc_pov',`% Bachelor's` = 'perc_bach',`# facilities` = 'num_facilities',
                              #`Tax base ($100M)` = 'assessed_value_100m',
                              `Med. income ($10k)` = 'med_income_10k',
                              #`Med. income ($10k)` = 'med_income_10k',
                              `ETT score>10` = 'ett_score',`Outstanding debt ($1M)` = 'debt_outstanding_1m')

coefs_gauss$COEF = factor(coefs_gauss$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty","% Bachelor's",
                                                      "Med. income ($10k)",
                                                      '# facilities','System age (y)','Groundwater','Purchaser',
                                                      # 'Purchaser:Groundwater',
                                                      #  'Wholesaler',
                                                      'FWSD','WCID',
                                                      "Service pop. (1k)",
                                                      'Outstanding debt ($1M)',
                                                      'Fund balance ($100k)',
                                                      #'Tax base ($100M)',
                                                      'Revenue ($1M)',
                                                      'Management viol. in past 3 yrs','Health viol. in past 3 yrs',
                                                      'Revenue ($1M):Health viol.',
                                                      'ETT score>10',
                                                      "Revenue ($1M):ETT score>10"))


coefs_gauss$COEF = fct_rev(coefs_gauss$COEF)


gg_gauss = ggplot(coefs_gauss %>% filter(!grepl('^mu',COEF))) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=3) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=1) + 
  coord_flip() + facet_grid(~MODEL) +
  theme_tufte(ticks=F) + theme(legend.position = c(0.6,0.1)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Likelihood component',values=c('white','white','#E69F00','black'),show) +
  scale_color_manual(name = 'Likelihood component',values=c('#E69F00','black'),labels=c('Gaussian','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=16),legend.position = c(0.5,0.1),legend.title=element_text(size=16),strip.text=element_text(size=16),axis.text=element_text(size=16),
        axis.title=element_blank()) + 
  guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))



