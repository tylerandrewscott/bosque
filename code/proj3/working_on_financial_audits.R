library(tidyverse)
library(lubridate)

system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
  rename(PWS_NAME = Name)

system_df$PWS_NAME <- gsub(' Fact.*','',system_df$PWS_NAME)

system_df$PWS_NAME[grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME)] = 
  str_extract(grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME,value=T),'FORT BEND COUNTY MUD [0-9]{1,}[A-Z]{0,2}')
system_df$`Owner Type`[system_df$PWS_NAME=='FORT BEND COUNTY MUD 194'] = 'Local government'
system_df$PWS_NAME = gsub('HCO',"HARRIS COUNTY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' NO '," ",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 24 COUNTRY COLONY","MONTGOMERY COUNTY MUD 24",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 6 CARRIAGE LANE","HARRIS COUNTY MUD 6",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 119 SPRING TRAILS","MONTGOMERY COUNTY MUD 119",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 400   WEST","HARRIS COUNTY MUD 400",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 55 HERITAGE PARK","HARRIS COUNTY MUD 55",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 148 KINGSLAKE","HARRIS COUNTY MUD 148",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 374 CYPRESS CREEK LAKE","HARRIS COUNTY MUD 374",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("WEST HARRIS COUNTY MUD 2 CHASE","WEST HARRIS COUNTY MUD 2",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('WATER SUPPLY CORPORATION','WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT','UD',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('REGIONAL WATER AUTHOR$|REGIONAL WATER AUTHORITY','RWA',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('HARRIS COUNTY MUD 400 - WEST','HARRIS COUNTY MUD 400',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 113 ENCHANTED VILLAGE","HARRIS COUNTY WCID 113",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THE WOODLANDS",'WOODLANDS',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('BRAZORIA COUNTY FWSD 1 DAMON','BRAZORIA COUNTY FWSD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD1",'MUD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD3",'MUD 3',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THUNDERBIRD UD 1","THUNDERBIRD UD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CYPRESS KLEIN UD WIMBLETON","CYPRESS KLEIN MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" TOWNE LAKE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GULF COAST WATER AUTHORITY TX CITY","GULF COAST WATER AUTHORITY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("LIBERTY COUNTY FWSD 1 HULL","HULL FWSD",system_df$PWS_NAME)

system_df$PWS_NAME = gsub("BROOK HOLLOW WEST S",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" FAIRFIELD VILLAGE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('TOWN OF','CITY OF',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND" ,"GALVESTON COUNTY FWSD 6" ,system_df$PWS_NAME)

system_df$PWS_NAME = gsub("CINCO SOUTHWEST MUD 3 DAYCARE","CINCO SOUTHWEST MUD 3",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 50 EL LAGO","HARRIS COUNTY WCID 50",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('DOBBIN PLANTERSVILLE WSC 1','DOBBIN PLANTERSVILLE WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 200 CRANBROOK","HARRIS COUNTY MUD 200",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('CENTRAL HARRIS COUNTY REGIONAL WATER AUT$','CENTRAL HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('NORTH HARRIS COUNTY REGIONAL WATER AUTHO$','NORTH HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 16 WHITE OAK PLANT","MONTGOMERY COUNTY MUD 16",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CITY OF WOOD BRANCH VILLAGE","CITY OF WOODBRANCH",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('-',' ',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' $','',system_df$PWS_NAME)

epa_systems = read_csv('input/epa_sdwis/water_system_detail.csv') %>% rename(PWS_ID = `PWS ID`) %>%
  filter(`Owner Type` != 'State government') %>% filter(!grepl('DETENTIO|ACADEMY',`PWS Name`))
system_df = system_df %>% filter(PWS_ID %in% epa_systems$PWS_ID)


#read_csv('input/tceq_audits/master_district_reference.csv')
district_audits = read_csv('input/tceq_audits/master_doc_reference.csv')
audit_yearly = read_csv('input/tceq_audits/yearly_district_audits.csv')

audit_yearly  = audit_yearly %>% mutate(
  `CURRENT ASSESSED VALUATION` = as.numeric(gsub('\\,','',gsub('\\)','',
                                                               gsub('\\$','',gsub('\\(','-',`CURRENT ASSESSED VALUATION`))))),
  `GENERAL FUND - FUND BALANCE` = as.numeric(gsub('\\,','',gsub('\\)','',
                                                                gsub('\\$','',gsub('\\(','-',`GENERAL FUND - FUND BALANCE`))))),
  `GENERAL FUND - ASSETS` = as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`GENERAL FUND - ASSETS`))))),
  `GENERAL FUND - TOTAL REVENUES` = as.numeric(gsub('\\)','',gsub('\\,','', gsub('\\$','',gsub('\\(','-',`GENERAL FUND - TOTAL REVENUES`))))),
  `GENERAL FUND - TOTAL EXPENDITURES` = as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`GENERAL FUND - TOTAL EXPENDITURES`))))),
  `ENTERPRISE FUND - OPERATING EXPENSES` = as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`ENTERPRISE FUND - OPERATING EXPENSES`))))),
  `ENTERPRISE FUND - OPERATING REVENUES` = as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`ENTERPRISE FUND - OPERATING REVENUES`))))),
  `ENTERPRISE FUND - NET ASSETS` =as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`ENTERPRISE FUND - NET ASSETS`))))),
  `CURRENT ASSESSED VALUATION` = ifelse(`CURRENT ASSESSED VALUATION` == 0,NA,`CURRENT ASSESSED VALUATION`),
  `BONDS OUTSTANDING` = as.numeric(gsub('\\,','',gsub('\\)','', gsub('\\$','',gsub('\\(','-',`BONDS OUTSTANDING`))))))


audit_yearly$`FISCAL YEAR ENDED`[audit_yearly$DOC_ID==219357] = '08/31/2005'
audit_yearly$`FISCAL YEAR ENDED`[audit_yearly$SYSTEM_NAME=='HARRIS COUNTY MUD 316'&
                                   audit_yearly$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`==466] <- "12/31/2013"
audit_yearly$`FISCAL YEAR ENDED`[audit_yearly$SYSTEM_NAME=='CY CHAMP PUD'&
                                   audit_yearly$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`==1452] = "12/31/2004"

audit_yearly = audit_yearly[!duplicated(
  audit_yearly[,!names(audit_yearly) %in% c('WATER CUSTOMERS - EQ SINGLE FAMILY UNITS','X1','DOC_ID','DOC_LINK')]),] %>%
  mutate(`FISCAL YEAR ENDED` = mdy(`FISCAL YEAR ENDED`)) %>%
  filter(!(SYSTEM_NAME=='THE WOODLANDS METRO CENTER MUD'&`GENERAL FUND - TOTAL REVENUES`==1308844),
         !(SYSTEM_NAME=='THE WOODLANDS METRO CENTER MUD'&`GENERAL FUND - TOTAL REVENUES`==0)) %>% 
  filter(!(DOC_ID %in% c(230572, 299490,232525,364602,232367,233707,233231)))

audit_yearly = audit_yearly %>% arrange(SYSTEM_NAME,`FISCAL YEAR ENDED`,
                                        -`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)
switch <- audit_yearly$`FISCAL YEAR ENDED`
switch[duplicated(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))&
         !(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED` %m-% years(1)) %in%
             paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))] <-
  audit_yearly$`FISCAL YEAR ENDED`[duplicated(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))&
                                     !(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED` %m-% years(1)) %in%
                                         paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))]%m-% years(1)
audit_yearly$`FISCAL YEAR ENDED` <- switch

audit_yearly = audit_yearly %>%
  arrange(SYSTEM_NAME,`FISCAL YEAR ENDED`,`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)

switch <- audit_yearly$`FISCAL YEAR ENDED`
switch[duplicated(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))&
         !(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED` %m+% years(1)) %in%
             paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))] <-
  audit_yearly$`FISCAL YEAR ENDED`[duplicated(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))&
                                     !(paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED` %m+% years(1)) %in%
                                         paste(audit_yearly$SYSTEM_NAME,audit_yearly$`FISCAL YEAR ENDED`))]%m+% years(1)
audit_yearly$`FISCAL YEAR ENDED` <- switch

#(tmp <- yes; tmp[!test] <- no[!test]; tmp)

audit_background = read_csv('input/tceq_audits/district_info.csv') %>% 
  dplyr::select(-X1) %>% mutate(SYSTEM_NAME = NAME)

audit_background = audit_background %>% 
  dplyr::select(`Activity Status:`,`Creation Date:`,`Dissolved Date:`,SYSTEM_ID,
                SYSTEM_NAME, `District Type:`,`Merged into:`,`Merged from:`,`Annexer Name:`)

audit_background = audit_background %>% arrange(-decimal_date(mdy(`Creation Date:`))) %>%
  filter(!duplicated(SYSTEM_NAME))

audit = left_join(audit_yearly,audit_background)

audit = audit %>% rename(CREATION_DATE = `Creation Date:`,PWS_NAME = SYSTEM_NAME) %>%
  mutate(CREATION_DATE = mdy(CREATION_DATE),
         CREATION_YEAR = year(CREATION_DATE),
         FYEAR = year(`FISCAL YEAR ENDED`))

audit$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',audit$PWS_NAME)
audit$PWS_NAME = gsub('PUBLIC UTILITY DISTRICT','PUD',audit$PWS_NAME)
audit$PWS_NAME = gsub('UTILITY DISTRICT','UD',audit$PWS_NAME)
audit$PWS_NAME = gsub('-',' ',audit$PWS_NAME)
audit$PWS_NAME = gsub(' $','',audit$PWS_NAME)
audit$PWS_NAME = gsub('THE WOODLANDS','WOODLANDS',audit$PWS_NAME)
#audit$PWS_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',audit$PWS_NAME)
audit$PWS_NAME = gsub('CLOVERCREEK','CLOVER CREEK',audit$PWS_NAME)
audit$PWS_NAME = gsub('POST WOOD','POSTWOOD',audit$PWS_NAME)
audit$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",audit$PWS_NAME)
audit$PWS_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",audit$PWS_NAME)
audit$PWS_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",audit$PWS_NAME)
audit$PWS_NAME = gsub(' OF BRAZORIA COUNTY$','',audit$PWS_NAME)
audit$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",audit$PWS_NAME)
audit$PWS_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",audit$PWS_NAME)
audit$PWS_NAME = gsub("CYPRESS KLEIN UD","CYPRESS KLEIN MUD",audit$PWS_NAME)
audit$PWS_NAME[audit$PWS_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
audit = audit[!duplicated(audit %>% dplyr::select(-DOC_ID,-DOC_LINK)),]
audit$audit_observed = TRUE

audit = do.call(rbind,list(
  audit %>% filter(!grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)) %>% 
    mutate(PWS_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 EDGEWOOD V',PWS_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',PWS_NAME)) %>%
    mutate(PWS_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 SHELDON RI',PWS_NAME))))

audit = do.call(rbind,list(
  audit %>% filter(!grepl('WOODLANDS MUD 1',PWS_NAME)),
  audit %>% filter(grepl('WOODLANDS MUD 1',PWS_NAME)) %>% 
    arrange(PWS_NAME,`FISCAL YEAR ENDED`,`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`) %>%
    mutate(PWS_NAME = ifelse(`GENERAL FUND - TOTAL REVENUES`>1000000,'MONTGOMERY COUNTY MUD 40','WOODLANDS MUD 2'))))

audit$CREATION_YEAR[audit$PWS_NAME=='WOODLANDS MUD 2'] = '1967'
audit$CREATION_YEAR[audit$PWS_NAME=='MONTGOMERY COUNTY MUD 40'] = '1979'
audit$FYEAR_END_DDATE  = decimal_date(audit$`FISCAL YEAR ENDED`)
audit$FYEAR = year(audit$`FISCAL YEAR ENDED`)
audit$FMONTH = month(audit$`FISCAL YEAR ENDED`)
audit$UQ = paste(audit$PWS_NAME,audit$FYEAR)
audit = audit %>% filter(!is.na(`FISCAL YEAR ENDED`))

audit = audit[!duplicated(audit %>% dplyr::select(-DOC_ID,-DOC_LINK,-DOC_URL)),]
district_audits = audit

district_info = read_csv('input/tceq_audits/district_info.csv')
district_audits$COUNTY = district_info$`Primary County:`[match(district_audits$SYSTEM_ID,district_info$SYSTEM_ID)]
district_audits = district_audits %>% filter(COUNTY %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER'))
district_audits$PWS_ID = system_df$PWS_ID[match(district_audits$PWS_NAME,system_df$PWS_NAME)]

system_reports = read_csv('input/tceq_audits/private_entity_financial_report_reference_list.csv') 
system_reports$SITE_ID = gsub('CCN/Reg ','',system_reports$SITE_ID)
#system_reports$SITE_ID[!(system_reports$SITE_ID %in%  tx_utils@data$CCN_NO)]
owners = read_csv('input/texas_dww/personnel_records.csv') %>% filter(Position == 'OW- Owner') %>%
  select(-Position) %>%
  rename(Owner = NAME) %>% mutate(Owner = gsub('PO BOX.*','',Owner)) %>%
  mutate(Owner = gsub('  .*','',Owner)) %>% rename(PWS_ID = System)
system_reports$PWS_ID = NA
system_reports$PWS_ID <- ifelse(paste(system_reports$`District Name`,'LP',sep=' ') %in% owners$Owner, owners$PWS_ID[match(paste(system_reports$`District Name`,'LP',sep=' '),owners$Owner)],system_reports$PWS_ID)
system_reports$PWS_ID <- ifelse(paste(system_reports$`District Name`,'LLC',sep=' ') %in% owners$Owner, owners$PWS_ID[match(paste(system_reports$`District Name`,'LLC',sep=' '),owners$Owner)],system_reports$PWS_ID)
system_reports$PWS_ID <- ifelse(system_reports$`District Name` %in% system_df$PWS_NAME, system_df$PWS_ID[match(system_reports$`District Name`,system_df$PWS_NAME)],system_reports$PWS_ID)


table(system_df$PWS_ID %in% system_reports$PWS_ID,system_df$PWS_ID %in% district_audits$PWS_ID)

table(system_df$PWS_ID %in% district_audits$PWS_ID)
table(system_df$PWS_ID %in% system_reports$PWS_ID)
system_reports$PWS_ID
system_df$PWS_NAME[!system_df$PWS_ID %in% system_reports$PWS_ID & !system_df$PWS_ID %in% district_audits$PWS_ID]


system_reports$DOC_URL[grep('CHAPARRAL',system_reports$`District Name`)]
system_reports$
grep('BEAU',system_reports$`District Name`,value=T)
system_df$Name[match(owners$PWS_ID,system_df$PWS_ID)]

systems$Name[!(systems$PWS_ID %in% system_reports$PWS_ID) & !(systems$PWS_ID %in% district_audits$PWS_ID)]
systems$PWS_ID[!(systems$PWS_ID %in% system_reports$PWS_ID) & !(systems$PWS_ID %in% district_audits$PWS_ID)]


grep('BROOKSHIRE',district_audits$`District Name`,value=T)


grep('396',district_audits$`District Name`,value=T)

district_audits[district_audits$`District Name` == 'HARRIS COUNTY MUD 396',] %>% select(PWS_ID)

unique(system_reports$`District Name`[is.na(system_reports$PWS_ID)])


system_reports[paste(system_reports$`District Name`,'LP',sep=' ') %in% owners$Owner,]
system_reports[paste(system_reports$`District Name`,'LLC',sep=' ') %in% owners$Owner,]


  
grep('LP|LLC',system_reports$`District Name`,value=T)

grep('QUAD',owners$Owner,value=T)
grep('QUAD',system_reports$`District Name`,value=T)




sort(grep(' [0-9]',test$Owner,value=T))





unique(test$`District Name`)[!(unique(test$`District Name`) %in% systems$Name)]
unique(test$`District Name`)[!(unique(test$`District Name`) %in% epa_systems$`PWS Name`)]
table((unique(test$`District Name`) %in% epa_systems$`PWS Name`))


grep('COZY',systems$Name,value=T)
