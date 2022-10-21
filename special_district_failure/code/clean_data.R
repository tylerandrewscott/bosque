library(tidyverse)
library(lubridate)
library(INLA)

#### SDWIS SYSTEM INFO #####
sdw <- read_csv('input/epa_sdwis/texas_comprehensive_water_system_detail.csv') 
sdw <- sdw %>% filter(`Owner Type` == 'Local government') %>% filter(!grepl('W\\.S\\.C\\.|WSC|WATER SUPPLY CORPORATION|CITY OF|^TOWN OF|^VILLAGE OF|LUBBOCK PUBLIC WATER SYSTEM|SCHOOL|MUNICIPAL WATER SYSTEM',`PWS Name`))
sdw$`PWS Name` <- gsub('^HCO','HARRIS COUNTY',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('SPECIAL UTILITY DISTRICT','SUD',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('PUBLIC UTILITY DISTRICT','PUD',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('MUNICIPAL UTILITY DISTRICT','MUD',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('FT BEND','FORT BEND',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' NO | NO\\. ',' ',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' CO ',' COUNTY ',sdw$`PWS Name`)
#sdw$`PWS Name` <- gsub(' UD ',' UTILITY DISTRICT ',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' UD$',' UTILITY DISTRICT',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('MUNICIPAL UTIL DIS$|MUNICIPAL UTILITY DIST$|MUNICIPAL UTILITY DI$','MUD',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('M U D|M\\.U\\.D\\.','MUD',sdw$`PWS Name`)

sdw$`PWS Name` <- gsub('COUNTIES','COUNTY',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('COUNTIES','COUNTY',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('WATER SEWER IRRIGATION AND DRAINAGE','WSID',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' DIST ',' ',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' U D$',' UTILITY DISTRICT',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('TUSCOLA-TAYLOR COUNTY WCID 1','TUSCOLA TAYLOR COUNTY WCID 1',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub(' UTILITY DISTRICT',' UD',sdw$`PWS Name`)
sdw$`PWS Name`[grepl("BRAZOS CO\\. MUD",sdw$`PWS Name`)] <- 'BRAZOS COUNTY MUD 1'
sdw$`PWS Name`[grepl("WELLS BRANCH MUD 1",sdw$`PWS Name`)] <- 'WELLS BRANCH MUD'

sdw <- sdw %>% filter(!grepl('^LAKE LIVINGSTON',sdw$`PWS Name`))
sdw$`PWS Name` <- gsub('ZAPATA COUNTY WCID-HWY 16 EAST','ZAPATA COUNTY WCID HWY 16 EAST',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('ZAPATA COUNTY SAN YGNACIO & RAMIRENO','ZAPATA COUNTY WCID SAN YGNACIO',sdw$`PWS Name`)
sdw$`PWS Name` <- gsub('-',' ',sdw$`PWS Name`)

sdw$DISTRICT_NAME <- sdw$`PWS Name`
sdw$CWS_NAME <- sdw$`PWS Name`
sdw$DISTRICT_NAME[grepl('^RRA',sdw$DISTRICT_NAME)] <- "RED RIVER AUTHORITY OF TEXAS"
sdw$DISTRICT_NAME[grepl('^SUNBELT',sdw$DISTRICT_NAME)] <- "SUNBELT FWSD"
sdw$DISTRICT_NAME[grepl('^TRA ',sdw$DISTRICT_NAME)] <- "TRINITY RIVER AUTHORITY"
sdw$DISTRICT_NAME[grepl('THUNDERBIRD',sdw$DISTRICT_NAME)] <- "THUNDERBIRD UTILITY DISTRICT"
sdw$DISTRICT_NAME[grepl('LCRA',sdw$DISTRICT_NAME)] <-"LOWER COLORADO RIVER AUTHORITY"
sdw$DISTRICT_NAME[grepl('BRAZOS RIVER AUTHORITY',sdw$DISTRICT_NAME)] <-"BRAZOS RIVER AUTHORITY"
sdw$DISTRICT_NAME[grepl('BMWD',sdw$DISTRICT_NAME)] <-"BEXAR METROPOLITAN WATER DISTRICT"
sdw$DISTRICT_NAME[grepl('WESTON MUD',sdw$DISTRICT_NAME)] <-"WESTON MUD"
sdw$DISTRICT_NAME[grepl('GBRA',sdw$DISTRICT_NAME)] <-"GUADALUPE BLANCO RIVER AUTHORITY"
sdw$DISTRICT_NAME[grepl("NEW CANEY MUD",sdw$DISTRICT_NAME)] <-"NEW CANEY MUD"
sdw$DISTRICT_NAME[grepl("WALKER COUNTY SUD",sdw$DISTRICT_NAME)] <-"WALKER COUNTY SUD"
sdw$DISTRICT_NAME[grepl("EAST MEDINA COUNTY SUD",sdw$DISTRICT_NAME)] <-"EAST MEDINA COUNTY SUD"
sdw$DISTRICT_NAME[grepl("HARRIS COUNTY MUD #39-CLAY PARKER",sdw$DISTRICT_NAME)] <-"HARRIS COUNTY MUD 39"
sdw$DISTRICT_NAME[grepl("WEST HARRIS COUNTY MUD 2 CHASE",sdw$DISTRICT_NAME)] <-"WEST HARRIS COUNTY MUD 2"
sdw$`Deactivation Date`[sdw$`Deactivation Date`=='-'] <- NA

#write_csv(sdw_date,'scratch/scratch_file.csv')


###### DISTRICT INFO ########
info = read_csv('input/texas_iwdd/infopage_wide.csv') %>% rename(SYSTEM_NAME = X2,ID = `District:`) %>% select(-X3,-X4)
info$STATUS = ifelse(grepl('[A-Z]',info$`Business Phone:`),info$`Business Phone:`,info$`Activity Status:`)
info$DISTRICT_TYPE <- NA
info$DISTRICT_TYPE[is.na(info$`Type:`)] <- info$`Activity Status:`[is.na(info$`Type:`)]
info$DISTRICT_TYPE[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Activity Status:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$DISTRICT_TYPE[is.na(info$DISTRICT_TYPE)] <- info$`Type:`[is.na(info$DISTRICT_TYPE)]
info$STATUS <- NA
info$STATUS[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Business Phone:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$STATUS[is.na(info$STATUS)] <- info$`Activity Status:`[is.na(info$STATUS)] 
info$FORMED_BY <- info$`Registration Received:`
info$BOARD_SELECTION <- NA
info$BOARD_SELECTION <- info$`Number of Directors:`
info$BOARD_SELECTION <- ifelse(info$BOARD_SELECTION ==  "Elected by Precinct",'Elected',info$BOARD_SELECTION)
info$BOARD_MEMBERS <- info$`Way Created:`
info$COUNTY <- NA
info$COUNTY <- info$`Way Chosen:`
info$TAX_RATE <- NA
info$`Actual Date of Notice:` <- gsub('\\$ |,','',info$`Actual Date of Notice:`)
info$TAX_RATE <- as.numeric(info$`Actual Date of Notice:`)
info$LEVY_TAX <- (!is.na(info$TAX_RATE)) + 0
library(stringr)
info$NUM_COUNTIES <- 1 + sapply(ifelse(is.na(info$`Main County:`),list(NULL),str_split(info$`Main County:`,"\\s{2,}")),length)
info$MULTI_COUNTY <- (info$NUM_COUNTIES >1) + 0
temp = read_csv('input/tceq_audits/district_info.csv')
temp = temp %>% filter(`Activity Status:`!='TRACKING') %>%
  filter(`Activity Status:`!='CONFIRMATION REQUIRED')

temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='DORMANCY AFFIDAVIT'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='DORMANT'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&is.na(temp$`Activity Reason:`)] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='CONFIRMATION REQUIRED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='CONVERSION'] <- 'ACTIVE'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='ISSUED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='ELECTION CONFIRMED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Financial Status:`=='DORMANT AFFIDAVIT FILED'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` %in% c('DORMANT','NOT STARTED','INACTIVE','UNKNOWN-NO ACTIVIT')] = 'OUT OF SAMPLE'
temp$SYSTEM_NAME <- info$SYSTEM_NAME[match(temp$SYSTEM_ID,info$ID)]
temp$`District Type:`[grepl('RIVER AUTHORITY',temp$SYSTEM_NAME) & temp$`District Type:` == 'OTHER'] <- 'RIVER AUTHORITY'
event_df <- data.frame(creation = mdy(temp$`Creation Date:`),
                        start = decimal_date(mdy(temp$`Creation Date:`)),
                       event = ifelse(temp$`Activity Status:` == 'ACTIVE',0,1),
                       end = mdy(temp$`Activity Date:`),
                       end2 = mdy(temp$`Dissolved Date:`),
                       id = temp$SYSTEM_ID,
                       status = temp$`Activity Status:`,
                       reason = temp$`Activity Reason:`) 
event_df <- event_df %>% mutate(fail_date = ifelse(event==0,NA,ifelse(!is.na(end2),as.character(end2),as.character(end))))
event_df$creation_year = year(event_df$creation)
event_df$deactivation_year = year(ymd(event_df$fail_date))
event_df1 = event_df %>% filter(status!='OUT OF SAMPLE') %>% group_by(creation_year) %>% summarise(create_by_year = n()) %>% rename(year = creation_year )
event_df2 = event_df %>% filter(status!='OUT OF SAMPLE') %>% group_by(deactivation_year) %>% summarise(delete_by_year = n()) %>% rename(year = deactivation_year)
event_df3 = event_df %>% filter(status=='OUT OF SAMPLE') %>% group_by(creation_year) %>% summarise(create_by_year_out = n()) %>% rename(year = creation_year)
event_df_count = Reduce(full_join,list(event_df1,event_df2,event_df3))
event_df_count = gather(event_df_count,action,count,-year)
event_df_count$count[is.na(event_df_count$count)] <- 0
event_df_count = event_df_count %>% filter(!is.na(year))

gg1 = ggplot(aes(y=count,x=year,fill = action),data=event_df_count) + geom_bar(stat='identity',position ='dodge') +
  #geom_density(stat='identity',aes(y=count,x=year,fill = action),data=temp,position ='dodge') +
  theme_bw() + scale_fill_brewer(name = '',labels=c('# created (in sample)','# created (out of sample)','# dissolved (in sample)'),
                                 type='qual',palette = 1) + theme(legend.position = c(0.2,0.85),axis.text=element_text(size=12),
                                                                  axis.title=element_text(size=12),legend.title=element_blank(),
                                                                  legend.text=element_text(size = 12))  +
  scale_y_continuous(expand=c(0,0),limits = c(0,150),name = '# of districts') + scale_x_continuous(name = 'Year',expand=c(0,0)) 

#time: for failed districts, age at failure; for active districts, age at 12/31/2016
event_df <- event_df %>% filter(status!='OUT OF SAMPLE') %>% filter(!is.na(start)) %>%
  filter(start < 2017) %>% filter(creation != fail_date | is.na(fail_date))
event_df$end = decimal_date(ymd(event_df$fail_date))
event_df$event = ifelse(is.na(event_df$fail_date),0,1)
event_df$time = ifelse(is.na(event_df$fail_date), decimal_date(mdy('12/31/2016')) - event_df$start,  decimal_date(ymd(event_df$fail_date)) - event_df$start)
event_df$SYSTEM_NAME <- info$SYSTEM_NAME[match(event_df$id,info$ID)]
event_df$COUNTY <- info$COUNTY[match(event_df$id,info$ID)]
event_df$MULTI_COUNTY <- info$MULTI_COUNTY[match(event_df$id,info$ID)]
event_df$NUM_COUNTIES <- info$NUM_COUNTIES[match(event_df$id,info$ID)]
event_df$Acre_Size <- temp$`Acre Size:`[match(event_df$id,temp$SYSTEM_ID)]
event_df$DISTRICT_TYPE <- info$DISTRICT_TYPE[match(event_df$id,info$ID)]
event_df$FORMED_BY <- info$FORMED_BY[match(event_df$id,info$ID)]
event_df$HAS_BOARD <- (info$BOARD_MEMBERS[match(event_df$id,info$ID)]>0) + 0
event_df$BOARD_MEMBERS <- info$BOARD_MEMBERS[match(event_df$id,info$ID)]
event_df$LEVY_TAX <- info$LEVY_TAX[match(event_df$id,info$ID)] 
event_df$TAX_RATE <- info$TAX_RATE[match(event_df$id,info$ID)] 
event_df <- event_df %>% filter(!DISTRICT_TYPE %in% c('SOIL AND WATER CONSERVATION DISTRIC','STORMWATER CONTROL DISTRICT','REGIONAL DISTRICT'))
event_df$Acre_Size[is.na(event_df$Acre_Size)] <- median(event_df$Acre_Size,na.rm=T)
event_df$Log_Acre_Size <- log(event_df$Acre_Size)

event_df$SYSTEM_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('UTILITY DISTRICT','UD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('COUNTIES','COUNTY',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('WATER SEWER IRRIGATION AND DRAINAGE DIST','WSID',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('-',' ',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub(' $','',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('THE WOODLANDS','WOODLANDS',event_df$SYSTEM_NAME)
#event_df$SYSTEM_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('CLOVERCREEK','CLOVER CREEK',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('POST WOOD','POSTWOOD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('OF BRAZORIA COUNTY$','',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("TANGLEWOOD FOREST LIMITED DISTRICT","TANGLEWOOD FOREST MUD",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME[event_df$SYSTEM_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
event_df$SYSTEM_NAME[event_df$SYSTEM_NAME=="ABLES SPRINGS SUD OF HUNT, KAUFMAN, AND VAN ZANDT COUNTY"] = "ABLES SPRINGS SUD"
event_df$SYSTEM_NAME <- gsub(' $','',event_df$SYSTEM_NAME)
event_df = event_df %>% rename(DISTRICT_NAME = SYSTEM_NAME)
event_df = event_df %>% filter((!is.na(deactivation_year) & deactivation_year>=2004)|is.na(deactivation_year)) %>%
  filter(creation_year < 2016,is.na(deactivation_year)|deactivation_year < 2017)

##### add audit_yearlys #####
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


audit_background = read_csv('input/tceq_audits/district_info.csv') %>% 
  dplyr::select(-X1) %>% mutate(SYSTEM_NAME = NAME)


audit_background = audit_background %>% 
  dplyr::select(`Activity Status:`,`Creation Date:`,`Dissolved Date:`,SYSTEM_ID,
                SYSTEM_NAME, `District Type:`,`Merged into:`,`Merged from:`,`Annexer Name:`)

audit_background = audit_background %>% arrange(-decimal_date(mdy(`Creation Date:`))) %>%
  filter(!duplicated(SYSTEM_NAME))

audit = left_join(audit_yearly,audit_background)

audit = audit %>% rename(CREATION_DATE = `Creation Date:`,DISTRICT_NAME = SYSTEM_NAME) %>%
  mutate(CREATION_DATE = mdy(CREATION_DATE),
         CREATION_YEAR = year(CREATION_DATE),
         FYEAR = year(`FISCAL YEAR ENDED`))

audit$DISTRICT_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('PUBLIC UTILITY DISTRICT','PUD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('LIMITED DISTRICT','MUD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('UTILITY DISTRICT','UD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('-',' ',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub(' $','',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('THE WOODLANDS','WOODLANDS',audit$DISTRICT_NAME)
#audit$DISTRICT_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('CLOVERCREEK','CLOVER CREEK',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub('POST WOOD','POSTWOOD',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub(' OF BRAZORIA COUNTY$','',audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",audit$DISTRICT_NAME)
audit$DISTRICT_NAME = gsub("CYPRESS KLEIN UD","CYPRESS KLEIN MUD",audit$DISTRICT_NAME)
audit$DISTRICT_NAME[audit$DISTRICT_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
audit = audit[!duplicated(audit %>% dplyr::select(-DOC_ID,-DOC_LINK)),]
audit$audit_observed = TRUE

audit = do.call(rbind,list(
  audit %>% filter(!grepl('NORTHEAST HARRIS COUNTY MUD 1$',DISTRICT_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',DISTRICT_NAME)) %>% 
    mutate(DISTRICT_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 EDGEWOOD V',DISTRICT_NAME)),
  audit %>% filter(grepl('NORTHEAST HARRIS COUNTY MUD 1$',DISTRICT_NAME)) %>%
    mutate(DISTRICT_NAME = gsub('NORTHEAST HARRIS COUNTY MUD 1$','NORTHEAST HARRIS COUNTY MUD 1 SHELDON RI',DISTRICT_NAME))))

audit = do.call(rbind,list(
  audit %>% filter(!grepl('WOODLANDS MUD 1',DISTRICT_NAME)),
  audit %>% filter(grepl('WOODLANDS MUD 1',DISTRICT_NAME)) %>% 
    arrange(DISTRICT_NAME,`FISCAL YEAR ENDED`,`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`) %>%
    mutate(DISTRICT_NAME = ifelse(`GENERAL FUND - TOTAL REVENUES`>1000000,'MONTGOMERY COUNTY MUD 40','WOODLANDS MUD 2'))))
audit$FYEAR_END_DDATE  = decimal_date(audit$`FISCAL YEAR ENDED`)
audit$FYEAR = year(audit$`FISCAL YEAR ENDED`)
audit$FMONTH = month(audit$`FISCAL YEAR ENDED`)
audit$UQ = paste(audit$DISTRICT_NAME,audit$FYEAR)
audit$CREATION_YEAR[audit$DISTRICT_NAME=='WOODLANDS MUD 2'] = '1967'
audit$CREATION_YEAR[audit$DISTRICT_NAME=='MONTGOMERY COUNTY MUD 40'] = '1979'
audit = audit %>% filter(!is.na(`FISCAL YEAR ENDED`))
audit = audit[!duplicated(audit %>% dplyr::select(-DOC_ID,-DOC_LINK,-DOC_URL)),]


test = event_df %>% filter(event==0|(!is.na(deactivation_year)&deactivation_year>=2004))



table(event_df$deactivation_year>=2004&event_df$deactivation_year<2017)
table(event_df$DISTRICT_NAME[(event_df$event==1&event_df$deactivation_year>2004)|is.na(event_df$deactivation_year)] %in% sdw$DISTRICT_NAME)
event_df$DISTRICT_NAME[!event_df$DISTRICT_NAME %in% sdw$DISTRICT_NAME & event_df$event==1]
event_df$DISTRICT_NAME[event_df$DISTRICT_NAME %in% sdw$DISTRICT_NAME & event_df$event==1]


ggplot(event_df,aes(x = creation_year,fill = as.factor(event))) + geom_bar() + 
  scale_fill_brewer(labels = c('Active','Deactivated'),type = 'qual')

ggplot(event_df,aes(x = creation_year, y = deactivation_year)) + geom_jitter(pch=21,alpha=0.5) +
  geom_abline(aes(intercept=0,slope=1),lty=2)




?geom_abline
table(event_df$creation_yearevent_df$deactivation_year)

fc_viol$`Violation First Reported Date`
table(fc_viol$`Rule Name`)
district_df = inner_join(sdw,event_df)
fc_viol$`Violation First Reported Date`
dmy(fc_viol$`Violation First Reported Date`)

table(year(dmy(fc_viol$`Violation First Reported Date`)[
  fc_viol$`Rule Name`=='Total Coliform Rule']))


hist(year(dmy(fc_viol$`Violation First Reported Date`)[
  fc_viol$`Rule Name`=='Lead and Copper Rule']),breaks = 50)
hist(year(dmy(fc_viol$`Compliance Period Begin Date`[fc_viol$`Rule Name`=='Total Coliform Rule'])),
     breaks = 50)




library(pbapply)
#### SDWIS VIOLATION INFO #####
sdw_date <- expand.grid(sdw$`PWS ID`,1980:2016,1:12) %>% rename(PWS_ID = Var1, YEAR = Var2, MONTH = Var3) %>%
  mutate(DAY = 1,DATE = dmy(paste(DAY,MONTH,YEAR,sep='/')))
fc_viol = #read_csv('input/epa_sdwis/sdwis_texas_fc_violations.csv') %>% 
  read_csv('input/epa_sdwis/texas_all_sdwa_viols_9-17.csv') %>% 
  rename(PWS_ID = `PWS ID`) %>%
  filter(PWS_ID %in% sdw$`PWS ID`)
#fc <- do.call(rbind,pblapply(1:nrow(fc_viol),function(i) sdw_date %>% filter(PWS_ID == fc_viol$PWS_ID[i],DATE>=dmy(fc_viol$`Compliance Period Begin Date`[i])) %>%
#         filter(DATE<dmy(fc_viol$`RTC Date`)[i]|is.na(fc_viol$`RTC Date`[i])) %>% mutate(HVIOL = (fc_viol$`Is Health Based`[i]=='Y') + 0,MVIOL = (fc_viol$`Is Health Based`[i]=='N') + 0)))
#fc_by_period <- fc %>% group_by(PWS_ID,YEAR,MONTH,DAY,DATE,MVIOL,HVIOL) %>% summarise(MVIOLS = sum(MVIOL),HVIOLS = sum(HVIOL))                                   
#write_csv(fc_by_period,'scratch/fc_by_period.csv')
sdw_date$DISTRICT_NAME <- sdw$DISTRICT_NAME[match(sdw_date$PWS_ID,sdw$`PWS ID`)]
sdw_date$PWS_NAME <- sdw$`PWS Name`[match(sdw_date$PWS_ID,sdw$`PWS ID`)]
sdw_date <- left_join(sdw_date,fc_by_period) %>% select(-MVIOL,-HVIOL)
sdw_date$HVIOLS[is.na(sdw_date$HVIOLS)] <- 0
sdw_date$MVIOLS[is.na(sdw_date$MVIOLS)] <- 0
sdw_date = sdw_date %>% group_by(YEAR,MONTH,DAY,DISTRICT_NAME) %>% summarise(MVIOLS = sum(MVIOLS),HVIOLS = sum(HVIOLS)) %>%
  filter(YEAR >= 2004)




audit_vars <- audit %>% select(`FISCAL YEAR ENDED`,`GENERAL FUND - FUND BALANCE`,`GENERAL FUND - TOTAL REVENUES`,
                 `GENERAL FUND - TOTAL EXPENDITURES`,`ENTERPRISE FUND - NET ASSETS`,`ENTERPRISE FUND - OPERATING REVENUES`,
               `ENTERPRISE FUND - OPERATING EXPENSES`,`TOTAL TAX RATE`,`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,
               DISTRICT_NAME,FYEAR_END_DDATE,FMONTH,UQ)


sdw_date <- sdw_date %>% mutate(Period_Start = mdy(paste(MONTH,DAY,YEAR,sep='/')))
audit_vars$`FISCAL YEAR START` <- audit_vars$`FISCAL YEAR ENDED` - years(1)
sdw_actives <-  sdw_date %>% filter(is.na(fail_date))
sdw_fails <-  sdw_date %>% filter(!is.na(fail_date) & (ymd(fail_date) < Period_Start + months(1)) &
                                    (ymd(fail_date) > Period_Start))






sdw_fails %>% arrange(DISTRICT_NAME) %>% select(-MVIOLS,-HVIOLS,-end2,-status)


length(unique(sdw_actives$DISTRICT_NAME))
length(unique(sdw_fails$DISTRICT_NAME))


test = sdw_date %>% filter(is.na(fail_date) | (ymd(fail_date) < Period_Start + months(1)))


sdw_date$fail_date
table(test$event)


length(unique(test$DISTRICT_NAME))



test = sdw_date[sdw_date$event==0 | (!duplicated(paste(sdw_date$event,sdw_date$DISTRICT_NAME))),]

table(test$event)


table(sdw_date$deactivation_year)


sdw_date$fail_date 
ymd(sdw_date$fail_date)

sdw_date[sdw_date$DISTRICT_NAME=='BEXAR METROPOLITAN WATER DISTRICT',]


test <- pbsapply(1:nrow(sdw_date), function(i) which(audit_vars$DISTRICT_NAME == sdw_date$DISTRICT_NAME[i] &
                                          audit_vars$`FISCAL YEAR ENDED` > sdw_date$Period_Start[i] &
                                           < (sdw_date$Period_Start[i])))











#audit = audit[!duplicated(paste0(audit$PWS_NAME,audit$FYEAR)),]
analyze_df = system_df %>% filter(system_type %in% c('MUD','FWSD','WCID'))


fyear_start = data.frame(expand.grid(YEAR = 2004:2016,
                                     MONTH = 1:12,
                                     PWS_NAME = analyze_df$PWS_NAME))
fyear_start$DATE = dmy(paste('1',fyear_start$MONTH,fyear_start$YEAR,sep='/'))
fyear_start$DEC_DATE = decimal_date(fyear_start$DATE)
master_df = left_join(analyze_df,fyear_start)

#test for duplicated audit data
# test = mclapply(1:nrow(master_df),function(i)
#   audit[intersect(which(audit$PWS_NAME==master_df$PWS_NAME[i]),
#                   which(audit$FYEAR_END_DDATE > master_df$DEC_DATE[i] &
#                           audit$FYEAR_END_DDATE < master_df$DEC_DATE[i]+1)),] %>% 
#     filter(FYEAR_END_DDATE == max(FYEAR_END_DDATE)),mc.cores=8,mc.cleanup=T)
# table(master_df$PWS_NAME[which(sapply(test,nrow)==2)],
#       master_df$YEAR[which(sapply(test,nrow)==2)])

master_df$UNIQUE_SYSTEM_MONTH = paste(master_df$PWS_NAME,master_df$YEAR,master_df$MONTH,sep='_')

audit$UNIQUE_SYSTEM_MONTH = NA

uq_month_audits = do.call(rbind,invisible(mclapply(1:nrow(master_df),
                                                   function(i)  audit[master_df$PWS_NAME[i] == audit$PWS_NAME &
                                                                        ((audit$FYEAR_END_DDATE-master_df$DEC_DATE[i]) ==
                                                                           min((audit$FYEAR_END_DDATE-master_df$DEC_DATE[i])[{
                                                                             {(audit$FYEAR_END_DDATE-master_df$DEC_DATE[i])<=1} &
                                                                             {(audit$FYEAR_END_DDATE-master_df$DEC_DATE[i])>0} &
                                                                             {master_df$PWS_NAME[i]== audit$PWS_NAME}}])),]%>% 
                                                     mutate(UNIQUE_SYSTEM_MONTH = master_df$UNIQUE_SYSTEM_MONTH[i]),mc.cores=8,mc.cleanup=T)))










mod_df$SYSTEM_NAME <- gsub('-',' ',mod_df$SYSTEM_NAME)
sdw$SYSTEM_NAME <- gsub('-',' ',sdw$`PWS Name`)





table(sdw$`PWS Name` %in% mod_df$SYSTEM_NAME)


table(mod_df$SYSTEM_NAME %in% sdw$`PWS Name`)



table(sdw$`Activity Status`)



surv_ob <- inla.surv(time = mod_df$time,event = mod_df$event)



formula=surv_ob ~ 0 + Log_Acre_Size + HAS_BOARD + HAS_BOARD:BOARD_MEMBERS + 
  MULTI_COUNTY + MULTI_COUNTY:NUM_COUNTIES + LEVY_TAX + LEVY_TAX:TAX_RATE + 
  f(inla.group(creation_year), model='rw1') + 
  f(COUNTY,model = 'iid') + f(DISTRICT_TYPE,model='iid') 

mod=inla(formula, family ="coxph", data = mod_df,verbose=F,
           control.hazard=list(model="rw1", n.intervals=20))





