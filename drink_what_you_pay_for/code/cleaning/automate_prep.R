library(tidyverse)
library(readxl)
library(rgdal)
library(sp)
library(rgeos)
library(lubridate)
library(stringr)
library(readxl)
library(pbapply)
library(parallel)
set = 'all'
core_num = 8
plot_map = TRUE
houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria','Chambers',"Galveston",
                     'Austin','Liberty','Waller')
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]

tx_tracts = readOGR('spatial_inputs/government_units','cb_2015_48_tract_500k')
tx_tracts@data$GEOID = as.character(tx_tracts@data$AFFGEOID)
houston_tracts = tx_tracts[paste(tx_tracts$STATEFP,tx_tracts$COUNTYFP,sep='') %in% hout_counties$FIPS_I,]

tx_blockgroups = readOGR('spatial_inputs/government_units','tl_2016_48_bg')
tx_blockgroups$GEOID12 = as.character(tx_blockgroups$GEOID)
cropped_blockgroups = tx_blockgroups[paste(tx_blockgroups$STATEFP,tx_blockgroups$COUNTYFP,sep='') %in% hout_counties$FIPS_I,]
hout_msa = gUnaryUnion(hout_counties)
cropped_blockgroups = raster::crop(cropped_blockgroups,hout_msa)
cropped_blockgroups@data$id = cropped_blockgroups@data$GEOID12
cropped_blockgroups.points = fortify(cropped_blockgroups, region="id")
cropped_blockgroups.df = plyr::join(cropped_blockgroups.points, cropped_blockgroups@data, by="id")
#cropped_blockgroups.df = left_join(cropped_blockgroups.df,blockgroups_acs_data %>% filter(YEAR==2014))

#### Load system summary from EPA
system_df = read_csv('input/epa_sdwis/hous_msa_sdwis_summary.csv',na = c("-"))%>%
  rename(PWS_Type = `PWS Type`,City = `City Name`) %>%
  # filter(PWS_Type == 'Community water system'&!`Owner Type` %in% c('State government','Federal government','Private')) %>%
  mutate(First_Report_Date = dmy(`First Reported Date`),Year_Start = year(First_Report_Date),
         City = str_to_title(City)) %>%
  dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) %>%
  rename(PWS_NAME = `PWS Name`,PWS_ID = `PWS ID`) %>% filter(`Population Served Count`!=0) %>%
  filter(!grepl('SHERIFF|ACADEMY|MONTESSORI',PWS_NAME))


system_df = system_df %>% filter(is.na(year(dmy(system_df$`Deactivation Date`)))|year(dmy(system_df$`Deactivation Date`))>2009)
#system_df  = system_df %>% filter(!grepl('CITY|TOWN ',system_df$PWS_NAME))
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

#write_csv(system_df,'input/epa_sdwis/system_base_master.csv')

# not_in = c("HARRIS COUNTY MUD 537","HARRIS COUNTY MUD 530","HARRIS COUNTY MUD 495","HARRIS COUNTY MUD 494","HARRIS COUNTY MUD 387",
#            "MONTGOMERY COUNTY MUD 126","MONTGOMERY COUNTY MUD 127","MONTGOMERY COUNTY MUD 141","MONTGOMERY COUNTY MUD 105","FORT BEND COUNTY MUD 134D")
# 
# for (i in sort(grep('MUD',system_df$PWS_NAME[!system_df$PWS_NAME %in% debt_df$Issuer],value=T)))
# {
#   print('search');print(i)
#   print(agrep(i,unique(debt_df$Issuer),value=T,costs = 4))
# }

#temp = as.data.frame(table(system_df$PWS_NAME %in% debt_df$Issuer,system_type)) %>% spread(Var1,Freq)
#colnames(temp) = c('type','no match','match')


######
# houston_wd_geo@data$id = houston_wd_geo@data$NAME
# hout_wd.points = fortify(houston_wd_geo,region='id')
# hout_wd.df = plyr::join(hout_wd.points,houston_wd_geo@data,by='id')
# hout_wd.df$PWS_NAME = hout_wd.df$NAME
# which_tract_wd = over(houston_wd_geo,tx_tracts)
# table(is.na(which_tract_wd$))
# houston_wd_geo@data$GEOID = as.character(which_tract_wd$GEOID)


system_type = rep(NA,nrow(system_df))
system_type[grepl('WCID',system_df$PWS_NAME)] = 'WCID'
system_type[grepl('MWA',system_df$PWS_NAME)] = 'MWA'
system_type[grepl('PUD$| PUD ',system_df$PWS_NAME)] = 'MUD'
system_type[grepl('MWD',system_df$PWS_NAME)] = 'MWD'
system_type[grepl(' MUD | MUD$',system_df$PWS_NAME)] = 'MUD'
system_type[grepl('WSC',system_df$PWS_NAME)] = 'WSC'
system_type[grepl('SUD$| SUD ',system_df$PWS_NAME)] = 'SUD'
system_type[grepl('FWSD',system_df$PWS_NAME)] = 'FWSD'

system_type[system_df$PWS_NAME=="GENERATION PARK MANAGEMENT DISTRICT"] = 'MMD'
system_type[system_df$PWS_NAME=="SIENNA PLANTATION MANAGEMENT DISTRICT"] = 'MMD'
system_type[system_df$PWS_NAME=="LOWER NECHES VALLEY AUTHORITY"] = 'Other District'
system_type[system_df$PWS_NAME %in% c("LAZY RIVER IMPROVEMENT DISTRICT"  , "INVERNESS FOREST IMPROVEMENT DISTRICT" ,
                                      "COMMODORE COVE IMPROVEMENT DISTRICT","CLEAR LAKE CITY WATER AUTHORITY" )] = 'WCID'
system_type[system_df$PWS_NAME=="HARRIS COUNTY IMPROVEMENT DISTRICT 18"] = 'MMD'
system_type[system_df$PWS_NAME=="SEQUOIA IMPROVEMENT DISTRICT"|system_df$PWS_NAME=="TIMBERLAKE IMPROVEMENT DISTRICT"] = 'MUD'
system_type[grepl(' UD | UD$',system_df$PWS_NAME)] = 'MUD'
system_type[is.na(system_type)&grepl('WATER AUTHORITY',system_df$PWS_NAME)] = 'Other District'
system_type[grepl('CITY OF|VILLAGE OF',system_df$PWS_NAME)] = 'City'
system_type[grepl('SUBDIVISION|MOBILE HOME|MHP',system_df$PWS_NAME)] = 'Private'
system_type[grepl('TBCD',system_df$PWS_NAME)] = 'Other District'
system_type[grepl("SIENNA PLANTATION THE WOODS",system_df$PWS_NAME)] = 'Other District'
system_type[system_df$`Owner Type`=='Private'] = 'Private'
system_type[system_df$PWS_NAME=="SUN RANCH WATER SYSTEM"] = 'Private'
system_type[system_df$PWS_NAME=="5TH STREET WATER SYSTEM"] = 'Other District'
system_type[system_df$PWS_NAME %in% c("HOOP N HOLLER LAKE ESTATES","OLD SNAKE RIVER ESTATES EAST", "BIG THICKET LAKE ESTATES 1","PATTON LAKE CLUB" ,"HUNTERS COVE SEC 1")] = 'WSC'
system_type[system_df$PWS_NAME %in% c("COE INDUSTRIAL PARK","ALICE ACRES MOBILE HOME SUBDIVISION","2920 WEST SUBDIVISION" ,"BRANDYWINE OAKS","BRANDYWINE PINES" ,"CYPRESS CROSSING",
                                      "KICKAPOO FARMS SUBDIVISION","CYPRESS PASS ESTATES" ,"WILLOW OAKS MOBILE HOME SUBDIVISION" ,"VILLAGE OF NEW KENTUCKY","RED OAK TERRACE" ,
                                      "HOUSE CORRAL STREET WATER SYSTEM" ,"TRAILWOOD SUBDIVISION"  ,"TIMBERWILDE MH SUBDIVISION" ,"GRANT ROAD ESTATES MOBILE HOME SUB",
                                      "TOWERING OAKS AND ROSEWOOD HILLS SUBDIVI","COE COUNTRY", "ALLENWOOD SUBDIVISION", "ARMADILLO WOODS SUBDIVISION" ,"RIMWICK FOREST" , "RUSTIC OAKS SUBDIVISION" , "MINK BRANCH VALLEY"  ,
                                      "KIPLING OAKS 1", "KIPLING OAKS AND TIMBERGREEN"  ,"ESTATES OF HOLLY LAKES","PLEASANT FOREST SUBDIVISION"  ,"DEER RIDGE SUBDIVISION", 
                                      "SHADY ACRES" , "HUNTERS RETREAT","TREICHEL WOODS ESTATES"  )] = 'SUD'

system_df$system_type = system_type

twd_geo = readOGR('input/scratch','district_shapes')

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

master_df = left_join(master_df,uq_month_audits) 


master_df = master_df %>% 
  rename(Pop_Served = `Population Served Count`,
         ENTERPRISE_REVENUE = `ENTERPRISE FUND - OPERATING REVENUES`,
         GENERAL_REVENUE = `GENERAL FUND - TOTAL REVENUES`,
         GENERAL_FUND_BALANCE = `GENERAL FUND - FUND BALANCE`,
         ENTERPRISE_FUND_BALANCE = `ENTERPRISE FUND - NET ASSETS`) %>%
mutate(TOTAL_FUND_BALANCE = ENTERPRISE_FUND_BALANCE + GENERAL_FUND_BALANCE,
                      TOTAL_REVENUE = GENERAL_REVENUE + ENTERPRISE_REVENUE)

# 
# master_df = do.call(rbind,mclapply(unique(analyze_df$PWS_NAME),function(uq)
#     analyze_df %>% filter(PWS_NAME == uq) %>%  arrange(FYEAR) %>% 
#       fill(GENERAL_FUND_BALANCE_P1,ENTERPRISE_FUND_BALANCE_P1,
#            GENERAL_REVENUE_P1,ENTERPRISE_REVENUE_P1,
#            TOTAL_REVENUE_P1,TOTAL_FUND_BALANCE_P1, .direction=c('up'))%>%
#       fill(GENERAL_FUND_BALANCE_P1,ENTERPRISE_FUND_BALANCE_P1,
#            GENERAL_REVENUE_P1,ENTERPRISE_REVENUE_P1,
#            TOTAL_REVENUE_P1,TOTAL_FUND_BALANCE_P1,.direction=c('down')),
#     mc.cores = core_num,mc.cleanup=TRUE))


#### load violation data

periods = data.frame(expand.grid(YEAR = 2004:2016,
                                 MONTH = 1:12,lag_years = 1:5))
periods$DATE = mdy(paste(periods$MONTH,'01',periods$YEAR,sep='/'))

viols_df = read_csv('input/epa_sdwis/violation_report.csv',na=c("-")) %>% 
  filter(`PWS ID` %in% master_df$PWS_ID) %>% 
  rename(PWS_ID = `PWS ID`,PWS_NAME = `PWS Name`) %>%
  mutate(`Compliance Period Begin Date` = dmy(`Compliance Period Begin Date`),
         `Compliance Period End Date` = dmy(`Compliance Period End Date`),
         `RTC Date` = dmy(`RTC Date`))


viol_count = do.call(rbind,mclapply(1:nrow(periods),function(i)
viols_df %>% group_by(PWS_ID,PWS_NAME,`Is Health Based`) %>% 
  filter(#violations that start before 5 years ago, less than lag + 5 years ago, still not solved or solved after period start
{`Compliance Period Begin Date` < (periods$DATE[i]%m-% years(periods$lag_years[i])) & 
`Compliance Period Begin Date` > (periods$DATE[i]%m-% years(periods$lag_years[i]+5)) &
(is.na(`RTC Date`) | `RTC Date`>(periods$DATE[i]%m-% years(periods$lag_years[i])))} |
#violations that started within past 5 years, before current date, still not solved or solved after period start
{(`Compliance Period Begin Date` > (periods$DATE[i]%m-% years(periods$lag_years[i]))) &
(`Compliance Period Begin Date` < (periods$DATE[i])) & 
(is.na(`RTC Date`)|`RTC Date`>(periods$DATE[i]%m-% years(periods$lag_years[i])))})  %>%
  summarise(viol_count = n()) %>% 
  mutate(viol_window = periods$lag_years[i], UNIQUE_SYSTEM_MONTH = paste(PWS_NAME,periods$YEAR[i],periods$MONTH[i],sep='_')),
mc.cores=8,mc.cleanup=T)) %>% spread(`Is Health Based`,viol_count)


viol_count_df = full_join(viol_count %>% group_by(UNIQUE_SYSTEM_MONTH) %>% dplyr::select(-PWS_NAME) %>% #spread(viol_window,Y) %>% 
  dplyr::select(-N) %>% spread(viol_window,Y) %>% rename(`hviol_p1_count`=`1`,`hviol_p2_count`=`2`,`hviol_p3_count`=`3`,
                                                             `hviol_p4_count`=`4`,`hviol_p5_count`=`5`),
  viol_count %>% group_by(UNIQUE_SYSTEM_MONTH) %>% dplyr::select(-PWS_NAME) %>% #spread(viol_window,Y) %>% 
    dplyr::select(-Y) %>% spread(viol_window,N) %>% 
    rename(`mviol_p1_count`=`1`,`mviol_p2_count`=`2`,`mviol_p3_count`=`3`,`mviol_p4_count`=`4`,`mviol_p5_count`=`5`))



library(parallel)
master_df = left_join(master_df,viol_count_df)

master_df  = master_df %>%
  mutate(hviol_p1_count = ifelse(is.na(hviol_p1_count),0,hviol_p1_count),
         mviol_p1_count = ifelse(is.na(mviol_p1_count),0,mviol_p1_count),
         hviol_p2_count = ifelse(is.na(hviol_p2_count),0,hviol_p2_count),
         mviol_p2_count = ifelse(is.na(mviol_p2_count),0,mviol_p2_count),
         hviol_p3_count = ifelse(is.na(hviol_p3_count),0,hviol_p3_count),
         mviol_p3_count = ifelse(is.na(mviol_p3_count),0,mviol_p3_count),
         hviol_p4_count = ifelse(is.na(hviol_p4_count),0,hviol_p4_count),
         mviol_p4_count = ifelse(is.na(mviol_p4_count),0,mviol_p4_count),
         hviol_p5_count = ifelse(is.na(hviol_p5_count),0,hviol_p5_count),
         mviol_p5_count = ifelse(is.na(mviol_p5_count),0,mviol_p5_count))
         



# 
# site_inspections = read_csv('input/epa_sdwis/water_system_site_visit.csv') %>%
#   filter(`Visit Reason`=='Sanitary Survey, Complete') %>%
#   mutate(Inspection_Date = dmy(`Visit Date`))
# 
# inspections_list = do.call(rbind,mclapply(1:nrow(master_df),function(x) 
# site_inspections[(site_inspections$`PWS ID`==master_df$PWS_ID[x]) & 
#   (site_inspections$Inspection_Date  ==
#     min(site_inspections$Inspection_Date[
#       site_inspections$`PWS ID`==master_df$PWS_ID[x] &
#         site_inspections$Inspection_Date < master_df$DATE[x]])),]%>%
#   mutate(UNIQUE_SYSTEM_MONTH = master_df$UNIQUE_SYSTEM_MONTH[x]),
# mc.cores = 8,mc.cleanup = T))
# 
# master_df = left_join(master_df,inspections_list %>% dplyr::select(-`EPA Region Code`))
# 


library(zoo)
library(pbapply)
  library(lubridate)
temp = master_df %>% arrange(PWS_NAME,DEC_DATE) %>% dplyr::select(PWS_NAME,DATE,FYEAR)
for (i in 2:nrow(temp))
{if (is.na(temp$FYEAR[i]) & ((temp$DATE[i] %m-% months(1)) == temp$DATE[i-1]))
{if (sum(temp$PWS_NAME[i]==temp$PWS_NAME & temp$FYEAR==(temp$FYEAR[i-1]),na.rm=T) <= 12) {temp$FYEAR[i] <- temp$FYEAR[i-1]}
if (sum(temp$PWS_NAME[i]==temp$PWS_NAME & temp$FYEAR==(temp$FYEAR[i-1]),na.rm=T) > 12) {temp$FYEAR[i] <- (temp$FYEAR[i-1]+1)}
}}

master_df = left_join(master_df %>% dplyr::select(-FYEAR),temp)


###DEBT
#debt_df = debt_df %>% filter(PWS_NAME %in% system_df$PWS_NAME)
##### load and rectify debt data
library(stringr)
head(fiscal_df)
fiscal_df = do.call(rbind,
                    lapply(paste0('input/fiscal_data/',grep('WDTR_clean',list.files('input/fiscal_data/'),value=T)),
                           function(x) read_excel(x) %>% mutate('File' = x) %>% 
                             filter(!is.na(Issuer))%>%
                             mutate(Issuer = str_to_upper(Issuer))))%>% rename(FYEAR = FYear)

fiscal_df$Issuer = str_to_upper(fiscal_df$Issuer)
fiscal_df$Issuer = gsub(' 0{1,}',' ',fiscal_df$Issuer)
fiscal_df$Issuer = gsub(' 0{1,}',' ',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('POST WOOD','POSTWOOD',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('CLOVERCREEK','CLOVER CREEK',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('MEADOW CREEK','MEADOWCREEK',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('SPECIAL UD','SUD',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('LAZY RIVER ID','LAZY RIVER IMPROVEMENT DISTRICT',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('TIMBERLAKE ID','TIMBERLAKE IMPROVEMENT DISTRICT',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('SEQUOIA ID','SEQUOIA IMPROVEMENT DISTRICT',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('INVERNESS FOREST ID','INVERNESS FOREST IMPROVEMENT DISTRICT',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('COMMODORE COVE ID','COMMODORE COVE IMPROVEMENT DISTRICT',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("CLEAR LAKE CITY WA","CLEAR LAKE CITY WATER AUTHORITY",fiscal_df$Issuer)
fiscal_df$Issuer = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",fiscal_df$Issuer)
fiscal_df$Issuer = gsub('REGIONAL WA','RWA',fiscal_df$Issuer)
fiscal_df$Issuer = gsub('-',' ',fiscal_df$Issuer)

fiscal_df$Issuer[fiscal_df$Issuer == "CYPRESS KLEIN UD"] <- "CYPRESS KLEIN MUD"
fiscal_df$Issuer = gsub("134 C",'134C',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("134 D",'134D',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("134 A",'134A',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("5\\\v",'5',fiscal_df$Issuer)
#fiscal_df$Issuer[fiscal_df$Issuer == "WOODCREEK RESERVE MUD"] = 'WOODCREEK MUD'
fiscal_df$Issuer = gsub("MUD THE$|MUD\\, THE$",'MUD',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("OAK MANOR UD",'OAK MANOR MUD',fiscal_df$Issuer)
fiscal_df$Issuer = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",fiscal_df$Issuer)
fiscal_df$Issuer = gsub("'",'',fiscal_df$Issuer)
fiscal_df = fiscal_df %>% rename(PWS_NAME = Issuer)


master_df = left_join(master_df,fiscal_df %>% dplyr::select(-GovtID,-File,-County)) 



debt_df = Reduce(full_join,lapply(paste0('input/fiscal_data/',grep('WD_debt',list.files('input/fiscal_data/'),value=T)),function(x) {print(x);read_excel(x, sheet=3) %>% 
                                .[,names(.)!=''] %>%
                                 mutate('File' = x) %>% filter(!is.na(Issuer)) %>%
                                 mutate(Issuer = str_to_upper(Issuer)) %>% rename(FYEAR = FYear) %>%
         mutate(Purpose = str_to_upper(Purpose)) %>% filter(!Purpose %in% c('FIRE','TRANSPORTATION','SOLID WASTE','RECREATION','COMMERCE',
                                                                                                    'RECYCLE MATERIALS','REFUND','POWER'))}))

debt_df$Issuer = gsub(' \\*','',debt_df$Issuer)
debt_df$Issuer = gsub(' 0{1,}',' ',debt_df$Issuer)
debt_df$Issuer = gsub(' 0{1,}',' ',debt_df$Issuer)
debt_df$Issuer = gsub('POST WOOD','POSTWOOD',debt_df$Issuer)
debt_df$Issuer = gsub('CLOVERCREEK','CLOVER CREEK',debt_df$Issuer)
debt_df$Issuer = gsub('MEADOW CREEK','MEADOWCREEK',debt_df$Issuer)
debt_df$Issuer = gsub('SPECIAL UD','SUD',debt_df$Issuer)
debt_df$Issuer = gsub('LAZY RIVER ID','LAZY RIVER IMPROVEMENT DISTRICT',debt_df$Issuer)
debt_df$Issuer = gsub('TIMBERLAKE ID','TIMBERLAKE IMPROVEMENT DISTRICT',debt_df$Issuer)
debt_df$Issuer = gsub('SEQUOIA ID','SEQUOIA IMPROVEMENT DISTRICT',debt_df$Issuer)
debt_df$Issuer = gsub('INVERNESS FOREST ID','INVERNESS FOREST IMPROVEMENT DISTRICT',debt_df$Issuer)
debt_df$Issuer = gsub('COMMODORE COVE ID','COMMODORE COVE IMPROVEMENT DISTRICT',debt_df$Issuer)
debt_df$Issuer = gsub("CLEAR LAKE CITY WA","CLEAR LAKE CITY WATER AUTHORITY",debt_df$Issuer)
debt_df$Issuer = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",debt_df$Issuer)
debt_df$Issuer = gsub('REGIONAL WA','RWA',debt_df$Issuer)
debt_df$Issuer = gsub('-',' ',debt_df$Issuer)

debt_df$Issuer[debt_df$Issuer == "CYPRESS KLEIN UD"] <- "CYPRESS KLEIN MUD"
debt_df$Issuer = gsub("134 C",'134C',debt_df$Issuer)
debt_df$Issuer = gsub("134 D",'134D',debt_df$Issuer)
debt_df$Issuer = gsub("134 A",'134A',debt_df$Issuer)
debt_df$Issuer = gsub("5\\\v",'5',debt_df$Issuer)
debt_df$Issuer = gsub("WOODCREEK RESERVE MUD",'WOODCREEK MUD',debt_df$Issuer)
debt_df$Issuer = gsub("MUD THE$|MUD\\, THE$",'MUD',debt_df$Issuer)
debt_df$Issuer = gsub("OAK MANOR UD",'OAK MANOR MUD',debt_df$Issuer)
debt_df$Issuer = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",debt_df$Issuer)
debt_df$Issuer = gsub("'",'',debt_df$Issuer)


# debt_df = debt_df %>% rename(PWS_NAME = Issuer) %>%
#   group_by(PWS_NAME,FYEAR,IssueType)%>%
#   summarise(IssueSize = sum(IssueSize,na.rm=T),
#             RefundingSize = sum(RefundingSize,na.rm=T),
#             NewMoneySize = sum(NewMoneySize,na.rm=T))


debt_df = debt_df %>% rename(PWS_NAME = Issuer) %>%
  group_by(PWS_NAME,FYEAR)%>%
  summarise(IssueSize = sum(IssueSize,na.rm=T),
            RefundingSize = sum(RefundingSize,na.rm=T),
            NewMoneySize = sum(NewMoneySize,na.rm=T))


master_df = left_join(master_df,debt_df)
master_df$NewMoneySize[is.na(master_df$NewMoneySize)] <- 0
master_df$NEW_DEBT = (master_df$NewMoneySize>0) + 0
#master_df$TotDebtServiceOutstanding[is.na(master_df$TotDebtServiceOutstanding)] = 0
master_df = master_df[!(master_df$`Is Wholesaler`=='Y'&master_df$Pop_Served==0),]

#master_df = master_df %>% filter(is.na(`Deactivation Date`)|(decimal_date(dmy(master_df$`Deactivation Date`))>FYEAR_END_DDATE)) 

master_df$GROUNDWATER = ifelse(grepl('Ground',master_df$`Primary Source`),1,0)
master_df$PURCHASED = ifelse(grepl('purch',master_df$`Primary Source`),1,0)
master_df$WHOLESALER = ifelse(master_df$`Is Wholesaler`=='Y',1,0)
master_df$IS_FWSD = ifelse(master_df$system_type=='FWSD',1,0)
master_df$IS_WCID = ifelse(master_df$system_type=='WCID',1,0)
master_df$log_Pop_Served = log(master_df$Pop_Served)



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

blockgroup_income = do.call(rbind,
                        lapply(paste0('input/census/block_data/',grep('B19013_with',list.files('input/census/block_data/'),value=T)),function(x)
                          read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                            mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))))) %>%
  rename(MEDIAN_HOUSEHOLD_INCOME = HD01_VD01,GEOID12 = GEO.id2)


blockgroup_med_home = do.call(rbind,
                       lapply(paste0('input/census/block_data/',grep('B25077_with',list.files('input/census/block_data/'),value=T)),function(x)
                         read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                           mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR))))))) %>%
  rename(MED_HOME_VALUE = HD01_VD01,GEOID12 = GEO.id2)


tract_home = do.call(rbind,
                     lapply(paste0('input/census/ACS_tracts/',grep('B25077_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
                       read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>% 
                         dplyr::select(GEO.id,HD01_VD01,YEAR) %>%
                         rename(AFFGEOID = GEO.id,MED_HOME_VALUE = HD01_VD01)))
tract_home = tract_home  %>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))

tract_race = do.call(rbind,lapply(paste0('input/census/ACS_tracts/',grep('DP05_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
    dplyr::select(GEO.id,HC03_VC43,HC03_VC49,YEAR) %>%    
    rename(AFFGEOID = GEO.id) %>%
    mutate(PERC_WHITE = as.numeric(ifelse(!grepl('ACS_1[3-4]',YEAR),HC03_VC43,HC03_VC49))) %>%
    dplyr::select(-HC03_VC43,-HC03_VC49)%>% 
    mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))))

tract_race2 = do.call(rbind,lapply(paste0('input/census/ACS_tracts/',grep('B02001_with',list.files('input/census/ACS_tracts/'),value=T)),function(x)
  read_csv(x,na = '-') %>% filter(1:nrow(.)!=1) %>% mutate(YEAR = x) %>%
    dplyr::select(GEO.id,HD01_VD01,HD01_VD02,YEAR) %>%  
    mutate(PERC_WHITE =  
             round(100 *as.numeric(as.character(HD01_VD02))/as.numeric(as.character(HD01_VD01)),1)) %>% 
    # dplyr::select(-HD01_VD01,-HD01_VD02) %>%
    rename(AFFGEOID = GEO.id) %>% dplyr::select(-HD01_VD01,-HD01_VD02)))%>% mutate(YEAR = as.numeric(paste0('20',gsub('_.*','',gsub('.*ACS_','',YEAR)))))

tract_race = full_join(tract_race,tract_race2)

tract_acs_data = plyr::join_all(list(tract_home,tract_ed,tract_poverty,tract_pops,tract_income,tract_race),type='full')
tract_acs_data$MED_HOME_VALUE  = as.numeric(gsub('-|\\+','',gsub('\\,','',tract_acs_data$MED_HOME_VALUE)))
tract_acs_data$HOUSEHOLD_MED_INCOME  = as.numeric(gsub('-|\\+','',gsub('\\,','',tract_acs_data$HOUSEHOLD_MED_INCOME)))
tract_acs_data$PERC_HOUSEHOLDS_POVERTY = as.numeric(tract_acs_data$PERC_HOUSEHOLDS_POVERTY)
tract_acs_data$PERC_BACH = as.numeric(tract_acs_data$PERC_BACH)
tract_acs_data$TOTAL_POP = as.numeric(tract_acs_data$TOTAL_POP)
tract_acs_data = tract_acs_data %>% arrange(AFFGEOID,YEAR)
tract_year_combos = tract_acs_data %>%
  tidyr::expand(AFFGEOID,YEAR = 2008:2016)
tract_acs_data = full_join(tract_acs_data,tract_year_combos)

tract_acs_data = mclapply(unique(tract_acs_data$AFFGEOID), function(i)
  tract_acs_data %>% arrange(AFFGEOID,YEAR) %>% filter(AFFGEOID == i) %>% 
    fill(MED_HOME_VALUE:PERC_WHITE,.direction = c( "up")) %>%
    fill(MED_HOME_VALUE:PERC_WHITE,.direction = c( "down")),mc.cores=core_num,mc.cleanup=T) %>% do.call(rbind,.)

blockgroup_data = full_join(blockgroup_income%>% dplyr::select(-HD02_VD01),blockgroup_med_home %>% dplyr::select(-HD02_VD01))
blockgroup_data$MEDIAN_HOUSEHOLD_INCOME  = as.numeric(gsub('-|\\+','',gsub('\\,','',blockgroup_data$MEDIAN_HOUSEHOLD_INCOME)))
blockgroup_data$MED_HOME_VALUE  = as.numeric(gsub('-|\\+','',gsub('\\,','',blockgroup_data$MED_HOME_VALUE)))
blockgroup_year_combos = blockgroup_data %>%
  tidyr::expand(GEOID12,YEAR = 2008:2016)
blockgroup_acs_data = full_join(blockgroup_data,blockgroup_year_combos)
blockgroup_acs_data = mclapply(unique(blockgroup_acs_data$GEOID12), function(i)
  blockgroup_acs_data %>% arrange(GEOID12,YEAR) %>% filter(GEOID12 == i) %>% 
    fill(MEDIAN_HOUSEHOLD_INCOME,MED_HOME_VALUE,.direction = c( "up")) %>%
    fill(MEDIAN_HOUSEHOLD_INCOME,MED_HOME_VALUE,.direction = c( "down")),mc.cores=core_num,mc.cleanup=T) %>% do.call(rbind,.)

write_csv(blockgroup_acs_data,'input/census/houston_blockgroup_master.csv')
tract_acs_data$POP_DENSITY_KMSQ = tract_acs_data$TOTAL_POP/
  (as.numeric(as.character(tx_tracts$ALAND[match(tract_acs_data$AFFGEOID,tx_tracts$AFFGEOID)]))/1000000)

write_csv(tract_acs_data,'input/census/houston_tract_master.csv')

### generate acs averages weighting tracts by district overlap

library(rgeos)
district_acs_tract_weighted_twd_geo = do.call(rbind,
                                        lapply(match(unique(master_df$PWS_NAME),twd_geo$NAME)[!is.na(match(unique(master_df$PWS_NAME),twd_geo$NAME))],
                                                 function(x){
                                                   overlap.geog <- gIntersection(twd_geo[x,], houston_tracts, 
                                                                                 byid = TRUE,id = as.character(houston_tracts@data$AFFGEOID),drop_lower_td =T)
                                                   overlap.percentage <- gArea(overlap.geog, byid = TRUE)/gArea(twd_geo[x,])
                                                   tract_acs_data[tract_acs_data$AFFGEOID %in% 
                                                                    sapply(overlap.geog@polygons,function(x) x@ID),] %>%
                                                     group_by(YEAR) %>% mutate(prop.weight = 
                                                                                  overlap.percentage[match(AFFGEOID,names(overlap.percentage))]) %>%
                                                     dplyr::select(-AFFGEOID) %>% 
                                                     summarise(MED_HOME_VALUE = weighted.mean(MED_HOME_VALUE,w=prop.weight,na.rm=T)) %>%
                                                     mutate(PWS_NAME = twd_geo$NAME[x])}))

district_acs_tract_weighted_twd_geo = district_acs_tract_weighted_twd_geo %>% 
rename(TRACT_WEIGHTED_MED_HOME_VALUE = MED_HOME_VALUE)

library(rgeos)
district_acs_blockgroup_weighted_twd_geo = do.call(rbind,
                                        mclapply(match(unique(master_df$PWS_NAME),twd_geo$NAME)[!is.na(match(unique(master_df$PWS_NAME),twd_geo$NAME))],
                                                 function(x){
                                                   overlap.geog <- gIntersection(twd_geo[x,], cropped_blockgroups, 
                                                                                 byid = TRUE,id = as.character(cropped_blockgroups@data$GEOID12),drop_lower_td =T)
                                                   overlap.percentage <- gArea(overlap.geog, byid = TRUE)/gArea(twd_geo[x,])
                                                   blockgroup_acs_data[blockgroup_acs_data$GEOID12 %in% 
                                                                    sapply(overlap.geog@polygons,function(x) x@ID),] %>%
                                                     group_by(YEAR) %>% mutate(prop.weight = 
                                                                                 overlap.percentage[match(GEOID12,names(overlap.percentage))]) %>%
                                                     dplyr::select(-GEOID12) %>%  
                                                     summarise(MED_HOME_VALUE = weighted.mean(MED_HOME_VALUE,w=prop.weight,na.rm=T),
                                                               MEDIAN_HOUSEHOLD_INCOME = weighted.mean( MEDIAN_HOUSEHOLD_INCOME,w=prop.weight,na.rm=T)) %>%
                                                     mutate(PWS_NAME = twd_geo$NAME[x])
                                                 },mc.cores=core_num,mc.cleanup = T))

district_acs_blockgroup_weighted_twd_geo = district_acs_blockgroup_weighted_twd_geo %>% rename(BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME = MEDIAN_HOUSEHOLD_INCOME,
                                                    BLOCK_WEIGHTED_MED_HOME_VALUE = MED_HOME_VALUE)


master_df = left_join(master_df,district_acs_tract_weighted_twd_geo)
master_df = left_join(master_df,district_acs_blockgroup_weighted_twd_geo)

rm(district_acs_tract_weighted_twd_geo)
rm(district_acs_blockgroup_weighted_twd_geo)
#master_df = left_join(master_df,tract_acs_data)

year_tract_medians = master_df %>% dplyr::select(YEAR,BLOCK_WEIGHTED_MED_HOME_VALUE,BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME,
                                                 TRACT_WEIGHTED_MED_HOME_VALUE) %>% group_by(YEAR) %>%
  summarise_all( median,na.rm=T)

master_df$BLOCK_WEIGHTED_MED_HOME_VALUE[is.na(master_df$BLOCK_WEIGHTED_MED_HOME_VALUE)] = year_tract_medians$BLOCK_WEIGHTED_MED_HOME_VALUE[match(master_df$YEAR[is.na(master_df$BLOCK_WEIGHTED_MED_HOME_VALUE)],year_tract_medians$YEAR)]
master_df$BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME[is.na(master_df$BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME)] = year_tract_medians$BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME[match(master_df$YEAR[is.na(master_df$BLOCK_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME)],year_tract_medians$YEAR)]
master_df$TRACT_WEIGHTED_MED_HOME_VALUE[is.na(master_df$TRACT_WEIGHTED_MED_HOME_VALUE)] = year_tract_medians$TRACT_WEIGHTED_MED_HOME_VALUE[match(master_df$YEAR[is.na(master_df$TRACT_WEIGHTED_MED_HOME_VALUE)],year_tract_medians$YEAR)]
master_df$TRACT_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME[is.na(master_df$TRACT_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME)] = year_tract_medians$TRACT_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME[match(master_df$YEAR[is.na(master_df$TRACT_WEIGHTED_MEDIAN_HOUSEHOLD_INCOME)],year_tract_medians$YEAR)]

# master_df$PERC_BACH[is.na(master_df$PERC_BACH)] = year_tract_medians$PERC_BACH[match(master_df$FYEAR[is.na(master_df$PERC_BACH)],year_tract_medians$FYEAR)]
# master_df$PERC_WHITE[is.na(master_df$PERC_WHITE)] = year_tract_medians$PERC_WHITE[match(master_df$FYEAR[is.na(master_df$PERC_WHITE)],year_tract_medians$FYEAR)]
#master_df$PERC_HOUSEHOLDS_POVERTY[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)] = year_tract_medians$PERC_HOUSEHOLDS_POVERTY[match(master_df$YEAR[is.na(master_df$PERC_HOUSEHOLDS_POVERTY)],year_tract_medians$YEAR)]
# master_df$HOUSEHOLD_MED_INCOME[is.na(master_df$HOUSEHOLD_MED_INCOME)] = year_tract_medians$HOUSEHOLD_MED_INCOME[match(master_df$FYEAR[is.na(master_df$HOUSEHOLD_MED_INCOME)],year_tract_medians$FYEAR)]

library(INLA)
master_df$audit_observed[is.na(master_df$audit_observed)] = FALSE
master_df$Pop[is.na(master_df$Pop)] = master_df$Pop_Served[is.na(master_df$Pop)]
master_df$log_Pop_Served = log(master_df$Pop+1)

master_df = master_df %>% 
  group_by(PWS_NAME) %>% arrange(PWS_NAME,FYEAR) %>%
  mutate(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS` = ifelse(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`==0,NA,`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`)) %>%
  fill(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`,.direction = c('down'))

master_df$TotDebtServiceOutstanding[is.na(master_df$TotDebtServiceOutstanding)] = 0
#match_vector = match(paste(master_df$PWS_NAME,master_df$YEAR-1,master_df$MONTH,sep='_'),master_df$UNIQUE_SYSTEM_MONTH)
match_vector = match(paste(master_df$PWS_NAME,master_df$FYEAR-1),paste(master_df$PWS_NAME,master_df$FYEAR))
master_df$NEW_DEBT_ISSUED_P1 = master_df$NewMoneySize[match_vector]
master_df$TOTAL_VALUATION_LAG1 = master_df$`CURRENT ASSESSED VALUATION`[match_vector]
master_df$SERVICE_CONNECTIONS_LAG1 = master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match_vector]
master_df$TOTAL_REVENUE_LAG1 = master_df$TOTAL_REVENUE[match_vector]
master_df$TOTAL_REVENUE_PER_CAPITA_LAG1 = master_df$TOTAL_REVENUE[match_vector]/master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match_vector]
master_df$TOTAL_DEBT_SERVICE_LAG1 = master_df$TotDebtServiceOutstanding[match_vector]
master_df$TOTAL_DEBT_SERVICE_PER_CAPITA_LAG1 = master_df$TotDebtServiceOutstanding[match_vector]/master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match_vector]
master_df$TOTAL_FUND_BALANCE_LAG1 = master_df$TOTAL_FUND_BALANCE[match_vector]
master_df$TOTAL_FUND_BALANCE_PER_CAPITA_LAG1 = master_df$TOTAL_FUND_BALANCE[match_vector]/master_df$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`[match_vector]
master_df$TOTAL_EXPENDITURES = master_df$`GENERAL FUND - TOTAL EXPENDITURES` + master_df$`ENTERPRISE FUND - OPERATING EXPENSES`
master_df$TOTAL_EXPENDITURES_LAG1 = master_df$TOTAL_EXPENDITURES[match_vector]
master_df$CREATION_DATE[master_df$PWS_NAME=='MONTGOMERY COUNTY MUD 40'] = "1979-01-09"
master_df$CREATION_DATE[master_df$PWS_NAME=='WOODLANDS MUD 2'] = "1967-06-16"
master_creation_dates = master_df %>% filter(!is.na(CREATION_DATE),!duplicated(paste(PWS_NAME,CREATION_DATE))) %>% dplyr::select(PWS_NAME,CREATION_DATE)
master_df = left_join(master_df %>% dplyr::select(-CREATION_DATE),master_creation_dates)

master_df$DISTRICT_AGE = decimal_date(master_df$DATE) - decimal_date(ymd(master_df$CREATION_DATE))
library(lattice)

master_df$hviol_p1_count_l1 = master_df$hviol_p1_count[match_vector]
master_df$hviol_p2_count_l1 = master_df$hviol_p2_count[match_vector]
master_df$hviol_p3_count_l1 = master_df$hviol_p3_count[match_vector]
master_df$hviol_p4_count_l1 = master_df$hviol_p4_count[match_vector]
master_df$hviol_p5_count_l1 = master_df$hviol_p5_count[match_vector]
master_df$mviol_p1_count_l1 = master_df$mviol_p1_count[match_vector]
master_df$mviol_p2_count_l1 = master_df$mviol_p2_count[match_vector]
master_df$mviol_p3_count_l1 = master_df$mviol_p3_count[match_vector]
master_df$mviol_p4_count_l1 = master_df$mviol_p4_count[match_vector]
master_df$mviol_p5_count_l1 = master_df$mviol_p5_count[match_vector]

impute_home_value = master_df %>% group_by(FYEAR) %>% summarise(avg_med_home_value = mean(TRACT_WEIGHTED_MED_HOME_VALUE,na.rm=T))
master_df$TRACT_WEIGHTED_MED_HOME_VALUE[is.na(master_df$TRACT_WEIGHTED_MED_HOME_VALUE)] = impute_home_value$avg_med_home_value[match(master_df$FYEAR[is.na(master_df$TRACT_WEIGHTED_MED_HOME_VALUE)],impute_home_value$FYEAR)]



write_excel_csv(master_df,'input/scratch/master_data.csv')
#write_excel_csv(model_df,'input/scratch/ready_model.csv')

