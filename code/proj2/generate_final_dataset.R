library(tidyverse)
library(lubridate)
library(INLA)
library(readxl)
library(gridExtra)
temp = read_csv('input/tceq_audits/district_info.csv') %>%
  mutate(NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',NAME)) %>%
  mutate(NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',NAME)) %>%
  mutate(NAME = gsub('FRESH WATER SUPPLY DISTRICT','FWSD',NAME)) %>%
  mutate(NAME = gsub('WATER SUPPLY DISTRICT','WSD',NAME)) %>%
  mutate(NAME = gsub('PUBLIC UTILITY DISTRICT','PUD',NAME)) %>%
  mutate(NAME = gsub('UTILITY DISTRICT','UD',NAME)) %>%
  rename(STATUS = `Activity Status:`,COUNTY = 'Primary County:',ACRE_SIZE = `Acre Size:`,DISTRICT_TYPE = `District Type:`)  %>%
  mutate(KEEP_TYPE = DISTRICT_TYPE %in% c("MUNICIPAL UTILITY DISTRICT")) #,"FRESH WATER SUPPLY DISTRICT" ,"WATER CONTROL AND IMPROVEMENT DISTR"))


#temp$DISTRICT_TYPE[grepl("MUD|MUNICIPAL UTILITY DISTRICT",temp$NAME)] <- "MUD"
#temp$DISTRICT_TYPE[!grepl("MUD|MUNICIPAL UTILITY DISTRICT",temp$NAME)] <- "FWSD"
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='DORMANCY AFFIDAVIT'] <- 'DORMANT'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='DORMANT'] <- 'DORMANT'
temp$STATUS[temp$STATUS == 'INACTIVE'&is.na(temp$`Activity Reason:`)] <- 'DORMANT'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='CONFIRMATION REQUIRED'] <- 'NOT STARTED'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='CONVERSION'] <- 'ACTIVE'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='ISSUED'] <- 'NOT STARTED'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Activity Reason:`=='ELECTION CONFIRMED'] <- 'NOT STARTED'
temp$STATUS[temp$STATUS == 'INACTIVE'&temp$`Financial Status:`=='DORMANT AFFIDAVIT FILED'] <- 'DORMANT'
temp$STATUS[temp$STATUS %in% c('NOT STARTED','INACTIVE','UNKNOWN-NO ACTIVIT')] = 'OUT OF SAMPLE'
temp = temp %>% rename(FORMED_BY = `Creation Type:`) %>% mutate(FORMED_BY = gsub('CITY ORDINANCE|BOARD RESOLUTION TO DIVIDE','COUNTY',FORMED_BY)) %>% 
  filter(FORMED_BY %in% c('TCEQ','LEGISLATURE','COUNTY'))
temp$NAME[temp$SYSTEM_ID==5057500] <- 'JFK MUD'

###### DISTRICT INFO ########
info = read_csv('input/texas_iwdd/infopage_wide.csv') %>% rename(SYSTEM_NAME = X2,ID = `District:`) %>% select(-X3,-X4)
info = as.tibble(info)
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

temp$TAX_RATE = info$TAX_RATE[match(temp$SYSTEM_ID,info$ID)]
temp$MULTI_COUNTY = info$MULTI_COUNTY[match(temp$SYSTEM_ID,info$ID)]
temp$ELECTED_BOARD = info$BOARD_SELECTION[match(temp$SYSTEM_ID,info$ID)]
temp$SYSTEM_NAME <- info$SYSTEM_NAME[match(temp$SYSTEM_ID,info$ID)]

temp = temp %>% filter(!is.na(temp$`Creation Date:`))
temp$`Dissolved Date:`[temp$STATUS=='DELETED/DISSOLVED'&is.na(temp$`Dissolved Date:`)] <- temp$`Activity Date:`[temp$STATUS=='DELETED/DISSOLVED'&is.na(temp$`Dissolved Date:`)] 
temp$`Dissolved Date:`[temp$STATUS=='MERGED/ANNEXED'&is.na(temp$`Dissolved Date:`)] <- temp$`Activity Date:`[temp$STATUS=='MERGED/ANNEXED'&is.na(temp$`Dissolved Date:`)] 
temp$STATUS[temp$STATUS=='MERGED/ANNEXED'] <- 'DELETED/DISSOLVED'


temp = temp %>% mutate(creation = mdy(`Creation Date:`),creation_year = year(creation),creation_decimal_date = decimal_date(creation),
                       event = ifelse(STATUS == 'DELETED/DISSOLVED',1,0),
                       fail = mdy(`Dissolved Date:`), fail_decimal_date = decimal_date(fail), fail_year = year(fail), 
                       final_decimal_date = decimal_date(mdy('12/31/2016')),
                       age_at_last_obs = ifelse(event==1,fail_decimal_date - creation_decimal_date,final_decimal_date - creation_decimal_date),
                       age_at_failure = fail_decimal_date - creation_decimal_date) %>% filter(!is.na(COUNTY)) %>%
  filter(!(event==1&is.na(fail)))


##### county district and population data
other_mun_uts = c("FRESH WATER SUPPLY DISTRICT" ,"WATER CONTROL AND IMPROVEMENT DISTR","LEVEE IMPROVEMENT DISTRICT","MUNICIPAL MANAGEMENT DISTRICT",
                  "SPECIAL UTILITY DISTRICT")
districts_by_county = expand.grid(COUNTY = as.character(sort(unique(temp$COUNTY))),YEAR = 1970:2017) 
districts_by_county$COUNTY = as.character(districts_by_county$COUNTY)
districts_by_county$ALL_ACTIVE_DEV_DISTRICTS =as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$fail_year) | y <= temp$fail_year}& temp$DISTRICT_TYPE%in%other_mun_uts),
                                                               districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))
districts_by_county$ALL_ACTIVE_MUDS <- as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$fail_year) | y <= temp$fail_year} & temp$DISTRICT_TYPE=="MUNICIPAL UTILITY DISTRICT"),
                                                        districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))
districts_by_county$ALL_ACTIVE_OTHER <- as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$fail_year) | y <= temp$fail_year} & temp$DISTRICT_TYPE!="MUNICIPAL UTILITY DISTRICT"),
                                                         districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))

all_state = districts_by_county %>% group_by(YEAR) %>% summarise(STATE_DISTRICTS = sum(ALL_ACTIVE_MUDS + ALL_ACTIVE_OTHER))
districts_by_county = left_join(districts_by_county,all_state)
districts_by_county = districts_by_county %>% mutate(PROP_IN_COUNTY_OUT_STATE = (ALL_ACTIVE_MUDS + ALL_ACTIVE_OTHER) / STATE_DISTRICTS)

temp = temp %>% filter(STATUS %in% c('ACTIVE','DELETED/DISSOLVED','DORMANT'))
temp =  temp %>% filter(KEEP_TYPE) %>% filter(creation_year>=1971) 
temp = temp %>% filter(age_at_last_obs>=1)
#temp = temp[is.na(temp$fail_decimal_date) | (temp$fail_decimal_date - 1 > temp$creation_decimal_date),]


temp = temp %>% mutate(NAME = gsub("MUNICIPAL UTILITY DISTRICT","MUD",NAME)) %>%
  mutate(NAME = gsub("BARKER CYPRESS MUD","BARKER-CYPRESS MUD",NAME)) %>%
  mutate(NAME = gsub("HARRIS FORT BEND","HARRIS-FORT BEND",NAME)) %>%
  mutate(NAME = gsub("LAKE LYNDON B JOHNSON MUD 2","LAKE LBJ MUD 2",NAME))

temp$FORMED_BY_LEGIS = ifelse(temp$FORMED_BY=='LEGISLATURE',1,0)
txtx = read_csv('input/texas_sd_data/tx_sd_tax_rates_2009_2014.csv') %>% mutate(NAME = toupper(`Special District`)) %>%
  mutate(NAME = gsub('#','',NAME)) %>% mutate(NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",NAME))
hc = read_delim('input/tax/jur_tax_dist_percent_rate.txt',delim = '\t',col_names = F) %>% filter(grepl("MUD",X3)) %>%
  rename(NAME = X3,RATE = X4) %>% mutate(NAME = gsub('^HC','HARRIS COUNTY',NAME)) %>% mutate(NAME = gsub(' HC',' HARRIS COUNTY',NAME)) %>%
  mutate(NAME = gsub('\\(.*','',NAME)) %>% mutate(NAME = gsub(' $','',NAME)) %>%
  mutate('0(?=[0-9])','',NAME)
temp$TAX_RATE[is.na(temp$TAX_RATE)] <- txtx$proptax_per100dollars[match(temp$NAME[is.na(temp$TAX_RATE)],txtx$NAME)]
temp$TAX_RATE[is.na(temp$TAX_RATE)] <- hc$RATE[match(temp$NAME[is.na(temp$TAX_RATE)],hc$NAME)]

temp$NAME[temp$SYSTEM_ID=='5189898'] <- 'KAUFMAN COUNTY MUD 11 V2'
temp$NAME[temp$SYSTEM_ID=='5189038'] <- 'KAUFMAN COUNTY MUD 12 V2'
temp$NAME[temp$SYSTEM_ID=='5189794'] <- 'KAUFMAN COUNTY MUD 9 V2'


p0 = read_csv('input/census/county_population.csv') %>% filter(state_fips==48,areaname!='Texas') %>%
  select(fips,areaname,contains('pop')) %>% gather(year,pop,-fips,-areaname) %>%
  filter(!is.na(pop)) %>% mutate(year = gsub('[A-Za-z]','',year)) %>% arrange(year) %>% 
  filter(nchar(year)==4) %>% rename(FIPS = fips,YEAR = year,POPULATION = pop,COUNTY = areaname) %>%
  mutate(COUNTY = toupper(gsub(' County','',COUNTY))) %>% mutate(COUNTY = gsub('\\,.*','',COUNTY))
p1 = read_csv('input/census/co-est2016-alldata.csv') %>% filter(STNAME == 'Texas') %>% rename(FIP = COUNTY,COUNTY = CTYNAME) %>%
  mutate(FIPS = paste0(STATE,FIP)) %>% select(FIPS,COUNTY,contains('POPESTIMATE')) %>% filter(FIPS!='48000') %>%
  gather(YEAR,POPULATION,-COUNTY,-FIPS) %>% mutate(YEAR = gsub('[A-Za-z]','',YEAR)) %>%
  mutate(COUNTY = toupper(gsub(' County','',COUNTY))) %>% mutate(COUNTY = gsub('\\,.*','',COUNTY)) %>%
  filter(YEAR>2014)
county_pops = full_join(p0,p1) %>% mutate(YEAR = as.numeric(YEAR))
county_pops$POPULATION_P1 = county_pops$POPULATION[match(paste(county_pops$COUNTY,county_pops$YEAR-1),paste(county_pops$COUNTY,county_pops$YEAR))]

districts_by_county = left_join(districts_by_county,county_pops)

##### financial data #####
#di = read_excel('input/fiscal_data/03WD_debtissuance.xlsx') %>% rename(NAME = Issuer) %>% mutate(NAME = toupper(NAME))
fi = lapply(grep('WDTR',list.files('input/fiscal_data/'),value=T),function(x) read_excel(paste0('input/fiscal_data/',x)) %>%
              rename(NAME = Issuer) %>% mutate(NAME = toupper(NAME)) %>%
              mutate(NAME = gsub(" THE$",'',NAME)) %>% mutate(NAME = gsub("’",'',NAME)) %>% 
              mutate(NAME = gsub(" 0",' ',NAME)))
fi_df = do.call(rbind,fi)
fi_df$NAME[grepl("MAURICEVILLE",fi_df$NAME)] <- "MAURICEVILLE MUD"
fi_df$NAME[grepl("SPRINGWOOD MUD",fi_df$NAME)] <- "SPRINGWOODS MUD"
fi_df$NAME = gsub('FRESH WATER SUPPLY DISTRICT','FWSD',fi_df$NAME)
fi_df$NAME = gsub('SPECIAL UD','SUD',fi_df$NAME)
fi_df$NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',fi_df$NAME)
fi_df$NAME = gsub('HARRIS-BRAZORIA COUNTY','HARRIS-BRAZORIA COUNTIES',fi_df$NAME)
fi_df$NAME = gsub('BRAZORIA-FORT BEND COUNTY','BRAZORIA-FORT BEND COUNTIES',fi_df$NAME)
fi_df$NAME[fi_df$NAME == "MEADOW CREEK MUD"] <- "MEADOWCREEK MUD"
fi_df$NAME[fi_df$NAME == "PORT OCONNOR ID"] <- "PORT OCONNOR IMPROVEMENT DISTRICT"
fi_df$NAME[fi_df$NAME== "ROCKWALL COUNTY CONS MUD 1"] <- "ROCKWALL COUNTY CONSOLIDATED MUD 1"
fi_df$NAME[fi_df$NAME=="WILLIAMSON COUNTY WATER SEWER IRRIG & DD 3"] <- "WILLIAMSON COUNTY WATER SEWER IRRIGATION AND DRAINAGE DIST 3"
fi_df = fi_df %>% rename(YEAR = FYear)
temp$NAME = gsub("WINDFERN FOREST UTILITY DISTRICT","WINDFERN FOREST UD",temp$NAME)
temp$NAME = gsub("MUNICIPAL WATER DISTRICT","MWD",temp$NAME)
fi_df = fi_df %>% filter(NAME %in% temp$NAME)
fi_df = fi_df %>% group_by(NAME) %>% arrange(NAME,YEAR) %>% fill(Pop)


##### bls economic data ####
library(stringr)
bls_date = full_join(read_csv('input/bls/bls_1990_2017_laborforce.csv') %>% 
                       gather(Year,Value,-`Series ID`,-Description) %>% 
                       spread(Description,Value) %>% mutate(Year = gsub('[A-Za-z]+| ','',Year)) %>% 
                       mutate(FIPS = gsub('[A-Z]+','',`Series ID`)) %>% select(-`Series ID`) %>% 
                       mutate(FIPS = str_extract(FIPS,'^[0-9]{5}')) %>% 
                       rename(perc_change_laborforce =`12-Month Percent Change`,YEAR = Year,laborforce = `Original Data Value`) %>%
                       mutate(YEAR = as.numeric(YEAR)),
                     read_csv('input/bls/bls_1990_2017_unemprate.csv') %>% 
                       gather(Year,Value,-`Series ID`,-Description) %>% 
                       spread(Description,Value) %>% mutate(Year = gsub('[A-Za-z]+| ','',Year)) %>% 
                       mutate(FIPS = gsub('[A-Z]+','',`Series ID`)) %>% select(-`Series ID`) %>% 
                       mutate(FIPS = str_extract(FIPS,'^[0-9]{5}')) %>% 
                       rename(perc_change_unemprate =`12-Month Percent Change`,YEAR = Year,unemp_rate = `Original Data Value`) %>%
                       mutate(YEAR = as.numeric(YEAR))) %>% mutate(laborforce = as.numeric(laborforce),unemp_rate = as.numeric(unemp_rate),
                                                                   perc_change_unemprate = as.numeric(perc_change_unemprate),
                                                                   perc_change_laborforce = as.numeric(perc_change_laborforce))

bls_date$unemp_rate_p1 = bls_date$unemp_rate[match(paste(bls_date$FIPS,bls_date$YEAR-1),paste(bls_date$FIPS,bls_date$YEAR))]
bls_date$laborforce_p1 = bls_date$laborforce[match(paste(bls_date$FIPS,bls_date$YEAR-1),paste(bls_date$FIPS,bls_date$YEAR))]
bls_date$perc_change_laborforce_p1 = 100*(bls_date$laborforce - bls_date$laborforce_p1) / bls_date$laborforce_p1
bls_date$perc_change_unemp_rate_p1 = 100*(bls_date$unemp_rate - bls_date$unemp_rate_p1) / bls_date$unemp_rate_p1


library(maps)
cfips = county.fips %>% filter(grepl('^48',fips)) %>% mutate(polyname = toupper(gsub('texas\\,','',polyname))) %>%
  rename(COUNTY = polyname,FIPS = fips) %>% mutate(FIPS = as.character(FIPS)) %>%
  mutate(COUNTY = gsub(':.*','',COUNTY))

system_year_df = expand.grid(unique(temp$SYSTEM_ID),1970:2016) %>% data.frame() %>% rename(SYSTEM_ID = Var1,YEAR = Var2)


system_year_df = left_join(system_year_df,temp %>% select(SYSTEM_NAME,NAME,FORMED_BY_LEGIS,MULTI_COUNTY,ELECTED_BOARD,COUNTY,SYSTEM_ID,DISTRICT_TYPE,ACRE_SIZE,STATUS,TAX_RATE,
                                                          fail,fail_decimal_date,fail_year,creation,creation_decimal_date,creation_year,event,age_at_last_obs,age_at_failure))

#keepE = which(temp$data$YEAR>=2003)
#temp$data = temp$data[temp$data$YEAR>=2003,]
system_year_df = left_join(system_year_df,districts_by_county)
system_year_df$COUNTY_POP_CHANGE_P1 = 100 * (system_year_df$POPULATION - system_year_df$POPULATION_P1) / system_year_df$POPULATION_P1
system_year_df$PERC_IN_COUNTY_OUT_STATE = system_year_df$PROP_IN_COUNTY_OUT_STATE*100
system_year_df$ALL_ACTIVE_DISTRICTS = system_year_df$ALL_ACTIVE_MUDS + system_year_df$ALL_ACTIVE_OTHER

system_year_df = left_join(system_year_df,cfips %>% filter(!duplicated(COUNTY)))
system_year_df= left_join(system_year_df,bls_date)
system_year_df = left_join(system_year_df,fi_df)
system_year_df$TAX_RATE[is.na(system_year_df$TAX_RATE)] <- system_year_df$TotalTaxRatePriorYear[is.na(system_year_df$TAX_RATE)]

system_year_df$ACRE_SIZE[is.na(system_year_df$ACRE_SIZE)] <- median(system_year_df$ACRE_SIZE,na.rm=T)
system_year_df = system_year_df %>%
  group_by(NAME) %>%
  arrange(NAME,YEAR) %>% mutate_at(vars(matches('Tax|Debt|Pop')),
                                   funs({if(all(is.na(.))){0} else{.}}))

repz = function(x) ifelse(is.na(x),0,x)
years_obs = system_year_df %>% group_by(NAME) %>% summarise(nn = n())
system_year_df = left_join(system_year_df,years_obs)

system_year_df = system_year_df %>% group_by(NAME) %>% arrange(NAME,YEAR) %>% 
  mutate_at(vars(matches('Tax|Debt|Pop$')),
            funs({if(sum(!is.na(.))==1){repz(.)} else{.}})) %>%
  mutate_at(vars(matches('Tax|Debt|Pop$')),
            funs(ifelse(nn<2,.,
                        imputeTS::na.interpolation(.,option = 'linear'))))

system_year_df$Log_RevDebtServiceOutstanding = log(system_year_df$RevDebtServiceOutstanding + 1)
system_year_df$Log_TaxDebtServiceOutstanding = log(system_year_df$TaxDebtServiceOutstanding + 1)
system_year_df$Log_TaxAVPriorYear = log(system_year_df$TaxAVPriorYear + 1)
system_year_df$Serv_Pop1000k = system_year_df$Pop/1000
system_year_df$LogServ_Pop = log(system_year_df$Pop+1)
system_year_df$Log_ACRE_SIZE = log(system_year_df$ACRE_SIZE+1)
system_year_df$ELECTED_BOARD <- ifelse(is.na(system_year_df$ELECTED_BOARD)|system_year_df$ELECTED_BOARD=='Appointed',0,1)
system_year_df$COUNTY_POP_CHANGE_P1 = 100 * (system_year_df$POPULATION - system_year_df$POPULATION_P1) / system_year_df$POPULATION_P1
system_year_df$PERC_IN_COUNTY_OUT_STATE = system_year_df$PROP_IN_COUNTY_OUT_STATE*100
system_year_df$LOG_PERC_IN_COUNTY_OUT_STATE <- log(system_year_df$PERC_IN_COUNTY_OUT_STATE+1)
system_year_df$ALL_ACTIVE_DISTRICTS = system_year_df$ALL_ACTIVE_MUDS + system_year_df$ALL_ACTIVE_OTHER
system_year_df$LOG_ALL_ACTIVE_DISTRICTS <- log(system_year_df$ALL_ACTIVE_DISTRICTS+1)

plot_df <- system_year_df %>% filter(!duplicated(SYSTEM_ID)) %>% ungroup() %>% select(creation_year,fail_year) %>% gather() %>%
  mutate(key = ifelse(key == 'creation_year','Year created','Year deactivated'))%>% filter(value<2017)


library(ggthemes)
figure1 = ggplot(plot_df,aes(x = value)) + geom_bar()  + facet_wrap(~key,nrow=2) + theme_tufte(ticks=F) +
  theme(axis.title.y=element_text(size=14),axis.title.x = element_blank(),legend.title=element_blank(),strip.text = element_text(size=14),
        axis.text = element_text(size = 14)) + ylab('# MUDs') +
  scale_x_continuous(breaks = seq(1971,2016,10))
ggsave(filename = 'output/figure1.tiff',plot=figure1,dpi = 300,units = 'in',width = 6.5,height=6)

system_year_df$intercept = 1
system_year_df <- system_year_df %>% filter(YEAR>=creation_year)


surv_data.0 = system_year_df %>% filter(!duplicated(SYSTEM_ID)) %>% 
  select(NAME,SYSTEM_ID,age_at_last_obs,event,fail_year,creation_decimal_date,fail_decimal_date,intercept,COUNTY)
y.surv.0 <- inla.surv(time =surv_data.0$age_at_last_obs,
                      event = surv_data.0$event)
surv_data.1 = system_year_df %>% filter(!duplicated(SYSTEM_ID),{event==0|fail_decimal_date>2003}) %>% 
  select(NAME,SYSTEM_ID,age_at_last_obs,event,fail_year,creation_decimal_date,fail_decimal_date,intercept,COUNTY) %>%
  mutate(age_in_2003 = ifelse(2003 - creation_decimal_date < 0, 0, 2003 - creation_decimal_date))
y.surv.1 = inla.surv(time = surv_data.1$age_at_last_obs, event = surv_data.1$event,truncation = surv_data.1$age_in_2003)


long_interval = 45
short_interval = 13

df_1971 = inla.coxph(y.surv.0 ~ -1 + intercept + 
                       f(COUNTY,model='iid'),
                     data = list(y.surv.0=y.surv.0,
                                 intercept = surv_data.0$intercept,
                                 COUNTY = surv_data.0$COUNTY,
                                 SYSTEM_ID = surv_data.0$SYSTEM_ID,
                                 creation_decimal_date = surv_data.0$creation_decimal_date),
                     control.hazard=list(model="rw1", n.intervals=long_interval,scale.model = TRUE))
df_1971$data = df_1971$data %>%   group_by(SYSTEM_ID) %>% arrange(SYSTEM_ID,baseline.hazard.idx) %>% 
  mutate(YEAR =  floor(creation_decimal_date + cumsum(E..coxph) - 1)) %>% ungroup() %>% select(-creation_decimal_date)
df_1971$data = left_join(df_1971$data,system_year_df)

df_2003 = inla.coxph(y.surv.1 ~ -1 + intercept + 
                       f(COUNTY,model='iid'),
                     data = list(y.surv.1=y.surv.1,
                                 intercept = surv_data.1$intercept,
                                 COUNTY = surv_data.1$COUNTY,
                                 SYSTEM_ID = surv_data.1$SYSTEM_ID,
                                 creation_decimal_date = surv_data.1$creation_decimal_date,
                                 age_in_2003 = surv_data.1$age_in_2003),
                     control.hazard=list(model="rw1", n.intervals=long_interval,scale.model = TRUE))

df_2003$data = df_2003$data %>% group_by(SYSTEM_ID) %>% 
  arrange(SYSTEM_ID,baseline.hazard.idx) %>% 
  mutate(YEAR = floor(creation_decimal_date + cumsum(E..coxph) + age_in_2003 - 1)) %>% ungroup() %>%
  select(-creation_decimal_date) 
df_2003$data = left_join(df_2003$data,system_year_df)


up_form_1971 = update.formula(df_1971$formula, ~ . + 
                                MULTI_COUNTY + FORMED_BY_LEGIS + 
                                LOG_ALL_ACTIVE_DISTRICTS + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                LOG_PERC_IN_COUNTY_OUT_STATE + 
                                f(inla.group(creation_year,n = 10),model='rw1'))

up_form_2003_0 = update.formula(df_2003$formula, ~ . + 
                                  MULTI_COUNTY + FORMED_BY_LEGIS + 
                                  LOG_ALL_ACTIVE_DISTRICTS + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                  LOG_PERC_IN_COUNTY_OUT_STATE +
                                  f(inla.group(creation_year,n = 10),model='rw1'))

up_form_2003_1 = update.formula(up_form_2003_0 , ~ . + 
                                  TAX_RATE + Log_ACRE_SIZE + 
                                  Log_RevDebtServiceOutstanding +
                                  Log_TaxDebtServiceOutstanding +
                                  Log_TaxAVPriorYear + ELECTED_BOARD + LogServ_Pop +
                                  perc_change_unemp_rate_p1 + unemp_rate_p1 + perc_change_laborforce_p1)
 
mod0 = inla(up_form_1971,control.compute = list(waic=TRUE,dic=TRUE),
            family=df_1971$family,data=c(as.list(df_1971$data),df_1971$data.list),
            E = df_1971$E)

mod1A = inla(up_form_2003_0,control.compute = list(waic=TRUE,dic=TRUE),
             family=df_2003$family,data=c(as.list(df_2003$data),df_2003$data.list),
             E = df_2003$E,control.update = list(result = mod0))

mod1B = inla(up_form_2003_1,control.compute = list(waic=TRUE,dic=TRUE),
             family=df_2003$family,data=c(as.list(df_2003$data),df_2003$data.list),
             E = df_2003$E,control.update = list(result = mod0))

coef0 = mod0$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))
coef1A = mod1A$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))
coef1B = mod1B$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))

coef0$Coef = as.factor(coef0$Coef)
coef1A$Coef = as.factor(coef1A$Coef)
coef1B$Coef = as.factor(coef1B$Coef)
library(forcats)

cp = full_join(coef0 %>% mutate(model = '1971-2016'),coef1A %>% mutate(model = '2003-2016'))
cp = full_join(cp,coef1B %>% mutate(model = '2003-2016 full'))
cp$model = as.factor(cp$model)

cp$Coef = fct_relevel(cp$Coef,  "FORMED_BY_LEGIS","MULTI_COUNTY",
                      'log(POPULATION)','COUNTY_POP_CHANGE_P1',
                      "LOG_ALL_ACTIVE_DISTRICTS" ,"LOG_PERC_IN_COUNTY_OUT_STATE",
                      'unemp_rate_p1','perc_change_unemp_rate_p1','perc_change_laborforce_p1',
                      "ELECTED_BOARD",  "Log_ACRE_SIZE" , "LogServ_Pop" ,
                       "Log_RevDebtServiceOutstanding", "Log_TaxDebtServiceOutstanding" ,"TAX_RATE","Log_TaxAVPriorYear")
cp$Coef = fct_recode(cp$Coef, `Legislative creation` = "FORMED_BY_LEGIS",`Multi-county` = "MULTI_COUNTY",
                     `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE"  ,
                     `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                     `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                     `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "Log_ACRE_SIZE"  ,       
                     `ln(Service pop.)` = "LogServ_Pop" ,
                     `Unemployment % (t-1)` = 'unemp_rate_p1',
                     `ln(county districts)`= "LOG_ALL_ACTIVE_DISTRICTS",
                     `ln(county districts/state)`  = 'LOG_PERC_IN_COUNTY_OUT_STATE',
                     `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                     `ln(county population)`=  "log(POPULATION)",
                     `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                     `% change laborforce (y-1)` = "perc_change_laborforce_p1")

cp$Coef = fct_rev(cp$Coef)

cp$model = fct_relevel(as.factor(cp$model),"1971-2016","2003-2016","2003-2016 full")

figure4 = ggplot(cp %>% filter(!grepl('intercept',Coef)),aes(y = Coef,yend=Coef,x=`0.025quant`,xend=`0.975quant`,group = model)) + 
  geom_segment(lwd=2,lineend ='round') + geom_point(aes(x = mean,fill=as.factor(Sig)),pch=21,size= 3) + 
  geom_vline(xintercept = 0,lty=2,col='grey50') + 
  scale_x_continuous(name = '95% credible interval (additive log-rate scale)')+
  facet_wrap(~model) + 
  #scale_colour_manual(values = c('grey70','grey40')) + facet_wrap(~model) + 
  #scale_y_discrete(labels=c('Elected board','Formed by legislature','Formed by TCEQ','log(Acreage)','Tax rate (%/$1000)','Multi-county MUD'))+
  scale_fill_manual(values = c('white','black'),labels = c('95% CI includes 0','95% CI does not include 0')) + 
  theme_bw() + theme(axis.title.y=element_blank(),legend.title = element_blank(),axis.title.x=element_text(size=14),
                     legend.text=element_text(size=12),axis.text= element_text(size=12),
                     legend.background = element_rect(fill= alpha('white',0.8)),
                     legend.position = c(0.25,0.1),strip.text = element_text(size=14),axis.ticks=element_blank())
ggsave(filename = 'output/figure4.tiff',plot = figure1,units='in',height=5.5,width=6.5,dpi =300)
require(grid)
basehaz <- mod0$summary.random$baseline.hazard
figure2A = ggplot() + ggtitle('Estimated baseline hazard rate for MUDs by age') +
  geom_ribbon(aes(x = ID,ymin = `0.025quant` ,ymax = `0.975quant`,fill = 'all_interval'),alpha = 0.1,data = basehaz) + 
  geom_path(aes(x = ID,y = mean,colour = 'hazard_nondorm',linetype = 'solid'),data = basehaz) + 
  #geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard_all',linetype = 'solid'),span = .25,data = basehaz) +
  scale_x_continuous(name = 'Age of MUD',expand = c(0,0)) + scale_y_continuous(name = 'Hazard rate') +
  theme_bw() +theme(legend.position = c(0.6,0.25),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),
                    legend.title = element_blank(),
                    legend.spacing.y = unit(-0.025, "cm"),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +   scale_linetype_identity() +
  scale_fill_manual(name ='', labels = '95% credible interval',values = c('grey60')) +
  scale_colour_manual(name ='', labels = c('Baseline hazard estimate (mean)'),values = c('black','grey60'))+
  guides(fill = guide_legend(order = 2,title.theme = element_blank()),colour = guide_legend(order = 1))

create_coef = mod0$summary.random$`inla.group(creation_year, n = 10)`

figure2B = ggplot() + ggtitle('Estimated coefficients for year of MUD creation, first-order random walk term') +
  geom_errorbar(aes(x = ID,ymin = `0.025quant` ,ymax = `0.975quant`,fill = 'all_interval'),alpha = 0.5,data = create_coef) +
  #geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard_all',linetype = 'solid'),span = .25,data = age_coef) +
  geom_path(aes(x = ID,y = mean,linetype = 'dotted'),colour = 'grey60',data = create_coef) + 
  geom_point(aes(x = ID,y = mean),size=1,colour = 'black',data = create_coef) + 
  scale_x_continuous(name = 'Year of MUD creation',expand = c(0,0)) + 
  scale_y_continuous(name = 'Posterior estimate') +
  theme_bw() +theme(legend.position = c(0.5,0.2),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),legend.title=element_text(size=14),legend.spacing.y = unit(-0.10, "cm"),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +   scale_linetype_identity() +
  scale_fill_manual(name ='', labels = '95% credible interval',values = c('grey60')) +
  scale_colour_manual(name = 'mean estimate',values='black')
ggsave(plot = grid.arrange(figure2A,figure2B),filename = 'output/figure2.tiff',dpi=300,units='in',width=6.5,height=6)


library(rgdal)
library(sp)
library(rgeos)
library(maptools)
random_county_vals <- mod0$summary.random$COUNTY
random_county_vals$scaled_mean = scale(mod0$summary.random$COUNTY$mean)
tx_counties <- readOGR('spatial_inputs/government_units/','county_nrcs_a_tx')
tx_counties$COUNTYNAME <- toupper(tx_counties$COUNTYNAME)
random_county_vals$COUNTYNAME = random_county_vals$ID
tx_counties@data <- left_join(tx_counties@data,random_county_vals)
# Next the shapefile has to be converted to a dataframe for use in ggplot2
tx_df <- fortify(tx_counties,region="OBJECTID")
tx_counties@data$id = tx_counties@data$OBJECTID
tx_df <- left_join(tx_df,tx_counties@data)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
library(viridis)
library(ggthemes)
map1 <- ggplot() + ggtitle('County random intercept estimates') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=scaled_mean),colour = 'grey70') + 
  scale_fill_viridis(option = 'D',name = 'mean') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))

map2 <- ggplot() + ggtitle('') + #ggtitle('Random intercept standard deviation') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=sd),colour = 'grey70') + 
  scale_fill_viridis(option = 'D',name = 'sd') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),legend.title.align = .5,
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))



ggsave(plot=grid.arrange(map1,map2,ncol=2),filename = 'output/figure3.tiff',dpi=300,units='in',width=6.5,height=4)


########### explore strata sensitivity ###############
strata_vars_0 = c("MULTI_COUNTY_gr","FORMED_BY_LEGIS_gr") #, "creation_year_gr")
df_1971$data$MULTI_COUNTY_gr = df_1971$data$MULTI_COUNTY + 1
df_1971$data$FORMED_BY_LEGIS_gr = df_1971$data$FORMED_BY_LEGIS + 1
#df_1971$data$creation_decade_gr <- as.numeric(cut(df_1971$data$creation_decimal_date, breaks=c(-Inf,1980,1990,2000,Inf),labels = c("1970s","1980s","1990s","2000s")))
df_1971$data$stratify_var = 1

long_hazard_set = lapply(seq_along(strata_vars_0),function(i) {
  print(strata_vars_0[i]) 
  temp_form = as.formula(gsub('scale.model = TRUE',paste0('scale.model = TRUE,replicate = ',strata_vars_0[i]),deparse(up_form_1971)))
  mod = inla(temp_form,control.compute = list(waic=TRUE,dic=TRUE),
             family=df_1971$family,data=c(as.list(df_1971$data),df_1971$data.list),
             E = df_1971$E)
  mod})
library(brinla)

juris_strata = long_hazard_set[[1]]$summary.random$baseline.hazard %>% 
  mutate(Model = 'Jurisidiction',Strata = rep(c('Single county','Multi-county'),each = nrow(.)/2))

figure_A2A = ggplot(juris_strata,aes(x = ID,y = mean,group = Strata,ymin = `0.025quant`,ymax = `0.975quant`)) + 
  #facet_wrap(~Strata,ncol=2) + 
  geom_ribbon(aes(fill=Strata),alpha = 0.5) + 
  geom_path(aes(col = Strata)) + 
  scale_fill_colorblind(name = 'Stratified baseline hazard estimate') + 
  scale_color_colorblind(name = 'Stratified baseline hazard estimate') + theme_bw() + 
  theme(legend.position = c(0.65,0.25),
        axis.text= element_text(size = 12),axis.title=element_text(size = 12),
        legend.text= element_text(size = 12),legend.title=element_blank()) + 
  scale_x_continuous(name = 'Time (years)',expand=c(0,0)) + scale_y_continuous(name = 'Baseline hazard') + 
  ggtitle('95% hazard credible interval stratified by jurisdiction type')

formed_strata = long_hazard_set[[2]]$summary.random$baseline.hazard %>% 
  mutate(Model = 'Formation',Strata = rep(c('Formed by TCEQ/local gov.','Formed by legislature'),each = nrow(.)/2))
figure_A2B = ggplot(formed_strata,aes(x = ID,y = mean,group = Strata,ymin = `0.025quant`,ymax = `0.975quant`)) + 
  #facet_wrap(~Strata,ncol=2) + 
  geom_ribbon(aes(fill=Strata),alpha = 0.5) + 
  geom_path(aes(col = Strata)) + 
  scale_fill_colorblind(name = 'Stratified baseline hazard estimate') + 
  scale_color_colorblind(name = 'Stratified baseline hazard estimate') + theme_bw() + 
  theme(legend.position = c(0.45,0.25),
        axis.text= element_text(size = 12),axis.title=element_text(size = 12),
        legend.text= element_text(size = 12),legend.title=element_blank()) + 
  scale_x_continuous(name = 'Time (years)',expand = c(0,0)) + scale_y_continuous(name = 'Baseline hazard') + 
  ggtitle('95% hazard credible interval stratified by formation type')
library(gridExtra)
ggsave(plot = grid.arrange(figure_A2A,figure_A2B,ncol=1),dpi = 300,
       filename = 'output/figureA2.tiff',units='in',width=6.5,height = 5)
#ggsave(plot = grid.arrange(figure_A1A,figure_A1B,ncol=1),filename = 'figureA1.tiff')


haz_dens_df = rbind(do.call(rbind,lapply(names(mod0$marginals.fixed),function(n) 
  mod0$marginals.fixed[[n]] %>% data.frame() %>% mutate(coef = n,model = 'restricted'))),
do.call(rbind,lapply(names(long_hazard_set[[1]]$marginals.fixed),function(n) 
  long_hazard_set[[1]]$marginals.fixed[[n]] %>% data.frame() %>% mutate(coef = n,model = 'jurisdiction'))),
do.call(rbind,lapply(names(long_hazard_set[[2]]$marginals.fixed),function(n) 
  long_hazard_set[[2]]$marginals.fixed[[n]] %>% data.frame() %>% mutate(coef = n,model = 'formation'))))
library(forcats)
haz_dens_df$coef <- as.factor(haz_dens_df$coef)

haz_dens_df$coef = fct_relevel(haz_dens_df$coef ,  'intercept','MULTI_COUNTY',"FORMED_BY_LEGIS" ,
                               "log(POPULATION)","COUNTY_POP_CHANGE_P1" ,"LOG_ALL_ACTIVE_DISTRICTS",
                               "LOG_PERC_IN_COUNTY_OUT_STATE")
haz_dens_df$coef  = fct_recode(haz_dens_df$coef, `Legislative creation` = "FORMED_BY_LEGIS",`Multi-county` = "MULTI_COUNTY",
                     `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE"  ,
                     `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                     `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                     `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "log(ACRE_SIZE)"  ,       
                     `ln(Service pop.)` = "LogServ_Pop" ,
                     `Unemployment % (t-1)` = 'unemp_rate',
                     `ln(county districts)`= "LOG_ALL_ACTIVE_DISTRICTS",
                     `ln(county districts/state)`  = 'LOG_PERC_IN_COUNTY_OUT_STATE',
                     `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                     `ln(county population)`=  "log(POPULATION)",
                     `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                     `% change laborforce (y-1)` = "perc_change_laborforce_p1")

figure_A3 = ggplot(haz_dens_df,aes(x = x,y = y,color=model)) + geom_path() + facet_wrap(~coef,scales = 'free')  +
  scale_y_continuous('posterior density') + scale_x_continuous('posterior estimate') +
  theme_bw() + scale_color_colorblind(name = 'Baseline hazard stratification',labels = c('Legislative formation','Multi-county','None')) + 
  theme(legend.position = c(0.7,0.1),axis.text = element_text(size = 10),axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),strip.text = element_text(size = 10),legend.text = element_text(size = 12))

ggsave(plot=figure_A3,filename = 'output/figureA3.tiff',units='in',dpi = 300,height=6,width=6.5)


strata_vars_1 = c("MULTI_COUNTY_gr","FORMED_BY_LEGIS_gr",'TAX_RATE_gr','Log_ACRE_SIZE_gr',
                  "ELECTED_BOARD_gr","Log_DEBT_gr","Log_TaxAVPriorYear_gr","LogServ_Pop_gr")

df_2003$data$MULTI_COUNTY_gr = df_2003$data$MULTI_COUNTY + 1
df_2003$data$FORMED_BY_LEGIS_gr = df_2003$data$FORMED_BY_LEGIS + 1
df_2003$data$ELECTED_BOARD_gr = df_2003$data$ELECTED_BOARD + 1

df_2003$data$TAX_RATE_gr <- as.numeric(cut(df_2003$data$TAX_RATE, breaks=c(-Inf,quantile(df_2003$data$TAX_RATE,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
df_2003$data$Log_ACRE_SIZE_gr <- as.numeric(cut(df_2003$data$Log_ACRE_SIZE, breaks=c(-Inf,quantile(df_2003$data$Log_ACRE_SIZE,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
df_2003$data$Log_DEBT_gr <-  as.numeric(cut(log(df_2003$data$RevDebtServiceOutstanding +
                                              df_2003$data$TaxDebtServiceOutstanding + 1),
                                        breaks=c(-Inf,quantile(log(df_2003$data$RevDebtServiceOutstanding +
                                                                     df_2003$data$TaxDebtServiceOutstanding + 1),probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
df_2003$data$Log_TaxAVPriorYear_gr <- as.numeric(cut(df_2003$data$Log_TaxAVPriorYear, breaks=c(-Inf,quantile(df_2003$data$Log_TaxAVPriorYear,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
df_2003$data$LogServ_Pop_gr <- as.numeric(cut(df_2003$data$LogServ_Pop, breaks=c(-Inf,quantile(df_2003$data$LogServ_Pop,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
df_2003$data$stratify_var = 1


short_hazard_set = lapply(seq_along(strata_vars_1),function(x) {
  print(strata_vars_1[x]) 
  temp_form = as.formula(gsub('scale.model = TRUE',paste0('scale.model = TRUE,replicate = ',strata_vars_1[x]),deparse(up_form_2003_1)))
  mod = inla(temp_form,control.compute = list(waic=TRUE,dic=TRUE),
             family=df_2003$family,data=c(as.list(df_2003$data),df_2003$data.list),
             E = df_2003$E,control.update = list(result = mod0))
  rm(temp_form)
  mod})


library(data.table)

ic_table = full_join(data.frame('Model' = c('Restricted',paste0('Stratify: ',strata_vars_0)),'Period' = '1971-2016',
           WAIC = round(c(mod0$waic$waic,sapply(long_hazard_set,function(x) x$waic$waic)),3),
           DIC = round(c(mod0$dic$dic,sapply(long_hazard_set,function(x) x$dic$dic)),3)),
data.frame('Model' = c('Restricted',paste0('Stratify: ',strata_vars_1)),'Period' = '2003-2016',
           WAIC = c(mod1B$waic$waic,sapply(short_hazard_set,function(x) x$waic$waic)),
           DIC = c(mod1B$dic$dic,sapply(short_hazard_set,function(x) x$dic$dic))))  %>% 
  data.table() %>% dcast(.,Model~Period,value.var = c("WAIC","DIC")) %>% arrange(-`WAIC_1971-2016`) %>%
  .[c(2,1,3:9),c(1,2,4,3,5)]

library(stargazer)
stargazer(ic_table,summary = F,out = 'output/table_A1.html')


haz_dens_df2 = rbind(do.call(rbind,lapply(names(mod1B$marginals.fixed),function(n) 
  mod1B$marginals.fixed[[n]] %>% data.frame() %>% mutate(coef = n,model = 'restricted'))),
  do.call(rbind,lapply(names(short_hazard_set[[3]]$marginals.fixed),function(n) 
    short_hazard_set[[3]]$marginals.fixed[[n]] %>% data.frame() %>% mutate(coef = n,model = 'Tax rate'))))
library(forcats)
haz_dens_df2$coef <- as.factor(haz_dens_df2$coef)

haz_dens_df2$coef = fct_relevel(haz_dens_df2$coef ,  'intercept','MULTI_COUNTY',"FORMED_BY_LEGIS" ,
                               "log(POPULATION)","COUNTY_POP_CHANGE_P1" ,"LOG_ALL_ACTIVE_DISTRICTS",
                               "LOG_PERC_IN_COUNTY_OUT_STATE")
haz_dens_df2$coef  = fct_recode(haz_dens_df2$coef, `Legislative creation` = "FORMED_BY_LEGIS",`Multi-county` = "MULTI_COUNTY",
                               `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE"  ,
                               `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                               `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                               `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "Log_ACRE_SIZE"  ,       
                               `ln(Service pop.)` = "LogServ_Pop" ,
                               `Unemployment % (t-1)` = 'unemp_rate_p1',
                               `ln(county districts)`= "LOG_ALL_ACTIVE_DISTRICTS",
                               `ln(county districts/state)`  = 'LOG_PERC_IN_COUNTY_OUT_STATE',
                               `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                               `ln(county population)`=  "log(POPULATION)",
                               `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                               `% change laborforce (y-1)` = "perc_change_laborforce_p1")

figure_A4 = ggplot(haz_dens_df2,aes(x = x,y = y,color=model)) + geom_path() + facet_wrap(~coef,scales = 'free')  +
  scale_y_continuous('posterior density') + scale_x_continuous('posterior estimate') +
  theme_bw() + scale_color_colorblind(name = 'Baseline hazard stratification') + 
                                  #    labels = c('Legislative formation','Multi-county','None')) + 
  theme(legend.position = c(0.7,0.1),axis.text = element_text(size = 10),axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),strip.text = element_text(size = 10),legend.text = element_text(size = 12))

figure_A4

########### explore interval sensitivity ###############
interval_seq = seq(10,90,10)
hazard_set = lapply(seq_along(interval_seq),function(i) {
  print(interval_seq[i])
  temp.surv_data.0 = system_year_df %>% filter(!duplicated(SYSTEM_ID)) %>% 
    select(NAME,SYSTEM_ID,age_at_last_obs,event,fail_year,creation_decimal_date,fail_decimal_date,intercept,COUNTY)
  temp.y.surv.0 <- inla.surv(time =temp.surv_data.0$age_at_last_obs,
                        event = temp.surv_data.0$event)
  temp_df0 = inla.coxph(y.surv.0 ~ -1 + intercept + 
                         f(COUNTY,model='iid'),
                       data = list(y.surv.0=temp.y.surv.0,
                                   intercept = temp.surv_data.0$intercept,
                                   COUNTY = temp.surv_data.0$COUNTY,
                                   SYSTEM_ID = temp.surv_data.0$SYSTEM_ID,
                                   creation_decimal_date = temp.surv_data.0$creation_decimal_date),
                       control.hazard=list(model="rw1", n.intervals=interval_seq[i],scale.model = TRUE))
  temp_df0$data = temp_df0$data %>%   group_by(SYSTEM_ID) %>% arrange(SYSTEM_ID,baseline.hazard.idx) %>% 
    mutate(YEAR =  floor(creation_decimal_date + cumsum(E..coxph) - 1)) %>% ungroup() %>% select(-creation_decimal_date)
  temp_df0$data = left_join(temp_df0$data,system_year_df)
  temp.up_form_1971 = update.formula(temp_df0$formula, ~ . + 
                                  MULTI_COUNTY + FORMED_BY_LEGIS + 
                                  LOG_ALL_ACTIVE_DISTRICTS + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                  LOG_PERC_IN_COUNTY_OUT_STATE + 
                                  f(inla.group(creation_year,n = 10),model='rw1'))

  tmod = inla(temp.up_form_1971,control.compute = list(waic=TRUE,dic=TRUE),
              family=temp_df0$family,data=c(as.list(temp_df0$data),temp_df0$data.list),
              E = temp_df0$E)
  tmod
})

hazard_set_short = lapply(seq_along(interval_seq),function(i) {
  print(interval_seq[i])
  temp.surv_data.1 = system_year_df %>% filter(!duplicated(SYSTEM_ID),{is.na(fail_year)|fail_year>2003}) %>%
    select(NAME,SYSTEM_ID,age_at_last_obs,event,fail_year,creation_decimal_date,fail_decimal_date,intercept,COUNTY) %>%
    mutate(age_in_2003 = ifelse(2003 - creation_decimal_date < 0, 0, 2003 - creation_decimal_date))
  temp.y.surv.1 = inla.surv(time = temp.surv_data.1$age_at_last_obs, event = temp.surv_data.1$event,
                            truncation = temp.surv_data.1$age_in_2003)
  temp_df1 = inla.coxph(temp.y.surv.1 ~ -1 + intercept + 
                          f(COUNTY,model='iid'),
                        data = list(temp.y.surv.1=temp.y.surv.1,
                                    intercept = temp.surv_data.1$intercept,
                                    COUNTY = temp.surv_data.1$COUNTY,
                                    SYSTEM_ID = temp.surv_data.1$SYSTEM_ID,
                                    creation_decimal_date = temp.surv_data.1$creation_decimal_date,
                                    age_in_2003 = temp.surv_data.1$age_in_2003),
                        control.hazard=list(model="rw1", n.intervals=interval_seq[i],scale.model = TRUE))

  temp_df1$data = temp_df1$data %>% group_by(SYSTEM_ID) %>%
    arrange(SYSTEM_ID,baseline.hazard.idx) %>%
    mutate(YEAR = floor(creation_decimal_date + cumsum(E..coxph) + age_in_2003 - 1)) %>% ungroup() %>%
    select(-creation_decimal_date)
  temp_df1$data = left_join(temp_df1$data,system_year_df)
  temp.up_form_2003_1 = update.formula(temp_df1$formula, ~ . +
                                         MULTI_COUNTY + FORMED_BY_LEGIS + 
                                         LOG_ALL_ACTIVE_DISTRICTS + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                         LOG_PERC_IN_COUNTY_OUT_STATE +
                                         f(inla.group(creation_year,n = 10),model='rw1') +
                                         Log_RevDebtServiceOutstanding +
                                         Log_TaxDebtServiceOutstanding +
                                         TAX_RATE + Log_ACRE_SIZE +
                                         Log_TaxAVPriorYear + ELECTED_BOARD + LogServ_Pop +
                                         perc_change_unemp_rate_p1 + unemp_rate_p1 + perc_change_laborforce_p1)

  tmod1 = inla(temp.up_form_2003_1,control.compute = list(waic=TRUE,dic=TRUE),
              family=temp_df1$family,data=c(as.list(temp_df1$data),temp_df1$data.list),
              E = temp_df1$E,control.update = list(result = hazard_set[[i]]))
  tmod1
})

haz_df = do.call(rbind,lapply(seq_along(hazard_set),function(h) {
data.frame(ninterval = interval_seq[h],
  ID  = hazard_set[[h]]$summary.random$baseline.hazard$ID,
mod0 = hazard_set[[h]]$summary.random$baseline.hazard$mean,
mod1 = hazard_set_short[[h]]$summary.random$baseline.hazard$mean)}))

haz_df = gather(haz_df,model,mean,-ninterval,-ID)
library(viridis)

figure_A1A = ggplot(haz_df %>% filter(model == 'mod0'),aes(x = ID,y = mean,group = paste0(ninterval,model),color=ninterval)) + 
  geom_path() + theme_bw() + 
  scale_color_viridis(option = 'C',name = '# intervals',breaks=c(10,30,50,70,90)) +
  ggtitle('Estimated baseline hazard by interval length, 1971-2016 model') + 
  scale_x_continuous(name = 'Age of MUD (years)') + scale_y_continuous(name = 'Baseline hazard') + 
  theme(axis.ticks=element_blank(), axis.text = element_text(size = 12),axis.title = element_text(size=12),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))

figure_A1B = ggplot(haz_df %>% filter(model == 'mod1'),
                    aes(x = ID,y = mean,group = paste0(ninterval,model),color=ninterval)) + 
  geom_path() + theme_bw() + 
scale_color_viridis(option = 'C',name = '# intervals',breaks=c(10,30,50,70,90)) +
  ggtitle('Estimated baseline hazard by interval length, 2003-2016 model') + 
  scale_x_continuous(name = 'Age of MUD (years)') + scale_y_continuous(name = 'Baseline hazard') + 
  theme(axis.ticks=element_blank(), axis.text = element_text(size = 12),axis.title = element_text(size=12),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12)) 

ggsave(plot = grid.arrange(figure_A1A,figure_A1B),file = 'output/figureA1.tiff',dpi = 300,units='in',height=6,width=6.5)



# base_surv  = inla(y.surv ~ -1 + intercept_1971 + FORMED_BY_LEGIS + 
#              MULTI_COUNTY + 
#              f(inla.group(creation_year),model='rw1') + 
#              f(COUNTY,model='iid'),family = 'coxph',
#            data = list(y.surv=y.surv, intercept_1971 = temp$intercept_1971, NAME = temp$NAME, COUNTY = temp$COUNTY,creation_year = temp$creation_year,
#                        MULTI_COUNTY = temp$MULTI_COUNTY, SYSTEM_ID = temp$SYSTEM_ID,FORMED_BY_LEGIS = temp$FORMED_BY_LEGIS),
#            control.hazard=list(model="rw1", n.intervals=46,scale.model = TRUE))
# basehaz <- base_surv$summary.random$baseline.hazard
# ggplot() + 
#   geom_ribbon(aes(x = ID,ymin = `0.025quant` ,ymax = `0.975quant`,fill = 'all_interval'),alpha = 0.1,data = basehaz) + 
#   geom_path(aes(x = ID,y = mean,colour = 'hazard_nondorm',linetype = 'dotted'),data = basehaz) + 
#   geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard_all',linetype = 'solid'),span = .25,data = basehaz) +
#   scale_x_continuous(name = 'Age of MUD') + scale_y_continuous(name = 'Hazard rate') +
#   theme_bw() +theme(legend.position = c(0.7,0.2),axis.text=element_text(size=14),
#                     axis.title=element_text(size=14),legend.title=element_text(size=14),legend.spacing.y = unit(-0.25, "cm"),
#                     legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +   scale_linetype_identity() +
#   scale_fill_manual(name ='', labels = '95% credible interval',values = c('grey60')) +
#   scale_colour_manual(name ='Baseline hazard', labels = c('mean estimate','smoothed rate'),values = c('black','grey60','blue')) +
#   guides(fill = guide_legend(order = 2,title.theme = element_blank()),colour = guide_legend(order = 1,override.aes = list(linetype = c('dotted','solid'))))


t0a = mod0$summary.fixed %>% mutate(ID = rownames(.)) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '1971-2016')

t0b = mod0$summary.hyperpar %>% mutate(ID = rownames(.)) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '1971-2016')

t1a = mod1A$summary.fixed %>% 
  mutate(ID = rownames(.)) %>% select(ID,mean,`0.025quant`,`0.975quant`) %>%
  mutate(`0.025quant` = round(`0.025quant`,3),
         mean = round(mean,3),`0.975quant`=round(`0.975quant`,3))  %>%
  mutate(model = '2003-2016')

t1b = mod1A$summary.hyperpar %>% mutate(ID = rownames(.)) %>%select(ID,mean,`0.025quant`,`0.975quant`) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '2003-2016')

t2a = mod1B$summary.fixed %>% 
  mutate(ID = rownames(.)) %>% select(ID,mean,`0.025quant`,`0.975quant`) %>%
  mutate(`0.025quant` = round(`0.025quant`,3),
         mean = round(mean,3),`0.975quant`=round(`0.975quant`,3))  %>%
  mutate(model = '2003-2016 full')

t2b = mod1B$summary.hyperpar %>% mutate(ID = rownames(.)) %>%select(ID,mean,`0.025quant`,`0.975quant`) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '2003-2016 full')


ct = Reduce(full_join,list(t0a,t0b,t1a,t1b,t2a,t2b)) %>% mutate(P = grepl('Precision',ID)) %>% arrange(P)

ct$Coef = as.factor(ct$ID)
ct$Coef = fct_relevel(ct$Coef, 'intercept', "FORMED_BY_LEGIS","MULTI_COUNTY","ELECTED_BOARD", 
                      "TAX_RATE"  ,"Log_ACRE_SIZE" , "LogServ_Pop" ,
                      "Log_RevDebtServiceOutstanding", "Log_TaxDebtServiceOutstanding" ,"Log_TaxAVPriorYear" ,
                      'log(POPULATION)','COUNTY_POP_CHANGE_P1',
                      "LOG_ALL_ACTIVE_DISTRICTS" ,"LOG_PERC_IN_COUNTY_OUT_STATE",
                      'unemp_rate_p1','perc_change_unemp_rate_p1','perc_change_laborforce_p1')

ct$Coef = fct_recode(ct$Coef, `(intercept)` = 'intercept',
                     `Legislative creation` = "FORMED_BY_LEGIS",
                     `Multi-county` = "MULTI_COUNTY",
                     `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE" ,
                     `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                     `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                     `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "Log_ACRE_SIZE"  ,       
                     `ln(Service pop.)` = "LogServ_Pop" ,
                     `Unemployment % (t-1)` = 'unemp_rate_p1',
                     `ln(county districts)`= "LOG_ALL_ACTIVE_DISTRICTS",
                     `ln(county districts/state)`  = 'LOG_PERC_IN_COUNTY_OUT_STATE',
                     `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                     `ln(county population)`=  "log(POPULATION)",
                     `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                     `% change laborforce (y-1)` = "perc_change_laborforce_p1")
ctt = ct %>% select(-mean) %>% 
  mutate(CI = paste(`0.025quant`,`0.975quant`,sep=', '))  %>% 
  select(-`0.025quant`,-`0.975quant`,-P,-ID) %>% spread(model,CI) 
ctt$`1971-2016`[is.na(ctt$`1971-2016`)] <- '---'
ctt$`2003-2016`[is.na(ctt$`2003-2016`)] <- '---'
stargazer(ctt,type = 'html',out = 'output/table_B1.html',summary = F,digits = 3)




library(stargazer)

library(texreg)
library(knitr)


#random_type_vals <- mod$summary.random$DISTRICT_TYPE


library(survival)
library(KMsurv)
test = temp %>% filter(creation_decimal_date>1971)
my.surv.object1 <- Surv(ifelse(test$event==1,test$age_at_failure, test$age_at_end ),test$event)
test2 = test %>% filter(event==0|fail_decimal_date>2003.00)
my.surv.object2 <- Surv(ifelse(test2$creation_decimal_date > 2003, 0, 2003 - test2$creation_decimal_date),
     ifelse(test2$event == 1,test2$age_at_failure,test2$age_at_end),
     test2$event)
my.fit1 <- survfit(my.surv.object1~1)
my.fit2 <- survfit(my.surv.object2~1)
plot(my.fit1, main="Kaplan-Meier estimate with 95% confidence bounds",
     xlab="time", ylab="survival function")
lines(my.fit2)




