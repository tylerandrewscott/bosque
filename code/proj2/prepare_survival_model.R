library(tidyverse)
library(lubridate)
library(INLA)
library(readxl)

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


temp %>% mutate(creation = mdy(`Creation Date:`),creation_decimal_date = decimal_date(creation),
                       event = ifelse(STATUS == 'DELETED/DISSOLVED',1,0),fail = mdy(`Dissolved Date:`)) %>%
  mutate(creation_year = year(creation), deactivation_year = year(fail)) %>% filter(!is.na(COUNTY))

other_mun_uts = c("FRESH WATER SUPPLY DISTRICT" ,"WATER CONTROL AND IMPROVEMENT DISTR","LEVEE IMPROVEMENT DISTRICT","MUNICIPAL MANAGEMENT DISTRICT",
                  "SPECIAL UTILITY DISTRICT")

temp = temp %>% filter(STATUS %in% c('ACTIVE','DELETED/DISSOLVED','DORMANT'))
temp =  temp %>% filter(KEEP_TYPE) %>%
  filter(is.na(deactivation_year)|deactivation_year>=1971) %>%
  filter(creation_year>=1971) %>% filter(!(event==1&is.na(fail)))

temp$final_date = ifelse(temp$event==0,decimal_date(mdy('01/01/2017')),
                         decimal_date(ymd(temp$fail)))
temp$time = temp$final_date - temp$start_date
temp = temp %>% filter(time > 1)

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

temp$start_2003 = decimal_date(mdy('01/01/2003'))
temp$creation_year <- year(mdy(temp$`Creation Date:`))
temp$init = ifelse(temp$creation_year<2003,temp$start_2003,decimal_date(mdy(temp$`Creation Date:`)))
temp$final_date = decimal_date(mdy('01/01/2017'))
temp$time_2003 = temp$final_date - temp$init

temp$age_at_2003 = ifelse(temp$creation_year<2003,temp$start_2003-decimal_date(mdy(temp$`Creation Date:`)),0)
temp$final_age = temp$init + temp$final_date - ifelse(temp$creation_year<2003,temp$start_2003,
                                                      decimal_date(mdy(temp$`Creation Date:`)))

##### county district and population data

districts_by_county = expand.grid(COUNTY = as.character(sort(unique(temp$COUNTY))),YEAR = 1971:2016) 
districts_by_county$COUNTY = as.character(districts_by_county$COUNTY)
districts_by_county$ALL_ACTIVE_DEV_DISTRICTS =as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$deactivation_year) | y <= temp$deactivation_year}& temp$DISTRICT_TYPE%in%other_mun_uts),
                                                               districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))
districts_by_county$ALL_ACTIVE_MUDS <- as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$deactivation_year) | y <= temp$deactivation_year} & temp$DISTRICT_TYPE=="MUNICIPAL UTILITY DISTRICT"),
                                                        districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))
districts_by_county$ALL_ACTIVE_OTHER <- as.vector(mapply(function(c,y) sum(temp$COUNTY == c & temp$creation_year<=y & {is.na(temp$deactivation_year) | y <= temp$deactivation_year} & temp$DISTRICT_TYPE!="MUNICIPAL UTILITY DISTRICT"),
                                                         districts_by_county$COUNTY, districts_by_county$YEAR,SIMPLIFY = T))

all_state = districts_by_county %>% group_by(YEAR) %>% summarise(STATE_DISTRICTS = sum(ALL_ACTIVE_MUDS + ALL_ACTIVE_OTHER))
districts_by_county = left_join(districts_by_county,all_state)
districts_by_county = districts_by_county %>% mutate(PROP_IN_COUNTY_OUT_STATE = (ALL_ACTIVE_MUDS + ALL_ACTIVE_OTHER) / STATE_DISTRICTS)

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


system_year_df = expand.grid(unique(temp$SYSTEM_ID),1971:2016) %>% data.frame() %>% rename(SYSTEM_ID = Var1,YEAR = Var2)

names(temp)
temp$creation_decimal_date[1]

temp %>% select(creation_decimal_date)
temp %>% select(SYSTEM_NAME,FORMED_BY_LEGIS,MULTI_COUNTY,ELECTED_BOARD,COUNTY,SYSTEM_ID,DISTRICT_TYPE,ACRE_SIZE,STATUS,
                cr)



temp$data = temp$data %>% group_by(NAME) %>% arrange(NAME,baseline.hazard.idx) %>% 
  mutate(YEAR = max(creation_year,2003) + cumsum(intercept_2003)-1)

#keepE = which(temp$data$YEAR>=2003)
#temp$data = temp$data[temp$data$YEAR>=2003,]
temp$data = left_join(temp$data,districts_by_county)
temp$data$COUNTY_POP_CHANGE_P1 = 100 * (temp$data$POPULATION - temp$data$POPULATION_P1) / temp$data$POPULATION_P1
temp$data$PERC_IN_COUNTY_OUT_STATE = temp$data$PROP_IN_COUNTY_OUT_STATE*100
temp$data$ALL_ACTIVE_DISTRICTS = temp$data$ALL_ACTIVE_MUDS + temp$data$ALL_ACTIVE_OTHER
temp$data = left_join(temp$data,cfips %>% filter(!duplicated(COUNTY)))
temp$data = left_join(temp$data,bls_date)

temp$data = left_join(temp$data,fi_df)
temp$data$TAX_RATE[is.na(temp$data$TAX_RATE)] <- temp$data$TotalTaxRatePriorYear[is.na(temp$data$TAX_RATE)]

temp$data = temp$data  %>%
  group_by(NAME) %>%
  arrange(NAME,YEAR) %>% mutate_at(vars(matches('Tax|Debt|Pop')),
                                   funs({if(all(is.na(.))){0} else{.}}))

repz = function(x) ifelse(is.na(x),0,x)

years_obs = temp$data %>% group_by(NAME) %>% summarise(nn = n())
temp$data = left_join(temp$data,years_obs)

temp$data = temp$data %>% group_by(NAME) %>% arrange(NAME,YEAR) %>% 
  mutate_at(vars(matches('Tax|Debt|Pop$')),
            funs({if(sum(!is.na(.))==1){repz(.)} else{.}})) %>%
  mutate_at(vars(matches('Tax|Debt|Pop$')),
            funs(ifelse(nn<=2,.,
                        imputeTS::na.interpolation(.,option = 'linear'))))
temp$data$Log_RevDebtServiceOutstanding = log(temp$data$RevDebtServiceOutstanding + 1)
temp$data$Log_TaxDebtServiceOutstanding = log(temp$data$TaxDebtServiceOutstanding + 1)
temp$data$Log_TaxAVPriorYear = log(temp$data$TaxAVPriorYear + 1)
temp$data$Serv_Pop1000k = temp$data$Pop/1000
temp$data$LogServ_Pop = log(temp$data$Pop+1)

temp$data$ELECTED_BOARD <- ifelse(is.na(temp$data$ELECTED_BOARD)|temp$data$ELECTED_BOARD=='Appointed',0,1)








sub = temp %>% filter(is.na(deactivation_year)|deactivation_year>=2003)



plot_df = temp %>% select(creation_year,deactivation_year) %>% gather() %>%
  mutate(key = ifelse(key == 'creation_year','Year created','Year deactivated'))
library(ggthemes)
ggplot(plot_df,aes(x = value)) + geom_bar()  + facet_wrap(~key,nrow=2) + theme_tufte(ticks=F) +
  theme(axis.title.y=element_text(size=14),axis.title.x = element_blank(),legend.title=element_blank(),strip.text = element_text(size=14),
        axis.text = element_text(size = 14)) + ylab('# MUDs') +
  scale_x_continuous(breaks = seq(1971,2016,10))

#temp$NAME[temp$NAME == "KAUFMAN COUNTY MUD 11"&STATUS!='ACTIVE'] <- 'KINGSBOROUGH MUD 4'











temp$intercept_1971 = rep(1,nrow(temp))

y.surv <- inla.surv(time = temp$time,event = temp$event)
ninterval = 45
df_1971 = inla.coxph(y.surv ~ -1 + intercept_1971 + FORMED_BY_LEGIS + 
                       MULTI_COUNTY + 
                       f(inla.group(creation_year,n = 10),model='rw1') + 
                       f(COUNTY,model='iid'),
                     data = list(y.surv=y.surv, intercept_1971 = temp$intercept_1971, NAME = temp$NAME, COUNTY = temp$COUNTY,
                                 creation_year = temp$creation_year,
                                 MULTI_COUNTY = temp$MULTI_COUNTY, TAX_RATE = temp$TAX_RATE, ACRE_SIZE = temp$ACRE_SIZE,
                                 ELECTED_BOARD = temp$ELECTED_BOARD,
                                 SYSTEM_ID = temp$SYSTEM_ID,FORMED_BY_LEGIS = temp$FORMED_BY_LEGIS),
                     control.hazard=list(model="rw1", n.intervals=ninterval,scale.model = TRUE))

df_19712 = inla.coxph(y.surv ~ -1 + intercept_1971,
                      data = list(y.surv=y.surv, intercept_1971 = rep(1,1457)),
                     control.hazard=list(model="rw1", n.intervals=ninterval,scale.model = TRUE))



df_1971$data = df_1971$data %>% group_by(SYSTEM_ID) %>% arrange(SYSTEM_ID,baseline.hazard.idx) %>%
  mutate(YEAR = creation_year + floor((cumsum(intercept_1971)-1) * 45/ninterval))

df_1971$data = left_join(df_1971$data,districts_by_county)
df_1971$data$COUNTY_POP_CHANGE_P1 = 100 * (df_1971$data$POPULATION - df_1971$data$POPULATION_P1) / df_1971$data$POPULATION_P1
df_1971$data$PERC_IN_COUNTY_OUT_STATE = df_1971$data$PROP_IN_COUNTY_OUT_STATE*100
df_1971$data$ALL_ACTIVE_DISTRICTS = df_1971$data$ALL_ACTIVE_MUDS + df_1971$data$ALL_ACTIVE_OTHER

up_form_1971 = update.formula(df_1971$form, ~ . + log(ALL_ACTIVE_DISTRICTS) + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                log(PERC_IN_COUNTY_OUT_STATE))

mod0 = inla(df_1971$form,control.compute = list(waic=TRUE,dic=TRUE),
            family=df_1971$family,data=c(as.list(df_1971$data),df_1971$data.list),
            E = df_1971$E)
df_1971$formula
table(df_1971$E)




r_int = mod0$summary.random$COUNTY$mean
qqnorm(r_int)
qqline(r_int)
shapiro.test(r_int)




########### explore strata sensitivity ###############
strata_vars_0 = c("MULTI_COUNTY_gr","FORMED_BY_LEGIS_gr", "creation_year_gr")
temp$MULTI_COUNTY_gr = temp$MULTI_COUNTY + 1
temp$FORMED_BY_LEGIS_gr = temp$FORMED_BY_LEGIS + 1
temp$creation_year_gr <- as.numeric(cut(temp$creation_year, breaks=c(-Inf,quantile(temp$creation_year,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
temp$stratify_var = 1

long_hazard_set = lapply(seq_along(strata_vars_0),function(x) {
  print(strata_vars_0[x]) 
  ninterval = 45
  temp_form = as.formula(gsub('replicate = stratify_var',paste0('replicate = ',strata_vars_0[x]),deparse(up_form_temp_ph)))
  mod = inla(temp_form,control.compute = list(waic=TRUE,dic=TRUE),
             family=temp_ph$family,data=c(as.list(temp_ph$data),temp_ph$data.list),
             E = temp_ph$E)
  mod})

long_model_sensitivity_df = data.frame(Model = c('Restricted model',
                     'Stratified: Elected board',
                     'Stratified: Multi-county',
                     'Stratified: Formed by legislature'),
           WAIC = c(mod0$waic$waic,sapply(long_hazard_set,function(x) x$waic$waic)))








require(grid)
basehaz <- mod0$summary.random$baseline.hazard

ggplot() + 
  geom_ribbon(aes(x = ID,ymin = `0.025quant` ,ymax = `0.975quant`,fill = 'all_interval'),alpha = 0.1,data = basehaz) + 
  geom_path(aes(x = ID,y = mean,colour = 'hazard_nondorm',linetype = 'dotted'),data = basehaz) + 
  geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard_all',linetype = 'solid'),span = .25,data = basehaz) +
  scale_x_continuous(name = 'Age of MUD',expand = c(0,0)) + scale_y_continuous(name = 'Hazard rate') +
  theme_bw() +theme(legend.position = c(0.5,0.2),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),legend.title=element_text(size=14),legend.spacing.y = unit(-0.25, "cm"),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +   scale_linetype_identity() +
  scale_fill_manual(name ='', labels = '95% credible interval',values = c('grey60')) +
  scale_colour_manual(name ='Baseline hazard', labels = c('mean estimate','smoothed rate'),values = c('black','grey60','blue')) +
  guides(fill = guide_legend(order = 2,title.theme = element_blank()),colour = guide_legend(order = 1,override.aes = list(linetype = c('dotted','solid'))))


create_coef = mod0$summary.random$`inla.group(creation_year, n = 10)`

ggplot() + 
  geom_errorbar(aes(x = ID,ymin = `0.025quant` ,ymax = `0.975quant`,fill = 'all_interval'),alpha = 0.5,data = create_coef) +
  #geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard_all',linetype = 'solid'),span = .25,data = age_coef) +
  geom_path(aes(x = ID,y = mean,linetype = 'dotted'),colour = 'grey60',data = create_coef) + 
  geom_point(aes(x = ID,y = mean),size=1,colour = 'black',data = create_coef) + 
  scale_x_continuous(name = 'Year of MUD creation',expand = c(0,0)) + 
  scale_y_continuous(name = '95% credible interval') +
  theme_bw() +theme(legend.position = c(0.5,0.2),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),legend.title=element_text(size=14),legend.spacing.y = unit(-0.25, "cm"),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +   scale_linetype_identity() +
  scale_fill_manual(name ='', labels = '95% credible interval',values = c('grey60'))
scale_colour_manual(name = 'mean estimate',values='black')



########### explore interval sensitivity ###############
interval_seq = seq(5,135,10)
hazard_set = lapply(seq_along(interval_seq),function(x) {
  print(interval_seq[x]) 
  ninterval = interval_seq[x]
  temp_ph = inla.coxph(y.surv ~ -1 + intercept_1971 + FORMED_BY_LEGIS + 
                         MULTI_COUNTY + 
                         f(inla.group(creation_year,n = 10),model='rw1') + 
                         f(COUNTY,model='iid'),
                       data = list(y.surv=y.surv, intercept_1971 = temp$intercept_1971, NAME = temp$NAME, COUNTY = temp$COUNTY,creation_year = temp$creation_year,
                                   MULTI_COUNTY = temp$MULTI_COUNTY, TAX_RATE = temp$TAX_RATE, ACRE_SIZE = temp$ACRE_SIZE,
                                   ELECTED_BOARD = temp$ELECTED_BOARD,
                                   SYSTEM_ID = temp$SYSTEM_ID,FORMED_BY_LEGIS = temp$FORMED_BY_LEGIS),
                       control.hazard=list(model="rw1", n.intervals=ninterval,scale.model = TRUE))
  
  temp_ph$data = temp_ph$data %>% group_by(SYSTEM_ID) %>% arrange(SYSTEM_ID,baseline.hazard.idx) %>%
    mutate(YEAR = creation_year + floor((cumsum(intercept_1971)-1) * 45/ninterval))
  temp_ph$data = left_join(temp_ph$data,districts_by_county)
  
  temp_ph$data$COUNTY_POP_CHANGE_P1 = 100 * (temp_ph$data$POPULATION - temp_ph$data$POPULATION_P1) / temp_ph$data$POPULATION_P1
  temp_ph$data$PERC_IN_COUNTY_OUT_STATE = temp_ph$data$PROP_IN_COUNTY_OUT_STATE*100
  temp_ph$data$ALL_ACTIVE_DISTRICTS = temp_ph$data$ALL_ACTIVE_MUDS + temp_ph$data$ALL_ACTIVE_OTHER
  
  up_form_temp_ph = update.formula(temp_ph$form, ~ . + log(ALL_ACTIVE_DISTRICTS) + COUNTY_POP_CHANGE_P1 + log(POPULATION) +
                                     log(PERC_IN_COUNTY_OUT_STATE))
  mod = inla(up_form_temp_ph,control.compute = list(waic=TRUE,dic=TRUE),
             family=temp_ph$family,data=c(as.list(temp_ph$data),temp_ph$data.list),
             E = temp_ph$E)
  mod$summary.random$baseline.hazard
})


haz_df = do.call(rbind,lapply(seq_along(hazard_set),function(x) hazard_set[[x]] %>% mutate(interval = interval_seq[x])))

library(viridis)
ggplot(haz_df,aes(x = ID,y = mean,group = interval,color=interval)) + geom_path() + theme_bw() + 
  scale_color_viridis(option = 'C',breaks = c(5,35,65,95,125),name = '# intervals') +
  ggtitle('Estimated baseline hazard with varying interval length') + 
  scale_x_continuous(name = 'Age of MUD (years)') + scale_y_continuous(name = 'Baseline hazard') + 
  theme(axis.ticks=element_blank(), axis.text = element_text(size = 12),axis.title = element_text(size=12),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))

require(grid)
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








library(brinla)



#sub$final_date - sub$init
#sub$final = sub$final_date - sub$init
sub$intercept_2003 = rep(1,nrow(sub))

#y.surv.v2 <- inla.surv(time = sub$time_2003,event = sub$event)
#y.surv.v2 <- inla.surv(time = sub$time,event = sub$event)
y.surv.v2 = inla.surv(time = sub$time_2003+sub$age_at_2003, event = sub$event,truncation = sub$age_at_2003)
sub$time_2003+sub$age_at_2003

df_2003 = inla.coxph(y.surv.v2 ~ -1 + intercept_2003 + FORMED_BY_LEGIS + 
                       MULTI_COUNTY + 
                       f(inla.group(creation_year,n=10),model='rw1') + 
                       f(COUNTY,model='iid'),
                     data = list(y.surv=y.surv.v2, intercept_2003 = sub$intercept_2003, NAME = sub$NAME, COUNTY = sub$COUNTY,creation_year = sub$creation_year,
                                 MULTI_COUNTY = sub$MULTI_COUNTY, TAX_RATE = sub$TAX_RATE, ACRE_SIZE = sub$ACRE_SIZE,
                                 ELECTED_BOARD = sub$ELECTED_BOARD,init = sub$init,start_date = sub$start_date,
                                 SYSTEM_ID = sub$SYSTEM_ID,FORMED_BY_LEGIS = sub$FORMED_BY_LEGIS),
                     control.hazard=list(model="rw1", n.intervals=13,scale.model = TRUE))



### need to add:
#ln(percent of Active districts in county vs. All active districts in the state).
#% change county pop

up_form_2003 = update.formula(df_2003$form, ~ . + 
                                TAX_RATE + ELECTED_BOARD + log(ACRE_SIZE) + 
                                log(POPULATION) + log(PERC_IN_COUNTY_OUT_STATE) +
                                log(ALL_ACTIVE_DISTRICTS) + COUNTY_POP_CHANGE_P1 + 
                                unemp_rate_p1 + perc_change_unemp_rate_p1 +
                                perc_change_laborforce_p1 +
                                Log_RevDebtServiceOutstanding +
                                Log_TaxDebtServiceOutstanding +
                                Log_TaxAVPriorYear +
                                LogServ_Pop)

up_form_1971_2003 = update.formula(up_form_2003, ~ . -TAX_RATE - ELECTED_BOARD -log(ACRE_SIZE) -
                                     unemp_rate_p1 - perc_change_unemp_rate_p1 - 
                                     perc_change_laborforce_p1 - Log_RevDebtServiceOutstanding -
                                     Log_TaxDebtServiceOutstanding - Log_TaxAVPriorYear - LogServ_Pop)

mod1 = inla(up_form_2003,#control.compute = list(waic=TRUE,dic=TRUE),
            family=df_2003$family,data=c(as.list(df_2003$data),df_2003$data.list),
            E = df_2003$E,verbose=T)

########### explore strata sensitivity ###############
strata_vars_1 = c("MULTI_COUNTY_gr","FORMED_BY_LEGIS_gr", "creation_year_gr",
                  "TAX_RATE_gr","ACRE_SIZE_gr","ELECTED_BOARD_gr")

temp_2003$data$Log_RevDebtServiceOutstanding_gr <- (temp_2003$data$Log_RevDebtServiceOutstanding > 0) + 0
temp_2003$data$Log_TaxDebtServiceOutstanding_gr <-  (temp_2003$data$Log_TaxDebtServiceOutstanding > 0) + 0
temp_2003$Log_TaxAVPriorYear_gr <- as.numeric(cut(temp_2003$data$Log_TaxAVPriorYear, breaks=c(-Inf,quantile(temp_2003$data$Log_TaxAVPriorYear,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
temp_2003$data$LogServ_Pop_gr <- as.numeric(cut(temp_2003$data$LogServ_Pop, breaks=c(-Inf,quantile(temp_2003$data$LogServ_Pop,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))


temp$MULTI_COUNTY_gr = temp$MULTI_COUNTY + 1
temp$FORMED_BY_LEGIS_gr = temp$FORMED_BY_LEGIS + 1
temp$creation_year_gr <- as.numeric(cut(temp$creation_year, breaks=c(-Inf,quantile(temp$creation_year,probs = c(0.25,0.5,0.75)),Inf),labels = c("1","2","3","4")))
temp$stratify_var = 1

long_hazard_set = lapply(seq_along(strata_vars_0),function(x) {
  print(strata_vars_0[x]) 
  ninterval = 45
  temp_form = as.formula(gsub('replicate = stratify_var',paste0('replicate = ',strata_vars_0[x]),deparse(up_form_temp_ph)))
  mod = inla(temp_form,control.compute = list(waic=TRUE,dic=TRUE),
             family=temp_ph$family,data=c(as.list(temp_ph$data),temp_ph$data.list),
             E = temp_ph$E)
  mod})

long_model_sensitivity_df = data.frame(Model = c('Restricted model',
                                                 'Stratified: Elected board',
                                                 'Stratified: Multi-county',
                                                 'Stratified: Formed by legislature'),
                                       WAIC = c(mod0$waic$waic,sapply(long_hazard_set,function(x) x$waic$waic)))





mod1B = inla(up_form_1971_2003,#control.compute = list(waic=TRUE,dic=TRUE),
             family=df_2003$family,data=c(as.list(df_2003$data),df_2003$data.list),
             E = df_2003$E,verbose=T)

coef1 = mod1$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept_2003') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))
coef1B = mod1B$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept_2003') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))
coef0 = mod0$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.)) %>% filter(Coef!='intercept_2003') %>% 
  mutate(Sig = ifelse(`0.025quant`<0&`0.975quant`>0,0,1))
coef0$Coef = as.factor(coef0$Coef)
coef1$Coef = as.factor(coef1$Coef)
coef1B$Coef = as.factor(coef1B$Coef)
library(forcats)

cp = full_join(coef0 %>% mutate(model = '1971-2016'),coef1 %>% mutate(model = '2003-2016 full'))
cp = full_join(cp,coef1B %>% mutate(model = '2003-2016'))

cp$Coef = fct_relevel(cp$Coef,  "FORMED_BY_LEGIS","MULTI_COUNTY","ELECTED_BOARD", 
                      "TAX_RATE"  ,"log(ACRE_SIZE)" , "LogServ_Pop" ,
                      "Log_RevDebtServiceOutstanding", "Log_TaxDebtServiceOutstanding" ,"Log_TaxAVPriorYear" ,
                      'log(POPULATION)','COUNTY_POP_CHANGE_P1',
                      "log(ALL_ACTIVE_DISTRICTS)" ,"log(PERC_IN_COUNTY_OUT_STATE)",
                      'unemp_rate_p1','perc_change_unemp_rate_p1','perc_change_laborforce_p1')
cp$Coef = fct_recode(cp$Coef, `Legislative creation` = "FORMED_BY_LEGIS",`Multi-county` = "MULTI_COUNTY",
                     `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE"  ,
                     `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                     `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                     `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "log(ACRE_SIZE)"  ,       
                     `ln(Service pop.)` = "LogServ_Pop" ,
                     `Unemployment % (t-1)` = 'unemp_rate',
                     `ln(county districts)`= "log(ALL_ACTIVE_DISTRICTS)",
                     `ln(county districts/state)`  = 'log(PERC_IN_COUNTY_OUT_STATE)',
                     `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                     `ln(county population)`=  "log(POPULATION)",
                     `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                     `% change laborforce (y-1)` = "perc_change_laborforce_p1")

cp$Coef = fct_rev(cp$Coef)

cp$model = fct_relevel(as.factor(cp$model),"1971-2016","2003-2016","2003-2016 full")

ggplot(cp %>% filter(!grepl('intercept',Coef)),aes(y = Coef,yend=Coef,x=`0.025quant`,xend=`0.975quant`,group = model)) + 
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

t0a = mod0$summary.fixed %>% mutate(ID = rownames(.)) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '1971-2016')

t0b = mod0$summary.hyperpar %>% mutate(ID = rownames(.)) %>%
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3)) %>%
  mutate(model = '1971-2016')

t1a = mod1$summary.fixed %>% 
  mutate(ID = rownames(.)) %>% select(ID,mean,`0.025quant`,`0.975quant`) %>%
  mutate(`0.025quant` = round(`0.025quant`,3),
         mean = round(mean,3),`0.975quant`=round(`0.975quant`,3))  %>%
  mutate(model = '2003-2016')

t1b = mod1$summary.hyperpar %>% mutate(ID = rownames(.)) %>%select(ID,mean,`0.025quant`,`0.975quant`) %>%
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
ct$Coef = fct_relevel(ct$Coef, 'intercept_1971','intercept_2003' ,"FORMED_BY_LEGIS","MULTI_COUNTY","ELECTED_BOARD", 
                      "TAX_RATE"  ,"log(ACRE_SIZE)" , "LogServ_Pop" ,
                      "Log_RevDebtServiceOutstanding", "Log_TaxDebtServiceOutstanding" ,"Log_TaxAVPriorYear" ,
                      'log(POPULATION)','COUNTY_POP_CHANGE_P1',
                      "log(ALL_ACTIVE_DISTRICTS)" ,"log(PERC_IN_COUNTY_OUT_STATE)",
                      'unemp_rate_p1','perc_change_unemp_rate_p1','perc_change_laborforce_p1')

ct$Coef = fct_recode(ct$Coef, `(intercept)` = 'intercept_1971',`(intercept)` = 'intercept_2003',
                     `Legislative creation` = "FORMED_BY_LEGIS",
                     `Multi-county` = "MULTI_COUNTY",
                     `Elected board` = "ELECTED_BOARD", `Tax rate (% per $1k)`=  "TAX_RATE" ,
                     `ln(Revenue debt service)` = "Log_RevDebtServiceOutstanding", 
                     `ln(Tax debt service)` = "Log_TaxDebtServiceOutstanding" ,
                     `ln(Tax assessed value)` = "Log_TaxAVPriorYear" ,   `ln(Acreage)` = "log(ACRE_SIZE)"  ,       
                     `ln(Service pop.)` = "LogServ_Pop" ,
                     `Unemployment % (t-1)` = 'unemp_rate_p1',
                     `ln(county districts)`= "log(ALL_ACTIVE_DISTRICTS)",
                     `ln(county districts/state)`  = 'log(PERC_IN_COUNTY_OUT_STATE)',
                     `% change population (y-1)` = 'COUNTY_POP_CHANGE_P1',
                     `ln(county population)`=  "log(POPULATION)",
                     `% change unemp. (y-1)` = "perc_change_unemp_rate_p1" ,
                     `% change laborforce (y-1)` = "perc_change_laborforce_p1")


ctt = ct %>% select(-mean) %>% 
  mutate(CI = paste(`0.025quant`,`0.975quant`,sep=', '))  %>% 
  select(-`0.025quant`,-`0.975quant`,-P,-ID) %>% spread(model,CI) 

ctt$`1971-2016`[is.na(ctt$`1971-2016`)] <- '---'
ctt$`2003-2016`[is.na(ctt$`2003-2016`)] <- '---'

library(knitr)
kable(ctt,format = 'html')


library(stargazer)

library(texreg)
library(knitr)


#random_type_vals <- mod$summary.random$DISTRICT_TYPE

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
map1 <- ggplot() + ggtitle('Standardized random intercept mean estimate') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=scaled_mean),colour = 'grey70') + 
  scale_fill_viridis(option = 'D',name = 'Standardized \nestimate') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))

map2 <- ggplot() + ggtitle('Random intercept standard deviation') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=sd),colour = 'grey70') + 
  scale_fill_viridis(option = 'D',name = 'sd') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),legend.title.align = .5,
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))


library(gridExtra)
grid.arrange(map1,map2,ncol=2)





