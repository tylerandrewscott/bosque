

packs = c('tidyverse','lme4','data.table')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)
sapply(packs,require,character.only = T)

if(!require(INLA)){
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE);
  require(INLA)}

temp = readRDS('org_performance/input/df_for_model_V2.RDS')
temp$health_days <- floor((temp$health_prop * 365))
temp$management_days <- floor(temp$nonhealth_prop * 365)

temp$health_days_L1 <- floor((temp$health_prop_L1 * 365))
temp$management_days_L1 <- floor(temp$nonhealth_prop_L1 * 365)
temp$health_days_L1[is.na(temp$health_days_L1)]<-0
temp$management_days_L1[is.na(temp$management_days_L1)]<-0
temp$health_violations_L1[is.na(temp$health_violations_L1)]<-0
temp$management_violations_L1[is.na(temp$management_violations_L1)]<-0


temp$DROP_FROM_FULL_MODEL <- temp$DROP_FROM_FULL_MODEL
temp$DROP_FROM_FULL_MODEL <- temp$DROP_FROM_FULL_MODEL|temp$MASTER_ID %in% temp[,.N,by=.(MASTER_ID)][N<4,]$MASTER_ID
temp$DROP_YEAR_FROM_MODEL <- temp$DROP_FROM_FULL_MODEL | (temp$YEAR<2009|temp$YEAR>2021)

unique(temp$MASTER_ID[!temp$DROP_FROM_FULL_MODEL])
table(temp$YEAR[!temp$DROP_FROM_FULL_MODEL])


#temp$Prop_Nonwholesale[is.na(temp$Prop_Nonwholesale)]<-1
fill_forward_only <- c('REV_PER_CONNECTION_L1','Fund_Balance_L1','TotalDebtServiceOutstanding_L1_Per_Connection')
fill_forward_and_back <- c('SERVICE_CONNECTIONS_COUNT','Median_Home_Value','Prop_Bach','Median_Year_Structure_Built','CBSA_HHI')
temp<-temp[order(MASTER_ID,YEAR),]

temp[,(c(fill_forward_only,fill_forward_and_back)):=lapply(.SD,zoo::na.locf,na.rm = F),by=.(District_ID),.SDcols = c(fill_forward_only,fill_forward_and_back)]
temp[,(fill_forward_and_back):=lapply(.SD,zoo::na.locf,na.rm = F,fromLast = T),by=.(District_ID),.SDcols = fill_forward_and_back]
replace_homes<-temp[,median(Median_Home_Value,na.rm = T),by=.(YEAR)]
temp$Median_Home_Value[is.na(temp$Median_Home_Value)]<-replace_homes$V1[match(temp$YEAR[is.na(temp$Median_Home_Value)],replace_homes$YEAR)]

replace_bach<-temp[,median(Prop_Bach,na.rm = T),by=.(YEAR)]
temp$Prop_Bach[is.na(temp$Prop_Bach)]<-replace_bach$V1[match(temp$YEAR[is.na(temp$Prop_Bach)],replace_bach$YEAR)]

replace_structure<-temp[,median(Median_Year_Structure_Built,na.rm = T),by=.(YEAR)]
temp$Median_Year_Structure_Built[temp$Median_Year_Structure_Built<1900]<-NA
temp$Median_Year_Structure_Built[is.na(temp$Median_Year_Structure_Built)]<-replace_structure$V1[match(temp$YEAR[is.na(temp$Median_Year_Structure_Built)],replace_structure$YEAR)]
temp$Median_Year_Structure_Built[temp$Median_Year_Structure_Built<100]<NA
temp$Median_Structure_Age <- year(Sys.Date()) - temp$Median_Year_Structure_Built
temp$ln_SERVICE_CONNECTIONS_COUNT <- log(temp$SERVICE_CONNECTIONS_COUNT)
temp$ln_REV_PER_CONNECTION_L1 <- log(temp$REV_PER_CONNECTION_L1+1)
temp$ln_FUND_BALANCE_L1 <- log(temp$Fund_Balance_L1+1)
temp$ln_DEBT_OUTSTANDING_PER_CONNECTION_L1 <- log(temp$TotalDebtServiceOutstanding_L1_Per_Connection+1)
temp$ln_MEDIAN_HOME_VALUE <- log(temp$Median_Home_Value)
temp$District_Age <- temp$YEAR-decimal_date(mdy(temp$District_Created))
temp$management_violator <- (temp$management_violations>0) + 0
temp$health_violator <- (temp$health_violations>0) + 0
temp$management_violator_L1 <- (temp$management_violations_L1>0) + 0
temp$health_violator_L1 <- (temp$health_violations_L1>0) + 0
#betaConvert <- function(x){{x * (length(x)-1) + mean(x)}/length(x)}
#temp[,(paste0(repls,'_beta')):=lapply(.SD,betaConvert),.SDcols = repls]

vars_to_std= c("ln_REV_PER_CONNECTION_L1","Prop_Bach",
               "ln_FUND_BALANCE_L1","health_violations_L1", "management_violations_L1",'NUM_PWS_CBSA',
               "ln_DEBT_OUTSTANDING_PER_CONNECTION_L1",  "Severe_Weather_Events" ,"DSCI_1yr_Weekly_Avg" ,
               "ln_SERVICE_CONNECTIONS_COUNT" ,"Median_Structure_Age",
               "ln_MEDIAN_HOME_VALUE", "CBSA_HHI")
std_var_names = paste('std',vars_to_std,sep='_')
temp[, (std_var_names) := lapply(.SD,scale), .SDcols=vars_to_std]

fact_vars <- c('Groundwater','Wholesaler')
bin_var_names <- paste0('bin_',fact_vars)
temp[, (bin_var_names) := lapply(.SD,function(x) x), .SDcols=fact_vars]


replace_summary_df <- data.frame(old = c("health_violator","management_violator","health_violator_L1","management_violator_L1","ln_REV_PER_CONNECTION_L1","ln_FUND_BALANCE_L1","ln_DEBT_OUTSTANDING_PER_CONNECTION_L1",
                                         "Prop_Bach","Median_Structure_Age","ln_MEDIAN_HOME_VALUE",
                                         "NUM_PWS_CBSA","CBSA_HHI","ln_SERVICE_CONNECTIONS_COUNT","bin_Groundwater","bin_Wholesaler",
                                         "Severe_Weather_Events","DSCI_1yr_Weekly_Avg"),
                                 new = c("health violator, t","management violator, t",
                                         'health violator, t-1',
                                         'management violator, t-1',
                                         'ln(revenue/connection),t-1','ln(fund balance),t-1','ln(debt/connection),t-1',
                                         "% bachelor's degree","median structure age",'ln(median home value)',
                                         '# PWS in CBSA','HHI for CBSA','ln(service connections)','Primary groundwater','Wholesaler',
                                         '# weather events, t','Weekly drought avg., t'))

sum_order<-c(
  "health violator, t",
  "management violator, t",
  "health violator, t-1",
  "management violator, t-1",
  "# PWS in CBSA" ,
  "HHI for CBSA" ,
  "# weather events, t" ,
  "Weekly drought avg., t",
  "ln(median home value)",
  "% bachelor's degree",
  "ln(service connections)" ,
  "Wholesaler",
  "median structure age" ,
  "ln(debt/connection),t-1",
  "ln(revenue/connection),t-1" ,
  "ln(fund balance),t-1",
  "Primary groundwater" )

tab_temp <- temp[!temp$DROP_FROM_FULL_MODEL,][,c(replace_summary_df$old,'District_ID','YEAR'),with = F]
tab_temp$bin_Groundwater<-as.factor(ifelse(tab_temp$bin_Groundwater==1,'yes','no'))
tab_temp$bin_Wholesaler<-as.factor(ifelse(tab_temp$bin_Wholesaler==1,'yes','no'))

library(gt)
library(labelled)
names(tab_temp) <- replace_summary_df$new[match(names(tab_temp),replace_summary_df$old)]
facts <- c('Wholesaler','Primary groundwater')
tab_temp <- cbind(tab_temp %>% select(-contains('ln(')),
                  tab_temp %>% select(contains('ln(')) %>% mutate_all(exp))
tab_temp2 <- tab_temp[,sum_order,with = F]

library(vtable)
names(tab_temp2)<-str_remove_all(names(tab_temp2),'ln\\(|\\)')
sumtable(tab_temp2 ,file = 'org_performance/output/table1.html',
         summ = c('mean(x)','median(x)','sd(x)','min(x)','max(x)'))

# 
# #tab_temp$home_value_quartile <- ntile(tab_temp$`ln(median home value)`,4)
# vmins2 <- readRDS('org_performance/output/individual_violation_durations.RDS')
# id_crosswalk <- readRDS('org_performance/output/id_crosswalk.RDS')
# vmins2$District_ID <- id_crosswalk$District_ID[match(vmins2$PWSID,id_crosswalk$PWS_ID)]
# vmins2 <- vmins2[!is.na(District_ID),]
# vmins2 <- vmins2[YEAR %in% 2007:2022.]
# vmins2$median_home_value <- tab_temp$`ln(median home value)`[match(paste(vmins2$District_ID,vmins2$YEAR),paste(tab_temp$District_ID,tab_temp$YEAR))]
# tab_temp$home_quartile <- ntile(tab_temp$`ln(median home value)`,2)
# vls <- quantile(tab_temp$`ln(median home value)`,c(0,0.5,1))
# vmins2$home_quartile <- cut(vmins2$median_home_value,breaks = vls,labels = c(1:2),include.lowest = F)
# 
# vvals <- vmins2[!is.na(home_quartile),list(.N,mean(end-start)),by = .(IS_HEALTH_BASED_IND,home_quartile)]
# setnames(vvals,c('N','V2'),c('# unique violations','Avg. duration of non-compliance'))
# vvals2 <- dcast(vvals,home_quartile ~ IS_HEALTH_BASED_IND,value.var = c('# unique violations','Avg. duration of non-compliance'))
# num_by_quart <- tab_temp[,.N,by=.(home_quartile)][order(home_quartile),]
# vvals2$num_system_year_obs <- num_by_quart$N[match(vvals2$home_quartile,num_by_quart$home_quartile)]

#panel_structure <- "as.factor(District_ID) + as.factor(YEAR) + as.factor(CBSA_CFIPS)"
#panel_structure_alt <- "f(District_ID,model = 'iid') + f(YEAR,model = 'iid') + f(CBSA_FIPS,model = 'iid')"
mod_temp <- temp[,grep('^std|^bin|violator|^YEAR$|^MASTER_ID$|DROP',names(temp),value = T),with = F]
#mod_temp$MASTER_ID<-paste0('ID',mod_temp$MASTER_ID)
#mod_temp$YEAR <- paste0('Y',mod_temp$YEAR)
mod_temp$YEAR_ID <- as.numeric(as.factor(mod_temp$YEAR))
mod_temp$MASTER_GROUP_NUM <- as.numeric(as.factor(mod_temp$MASTER_ID))

mod_temp$violator <- (mod_temp$health_violator==1|mod_temp$management_violator==1)+0
mod_temp$violator_L1 <- (mod_temp$health_violator_L1==1|mod_temp$management_violator_L1==1)+0

u = 1
length(unique(mod_temp$MASTER_ID))
dinfo_dt = fread('input/twdd_records/district_list_2019-03-08.csv',stringsAsFactors = F,na.strings = "")
table(dinfo_dt$Type[dinfo_dt$District_ID %in% mod_temp$MASTER_ID & !duplicated(dinfo_dt$District_ID)])
length(dinfo_dt$Type[dinfo_dt$District_ID %in% mod_temp$MASTER_ID])


f1 <- management_violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid')
f2 <- health_violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid')
f3 <- management_violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid') +
  std_ln_REV_PER_CONNECTION_L1 + 
  std_ln_FUND_BALANCE_L1 + 
  std_ln_DEBT_OUTSTANDING_PER_CONNECTION_L1 +
  std_Prop_Bach + std_Median_Structure_Age + std_ln_MEDIAN_HOME_VALUE + 
  std_NUM_PWS_CBSA + std_CBSA_HHI + 
  std_ln_SERVICE_CONNECTIONS_COUNT + bin_Groundwater + bin_Wholesaler + 
  std_Severe_Weather_Events +  std_DSCI_1yr_Weekly_Avg + 
  management_violator_L1

f4 <- health_violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid') +
  std_ln_REV_PER_CONNECTION_L1 + 
  std_ln_FUND_BALANCE_L1 + 
  std_ln_DEBT_OUTSTANDING_PER_CONNECTION_L1 +
  std_Prop_Bach + std_Median_Structure_Age + std_ln_MEDIAN_HOME_VALUE + 
  std_NUM_PWS_CBSA + std_CBSA_HHI + 
  std_ln_SERVICE_CONNECTIONS_COUNT + bin_Groundwater + bin_Wholesaler + 
  std_Severe_Weather_Events +  std_DSCI_1yr_Weekly_Avg + 
  health_violator_L1

fB <- violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid') +
  std_ln_REV_PER_CONNECTION_L1 + 
  std_ln_FUND_BALANCE_L1 + 
  std_ln_DEBT_OUTSTANDING_PER_CONNECTION_L1 +
  std_Prop_Bach + std_Median_Structure_Age + std_ln_MEDIAN_HOME_VALUE + 
  std_NUM_PWS_CBSA + std_CBSA_HHI + 
  std_ln_SERVICE_CONNECTIONS_COUNT + bin_Groundwater + bin_Wholesaler + 
  std_Severe_Weather_Events +  std_DSCI_1yr_Weekly_Avg + 
  violator_L1
  
m1_pre <- inla(f1,family = "binomial", Ntrials = 1,data = mod_temp,verbose = T)
m2_pre <- inla(f2,family = "binomial", Ntrials = 1,data = mod_temp,verbose = T)
mB_pre <- inla(violator ~ f(YEAR_ID,model = 'iid') + f(MASTER_GROUP_NUM,model = 'iid'),
               family = "binomial", Ntrials = 1,data = mod_temp,verbose = T)

dim(mod_temp[!(mod_temp$DROP_FROM_FULL_MODEL|mod_temp$DROP_FROM_PANEL|mod_temp$DROP_YEAR_FROM_MODEL),])

m1 <- inla(f1,family = "binomial", control.compute = list(waic = T,dic = T,mlik = T),
           control.mode = list(theta = m1_pre$mode$theta) ,
           Ntrials = 1,data = mod_temp[!mod_temp$DROP_FROM_FULL_MODEL,],verbose = T)
m2 <- inla(f2,family = "binomial", control.compute = list(waic = T,dic = T,mlik = T),
           control.mode = list(theta = m2_pre$mode$theta) ,
           Ntrials = 1,data = mod_temp[!mod_temp$DROP_FROM_FULL_MODEL,],verbose = T)
m3 <- inla(f3,family = "binomial", control.compute = list(waic = T,dic = T,mlik = T),
           control.mode = list(theta = m1_pre$mode$theta) ,
           Ntrials = 1,data = mod_temp[!mod_temp$DROP_FROM_FULL_MODEL,],verbose = T)
m4 <- inla(f4,family = "binomial",control.compute = list(waic = T,dic = T,mlik = T),
           control.mode = list(theta = m2_pre$mode$theta) ,
           Ntrials = 1,data = mod_temp[!mod_temp$DROP_FROM_FULL_MODEL,],verbose = T)
mB <- inla(fB,family = "binomial",control.compute = list(waic = T,dic = T,mlik = T),
           control.mode = list(theta = mB_pre$mode$theta) ,
           Ntrials = 1,data = mod_temp[!mod_temp$DROP_FROM_FULL_MODEL,],verbose = T)

coefsB <- mB$summary.fixed[,c(1,3,5)] %>% 
  mutate(Coef = rownames(.),Noncompliance = 'All') %>% 
  filter(!grepl('factor',Coef))

coefs<-rbind(
  m3$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.),Noncompliance = 'Management') %>% filter(!grepl('factor',Coef)),
  m4$summary.fixed[,c(1,3,5)] %>% mutate(Coef = rownames(.),Noncompliance = 'Health') %>% filter(!grepl('factor',Coef))
)
library(forcats)
replace_df <- data.frame(old = c("std_ln_REV_PER_CONNECTION_L1","std_ln_FUND_BALANCE_L1","std_ln_DEBT_OUTSTANDING_PER_CONNECTION_L1",
                                 "std_Prop_Bach","std_Median_Structure_Age","std_ln_MEDIAN_HOME_VALUE",
                                 "std_NUM_PWS_CBSA","std_CBSA_HHI","std_ln_SERVICE_CONNECTIONS_COUNT","bin_Groundwater","bin_Wholesaler",
                                 "std_Severe_Weather_Events","std_DSCI_1yr_Weekly_Avg","health_violator_L1","management_violator_L1",
                                 'violator_L1'),
                         new = c('ln(revenue/connection),t-1','ln(fund balance),t-1','ln(debt/connection),t-1',
                                 "% bachelor's degree","median structure age",'ln(median home value)',
                                 '# PWS in CBSA','HHI for CBSA','ln(service connections)','Primary groundwater','Wholesaler',
                                 '# weather events, t','Weekly drought avg., t','health violator, t-1',
                                 'management violator, t-1','violator, t-1'))

coefs$Coef[coefs$Coef %in% replace_df$old]<-replace_df$new[match(coefs$Coef[coefs$Coef %in% replace_df$old],replace_df$old)]

coefsB$Coef[coefsB$Coef %in% replace_df$old]<-replace_df$new[match(coefsB$Coef[coefsB$Coef %in% replace_df$old],replace_df$old)]


library(htmlTable)
rownames(coefs)<-1:nrow(coefs)
coefs$mean<-round(coefs$mean,3)
coefs$`0.025quant`<-round(coefs$`0.025quant`,3)
coefs$`0.975quant`<-round(coefs$`0.975quant`,3)
coefs$total_coef <- paste0(coefs$mean,' (',coefs$`0.025quant`,",",coefs$`0.975quant`,")")

coefs$total_coef<-paste0(coefs$total_coef,ifelse(coefs$`0.025quant`<0&coefs$`0.975quant`>0,"","*"))

coefs<-data.table(coefs)

tab_order<-c("(Intercept)" ,
             "health violator, t-1",
             "management violator, t-1",
             "# PWS in CBSA" ,
             "HHI for CBSA" ,
             "# weather events, t" ,
             "Weekly drought avg., t",
             "ln(median home value)",
             "% bachelor's degree",
             "ln(service connections)" ,
             'Wholesaler','Primary groundwater',
             "median structure age" ,
             "ln(debt/connection),t-1",
             "ln(revenue/connection),t-1" ,
             "ln(fund balance),t-1")
big_tab <- dcast(coefs,Coef~Noncompliance,value.var = 'total_coef')
big_tab <- big_tab[match(tab_order,big_tab$Coef),]

big_tab <- rbind(big_tab,
data.table(cbind(c('LL','DIC','WAIC'),
      do.call(rbind,list(round(rbind(cbind(m3$mlik[1],m4$mlik[2]),
                                     cbind(m3$dic$dic,m4$dic$dic),
                                     cbind(m3$waic$waic,m4$waic$waic)),2))))),
use.names = F)


htmlTable(big_tab,rnames = F,file = 'org_performance/output/table2.html',
          n.cgroup = c(1,2),
          cgroup = c("",'Violation occurence'),
          #  header = c("","health regulations","management regulations"),
          n.rgroup = c(1,2,2,4,7,3),
          tfoot = "*95% credible interval does not include 0
          ^all continuous predictors are mean-centered and scaled
          ^^LL = log-likelihood; DIC = deviance information criterion; WAIC = Watanabe–Akaike information criterion",
          rgroup = c('','Prior performance',
                     'Complexity: competition and density',
                     'Control variables: context',
                     'Control variables: water district',
                     'Model fit statistics')
)


library(htmlTable)
rownames(coefsB)<-1:nrow(coefsB)
coefsB$mean<-round(coefsB$mean,3)
coefsB$`0.025quant`<-round(coefsB$`0.025quant`,3)
coefsB$`0.975quant`<-round(coefsB$`0.975quant`,3)
coefsB$total_coef <- paste0(coefsB$mean,' (',coefsB$`0.025quant`,",",coefsB$`0.975quant`,")")

coefsB$total_coef<-paste0(coefsB$total_coef,ifelse(coefsB$`0.025quant`<0&coefsB$`0.975quant`>0,"","*"))

coefsB<-data.table(coefsB)
tab_order<-c("(Intercept)" ,
             "violator, t-1",
             "# PWS in CBSA" ,
             "HHI for CBSA" ,
             "# weather events, t" ,
             "Weekly drought avg., t",
             "ln(median home value)",
             "% bachelor's degree",
             "ln(service connections)" ,
             'Wholesaler','Primary groundwater',
             "median structure age" ,
             "ln(debt/connection),t-1",
             "ln(revenue/connection),t-1" ,
             "ln(fund balance),t-1")
big_tabB <- dcast(coefsB,Coef~Noncompliance,value.var = 'total_coef')
big_tabB <- big_tabB[match(tab_order,big_tabB$Coef),]

summary(mB)

big_tabB <- rbind(big_tabB,
                 data.table(cbind(c('LL','DIC','WAIC'),
                                  do.call(rbind,list(round(rbind(cbind(mB$mlik[1]),
                                                                 cbind(mB$dic$dic),
                                                                 cbind(mB$waic$waic)),2))))),
                 use.names = F)


htmlTable(big_tabB,rnames = F,file = 'org_performance/output/table_allviols.html',
          n.cgroup = c(1,1),
          cgroup = c("",'Violation occurence'),
          #  header = c("","health regulations","management regulations"),
          n.rgroup = c(1,1,2,4,7,3),
          rgroup = c('','Prior performance',
                     'Complexity: competition and density',
                     'Control variables: context',
                     'Control variables: water district',
                     'Model fit statistics'),
          tfoot = "*95% credible interval does not include 0
          ^all continuous predictors are mean-centered and scaled
          ^^LL = log-likelihood; DIC = deviance information criterion; WAIC = Watanabe–Akaike information criterion",
        
)



coefs2 <- coefs
coefs2$Coef <- fct_relevel(coefs2$Coef,tab_order)
coefs2$Coef <- fct_rev(coefs2$Coef)
coefs2$SIG <- ifelse(coefs2$`0.025quant`<0&coefs2$`0.975quant`>0,0,1)
library(forcats)

ggplot(coefs2[!grepl("Intercept",Coef),]) + 
  geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`,y = Coef,col=Noncompliance)) + 
  geom_point(aes(x = mean,y = Coef,fill = as.factor(SIG),col = Noncompliance,alpha = SIG)) + 
   theme_bw() + 
  scale_fill_manual(values = c('white','black'))+
  scale_colour_colorblind(name = 'Violation type') + 
  theme(legend.position = c(0.8,0.2))



