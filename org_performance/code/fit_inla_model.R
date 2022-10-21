
packs = c('tidyverse','lme4','data.table')
need = packs[!packs %in% installed.packages()[,'Package']]
sapply(need,install.packages)
sapply(packs,require,character.only = T)

if(!require(INLA)){
  install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE);
require(INLA)}

temp = readRDS('org_performance/scratch/df_for_model.RDS')

pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))


form_string <- paste(c(grep('std_|bin_',names(modeldata_dt),value=T),
                       # "Health_L1", "NonHealth_L1", 
                       "Primary_Source",
                       "f(District_ID,model = 'iid',hyper=pcprior)",
                       "f(CBSA_FIPS,model = 'iid',hyper=pcprior)"),collapse='+')

mod0 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'nbinomial',
            data=modeldata_dt[modeldata_dt$PERIOD_ID%in%2:3,],quantiles = c(0.01,0.025,0.05,0.5,0.95,0.975,0.99),
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

summary(mod0)
sumlist = do.call(rbind,apply(modeldata_dt[PERIOD_ID!=1,vars_to_std,with=F],2,summary),use.names = T,fill =T)

data_summary = apply(modeldata_dt[PERIOD_ID!=1,vars_to_std,with=F],2,summary)


sum_table = rbindlist(lapply(seq_along(data_summary),function(x) data.table(COEF = names(data_summary)[x],data.table(rbind(data_summary[[x]])))),use.names = T,fill = T)

htmlTable::htmlTable(round(sumlist[,c(1,3,4,6)],3))

dvs = c('Health','Health+NonHealth','NonHealth')


mod_list = lapply(dvs,function(d){
  inla(as.formula(paste(d,form_string,sep='~')),family = 'nbinomial',
       data=modeldata_dt[modeldata_dt$PERIOD_ID%in%2:3,],quantiles = c(0.025,0.5,0.975),
       control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))
})


summary(mod_list[[2]])

tt = rbindlist(mapply(function(x,y) data.table(x$summary.fixed,Coef = rownames(x$summary.fixed),DV = y),x = mod_list,y = dvs,SIMPLIFY = F))

tt$Sig <- !(tt$`0.025quant`<0&tt$`0.975quant`>0)+0
tt$Coef <- as.factor(tt$Coef)
library(forcats)

tt$Coef <- fct_relevel(tt$Coef,
                       "(Intercept)"    ,
                       "std_Health_L1" , "std_NonHealth_L1"  ,
                       "std_NUM_PWS_IN_CBSA" , "std_CBSA_HHI_Index" ,
                       "std_Severe_Weather_Events" , "std_DSCI_3yr_Weekly_Avg"    ,
                       "std_Median_Home_Value" ,   "std_Prop_Bach" ,
                       "std_Total_Service_Connections"  ,    "std_Prop_Nonwholesale"    ,
                       "std_Median_Year_Structure_Built", "std_Urban_Prop" ,"std_Debt_Issued_In_Period_L1"   ,
                       "std_TotalDebtServiceOutstanding_L1_Per_Connection" ,  "std_REV_PER_CONNECTION_L1"  ,
                       "std_Fund_Balance_L1" ,"Primary_SourceSW", "Primary_SourceSWP"  , "Primary_SourceGWP"  )  


tt$Coef <- fct_recode(tt$Coef,'Health violations in prior 3 years'="std_Health_L1" ,
                      'Management violations in prior 3 years'= "std_NonHealth_L1"   ,
                      '# water systems in CBSA'="std_NUM_PWS_IN_CBSA"  ,
                      "% college degree" = "std_Prop_Bach",
                      'CBSA HHI (market concentration)'="std_CBSA_HHI_Index",
                      'Severe weather events'="std_Severe_Weather_Events" ,
                      'Drought index (weekly average)'="std_DSCI_3yr_Weekly_Avg"  ,
                      'Median home value'="std_Median_Home_Value",
                      'Service connections'="std_Total_Service_Connections" ,
                      '% retail connections'="std_Prop_Nonwholesale",
                      'Median structure build year'="std_Median_Year_Structure_Built",
                      'Debt issued during the last 3 years'= "std_Debt_Issued_In_Period_L1"   ,
                      'Debt per connection'="std_TotalDebtServiceOutstanding_L1_Per_Connection" ,
                      'Revenue per connection'= "std_REV_PER_CONNECTION_L1" ,
                      'Fund balance'= "std_Fund_Balance_L1"  ,
                      '% urban area' =  "std_Urban_Prop"  ,
                      'Source-surface water'="Primary_SourceSW" ,
                      'Source-purchased surface water' =  "Primary_SourceSWP",
                      'Source-purchased groundwater'="Primary_SourceGWP" )

tt$Coef <- fct_rev(tt$Coef)
tt[DV=='Health+NonHealth']
require(ggthemes)
ggplot(data=tt[{!grepl('Intercept',Coef)}&DV=="Health+NonHealth",]) + 
  geom_hline(yintercept = 0,lty = 2,col = 'grey50')+
  #geom_errorbar(aes(ymin = `0.01quant`,ymax = `0.99quant`,x = Coef,color = DV,fill = DV),width=0,lwd=0.4)+ 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`,x = Coef),width=0,lwd=1,position = position_dodge(0.5))+ 
  #geom_errorbar(aes(ymin = `0.05quant`,ymax = `0.95quant`,x = Coef,color=DV,fill = DV),width=0,lwd=1.6) +
  #facet_wrap(~DV) +
  geom_point(aes(x = Coef,y = mean,fill = as.factor(`0.975quant`>0&`0.025quant`<0)),shape=21,size = 2,position = position_dodge(0.5))+
  # scale_y_continuous(limits=c(-3,2),name = ~Delta[ln(violations)]/Delta[Beta])+
  # scale_fill_manual(values=c('white','black'),labels=c('0 in 95% interavl','0 !in 95% interval')) + 
  #  scale_color_manual(values = rep('black',3))+
  scale_fill_manual(values = c('black','white'),labels=c('no','yes'),name ='95% CI includes 0')+
  # guides(color=guide_legend(override.aes = list(lwd = c(0.4,1,1.6), pch=c(NA,NA,NA))))+
  coord_flip() + geom_vline(xintercept=0) + theme_bw() +
  scale_y_continuous(name = 'Additive log-scale parameter estimate')+
  #scale_color_colorblind(labels = c('Health viols.','All viols.','Manage. viols.'))+
  # guides(fill = F)+
  theme(axis.text = element_text(size = 12),axis.title.y = element_blank(),legend.position = c(0.8,0.4))+
  ggtitle('Posterior mean and 95% credible interval estimates')


tt = tt[,.(Coef,mean,`0.025quant`,`0.975quant`,DV)]
tt$present = paste0(formatC(tt$mean,drop0trailing = F,digits = 2,format = 'f'),' (',formatC(tt$`0.025quant`,drop0trailing = F,digits = 2,format = 'f'),', ',formatC(tt$`0.975quant`,drop0trailing = F,digits = 2,format = 'f'),')')

htmlTable::htmlTable(dcast(tt,Coef~DV,value.var = 'present'))



library(plm)
library(pglm)

pld <- as.data.frame(modeldata_dt)
pld <- pld[pld$PERIOD_ID %in% 2:3,]
pld <- pld[,colnames(pld) %in% c('Health','std_Fund_Balance_L1','std_Prop_Nonwhite','std_Urban_Prop','std_ln_REV_PER_CONNECTION_L1',
                                 'std_ln_Median_Home_Value','std_ln_Total_Service_Connections','District_ID','PERIOD_ID',
                                 'std_NonHealth_L1','std_CBSA_HHI_Index','std_Severe_Weather_Events','std_DSCI_3yr_Weekly_Avg')]

pld <- pld[rowSums(is.na(pld))==0,]
form_test = as.formula(paste0('Health~',paste(grep('^std',colnames(pld),value=T),collapse='+')))
est = pglm(formula = form_test,
           data = pld,model = 'random',family = negbin,method = "nr",index = c('District_ID','PERIOD_ID'))
summary(est)

library(corrplot)
cvals <- cor(as.matrix(mod0$model.matrix[,-1]))
test = gather(as.data.frame(cvals) %>% mutate(v1 = rownames(.)),key,value,-v1)
ggplot(data = test) + geom_tile(aes(x = v1,y=key,fill=value)) + scale_fill_gradient2()



corrplot(cvals,type = 'upper')


library(INLAutils)
observed = master_dt[PERIOD_ID%in%2:3,Health]

ggplot_inla_residuals2(mod0, observed, se = FALSE)
INLAutils::plot_fixed_marginals(mod0)


mod1 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'poisson',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod2 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'zeroinflatedpoisson0',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod3 = inla(as.formula(paste('Health',form_string,sep='~')),family = 'zeroinflatednbinomial0',data=master_dt[master_dt$PERIOD_ID%in%2:3,],
            control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE))

mod0$waic$waic
mod1$waic$waic
mod2$waic$waic


inla.list.models()
summary(test)
as.formula(paste('Health',form_string,sep='~'))
#dt <- demo_dt[order(District_ID,Year),]
#dt[,Total_Population3:=lag(Total_Population, 3),by = .(District_ID)]
#dt$Total_Population[dt$Total_Population==0] <- 1
#dt$Total_Population3[!is.na(dt$Total_Population3) & dt$Total_Population3==0] <- 1
#dt[dt$Year%in%c(2012,2015),Pop_Change:=100 * {(Total_Population-{Total_Population3})/{Total_Population3}},]

master_dt[1:3,]

summary(test)


test$model.matrix
dt[,Total_Population2:=shift(Total_Population, 2)]
dt[,mpg_forward1:=shift(mpg, 1, type='lead')]

library(tidycensus)



setkey(fill_grid,District_ID,Year)
fill_grid = audits[fill_grid,]
fill_grid = fill_grid %>% group_by(District_ID) %>% arrange(Year,-Date) %>%   filter(!duplicated(Year)) %>% ungroup()
fill_grid = as.data.table(fill_grid)
setDT(fill_grid)[, fill_Date := na.locf(Date,na.rm = FALSE), by=District_ID]
setDT(fill_grid)[, fill_Date := na.locf0(fill_Date,fromLast = T), by=District_ID]


test = fread('https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/TX0700020/JSON')
https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/IL3141937/JSON
pref <- 'https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/PWSID/TX0700020/JSON'
suf <- '/JSON'
viols <- pblapply(return_systems$PWS_ID,function(x) 
  fread(paste0(pref,x,suf),fill=T),cl = 10)

qnames <- gsub(' ','%20',toupper(tx_counties$NAME))
urls = paste0('https://iaspub.epa.gov/enviro/efservice/SDW_VIOL_ENFORCEMENT/COUNTYSERVED/',qnames,'/STATE/TX/excel')
library(RCurl)
vlist <- pblapply(urls,function(x) fread(x,fill=T),cl = 5)
vdt <- rbindlist(vlist,fill=T)

qnames[which(sapply(vlist,nrow)>=10000)]
#Drought score
#Population served, logged
#Total storage per 1,000 people 
#Number of interconnections, logged 
# Groundwater

#% customers served retail 
#Average daily consumption per1,000 people
#% Democratic vote

#Median household income, logged, #% houses built after 1980, #% rural,#% Black, #% Hispanic, #% four-year college degree



tx_info = tx_info[!is.na(tx_info$`Activity Status`),]
tx_info = tx_info[tx_info$Owner_Type=='District',]




setkey(tx_district_info,District_ID)
setkey(dinfo,District_ID)
dinfo = tx_district_info[dinfo,]


dinfo$`First Reported Date` = dmy(dinfo$`First Reported Date`)


fill_grid = data.table(expand.grid(District_ID = as.character(unique(dinfo$District_ID)),Year = 2000:2020))


fill_grid = fill_grid[order(District_ID,Year),]
fill_grid$fill_Date = fill_grid$fill_Date - year(date_decimal(fill_grid$fill_Date)) + fill_grid$Year


library(sf)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
pws_combined = readRDS('scratch/proj5/pws_combined.RDS')
pws_combined = pws_combined[pws_combined$PWS_ID %in% unlist(dinfo$PWS_ID),]
pws_combined$District_ID = tx_info$District_ID[match(pws_combined$PWS_ID,tx_info$PWS_ID)]
district_sf = aggregate(pws_combined[,!colnames(pws_combined) %in% c('PWS_ID','District_ID','Adopt','In_Analysis','In_Houston','Prop_Urban')],
                        list(District_ID = pws_combined$District_ID),FUN = sum)
district_sf <- st_union(district_sf,by_feature = T)

# %>% group_by(a.x) %>% summarise(mean(a.y))

tceq_wd = st_read('spatial_inputs/water_districts_shp/TCEQ_WaterDistricts.shp')
tceq_wd = st_make_valid(tceq_wd)
tceq_wd = st_transform(tceq_wd,albersNA)
tceq_wd = tceq_wd[!tceq_wd$DISTRICT_I %in% district_sf$District_ID,]
tceq_wd = tceq_wd %>% rename(District_ID = DISTRICT_I)

district_sf = rbind(district_sf,tceq_wd[,'District_ID'])
district_sf = st_make_valid(district_sf)




fill_grid$Prop_Urban <- district_sf$Prop_Urban[match(fill_grid$District_ID,district_sf$District_ID)]


#saveRDS(object = demo_dt,'scratch/proj6/district_demo_data.RDS')
#demo_dt = readRDS('scratch/proj5/pws_demo_data.RDS')
setkey(demo_dt,District_ID,Year)
fill_grid = as.data.table(fill_grid)
setkey(fill_grid,District_ID,Year)
fill_grid = demo_dt[fill_grid ,]




setnames(debt_dt,"FiscalYear",'Year')
setkey(debt_dt,District_ID,Year)
setkey(fill_grid,District_ID,Year)
fill_grid = debt_dt[fill_grid,]

fill_grid$Total_Revenue = replace_na(fill_grid$`ENTERPRISE FUND - OPERATING REVENUES`,0) + replace_na(fill_grid$`GENERAL FUND - TOTAL REVENUES`,0)
fill_grid$Total_Expenditure = replace_na(fill_grid$`ENTERPRISE FUND - OPERATING EXPENSES`,0) + replace_na(fill_grid$`GENERAL FUND - TOTAL EXPENDITURES`,0)
fill_grid$Fund_Balance = replace_na(fill_grid$`GENERAL FUND - FUND BALANCE`,0)
fill_grid$Health_Violation_Count = replace_na(fill_grid$Health_Viol,0)
fill_grid$Management_Violation_Count = replace_na(fill_grid$NonHealth_Viol,0)
fill_grid$Water_Units = fill_grid$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`



vars_to_lag = c("Total_Debt_Service_Outstanding",'Total_Revenue','Total_Expenditure',
                'Fund_Balance','Total_Tax_Rate','DOC_ID','DOC_URL','Health_Violation_Count',
                'Management_Violation_Count','Water_Units')
lag_vars = paste(vars_to_lag,'L1',sep='_')
fill_grid[order(District_ID,Year), (lag_vars) := data.table::shift(.SD, n = 1,fill= NA,type="lag"), .SDcols=vars_to_lag,by = District_ID]

setkey(fill_grid,District_ID,Year)
setkey(dinfo,District_ID)
dinfo_dt = fill_grid[dinfo,]

dinfo_dt = dinfo_dt[,!c('CCN','First Reported Date','BOARD_NUMBER','i.District_Name','join_time','common','zeros','y','Date','DATE_SUBMITTED','GO_Service_Outstanding','REV_Service_Outstanding'),with=F]
dinfo_dt = dinfo_dt[,!grep('^ENTERPRISE FUND|^GENERAL FUND|^WASTEWATER CUST|^WATER CUST|TAX|FISCAL YEAR|BONDS OUTSTANDING|CURRENT ASSESSED',colnames(dinfo_dt),value=T),with=F]

# Last observation carried forward from last row of group
dinfo_dt <- dinfo_dt[, District_Name := na.locf0(District_Name, fromLast = TRUE), by = District_ID]
# Last observation carried forward for first row of group
dinfo_dt <- dinfo_dt[, District_Name := na.locf(District_Name), by = District_ID]

dinfo_dt = dinfo_dt[dinfo_dt$Year %in% 2009:2017,]
dinfo_dt = dinfo_dt[dinfo_dt$Status=='ACTIVE',]
dinfo_dt = dinfo_dt[!is.na(dinfo_dt$DOC_ID_L1),]
dinfo_dt = dinfo_dt[dinfo_dt$District_Type %in% c('FWSD','MUD','SUD','WCID'),]
dinfo_dt = dinfo_dt[!is.na(dinfo_dt$Health_Violation_Count),]

vars_to_ratio = c("Total_Debt_Service_Outstanding_L1",'Total_Revenue_L1','Total_Expenditure_L1','Fund_Balance_L1')
ratio_vars = paste(vars_to_ratio,'Per_Connection',sep='_')
dinfo_dt[ , (ratio_vars) := .SD/(Total_Service_Connections), .SDcols=vars_to_ratio,by = District_ID]


dinfo_dt$Perc_Change_Service_Connections = 100 *{(dinfo_dt$Water_Units - dinfo_dt$Water_Units_L1)/dinfo_dt$Water_Units_L1}
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(-Inf)] <- 0
dinfo_dt$Perc_Change_Service_Connections[dinfo_dt$Perc_Change_Service_Connections==(Inf)] <- 0
dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50] = dinfo_dt$Total_Tax_Rate_L1[dinfo_dt$Total_Tax_Rate_L1>50]/100



dinfo_dt$Management_Violation_L1 = (dinfo_dt$Management_Violation_Count_L1>0) + 1
dinfo_dt$Health_Violation_L1 = (dinfo_dt$Health_Violation_Count_L1>0) + 1
dinfo_dt$Daily_MGD_Connection = dinfo_dt$Average_Daily_Consump_MGD/dinfo_dt$Total_Service_Connections
vars_to_binary = c('Wholesaler','Multiple_Systems','Retail_Wastewater')
vars_to_scale = c("Median_Year_Structure_Built"  ,"Median_Home_Value" ,"Prop_Bach",'Prop_Nonwhite',"Prop_Owner_Occupied",
                  "Prop_Urban","CBSA_Service_HHI","Perc_Change_Service_Connections",'DSCI_52week_Average',
                  "Total_Debt_Service_Outstanding_L1_Per_Connection","Total_Revenue_L1_Per_Connection"   ,              
                  "Total_Expenditure_L1_Per_Connection","Fund_Balance_L1_Per_Connection",
                  'Management_Violation_Count_L1',"Health_Violation_Count_L1",'Total_Service_Connections','Daily_MGD_Connection')
scale_labs = paste0('Std_',vars_to_scale)
dinfo_dt[ , (scale_labs) := lapply(.SD, scale), .SDcols=vars_to_scale]
dinfo_dt$Have_Health_Violation = (dinfo_dt$Health_Violation_Count>0)+0
dinfo_dt$Total_Viol_Count = dinfo_dt$Health_Violation_Count + dinfo_dt$Management_Violation_Count
dinfo_dt$Total_Viol_Count_L1 = dinfo_dt$Health_Violation_Count_L1 + dinfo_dt$Management_Violation_Count_L1

saveRDS(file = 'org_performance/input/generated_inputs/df_for_model.RDS',object = dinfo_dt)










temp = data.table(temp)
temp = temp[District_Type %in% c('FWSD','MUD','SUD','WCID'),]
temp = temp[!is.na(DOCID_L1),]

names(temp)
temp = temp[order()] %>% group_by(PWS_ID) %>% arrange(PERIOD)

temp = temp[temp$FISCAL_YEAR_START_YEAR>2006,]
temp$TOTAL_TAX_RATE_L1[temp$TOTAL_TAX_RATE_L1>500] <- NA

#more_than_five = temp %>% group_by(PWS_ID) %>% summarise(co = n()) %>% filter(co>5)
#temp = temp[temp$PWS_ID %in% more_than_five$PWS_ID,]
temp$HEALTH_VIOL_OCCURENCE_PERIOD = (temp$HEALTH_VIOL_COUNT_PERIOD >0) + 0
temp$MANAGEMENT_VIOL_OCCURENCE_PERIOD = (temp$MANAGEMENT_VIOL_COUNT_PERIOD >0) + 0

dvs = c('HEALTH_VIOL_OCCURENCE_PERIOD')

temp = temp %>% group_by(DISTRICT_ID,PWS_ID) %>% 
  arrange(PWS_ID,DISTRICT_ID,PERIOD) %>% 
  mutate(WASTEWATER_SFU_P1 = lag(WASTEWATER_SFU),
         WASTEWATER_SFU_F1 = lead(WASTEWATER_SFU),
         WATER_SFU_P1 = lag(WATER_SFU),
         WATER_SFU_F1 = lead(WATER_SFU)) %>% 
  mutate(WASTEWATER_SFU = ifelse({is.na(WASTEWATER_SFU) | (WASTEWATER_SFU==0 & WASTEWATER_SFU_P1>0 & WASTEWATER_SFU_F1>0)},
                                 (WASTEWATER_SFU_P1+WASTEWATER_SFU_F1)/2,WASTEWATER_SFU),
         WATER_SFU = ifelse({is.na(WATER_SFU)| (WATER_SFU==0 & WATER_SFU_P1>0 & WATER_SFU_F1>0)},
                                 (WATER_SFU_P1+WATER_SFU_F1)/2,WATER_SFU))

temp = temp %>% group_by(DISTRICT_ID,PWS_ID) %>%  arrange(PWS_ID,DISTRICT_ID,PERIOD) %>% 
  mutate(MVP1 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n=1),MVP2 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n=2),MVP3 = lag(MANAGEMENT_VIOL_COUNT_PERIOD,n =3),
         HVP1 = lag(HEALTH_VIOL_COUNT_PERIOD,n=1),HVP2 = lag(HEALTH_VIOL_COUNT_PERIOD,n=2),HVP3 = lag(HEALTH_VIOL_COUNT_PERIOD,n =3)) %>%
  mutate(MVP1 = ifelse(is.na(MVP1),0,MVP1),MVP2 = ifelse(is.na(MVP2),0,MVP2),MVP3 = ifelse(is.na(MVP3),0,MVP3),
         HVP1 = ifelse(is.na(HVP1),0,HVP1),HVP2 = ifelse(is.na(HVP2),0,HVP2),HVP3 = ifelse(is.na(HVP3),0,HVP3)) %>%
  mutate(MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years = MVP1 + MVP2 + MVP3,HEALTH_VIOL_COUNT_PERIOD_Prior3Years = HVP1 + HVP2 + HVP3)

temp = temp[temp$TOTAL_REVENUE_L1  <100000000,]
temp$REV_PER_CONNECTION_L1 = temp$TOTAL_REVENUE_L1/temp$Service_Connections
temp$EXP_PER_CONNECTION_L1 = temp$TOTAL_EXPENDITURE_L1/temp$Service_Connections
temp$Median_Structure_Age = temp$FISCAL_YEAR_END_YEAR - temp$Median_Year_Structure_Built 
temp$Median_Structure_Age[temp$Median_Structure_Age<0] <- 0
temp$WHOLESALE_REGULAR_POP[is.na(temp$WHOLESALE_REGULAR_POP)] <- 0
temp$Any_Debt_Issued_P5 = (temp$Debt_Issued_P5 >0) + 0
temp$log_debt = log(temp$Debt_Issued_P5+1)
temp$WHOLESALE <- (temp$WHOLESALE_REGULAR_POP>0) + 0
temp = temp %>% group_by(PWS_ID) %>% arrange(PWS_ID,FISCAL_YEAR_END_YEAR) %>% mutate(Prior_Total_Population = lag(Total_Population,1)) %>%
  mutate(Prop_Population_Change = (Total_Population - Prior_Total_Population) / Prior_Total_Population)


temp = temp[!is.na(temp$CBSA_CODE),]

temp$REV_PER_CONNECTION_L1 = temp$TOTAL_REVENUE_L1/temp$SERVICE_CONNECTIONS_L1

library(arm)
library(sjPlot)
form0 = "~

rescale(WHOLESALE_REGULAR_POP) + RETAIL_WASTEWATER + 
BOARD_SELECTION + SPECIAL_LAW +

rescale(DISTRICT_AGE) + rescale(Service_Connections) + 
rescale(Prop_Owner_Occupied_Household) + 
rescale(Median_Structure_Age) +

rescale(HEALTH_VIOL_COUNT_PERIOD_Prior3Years) +
rescale(MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years) + 
rescale(TOTAL_REVENUE_L1) + 
rescale(REV_PER_CONNECTION_L1) +
#rescale(TotDebtServiceOutstanding_P1) + 
rescale(TOTAL_TAX_RATE_L1) +
Any_Debt_Issued_P5 + Any_Debt_Issued_P5:rescale(Debt_Issued_P5) + 
  
rescale(CBSA_HHI) + rescale(county_district_count) + rescale(MEDIAN_HOME_PRICE) +  
rescale(Prop_Population_Change) + 

f(DISTRICT_ID,model = 'iid',hyper=pcprior) + 
f(CBSA,model = 'iid',hyper=pcprior) + 
f(FISCAL_YEAR_START_YEAR,model = 'rw1',hyper=pcprior)"

form1 = paste(form0,
"rescale(Service_Connections):rescale(TOTAL_REVENUE_L1) + rescale(Service_Connections):rescale(CBSA_HHI)",
sep = '+')


#### grouping indicators
form_set = as.list(c(paste(dvs,form0)))
names(form_set) <- c('Health_0')

pcprior <- list(prec = list(prior = "pc.prec",param = c(3, 0.01)))

inlist = lapply(names(form_set),function(f) inla(as.formula(form_set[[f]]),family = 'binomial',data = temp,
control.predictor=list(compute=TRUE),
control.compute=list(dic=TRUE, cpo=TRUE,waic=TRUE),
control.fixed = list(prec.intercept=1)))

names(inlist) = c('mod_health0')
saveRDS(object = inlist,file = 'scratch/proj6/mod_results.RDS')
summary(inlist[[1]])
library(stargazer)
tabv = c("HEALTH_VIOL_COUNT_PERIOD_Prior3Years",
  "MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years", 
  "TOTAL_REVENUE_L1", "TotDebtServiceOutstanding_P1", "TOTAL_TAX_RATE_L1",
  "Debt_Issued_P5", 
  "HHI", "county_district_count", "Service_Connections", 
  "Prop_Population_Change", "MEDIAN_HOME_PRICE",
 "DISTRICT_AGE", "Prop_Owner_Occupied_Household","Median_Structure_Age")
  
#BOARD_SELECTION + SPECIAL_LAW",
library(htmlTable)




btab = round(do.call(rbind,lapply(apply(temp[,colnames(temp) %in% tabv] ,2,summary),function(x) x[1:6])),2)#
library(htmlTable)
htmlTable(btab[,c(1,3,4,6)],
          header =  c("Min.", "Mean",
                      "Median", "Max."),
          rnames = rownames(btab))


temp$DOC_ID[temp$TOTAL_TAX_RATE_L1>100]
temp$FISCAL_YEAR_ENDED[temp$TOTAL_TAX_RATE_L1>100]
temp[temp$TOTAL_TAX_RATE_L1>,]
table(temp$BOARD_SELECTION[!duplicated(temp$DISTRICT_ID)])
table(temp$SPECIAL_LAW[!duplicated(temp$DISTRICT_ID)])
table(temp$WHOLESALE_REGULAR[!duplicated(temp$DISTRICT_ID)])
table(temp$RETAIL_WASTEWATER[!duplicated(temp$DISTRICT_ID)])

rgroup = c("Numeric attributes",
           "Categorical attributes")
n.rgroup = c(length(tabv),0),
caption="Basic table with both column spanners (groups) and row groups",
tfoot="&dagger; A table footer commment")

cor(temp$Debt_Issued_P5,temp$TotDebtServiceOutstanding,use = 'pairwise.complete.obs')

test = glmer(HEALTH_VIOL_OCCURENCE_PERIOD ~ 
        WHOLESALE + WHOLESALE:rescale(WHOLESALE_REGULAR_POP)  +    
       RETAIL_WASTEWATER + 
       #BOARD_SELECTION + SPECIAL_LAW +
       rescale(DISTRICT_AGE) + rescale(Service_Connections) + 
       rescale(Prop_Owner_Occupied_Household) + 
       rescale(Median_Structure_Age) +
       rescale(HEALTH_VIOL_COUNT_PERIOD_Prior3Years) +
       rescale(MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years) + 
       rescale(TOTAL_REVENUE_L1) +
      rescale(TOTAL_TAX_RATE_L1) +
       Any_Debt_Issued_P5 + Any_Debt_Issued_P5:rescale(Debt_Issued_P5) + 
       rescale(CBSA_HHI) + rescale(county_district_count) + rescale(MEDIAN_HOME_PRICE) +  
       rescale(Prop_Population_Change) + 
       (1|DISTRICT_ID) + 
       #(1|CBSA) + 
       (1|FISCAL_YEAR_START_YEAR),data = temp,family = 'binomial')
summary(test)

