library(INLA)
library(tidyverse)
library(lme4)
temp = readRDS('scratch/proj6/df_for_model.RDS')
temp = temp[!is.na(temp$AUDIT_DOCID_L1),]
temp = temp %>% group_by(PWS_ID) %>% arrange(PERIOD)
temp = temp[temp$Type %in% c('FWSD','MUD','SUD','WCID'),]
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

