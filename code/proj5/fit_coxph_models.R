library(data.table)
library(tidyverse)
library(INLA)
library(lubridate)
library(MASS)
library(tidyverse)
library(lubridate)
library(survival)
library(mstate)
library(sf)
library(geojsonsf)
library(data.table)
library(tigris)
library(lwgeom)
library(pbapply)
library(ggthemes)
library(esri2sf)
library(rgeos)
require(spdep)


cox_dt = readRDS('scratch/data_for_coxph_model.RDS')


#cox_dt[is.na(Yearly_Debt_Payment)&`BONDS OUTSTANDING`>0&!duplicated(District_Name),][,District_Name]
cox_dt$sub_index = 1:nrow(cox_dt)
cox_dt = cox_dt[HAVE_SHAPEFILE==1,]

#find_ids = cox_dt[,all(!is.na(DOC_ID)),by=.(PWS_ID)][V1==T,]
#cox_dt = cox_dt[PWS_ID%in%find_ids$PWS_ID,]

cox_mud[District_Name=='HARRIS COUNTY MUD 432',.(`BONDS OUTSTANDING`,Debt_Per_Connection,
                                                 Total_Debt_Service_Outstanding,
                                                 Total_Principal_Outstanding,
                                                 Fund_Balance,Connections,Total_Revenue,DOC_ID,DOC_URL)]

weeks = length(seq.Date(from = mdy('05/04/2010'),to = mdy('07/07/2015'),by = 'week'))
months = round(weeks/4)
months2 = months/2

cox_dt$Owner_Type = as.factor(cox_dt$Owner_Type)
cox_dt$Municipal = (cox_dt$Owner_Type=='Municipality')+0
cox_dt$Investor_Owned = (cox_dt$Owner_Type=='Investor Owned')+0
cox_dt$Private = (cox_dt$Owner_Type=='Private')+0
cox_dt$WSC= (cox_dt$Owner_Type=='Water Supply Corporation')+0
#summary(cox_dt$Connections)
#cox_dt$PopulationType_Population_Total

cox_dt$Median_Structure_Age[is.na(cox_dt$Median_Structure_Age)] <- median(cox_dt$Median_Structure_Age,na.rm=T)



base_form = inla.surv(time = Time,event = Event) ~ -1 + intercept + 
  scale(asinh(Connections),scale=F) +
  scale(asinh(Median_Home_Value),scale=F) +
  scale(asinh(Storage_Per_Connection_G),scale=F) +
  Has_Emergency_Interconnect +
  #Wholesale +
  Groundwater +
  Purchaser +
  Urban_District +
  #Median_Structure_Age +
  Municipal + Private + Investor_Owned + WSC +
  Groundwater + Purchaser + Urban_District + 
  #scale(asinh(Median_Home_Value),scale=F) +
  #scale(asinh(Perc_Nonwhite),scale=F) +
  f(Primary_CFIPS,model="iid",hyper = list(prec = list(param=c(1, 1))))
 # f(CFIPS_ID,model="besag",graph="tx.adj",hyper = list(prec = list(param=c(1, 1))))

modA = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(diagonal = 1000,  int.strategy = "eb"),
            family='coxph',data=cox_dt,verbose = F,num.threads=8)

modB = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(diagonal = 100,  int.strategy = "eb"),
            control.mode = list(result = modA, restart = TRUE),
            family='coxph',data=cox_dt,verbose = F,num.threads=8)

modC = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(diagonal = 10,  int.strategy = "eb"),
            control.mode = list(result = modB, restart = TRUE),
            family='coxph',data=cox_dt,verbose = F,num.threads=8)

modD = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(diagonal = 1,  int.strategy = "eb"),
            control.mode = list(result = modC, restart = TRUE),
            family='coxph',data=cox_dt,verbose = F,num.threads=8)

modE = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(int.strategy = "eb") ,
            control.mode = list(result = modD, restart = TRUE),
            family='coxph',data=cox_dt,verbose = F,num.threads=8)
gc()


mod_base = inla(base_form, control.mode = list(result = modE, restart = TRUE),
                family='coxph',data=cox_dt,verbose = F,num.threads=8)
saveRDS(mod_base,'scratch/proj5/base_coxphmod.RDS')

mod_base = readRDS('scratch/proj5/base_coxphmod.RDS')

cox_mud = cox_dt[District_Type == 'MUD',] 
cox_mud = cox_mud[Status == 'ACTIVE',]
cox_mud = cox_mud[!is.na(FiscalYear),]
cox_mud = cox_mud[!is.na(DOC_ID),]
cox_mud = cox_mud[Connections>0,]


mud_form = update.formula(base_form,  ~ . - Municipal - Private - Investor_Owned - WSC + 
                            scale(asinh(Growth_Over_Drought),scale=F))
mud_mod  = inla(mud_form,control.compute = list(waic=TRUE,dic=TRUE),
                   family='coxph',data=cox_mud,num.threads = 8, 
                   verbose = F,control.update = list(result = mod_base))


mud_fiscal_form = update.formula(mud_form,  ~ . +
                                   #scale(asinh(Expenditure_Per_Connection),scale=F) + 
                                   scale(asinh(Debt_Per_Connection),scale=F) + 
                                   scale(asinh(Fund_Balance),scale=F))
fiscal_mod  = inla(mud_fiscal_form,control.compute = list(waic=TRUE,dic=TRUE),
                family='coxph',data=cox_mud,num.threads = 8, 
              verbose = F,control.update = list(result = mod_base))

fiscal_form2 = update.formula(mud_form,  ~ . +
                               scale(asinh(Expenditure_Per_Connection),scale=F) + 
                              scale(asinh(Debt_Per_Connection),scale=F)*scale(asinh(Connections_Added_During_Drought),scale=F) + 
                               scale(asinh(Fund_Balance),scale=F))

fiscal_mod2  = inla(fiscal_form2,control.compute = list(waic=TRUE,dic=TRUE),
                   family='coxph',data=cox_mud,num.threads = 8, 
                   verbose = F,control.update = list(result = mod_base))





fiscal_form3 = update.formula(mud_form,  ~ . +
                                scale(asinh(Expenditure_Per_Connection),scale=F) +
                                scale(asinh(Debt_Per_Connection),scale=F) *  scale(asinh(Median_Home_Value), scale = F) +
                                scale(asinh(Fund_Balance),scale=F))
summary(fiscal_mod2)
fiscal_mod3  = inla(fiscal_form3,control.compute = list(waic=TRUE,dic=TRUE),
                    family='coxph',data=cox_mud,num.threads = 8, 
                    verbose = F,control.update = list(result = mod_base))
library(coxme)


#table(cox_mud$Debt_Tax,cox_mud$Debt_Per_Connection==0)

cox_mud$asinh_median_home_value = scale(asinh(cox_mud$Median_Home_Value),scale = F) 
cox_mud$asinh_debt_per_connection = scale(asinh(cox_mud$Debt_Per_Connection),scale = F)
cox_mud$asinh_exp_per_connection = scale(asinh(cox_mud$Expenditure_Per_Connection),scale = F)
cox_mud$asinh_connections = scale(asinh(cox_mud$Connections),scale = F)
cox_mud$asinh_storage_per_connection = scale(asinh(cox_mud$Storage_Per_Connection_G),scale= F)
cox_mud$asinh_fund_balance = scale(asinh(cox_mud$Fund_Balance),scale=F)
cox_mud$asinh_perc_nonwhite = scale(asinh(cox_mud$Perc_Nonwhite))
cox_mud$asinh_debt_outstanding = scale(asinh(cox_mud$Total_Debt_Service_Outstanding))
cox_mud$asinh_median_structure_age = scale(asinh(cox_mud$Median_Structure_Age))
cox_mud$asinh_use_per_connection = scale(asinh(cox_mud$Usage_Per_Connection))

library(coxme)

avgs = cox_mud[,lapply(.SD,median,na.rm=T),
        .SDcols = c('Median_Home_Value','Perc_Nonwhite','Connections','Usage_Per_Connection','Fund_Balance','Debt_Per_Connection',
                    'Expenditure_Per_Connection','Total_Revenue_Per_Connection','Median_Structure_Age'),
        by = .(Event)]
avgs







cox_mud[Debt_Per_Connection>200000,]
ggplot(cox_mud,aes(x = as.factor(Event),y = Debt_Per_Connection)) + geom_boxplot()


cox_dt[DISTRICT_NAME=='HARRIS COUNTY MUD 432',.(Total_Debt_Service_Outstanding,Total_Principal_Outstanding,`BONDS OUTSTANDING`,Debt_Per_Connection)]



cox_mud$Total_Revenue_Per_Connection

tapply(cox_mud$Median_Home_Value,cox_mud$Event,summary)


efit2 <- coxme(Surv(time = Time,event = Event) ~  
                 asinh_connections + 
                 asinh_storage_per_connection + 
                 Has_Emergency_Interconnect +
                 #Wholesale +
                 Groundwater +
                 #Purchaser +
                 Urban_District +
                 asinh_median_structure_age +
                 asinh_use_per_connection +
                 asinh_median_home_value +
                 asinh_use_per_connection +
                 asinh_fund_balance +
                 #asinh_perc_nonwhite + 
                 (1|Primary_CFIPS), cox_mud[cox_mud$Event==1,])



summary(efit2)
AIC(efit2)
library(sjplot)

table(!is.na(cox_mud$Usage_Per_Connection))

cor(cox_mud$Usage_Per_Connection,cox_mud$Median_Home_Value,use='pairwise.complete.obs')


cor(cox_mud$Median_Home_Value,cox_mud$Debt_Per_Connection,use = 'pairwise.complete.obs')

cox_mud[is.na(Debt_Per_Connection),.(`BONDS OUTSTANDING`)]
library(brinla)
install.packages('brinla')
summary(fiscal_mod2)
table(cox_mud$Debt_Per_Connection>0)
fiscal_mod$waic$waic
fiscal_mod2$waic$waic
fiscal_mod3$waic$waic

  model=inla(formula, family ="coxph", data=data, verbose=T,
             control.hazard=list(model="rw1", n.intervals=20))

setkey(tx_population,'PWS_ID','Year')
cox_dt$Year <- year(cox_dt$Date)
setkey(cox_dt,'PWS_ID','Year')
cox_dt = data.table(left_join(cox_dt,tx_population))
fvals = c('PopServed','Connections','Usage')

floc = '../../../../net/tmp/tscott1/bosque_scratch/proj5/'
mod_base_priors = readRDS(paste0(floc,'basemod_hyperparameters.RDS'))
cox_dt = readRDS('scratch/proj5/full_panel_data.RDS')
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')




cox_base = readRDS('scratch/proj5/base_inlacoxph_object.RDS')


cox_dt <- cox_dt[PWS_ID %in% cox_dt$PWS_ID[cox_dt$Total_Revenue>0&cox_dt$Total_Expenditure>0],]
cox_dt = cox_dt[!grepl('CINCO|HARRIS COUNTY MUD 50[0:3]|HARRIS COUNTY MUD 321',PWS_NAME),]
cox_dt = cox_dt[!grepl('TRAVIS COUNTY MUD [0-9]$',District_Name),]
cox_dt <- cox_dt[PWS_ID %in% cox_dt$PWS_ID[cox_dt$Total_Revenue>0&cox_dt$Total_Expenditure>0],]
keep = cox_dt[,all(Total_Revenue>0)&all(Total_Expenditure>0),by=.(PWS_ID)][V1==T,]
cox_dt = cox_dt[PWS_ID %in% keep$PWS_ID,]
cox_dt$Usage_Per_Connection_P1[is.na(cox_dt$Usage_Per_Connection_P1)] <- 0

#cox_dt$Debt_Tax_Over_Debt_Outstanding = cox_dt$`DEBT SERVICE TAX LEVIED`/cox_dt$Total_Debt_Service_Outstanding
#cox_dt$Debt_Tax_Over_Debt_Outstanding[cox_dt$`DEBT SERVICE TAX LEVIED`==0] <- 0
#cox_dt$Debt_Tax_Over_Debt_Outstanding[cox_dt$Total_Debt_Service_Outstanding==0] <- 0
saveRDS(cox_dt,'scratch/proj5/data_used_in_model.RDS')
#cox_dt$Debt_Quintile = dplyr::ntile(cox_dt$Debt_Per_Connection,n = 5)
#cox_dt$Fund_Balance_Quintile = dplyr::ntile(cox_dt$,n = 5)

#table(ifelse(cox_dt$Yearly_Payment_Per_Connection<avgbill,'<1x',ifelse(cox_dt$Yearly_Payment_Per_Connection>3*avgbill,'>3x','1x-3x')))
restricted_form = update.formula(mod_base_priors$.args$formula, ~ . +
                                   scale(asinh(Connections),scale=F) +
                                   #scale(asinh(Median_Home_Value),scale=F) +
                                   scale(asinh(Storage_Per_Connection_G),scale=F) +
                                   scale(asinh(Usage_Per_Connection_P1),scale=F) + 
                                   Has_Emergency_Interconnect +
                                   #Wholesale +
                                   Groundwater +
                                   Purchaser +
                                   # Urban_District +
                                   Median_Structure_Age +
                                   # WCID + SUD + FWSD +
                                   scale(asinh(KBDI_Avg),scale=F))
#scale(asinh(Perc_College_Grad) +
#asinh(Perc_Nonwhite))

#restricted_form_mud_hazard = as.formula(gsub('scale.model = TRUE',
#                paste0('scale.model = TRUE,replicate = ','MUD_gr'),deparse(restricted_form)))
#cox_dt[is.na(Yearly_Payment_Per_Connection),][!duplicated(PWS_ID),.(District_Name,BONDS OUTSTANDING,Year)]
#cox_dt[,.(.N,sum(is.na(Yearly_Payment_Per_Connection))),by = .(PWS_ID)][N>V2,]

#cox_dt$OR_Group = inla.group(asinh(cox_dt$Operating_Ratio), n = 3, method = "quantile")
#cox_dt$Debt_Group = inla.group(asinh(cox_dt$Debt_Per_Connection), n = 3, method = "quantile")
#cox_dt$FB_Group = inla.group(asinh(cox_dt$Fund_Balance_Per_Connection), n = 3, method = "quantile")



form_0 = restricted_form
form_1 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F))
form_2 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) * Total_Tax_Rate)
form_3 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) * Debt_Tax) 
form_4 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) * MO_Tax) 
form_5 = update.formula(form_0, ~ . + scale(asinh(Operating_Ratio),scale=F) + 
                                        scale(asinh(Debt_Per_Connection),scale=F) + 
                                        scale(asinh(Fund_Balance_Per_Connection),scale=F))
form_6 = update.formula(form_0, ~ . + 
                          scale(asinh(Median_Home_Value),scale=F) + 
                          scale(asinh(Operating_Ratio),scale=F) + 
                          scale(asinh(Debt_Per_Connection),scale=F) + 
                          scale(asinh(Fund_Balance_Per_Connection),scale=F))
form_7 = update.formula(form_0, ~ . + 
                          scale(asinh(Operating_Ratio),scale=F) * scale(asinh(Median_Home_Value),scale=F) + 
                          scale(asinh(Debt_Per_Connection),scale=F) + 
                          scale(asinh(Fund_Balance_Per_Connection),scale=F))
form_8 = update.formula(form_0, ~ . + 
                          scale(asinh(Operating_Ratio),scale=F) + 
                          scale(asinh(Debt_Per_Connection),scale=F) * scale(asinh(Median_Home_Value),scale=F) + 
                          scale(asinh(Fund_Balance_Per_Connection),scale=F))
form_9 = update.formula(form_0, ~ . + 
                          scale(asinh(Operating_Ratio),scale=F) + 
                          scale(asinh(Debt_Per_Connection),scale=F) + 
                          scale(asinh(Fund_Balance_Per_Connection),scale=F) * scale(asinh(Median_Home_Value),scale=F))
form_10 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) +
                          scale(asinh(Operating_Ratio),scale=F) + 
                          scale(asinh(Debt_Per_Connection),scale=F) + 
                          scale(asinh(Fund_Balance_Per_Connection),scale=F) +
                           Debt_Tax + MO_Tax)

form_11 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) +
                           scale(asinh(Operating_Ratio),scale=F) + 
                           scale(asinh(Debt_Per_Connection),scale=F) * Debt_Tax + 
                           scale(asinh(Fund_Balance_Per_Connection),scale=F) + MO_Tax)

form_12 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) +
                           scale(asinh(Operating_Ratio),scale=F) + 
                           scale(asinh(Debt_Per_Connection),scale=F)  + 
                           scale(asinh(Fund_Balance),scale=F))

form_13 = update.formula(form_0, ~ . + scale(asinh(Median_Home_Value),scale=F) +
                           scale(asinh(Operating_Ratio),scale=F) + 
                           scale(asinh(Debt_Per_Connection),scale=F)  + 
                           scale(asinh(Fund_Balance),scale=F) * scale(asinh(Connections),scale=F))

form_14 = update.formula(form_0, ~ . +
                           scale(asinh(Total_Revenue_Per_Connection),scale=F) * scale(asinh(Usage_Per_Connection_P1),scale=F) + 
                           scale(asinh(Debt_Per_Connection),scale=F) * scale(asinh(Median_Home_Value),scale=F)  + 
                           scale(asinh(Fund_Balance),scale=F) * scale(asinh(Connections),scale=F))


fnames = grep('form_[0-9]{1,2}',ls(),value=T)


# 
# form_financial_nominal_interaction = update.formula(form_0, ~ . - scale(asinh(Connections),scale=F) +  
#                                           scale(asinh(Connections),scale=F) * scale(asinh(Total_Debt_Service_Outstanding),scale=F) + 
#                                           scale(asinh(Connections),scale=F) * scale(asinh(Total_Expenditure),scale=F) + 
#                                           scale(asinh(Connections),scale=F) * scale(asinh(Fund_Balance),scale=F))
# 
# form_financial_int = update.formula(form_0, ~ . + 
#                                          scale(asinh(Expenditure_Per_Connection),scale=F) * MO_Tax + 
#                                          scale(asinh(Debt_Per_Connection),scale=F) * Debt_Tax + 
#                                          scale(asinh(Fund_Balance_Per_Connection),scale=F))

#form_revenue_MOTax = update.formula(form_financial, ~. + MO_Tax + MO_Tax:scale(asinh(Operating_Ratio),scale=F))
#form_debt_DebtTax = update.formula(form_financial, ~. + Debt_Tax + Debt_Tax:scale(asinh(Debt_Per_Connection),scale=F))
#form_financial_interactions = update.formula(form_0, ~ . - asinh(Total_Revenue_Per_Connection) + asinh(Fund_Balance_Per_Connection) + asinh(Debt_Per_Connection) +
#                                               asinh(Total_Revenue_Per_Connection)*asinh(Fund_Balance_Per_Connection)*asinh(Debt_Per_Connection))

fin_mods = lapply(seq_along(fnames),function(x)inla(get(fnames[[x]]),control.compute = list(waic=TRUE,dic=TRUE),
                                                       control.inla = list(int.strategy = "eb"),
                                                       family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),num.threads = 8,
                                                       E = cox_base$E[cox_dt$sub_index],verbose = TRUE,control.update = list(result = mod_base_priors)))
mods = fnames

gof_table = data.table(mod = fnames,WAIC = sapply(fin_mods,function(x) x$waic$waic),
                                       DIC = sapply(fin_mods,function(x) x$dic$dic),formula = sapply(fnames,get))

saveobjs = which(gof_table$WAIC<gof_table$WAIC[gof_table$mod=='form_0'])
saveRDS(gof_table,'scratch/proj5/gof_table.RDS')
sapply(saveobjs,function(x) saveRDS(fin_mods[[x]],paste0('../../../../net/tmp/tscott1/bosque_scratch/proj5/',mods[x],'.RDS')))






#saveRDS(fin_mods,'../../../../net/tmp/tscott1/bosque_scratch/proj5/mod_object_list.RDS')



