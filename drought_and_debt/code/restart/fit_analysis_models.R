library(data.table)
library(tidyverse)
library(INLA)
library(lubridate)
floc = '../../../../net/tmp/tscott1/bosque_scratch/proj5/'
mod_base_priors = readRDS(paste0(floc,'basemod_hyperparameters.RDS'))
cox_dt = readRDS('scratch/proj5/full_panel_data.RDS')
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')
cox_dt <- cox_dt[District_Type=='MUD',]
cox_base = readRDS('scratch/proj5/base_inlacoxph_object.RDS')

#cox_dt[is.na(Yearly_Debt_Payment)&`BONDS OUTSTANDING`>0&!duplicated(District_Name),][,District_Name]
cox_dt$sub_index = 1:nrow(cox_dt)
cox_dt = cox_dt[Status == 'ACTIVE',]
cox_dt = cox_dt[!is.na(FiscalYear),]
find_ids = cox_dt[,all(!is.na(DOC_ID)),by=.(PWS_ID)][V1==T,]
cox_dt = cox_dt[PWS_ID%in%find_ids$PWS_ID,]

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



