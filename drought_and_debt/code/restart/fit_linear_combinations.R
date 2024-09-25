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
cox_dt$Usage_Per_Connection_P1[is.na(cox_dt$Usage_Per_Connection_P1)] <- 0
cox_dt$Debt_Per_Connection[is.na(cox_dt$Debt_Per_Connection)] <- 0
cox_dt <- cox_dt[PWS_ID %in% cox_dt$PWS_ID[cox_dt$Total_Revenue>0&cox_dt$Total_Expenditure>0],]
cox_dt = cox_dt[!grepl('CINCO|HARRIS COUNTY MUD 50[0:3]|HARRIS COUNTY MUD 321',PWS_NAME),]
cox_dt = cox_dt[!grepl('TRAVIS COUNTY MUD [0-9]$',District_Name),]
cox_dt <- cox_dt[PWS_ID %in% cox_dt$PWS_ID[cox_dt$Total_Revenue>0&cox_dt$Total_Expenditure>0],]
keep = cox_dt[,all(Total_Revenue>0)&all(Total_Expenditure>0),by=.(PWS_ID)][V1==T,]
cox_dt = cox_dt[PWS_ID %in% keep$PWS_ID,]

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
form_14 = update.formula(form_0, ~ . +
                           scale(asinh(Total_Revenue_Per_Connection),scale=F) * scale(asinh(Usage_Per_Connection_P1),scale=F) + 
                           scale(asinh(Debt_Per_Connection),scale=F) * scale(asinh(Median_Home_Value),scale=F)  + 
                           scale(asinh(Fund_Balance),scale=F) * scale(asinh(Connections),scale=F))

#[14] "scale(asinh(Usage_Per_Connection_P1), scale = F):scale(asinh(Total_Revenue_Per_Connection), scale = F)"
#[15] "scale(asinh(Debt_Per_Connection), scale = F):scale(asinh(Median_Home_Value), scale = F)"               
#[16] "scale(asinh(Connections), scale = F):scale(asinh(Fund_Balance), scale = F)"                            

fund_range = asinh(seq(-1000000,5000000,100000)) - mean(asinh(cox_dt$Fund_Balance))
debt_range = asinh(seq(0,90000,500)) - mean(asinh(cox_dt$Debt_Per_Connection))
rev_range =  asinh(seq(0,15000,250)) - mean(asinh(cox_dt$Total_Revenue_Per_Connection))

usage_type = asinh(quantile(cox_dt$Usage_Per_Connection_P1,c(0.2,0.8))) - mean(asinh(cox_dt$Usage_Per_Connection_P1))
home_type = asinh(quantile(cox_dt$Median_Home_Value,c(0.2,0.8))) - mean(asinh(cox_dt$Median_Home_Value))
size_type = asinh(quantile(cox_dt$Connections,c(0.2,0.8))) - mean(asinh(cox_dt$Connections))

debt_combos = expand.grid(debt = debt_range, home_type = home_type)
fund_combos = expand.grid(funds = fund_range, size_type = size_type)
rev_combos = expand.grid(revenue = rev_range, usage_type = usage_type)

lc_rev = inla.make.lincombs("scale(asinh(Total_Revenue_Per_Connection), scale = F)" = rev_combos$revenue,
                   "scale(asinh(Usage_Per_Connection_P1), scale = F):scale(asinh(Total_Revenue_Per_Connection), scale = F)" = rev_combos$revenue * rev_combos$usage_type)
lc_debt = inla.make.lincombs("scale(asinh(Debt_Per_Connection), scale = F)" = debt_combos$debt,
                   "scale(asinh(Debt_Per_Connection), scale = F):scale(asinh(Median_Home_Value), scale = F)" = debt_combos$debt * debt_combos$home_type)
lc_fund = inla.make.lincombs("scale(asinh(Fund_Balance), scale = F)" =fund_combos$funds,
                   "scale(asinh(Connections), scale = F):scale(asinh(Fund_Balance), scale = F)" = fund_combos$funds * fund_combos$size_type)

lc_all = inla.make.lincombs(lc_rev,lc_debt,lc_fund)
mod_14_lc = inla(form_14 ,control.compute = list(waic=TRUE,dic=TRUE),control.predictor=list(compute=TRUE),
                       verbose=T,lincomb=lc_all,control.inla = list(lincomb.derived.only=FALSE,int.strategy = "eb"),
                        family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),E = cox_base$E[cox_dt$sub_index],control.update = list(result = mod_base_priors))


# #mo_tax_rate = c(0,quantile(cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`[cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`>0],c(0.25,0.5,0.75)))
# #debt_tax_rate = c(0,quantile(cox_dt$`DEBT SERVICE TAX RATE`[cox_dt$`DEBT SERVICE TAX RATE`>0],c(0.25,0.5,0.75)))
# 
# debt_combos =expand.grid(debt = debt_range,debt_tax = c(0,1))#debt_tax_rate)
# rev_combos = expand.grid(rev = rev_range,mo_tax = c(0,1))#mo_tax_rate)
# 
# lc_debt_tax <- inla.make.lincombs(
#   "scale(asinh(Debt_Per_Connection), scale = F)" = debt_combos$debt ,
#   "Debt_Tax:scale(asinh(Debt_Per_Connection), scale = F)" = debt_combos$debt_tax)
# 
# lc_rev_tax <- inla.make.lincombs(
#   "scale(asinh(Operating_Ratio), scale = F)" = rev_combos$rev,
#   "MO_Tax:scale(asinh(Operating_Ratio), scale = F)" = rev_combos$mo_tax)
# 
# 
# mod_debt_tax_lc = inla(form_debt_DebtTax ,control.compute = list(waic=TRUE,dic=TRUE),control.predictor=list(compute=TRUE),
#                        verbose=T,lincomb=lc_debt_tax,control.inla = list(lincomb.derived.only=FALSE,int.strategy = "eb"),
#                        family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),E = cox_base$E[cox_dt$sub_index],control.update = list(result = mod_base_priors))
# mod_rev_tax_lc = inla(form_revenue_MOTax,control.compute = list(waic=TRUE,dic=TRUE),control.predictor=list(compute=TRUE),
#                       verbose=T,lincomb=lc_rev_tax,control.inla = list(lincomb.derived.only=FALSE,int.strategy = "eb"),
#                       family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),E = cox_base$E[cox_dt$sub_index],control.update = list(result = mod_base_priors))
# 
# saveRDS(list(mod_debt_tax_lc,mod_rev_tax_lc),'../../../../net/tmp/tscott1/bosque_scratch/proj5/interaction_mod_object_list.RDS')


