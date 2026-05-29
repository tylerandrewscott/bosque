library(data.table)
library(tidyverse)
library(INLA)
library(lubridate)
floc = '../../../../net/tmp/tscott1/bosque_scratch/proj5/'
mod_base_priors = readRDS(paste0(floc,'basemod_hyperparameters.RDS'))
cox_dt = readRDS('scratch/proj5/full_panel_data.RDS')
cox_base = readRDS('scratch/proj5/base_inlacoxph_object.RDS')
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')
cox_dt <- cox_dt[District_Type=='MUD',]

#cox_dt[is.na(Yearly_Debt_Payment)&`BONDS OUTSTANDING`>0&!duplicated(District_Name),][,District_Name]
cox_dt$sub_index = 1:nrow(cox_dt)
find_ids = cox_dt[,all(!is.na(DOC_ID)),by=.(PWS_ID)][V1==T,]
cox_dt = cox_dt[PWS_ID%in%find_ids$PWS_ID,]

cox_dt = cox_dt[!grepl('CINCO|HARRIS COUNTY MUD 50[0:3]|HARRIS COUNTY MUD 321',PWS_NAME),]
cox_dt = cox_dt[!grepl('TRAVIS COUNTY MUD [0-9]$',District_Name),]

cox_dt <- cox_dt[PWS_ID %in% cox_dt$PWS_ID[cox_dt$Total_Revenue_Per_Connection>0],]
#table(ifelse(cox_dt$Yearly_Payment_Per_Connection<avgbill,'<1x',ifelse(cox_dt$Yearly_Payment_Per_Connection>3*avgbill,'>3x','1x-3x')))
restricted_form = update.formula(mod_base_priors$.args$formula, ~ . +
                                   asinh(Connections) +
                                   asinh(Median_Home_Value) +
                                   asinh(Storage_Per_Connection_G) +
                                   Has_Emergency_Interconnect +
                                   Wholesale +
                                   Groundwater +
                                   Purchaser +
                                   Urban_District +
                                   asinh(Median_Structure_Age) +
                                   # WCID + SUD + FWSD +
                                   asinh(KBDI_Avg) +
                                   asinh(Perc_College_Grad) +
                                   asinh(Perc_Nonwhite))

#restricted_form_mud_hazard = as.formula(gsub('scale.model = TRUE',
#                paste0('scale.model = TRUE,replicate = ','MUD_gr'),deparse(restricted_form)))
#cox_dt[is.na(Yearly_Payment_Per_Connection),][!duplicated(PWS_ID),.(District_Name,BONDS OUTSTANDING,Year)]
#cox_dt[,.(.N,sum(is.na(Yearly_Payment_Per_Connection))),by = .(PWS_ID)][N>V2,]


form_0 = restricted_form
form_financial = update.formula(form_0, ~ . + asinh(Total_Revenue_Per_Connection) + asinh(Fund_Balance_Per_Connection) + asinh(Debt_Per_Connection))
form_revenue_MOTax = update.formula(form_financial, ~. + MO_Tax + MO_Tax:asinh(Total_Revenue_Per_Connection))
form_debt_DebtTax = update.formula(form_financial, ~. + Debt_Tax + Debt_Tax:asinh(Debt_Per_Connection))
form_financial_interactions = update.formula(form_0, ~ . - asinh(Total_Revenue_Per_Connection) + asinh(Fund_Balance_Per_Connection) + asinh(Debt_Per_Connection) +
                                               asinh(Total_Revenue_Per_Connection)*asinh(Fund_Balance_Per_Connection)*asinh(Debt_Per_Connection))

fin_forms = list(form_0,form_financial,form_revenue_MOTax,form_debt_DebtTax,form_financial_interactions)

fin_mods = lapply(seq_along(fin_forms),function(x)inla(fin_forms[[x]],control.compute = list(waic=TRUE,dic=TRUE),
                                                       control.inla = list(int.strategy = "eb"),
                                                       family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),num.threads = 8,
                                                       E = cox_base$E[cox_dt$sub_index],verbose = TRUE,control.update = list(result = mod_base_priors)))
saveRDS(fin_mods,'../../../../net/tmp/tscott1/bosque_scratch/proj5/mod_object_list.RDS')


debt_range = asinh(seq(0,90000,500))#c(0,quantile(cox_dt$Debt_Per_Connection[cox_dt$Debt_Per_Connection>0],seq(0.05,0.95,0.05)))
rev_range =  asinh(seq(0,15000,250))#quantile(cox_dt$Total_Revenue_Per_Connection,seq(0.05,0.95,0.05))
#mo_tax_rate = c(0,quantile(cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`[cox_dt$`OPERATIONS & MAINTENANCE TAX RATE`>0],c(0.25,0.5,0.75)))
#debt_tax_rate = c(0,quantile(cox_dt$`DEBT SERVICE TAX RATE`[cox_dt$`DEBT SERVICE TAX RATE`>0],c(0.25,0.5,0.75)))

debt_combos =expand.grid(debt = debt_range,debt_tax = c(0,1))#debt_tax_rate)
rev_combos = expand.grid(rev = rev_range,mo_tax = c(0,1))#mo_tax_rate)

lc_debt_tax <- inla.make.lincombs(
  "asinh(Debt_Per_Connection)" = debt_combos$debt ,
  "Debt_Tax:asinh(Debt_Per_Connection" = debt_combos$debt_tax)

lc_rev_tax <- inla.make.lincombs(
  "asinh(Total_Revenue_Per_Connection)" = rev_combos$rev,
  "MO_Tax:asinh(Total_Revenue_Per_Connection)" = rev_combos$mo_tax)


mod_debt_tax_lc = inla(form_debt_DebtTax ,control.compute = list(waic=TRUE,dic=TRUE),control.predictor=list(compute=TRUE),
                       verbose=T,lincomb=lc_debt_tax,control.inla = list(lincomb.derived.only=FALSE,int.strategy = "eb"),
                       family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),E = cox_base$E[cox_dt$sub_index],control.update = list(result = mod_base_priors))
mod_rev_tax_lc = inla(form_revenue_MOTax,control.compute = list(waic=TRUE,dic=TRUE),control.predictor=list(compute=TRUE),
                      verbose=T,lincomb=lc_rev_tax,control.inla = list(lincomb.derived.only=FALSE,int.strategy = "eb"),
                      family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),E = cox_base$E[cox_dt$sub_index],control.update = list(result = mod_base_priors))

saveRDS(list(mod_debt_tax_lc,mod_rev_tax_lc),'../../../../net/tmp/tscott1/bosque_scratch/proj5/interaction_mod_object_list.RDS')





