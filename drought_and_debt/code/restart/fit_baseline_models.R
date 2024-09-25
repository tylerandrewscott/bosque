library(data.table)
library(tidyverse)
library(lubridate)
library(INLA)
cox_dt = readRDS('scratch/proj5/full_panel_data.RDS')
cox_base = readRDS('scratch/proj5/base_inlacoxph_object.RDS')
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')

# restricted_form = update.formula(cox_base$formula, ~ . + 
#                                    scale(asinh(Connections),scale=F) + 
#                                    scale(asinh(Median_Home_Value),scale=F) + 
#                                    scale(asinh(Perc_Nonwhite),scale=F) + 
#                                    scale(asinh(Perc_College_Grad),scale=F) +
#                                    scale(asinh(Storage_Per_Connection_G),scale=F) +
#                                    Has_Emergency_Interconnect +
#                                    Wholesale +
#                                    Groundwater +
#                                    Purchaser + 
#                                    Urban_District + 
#                                    scale(asinh(LCV_lifetime),scale=F))

modA = inla(cox_base$formula,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(diagonal = 1000,  int.strategy = "eb"),
            family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),
            E = cox_base$E,verbose = F,num.threads=8)
gc()

modB = inla(cox_base$formula,#control.compute = list(waic=TRUE,dic=TRUE),
            control.inla = list(int.strategy = "eb",diagonal = 100),
             control.mode = list(result = modA, restart = TRUE),
             family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),
             E = cox_base$E,verbose = F,num.threads=8)
gc()
modC = inla(cox_base$formula,#control.compute = list(waic=TRUE,dic=TRUE),
             control.inla = list(diagonal = 10,  int.strategy = "eb") ,
             control.mode = list(result = modB, restart = TRUE),
             family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),
             E = cox_base$E,verbose = F,num.threads=8)
 gc()
 
modD = inla(cox_base$formula,#control.compute = list(waic=TRUE,dic=TRUE),
             control.inla = list(diagonal = 1, int.strategy = "eb") ,
            control.mode = list(result = modC, restart = TRUE),
            family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),
             E = cox_base$E,verbose = F,num.threads=8)
gc()


modE = inla(cox_base$formula,#control.compute = list(waic=TRUE,dic=TRUE),
                                   control.inla = list(int.strategy = "eb") ,
                                  control.mode = list(result = modD, restart = TRUE),
                                  family=cox_base$family,data=c(as.list(cox_dt),cox_base$data.list),
                                   E = cox_base$E,verbose = F,num.threads=8)
gc()
 
saveRDS(modE,'../../../../net/tmp/tscott1/bosque_scratch/proj5/basemod_hyperparameters.RDS')
 

 
 

