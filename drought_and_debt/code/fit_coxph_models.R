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

library(rgeos)
require(spdep)


cox_dt = readRDS('scratch/data_for_coxph_model.RDS')
start_date = mdy('5/4/2010')
start_date_dec = decimal_date(start_date)
end_date = mdy('7/7/2015')

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


#Number of observations in each dataset
n.l <- nrow(cox_dt)
n.s <- nrow(cox_dt[Event==1,])
#Vector of NA's
NAs.l <- rep(NA, n.l)
NAs.s <- rep(NA, n.s)
#Vector of zeros
zeros.l <- rep(0, n.l)
zeros.s <- rep(0, n.s)




#Long. and survival responses
Y.long <- c(cox_dt$Event, NAs.s)
Y.surv <- inla.surv(time = c(NAs.l, cox_dt$Time[cox_dt$Event==1]),
                    event = c(NAs.l, cox_dt$Event[cox_dt$Event==1]))

Y.surv <- inla.surv(time = cox_dt$Time[cox_dt$Event==1],
                    event =cox_dt$Event[cox_dt$Event==1])
Y.coxph = inla.coxph(Y.surv~1,data = Y.surv,control.hazard = list(model = 'rw1',constr=T,n.intervals = 1,scale.model = T))

class(Y.coxph)

Y.joint <- list(Y.long, Y.coxph)

library(tigris)
require(sp)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
tx_county_sp = tigris::counties(state = 48,class = 'sp')
tx_county_sp = spTransform(tx_county_sp,CRS(albersNA))

#Create adjacency matrix
tx.nb <- poly2nb(tx_county_sp,row.names = tx_county_sp$GEOID,queen=F)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("tx.adj", tx.nb)
g <- inla.read.graph(tx.nb)


#Indices for random effects
r.effects <- list(
  #Patiend id (long.)
  id.l = c(cox_dt$Primary_CFIPS, NAs.s),
  #Patient id (surv.)
  id.s = c(NAs.l, cox_dt$Primary_CFIPS[cox_dt$Event==1]),
  icar.l = c(match(cox_dt$CFIPS_ID,tx_county_sp$GEOID),NAs.s),
  icar.s = c(NAs.l,match(cox_dt$CFIPS_ID,tx_county_sp$GEOID)[cox_dt$Event==1]))

cox_dt$std_Perc_Nonwhite <- scale(cox_dt$Perc_Nonwhite)
cox_dt$std_Perc_Owner_Occupied <- scale(cox_dt$Prop_Owner_Occupied*100)
cox_dt$std_ln_Connection <- scale(log(cox_dt$Connections+1))
cox_dt$std_ln_Median_Home_Value <- scale(log(cox_dt$Median_Home_Value+1))
cox_dt$std_Median_Structure_Age <- scale(-cox_dt$Median_Year_Structure_Built)
cox_dt$std_ln_Storage_Per_Connection_G <- scale(log(cox_dt$Storage_Per_Connection_G+0.1))
cox_dt$Debt_Per_Connection[is.na(cox_dt$Debt_Per_Connection)]<-0
median_debt = quantile(cox_dt$Debt_Per_Connection[!cox_dt$District_Type%in%c('Not a district','Other')&cox_dt$Debt_Per_Connection>0],0.5)

rev_quantiles = quantile(cox_dt$Total_Revenue_Per_Connection[!cox_dt$District_Type%in%c('Not a district','Other')&cox_dt$Total_Revenue_Per_Connection>0],c(0.25,0.75),na.rm = T)
fund_quantiles = quantile(cox_dt$Fund_Balance_Per_Connection[!cox_dt$District_Type%in%c('Not a district','Other')&cox_dt$Fund_Balance_Per_Connection>0],c(0.25,0.75),na.rm = T)

cox_dt$Debt_Group[cox_dt$Debt_Per_Connection==0]<-'No.debt'
cox_dt$Debt_Group[cox_dt$Debt_Per_Connection>0&cox_dt$Debt_Per_Connection<median_debt]<-'Low.debt'
cox_dt$Debt_Group[cox_dt$Debt_Per_Connection>=median_debt]<-'High.debt'

cox_dt$Rev_Group[cox_dt$Total_Revenue_Per_Connection<rev_quantiles[1]]<-'Low.rev'
cox_dt$Rev_Group[cox_dt$Total_Revenue_Per_Connection>=rev_quantiles[2]]<-'High.rev'
cox_dt$Rev_Group[is.na(cox_dt$Rev_Group)] <- 'Middle.rev'

cox_dt$Fund_Group[cox_dt$Fund_Balance_Per_Connection<rev_quantiles[1]]<-'Low.fund'
cox_dt$Fund_Group[cox_dt$Fund_Balance_Per_Connection>=rev_quantiles[2]]<-'High.fund'
cox_dt$Fund_Group[is.na(cox_dt$Fund_Group)] <- 'Middle.fund'


cox_dt = merge(cox_dt,Reduce(merge,
                             list(dcast(cox_dt[!District_Type%in%c('Not a district','Other')&!is.na(common),
                                               .(PWS_ID,common,Debt_Group)],PWS_ID + common ~ Debt_Group,value.var = 'Debt_Group',fill = 0,
                                        fun.aggregate = function(x) as.integer(!is.na(x))),
                                  dcast(cox_dt[!District_Type%in%c('Not a district','Other')&!is.na(common),
                                               .(PWS_ID,common,Rev_Group)],PWS_ID + common ~ Rev_Group,value.var = 'Rev_Group',fill = 0,
                                        fun.aggregate = function(x) as.integer(!is.na(x))),
                                  dcast(cox_dt[!District_Type%in%c('Not a district','Other')&!is.na(common),
                                               .(PWS_ID,common,Fund_Group)],PWS_ID + common ~ Fund_Group,value.var = 'Fund_Group',fill = 0,
                                        fun.aggregate = function(x) as.integer(!is.na(x))))),
               ,all.x = T,by= c('common','PWS_ID'))


corm = round(cor(cox_dt[!is.na(cox_dt$DOC_ID) & !District_Type %in% c('Not a district','Other'),
                        .(Total_Debt_Service_Outstanding,Total_Revenue,Fund_Balance,Debt_Per_Connection,Total_Revenue_Per_Connection,Fund_Balance_Per_Connection)],
                 use = 'pairwise.complete.obs'),3)

require(corrplot)
corrplot(corm, type = "upper",diag = F,method = 'number',
         title = 'Correlation of financial variables',tl.col = 'black'
)

covariates <- data.frame(
  #Intercepts (as factor with 2 levels)
  inter = as.factor(c(rep("b.l", n.l), rep("b.s", n.s))),
  connections.l = c(cox_dt$std_ln_Connection, NAs.s),
  connections.s = c(NAs.l, cox_dt$std_ln_Connection[cox_dt$Event==1]),
  storage.l = c(cox_dt$std_ln_Storage_Per_Connection_G, NAs.s),
  storage.s = c(NAs.l, cox_dt$std_ln_Storage_Per_Connection_G[cox_dt$Event==1]),
  home.l = c(cox_dt$std_ln_Median_Home_Value, NAs.s),
  home.s = c(NAs.l, cox_dt$std_ln_Median_Home_Value[cox_dt$Event==1]),
  nonwhite.l = c(cox_dt$std_Perc_Nonwhite, NAs.s),
  nonwhite.s = c(NAs.l, cox_dt$std_Perc_Nonwhite[cox_dt$Event==1]),
  build.l = c(cox_dt$std_Median_Structure_Age, NAs.s),
  build.s = c(NAs.l, cox_dt$std_Median_Structure_Age[cox_dt$Event==1]),
  purchaser.l = c(cox_dt$Purchaser, NAs.s),
  purchaser.s = c(NAs.l, cox_dt$Purchaser[cox_dt$Event==1]),
  wholesaler.l = c(cox_dt$Wholesale, NAs.s),
  wholesaler.s = c(NAs.l, cox_dt$Wholesale[cox_dt$Event==1]),
  groundwater.l = c(cox_dt$Groundwater, NAs.s),
  groundwater.s = c(NAs.l, cox_dt$Groundwater[cox_dt$Event==1]),
  interconnect.l = c(cox_dt$Has_Emergency_Interconnect , NAs.s),
  interconnect.s = c(NAs.l, cox_dt$Has_Emergency_Interconnect[cox_dt$Event==1]),
  municipal.l = c(cox_dt$Municipal , NAs.s),
  municipal.s = c(NAs.l, cox_dt$Municipal[cox_dt$Event==1]),
  wsc.l = c(cox_dt$WSC , NAs.s),
  wsc.s = c(NAs.l, cox_dt$WSC[cox_dt$Event==1]),
  private.l = c(cox_dt$Private , NAs.s),
  private.s = c(NAs.l, cox_dt$Private[cox_dt$Event==1]),
  iou.l = c(cox_dt$Investor_Owned, NAs.s),
  iou.s = c(NAs.l, cox_dt$Investor_Owned[cox_dt$Event==1]),
  debt0.l = c(cox_dt$No.debt,NAs.s),
  debt0.s = c(NAs.l,cox_dt$No.debt[cox_dt$Event==1]),
  debt_low.l =  c(cox_dt$Low.debt,NAs.s),
  debt_low.s = c(NAs.l,cox_dt$Low.debt[cox_dt$Event==1]),
  debt_high.l =  c(cox_dt$High.debt,NAs.s),
  debt_high.s = c(NAs.l,cox_dt$High.debt[cox_dt$Event==1]),
  rev_low.l =  c(cox_dt$Low.rev,NAs.s),
  rev_low.s = c(NAs.l,cox_dt$Low.rev[cox_dt$Event==1]),
  rev_med.l =  c(cox_dt$Middle.rev,NAs.s),
  rev_med.s = c(NAs.l,cox_dt$Middle.rev[cox_dt$Event==1]),
  rev_high.l =  c(cox_dt$High.rev,NAs.s),
  rev_high.s = c(NAs.l,cox_dt$High.rev[cox_dt$Event==1]),
  fund_low.l =  c(cox_dt$Low.fund,NAs.s),
  fund_low.s = c(NAs.l,cox_dt$Low.fund[cox_dt$Event==1]),
  fund_med.l =  c(cox_dt$Middle.fund,NAs.s),
  fund_med.s = c(NAs.l,cox_dt$Middle.fund[cox_dt$Event==1]),
  fund_high.l =  c(cox_dt$High.fund,NAs.s),
  fund_high.s = c(NAs.l,cox_dt$High.fund[cox_dt$Event==1]),
  fwsd.l =  c(as.integer(cox_dt$District_Type=='FWSD'),NAs.s),
  fwsd.s = c(NAs.l,as.integer(cox_dt$District_Type=='FWSD')[cox_dt$Event==1]),
  wcid.l =  c(as.integer(cox_dt$District_Type=='WCID'),NAs.s),
  wcid.s = c(NAs.l,as.integer(cox_dt$District_Type=='WCID')[cox_dt$Event==1]),
  sud.l =  c(as.integer(cox_dt$District_Type=='SUD'),NAs.s),
  sud.s = c(NAs.l,as.integer(cox_dt$District_Type=='SUD')[cox_dt$Event==1]),
appointed.l =  c(as.integer(cox_dt$BOARD_SELECTION=='Appointed'),NAs.s),
  appointed.s = c(NAs.l,as.integer(cox_dt$BOARD_SELECTION=='Appointed')[cox_dt$Event==1]),
  id.check = c(cox_dt$District_ID,cox_dt$District_ID[cox_dt$Event==1]))

joint.data  <- c(covariates, r.effects)
joint.data$Y <- Y.joint

inla(formula =Y[[2]] ~ -1 + inter +
       f(id.s, model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))) + 
       f(id.l,model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))),data = joint.data,
     family = c( "coxph"),
     #  control.hazard = list(model = 'rw1',constr=T,n.intervals = 30,scale.model = T),
     control.compute = list(waic=T,dic=T,cpo = T))



joint.inla <- inla(formula =Y ~ -1 + inter +
                      f(id.s, model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))) + 
                      f(id.l,model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))),data = joint.data,
                   family = c("zeroinflatedbinomial1", "coxph"),Ntrials = 1,
                 #  control.hazard = list(model = 'rw1',constr=T,n.intervals = 30,scale.model = T),
                   control.compute = list(waic=T,dic=T,cpo = T))


joint.inla1 <- inla(Y ~ -1 + inter +
                     f(id.s, model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))) + 
                     f(id.l,model = "iid",hyper = list(prec = list(param = c(0.001, 0.001)))) + 
                     connections.l + connections.s + 
                     wholesaler.l + wholesaler.s +
                     home.l + home.s + #build.s + build.l +
                     storage.l + storage.s +
                     groundwater.l + groundwater.s + 
                     purchaser.l + purchaser.s + 
                     interconnect.l + interconnect.s + 
                     municipal.l + municipal.s + private.l + private.s + iou.l + iou.s + wsc.l + wsc.s,
                   data = joint.data, family = c("zeroinflatedbinomial1", "lognormal.surv"),Ntrials = 1,
                   control.compute = list(waic=T,dic=T,cpo = T))


joint.inla0$waic$waic
joint.inla1$waic$waic
# Grid of times
times <- seq(0.01, max(cox_dt$Time), by = 0.05)

# Marginal of alpha
alpha.marg <- joint.inla$marginals.fixed[["interb.s"]]
alpha.marg <- joint.inla1$marginals.fixed[["interb.s"]]
# Compute post. marg. of survival function
S.inla <- mclapply(times, function(t){
  S.marg <- inla.tmarginal(fun = function(x) {exp(- exp(x) * t)}, marginal = alpha.marg)
  S.stats <- inla.zmarginal(S.marg, silent = TRUE)
  return(unlist(S.stats[c("quant0.025", "mean", "quant0.975")]))
})
S.inla <- as.data.frame(do.call(rbind, S.inla))
S.inla$time = times
ggplot(S.inla) + geom_ribbon(aes(x = time,ymin = quant0.025,ymax = quant0.975),fill = 'grey50') +
  geom_path(aes(x = time,y = mean),col = 'red') + theme_bw() 

surv_object = Surv(time = cox_dt$Time[cox_dt$Event==1])
km.curv <- survfit(surv_object ~ 1, data = cox_dt[Event==1,])
km_owner.curv <- survfit(surv_object ~ -1 + Owner_Type , data = cox_dt[Event==1,])
require(ggfortify)
autoplot(km_owner.curv) + theme_bw()

summary(cox_dt$Time[cox_dt$Event==1])
plot(km_owner.curv)
class(km_owner.curv)
?autoplot
table(cox_dt$Owner_Type)
max(cox_dt$Time)

joint.inla2 <- inla(formula = Y ~ -1 + inter,data = joint.data,
                  control.hazard = list(model = 'rw1',constr=T,n.intervals = 30,scale.model = T),
                   family = c("zeroinflatedbinomial1", "lognormal.surv"),Ntrials = 1,
                   control.compute = list(waic=T,dic=T,cpo = T))

joint.inla2$marginals.fixed$interb.s



joint.inla$marginals.fixed


library("parallel")
# Set number of cores to use
options(mc.cores = 4)
# Grid of times



times <- seq(decimal_date(start_date),decimal_date(end_date),0.01)
times = times - decimal_date(start_date)
# Marginal of alpha

alpha.marg <- joint.inla$marginals.fixed[["interb.s"]]
alpha.marg
# Compute post. marg. of survival function
S.inla <- mclapply(times, function(t){
  t = times[1]
  S.marg <- inla.tmarginal(function(x) {exp(- exp(x) * t)}, alpha.marg)
  S.stats <- inla.zmarginal(S.marg, silent = TRUE)
  return(unlist(S.stats[c("quant0.025", "mean", "quant0.975")]))
})


S.marg
# Compute post. marg. of survival function
S.inla <- lapply(times, function(t){

  sapply(times,function(x) exp(- exp(x) * t))
  S.marg <- inla.tmarginal(function(x) {exp(- exp(x) * t)}, alpha.marg)
  S.stats <- inla.zmarginal(S.marg, silent = TRUE)
  
  return(unlist(S.stats[c("quant0.025", "mean", "quant0.975")]))})

S.inla <- as.data.frame(do.call(rbind, S.inla))

for(t in times){
  S.marg <- inla.tmarginal(function(x) {exp(-exp(x) * t)}, alpha.marg)
  S.stats <- inla.zmarginal(S.marg, silent = TRUE)
}

#district_index = c(which(!cox_dt$District_Type%in%c('Not a district','Other')),length(cox_dt$District_ID)+which(!cox_dt$District_Type[cox_dt$Event==1]%in%c('Not a district','Other')))

district_index  = c(which(cox_dt$District_Type%in%c('MUD','FWSD','WCID','SUD') & cox_dt$Status=='ACTIVE' & !is.na(cox_dt$FiscalYear) & !is.na(cox_dt$DOC_ID) & cox_dt$Connections > 0),
                    length(cox_dt$District_ID) + which({cox_dt$District_Type%in%c('MUD','FWSD','WCID','SUD')  & cox_dt$Status=='ACTIVE' & !is.na(cox_dt$FiscalYear) & !is.na(cox_dt$DOC_ID) & cox_dt$Connections > 0}[cox_dt$Event==1])) 
district_index  = c(which(cox_dt$District_Type%in%c('MUD') & cox_dt$Status=='ACTIVE' & !is.na(cox_dt$FiscalYear) & !is.na(cox_dt$DOC_ID) & cox_dt$Connections > 0),
                    length(cox_dt$District_ID) + which({cox_dt$District_Type%in%c('MUD')  & cox_dt$Status=='ACTIVE' & !is.na(cox_dt$FiscalYear) & !is.na(cox_dt$DOC_ID) & cox_dt$Connections > 0}[cox_dt$Event==1])) 


district_covariates = covariates[district_index,]
district_r.effects = lapply(r.effects,function(x) x[district_index])

district_Y.long <- c(cox_dt$Event, NAs.s)[district_index]
district_Y.surv <- inla.surv(time = c(NAs.l, cox_dt$Time[cox_dt$Event==1])[district_index],
                             event = c(NAs.l, cox_dt$Event[cox_dt$Event==1])[district_index])
district_Y.joint <- list(district_Y.long, district_Y.surv)

district_joint.data  <- c(district_covariates, district_r.effects)
district_joint.data$Y <- district_Y.joint

mud_form = update.formula(joint.inla$.args$formula,  ~ . - private.l - private.s - wsc.l - wsc.s - iou.l - iou.s - municipal.l - municipal.s + fwsd.l + fwsd.s + wcid.l + wcid.s + sud.l + sud.s + board.l + board.s)
mud_debt_form = update.formula(mud_form, ~ . + debt_low.l + debt_low.s + debt_high.l + debt_high.s)
mud_rev_form = update.formula(mud_form, ~ . + rev_med.l  + rev_med.s + rev_high.l + rev_high.s)
mud_fund_form = update.formula(mud_form, ~ . + fund_med.l  + fund_med.s + fund_high.l + fund_high.s)

mud_all_form = update.formula(mud_form, ~ . +  debt_low.l + debt_low.s + debt_high.l + debt_high.s + 
                                rev_med.l  + rev_med.s + rev_high.l + rev_high.s)
#fund_med.l  + fund_med.s + fund_high.l + fund_high.s)


test = cox_dt[!District_Type%in%c('Not a district','Other')]
cor(test[,.(Total_Revenue,Fund_Balance,Total_Revenue_Per_Connection,Total_Debt_Service_Outstanding,Debt_Per_Connection,Fund_Balance_Per_Connection)], use = 'pairwise.complete.obs'),3)

mud_mod  = inla(mud_form,control.compute = list(waic=TRUE,dic=TRUE,cpo= TRUE),
                data=district_joint.data,num.threads = 8, 
                family = c("zeroinflatedbinomial1", "lognormalsurv"),Ntrials = 1,
                verbose = F,control.update = list(result =joint.inla))
mud_mod_rev  = inla(mud_rev_form,control.compute = list(waic=TRUE,dic=TRUE,cpo= TRUE),
                    data=district_joint.data,num.threads = 8, 
                    family = c("zeroinflatedbinomial1", "lognormalsurv"),Ntrials = 1,
                    verbose = F,control.update = list(result =joint.inla))
mud_mod_debt  = inla(mud_debt_form,control.compute = list(waic=TRUE,dic=TRUE,cpo= TRUE),
                     data=district_joint.data,num.threads = 8, 
                     family = c("zeroinflatedbinomial1", "lognormalsurv"),Ntrials = 1,
                     verbose = F,control.update = list(result =joint.inla))
mud_mod_fund  = inla(mud_fund_form,control.compute = list(waic=TRUE,dic=TRUE,cpo= TRUE),
                     data=district_joint.data,num.threads = 8, 
                     family = c("zeroinflatedbinomial1", "lognormalsurv"),Ntrials = 1,
                     verbose = F,control.update = list(result =joint.inla))
mud_mod_all  = inla(mud_all_form,control.compute = list(waic=TRUE,dic=TRUE,cpo= TRUE),
                    data=district_joint.data,num.threads = 8, 
                    family = c("zeroinflatedbinomial1", "lognormalsurv"),Ntrials = 1,
                    verbose = F,control.update = list(result =joint.inla))
summary(mud_mod_all)



mud_mod$waic$waic
mud_mod_rev$waic$waic
mud_mod_debt$waic$waic
mud_mod_fund$waic$waic
mud_mod_all$waic$waic

table(cox_dt$BOARD_SELECTION)
summary(mud_mod_all)

summary(mud_mod)
summary(mud_mod_debt)
mud_mod$waic$waic
mud_mod_debt$waic$waic

summary(mud_mod_debt)

dcast(cox_dt[!District_Type%in%c('Not a district','Other'),.(PWS_ID,common,Debt_Group)], common ~ Debt_Group)

test[Year %in% 2010:2015&grepl('MUD 387',Organization),]


dcast(cox_dt[!District_Type%in%c('Not a district','Other')&!is.na(common),.(PWS_ID,common,Debt_Group)], common ~ Debt_Group)
<- merge(cox_dt,dcast(cox_dt[!District_Type%in%c('Not a district','Other')&!is.na(common),.(PWS_ID,common,Debt_Group)], common ~ Debt_Group),by='common')


mud_debt_form = update.formula(mud_form,  ~ . + f(inla.group(scale(log(Debt_Per_Connection+1),scale=F),n = 4),model = 'rw1'))
mud_debt_form = update.formula(mud_form,  ~ . + d0 + d5 + d10)




joint.inla$waic$waic


f(id.s, model = "iid",
  hyper = list(prec = list(param = c(0.001, 0.001)))) + 
  
  summary(joint.inla)

class(joint.data$Y[[2]])
> joint.inla$waic$waic
[1] 5523.158

joint.inla$waic$waic

joint.inla$waic$waic
f(id, model = "iid",
  hyper = list(prec = list(param = c(0.001, 0.001)
                           
                           summary(joint.inla)
                           
                           base_form = Event ~ -1 + intercept + 
                             scale(log(Connections+1),scale=F) +
                             scale(log(Median_Home_Value+1),scale=F) +
                             scale(log(Storage_Per_Connection_G+0.1),scale=F) +
                             Has_Emergency_Interconnect +
                             Groundwater +
                             Purchaser +
                             Municipal + Private + Investor_Owned + WSC +
                             f(Primary_CFIPS,model="iid",hyper = list(prec = list(param=c(1, 1))))
                           
                           mod1A = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE), 
                                        control.inla = list(diagonal = 1000,  int.strategy = "eb"),
                                        family='binomial',data=cox_dt,verbose = F,num.threads=8)
                           
                           mod1B = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
                                        control.inla = list(diagonal = 100,  int.strategy = "eb"),
                                        control.mode = list(result = modA, restart = TRUE),
                                        family='binomial',data=cox_dt,verbose = F,num.threads=8)
                           
                           mod1C = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
                                        control.inla = list(diagonal = 10,  int.strategy = "eb"),
                                        control.mode = list(result = modB, restart = TRUE),
                                        family='binomial',data=cox_dt,verbose = F,num.threads=8)
                           
                           mod1D = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
                                        control.inla = list(diagonal = 1,  int.strategy = "eb"),
                                        control.mode = list(result = modC, restart = TRUE),
                                        family='binomial',data=cox_dt,verbose = F,num.threads=8)
                           
                           mod1E = inla(base_form,#control.compute = list(waic=TRUE,dic=TRUE),
                                        control.inla = list(int.strategy = "eb") ,
                                        control.mode = list(result = modD, restart = TRUE),
                                        family='binomial',data=cox_dt,verbose = F,num.threads=8)
                           gc()
                           
                           
                           mod1_base = inla(base_form, control.mode = list(result = modE, restart = TRUE),
                                            family='coxph',data=cox_dt,verbose = F,num.threads=8)
                           
                           
                           
                           
                           
                           
                           cox_dt_event_only = cox_dt[Event==1,]
                           
                           
                           base_form = inla.surv(time = Time,event = Event) ~ -1 + intercept + 
                             scale(log(Connections+1),scale=F) +
                             scale(log(Median_Home_Value+1),scale=F) +
                             scale(log(Storage_Per_Connection_G+0.1),scale=F) +
                             Has_Emergency_Interconnect +
                             #Wholesale +
                             Groundwater +
                             Purchaser +
                             #Urban_District +
                             #Median_Structure_Age +
                             Municipal + Private + Investor_Owned + WSC +
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
                           
                           # cox_mud = cox_dt[!District_Type %in%c('Other','Not a district'),] 
                           # cox_mud = cox_mud[Status == 'ACTIVE',]
                           # cox_mud = cox_mud[!is.na(FiscalYear),]
                           # cox_mud = cox_mud[!is.na(DOC_ID),]
                           # cox_mud = cox_mud[Connections>0,]
                           
                           #quantile(cox_mud$Debt_Per_Connection,seq(0,1,0.05),na.rm = T)
                           #cox_mud[Debt_Per_Connection>70e3,.(Connections,Debt_Per_Connection,Total_Principal_Outstanding,District_Connections,common)]
                           
                           
                           mud_form = update.formula(base_form,  ~ . - Municipal - Private - Investor_Owned - WSC,scale=F)
                           mud_mod  = inla(mud_form,control.compute = list(waic=TRUE,dic=TRUE),
                                           family='coxph',data=cox_mud,num.threads = 8, 
                                           verbose = F,control.update = list(result = mod_base))
                           cox_mud[Debt_Per_Connection>700000,.(Connections,Debt_Per_Connection,Total_Principal_Outstanding,District_Connections)]
                           
                           
                           cox_mud$Debt_Per_Connection[is.na(cox_mud$Debt_Per_Connection)]<-0
                           cox_mud$Debt = ifelse(cox_mud$Debt_Per_Connection<=0,'No debt','Debt')
                           cox_mud$Debt_Group[cox_mud$Debt_Per_Connection==0]<-'No debt'
                           cox_mud$Debt_Group[cox_mud$Debt_Per_Connection>0&cox_mud$Debt_Per_Connection<5e3]<-'d0'
                           cox_mud$Debt_Group[cox_mud$Debt_Per_Connection>=5e3&cox_mud$Debt_Per_Connection<10e3]<-'d5'
                           cox_mud$Debt_Group[cox_mud$Debt_Per_Connection>=10e3]<-'d10'
                           
                           cox_mud <- merge(cox_mud,dcast(cox_mud[,.(PWS_ID,common,Debt_Group)], common ~ Debt_Group),by='common')
                           
                           mud_debt_form = update.formula(mud_form,  ~ . + f(inla.group(scale(log(Debt_Per_Connection+1),scale=F),n = 4),model = 'rw1'))
                           mud_debt_form = update.formula(mud_form,  ~ . + d0 + d5 + d10)
                           
                           adopt_form =  Event ~ -1 + intercept + 
                             scale(log(Connections+1),scale=F) +
                             scale(log(Median_Home_Value+1),scale=F) +
                             scale(log(Storage_Per_Connection_G+0.1),scale=F) +
                             Has_Emergency_Interconnect +
                             Groundwater +
                             Purchaser +
                             f(Primary_CFIPS,model="iid",hyper = list(prec = list(param=c(1, 1))))
                           
                           
                           
                           
                           n = nrow(cox_mud)
                           dlist = list(Y = data.table(NA,ncol = 2,nrow = nrow(cox_mud)*2))
                           dlist$Y[1:nrow(cox_mud),1] <- cox_mud$Event
                           dlist$Y[nrow(cox_mud) + (1:nrow(cox_mud)),2] <- inla.surv(time = cox_mud$Time,event = cox_mud$Event)
                           
                           str(dlist$Y)
                           
                           dlist$u_std_ln_Connections = c(scale(log(cox_mud$Connections+1)),rep(NA,n))
                           dlist$u_std_ln_Connections = c(scale(log(cox_mud$Connections+1)),rep(NA,n))
                           
                           log_mod  = inla(adopt_form,control.compute = list(waic=TRUE,dic=TRUE),
                                           family='binomial',data=cox_mud,num.threads = 8, 
                                           control.fixed = list(expand.factor.strategy="model.matrix"),
                                           verbose = F,control.update = list(result = mod_base))
                           
                           summary(log_mod)
                           
                           
                           debt_mod  = inla(mud_debt_form,control.compute = list(waic=TRUE,dic=TRUE),
                                            family='coxph',data=cox_mud,num.threads = 8, 
                                            control.fixed = list(expand.factor.strategy="model.matrix"),
                                            verbose = F,control.update = list(result = mod_base))
                           
                           
                           test = debt_mod$summary.fixed
                           test$ID <- rownames(debt_mod$summary.fixed)
                           ggplot(test) + geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`,y = ID),
                                                         alpha = 0.5,col = 'grey40')+
                             geom_point(aes(y = ID,x = mean))
                           
                           
                           theme_bw() + xlab('Standard deviation of ln(debt/connection)') + 
                             ylab('Additive log odds') + 
                             ggtitle('Predicted change in hazard rate','debt outstanding/connection')
                           
                           summary(debt_mod)
                           
                           
                           scale(asinh(Fund_Balance),scale=F))
               
               
               
               
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
               
               
               summary(fiscal_mod3)
               
               
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
               
               
               
               