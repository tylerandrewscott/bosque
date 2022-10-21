library(data.table)
library(tidyverse)

library(INLA)
inla.setOption(pardiso.license="~/Documents/pardiso.lic")
inla.pardiso.check()
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


months = length(seq.Date(from = mdy('05/04/2010'),to = mdy('07/07/2015'),by = 'months'))

cox_dt$Owner_Type = as.factor(cox_dt$Owner_Type)
cox_dt$Municipal = (cox_dt$Owner_Type=='Municipality')+0
cox_dt$Investor_Owned = (cox_dt$Owner_Type=='Investor Owned')+0
cox_dt$Private = (cox_dt$Owner_Type=='Private')+0
cox_dt$WSC= (cox_dt$Owner_Type=='Water Supply Corporation')+0
#summary(cox_dt$Connections)
#cox_dt$PopulationType_Population_Total

cox_dt$Median_Structure_Age[is.na(cox_dt$Median_Structure_Age)] <- median(cox_dt$Median_Structure_Age,na.rm=T)

library(tigris)
require(sp)
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
tx_county_sp = tigris::counties(state = 48,class = 'sp',year = 2018)
tx_county_sp = spTransform(tx_county_sp,CRS(albersNA))

#Create adjacency matrix
tx.nb <- poly2nb(tx_county_sp,row.names = tx_county_sp$GEOID,queen=T)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("tx.adj", tx.nb)
g <- inla.read.graph(tx.nb)
cox_dt$CFIPS_ID = match(cox_dt$Primary_CFIPS,tx_county_sp$GEOID)

cox_dt$std_Perc_Nonwhite <- scale(cox_dt$Perc_Nonwhite)
cox_dt$std_Perc_Owner_Occupied <- scale(cox_dt$Prop_Owner_Occupied*100)
cox_dt$std_ln_Connection <- scale(log(cox_dt$Connections+1))
cox_dt$std_ln_Median_Home_Value <- scale(log(cox_dt$Median_Home_Value+1))
cox_dt$std_Median_Structure_Age <- scale(-cox_dt$Median_Structure_Age)
cox_dt$std_ln_Storage_Per_Connection_G <- scale(log(cox_dt$Storage_Per_Connection_G+0.1))
cox_dt$Debt_Per_Connection[is.na(cox_dt$Debt_Per_Connection)]<-0


## Standard example of how to convert a coxph into a Poisson regression
n.s = nrow(cox_dt[Event==1,])
n.l = nrow(cox_dt)
y_std_ln_connections = cox_dt$std_ln_Connection[cox_dt$Event==1]
y_std_ln_home_value = cox_dt$std_ln_Median_Home_Value[cox_dt$Event==1]
y_std_structure_age = cox_dt$std_Median_Structure_Age[cox_dt$Event==1]
y_std_ln_storage = cox_dt$std_ln_Storage_Per_Connection_G[cox_dt$Event==1]
y_wholesale= as.integer(cox_dt$Wholesale[cox_dt$Event==1])
y_groundwater= as.integer(cox_dt$Groundwater[cox_dt$Event==1])
y_interconnect = as.integer(cox_dt$Interconnections>0)[cox_dt$Event==1]

u_std_ln_connections = cox_dt$std_ln_Connection
u_std_ln_home_value = cox_dt$std_ln_Median_Home_Value
u_std_structure_age = cox_dt$std_Median_Structure_Age
u_std_ln_storage = cox_dt$std_ln_Storage_Per_Connection_G
u_wholesale= as.integer(cox_dt$Wholesale)
u_groundwater= as.integer(cox_dt$Groundwater)
u_interconnect = as.integer(cox_dt$Interconnections>0)

#y_muni = as.integer(cox_dt$Owner_Type[cox_dt$Event==1]=='Municipality')
#y_priv_iou = as.integer(cox_dt$Owner_Type[cox_dt$Event==1]%in%c('Investor Owned','Private'))
#y_wsc = as.integer(cox_dt$Owner_Type[cox_dt$Event==1]=='Water Supply Corporation')

#u_muni = as.integer(cox_dt$Owner_Type=='Municipality')
#u_priv_iou = as.integer(cox_dt$Owner_Type%in%c('Investor Owned','Private'))
#u_wsc = as.integer(cox_dt$Owner_Type=='Water Supply Corporation')

type_of_owner = as.character(cox_dt$Owner_Type[cox_dt$Event==1])
type_of_owner[type_of_owner %in% c('Private','Investor Owned')]<- 'IOU/Private'
#lambda = exp(1+x)
#y = rexp(n, rate=lambda)
failure.time = cox_dt$Time[cox_dt$Event==1]
cox_data = lapply(ls(pattern = '^y_'),get)
names(cox_data)<-ls(pattern = '^y_')

#data$y = failure.time

intercept.u = rep(1,nrow(cox_dt))
county.ids.u = cox_dt$CFIPS_ID
county.spatial.u = cox_dt$CFIPS_ID

cox_data$id = cox_dt$PWS_ID[cox_dt$Event==1]
cox_data$time = failure.time
cox_data$event = rep(1,length(failure.time))
cox_data$intercept.y = rep(1,sum(cox_dt$Event))
cox_data$county.ids.y = cox_dt$CFIPS_ID[cox_dt$Event==1]
cox_data$county.spatial.y =  cox_dt$CFIPS_ID[cox_dt$Event==1]
cox_data$owner_type = type_of_owner
cox_data$d_index = cox_dt$District_Type[cox_dt$Event==1] %in% c('MUD','FWSD','SUD','WCID') & !is.na(cox_dt$DOC_ID[cox_dt$Event==1])
cox_data$district_type = ifelse(cox_data$d_index,cox_dt$District_Type[cox_dt$Event==1],NA)

cox_data$size_quartile = as.numeric(cut(cox_dt$Connections[cox_dt$Event==1], breaks = c(-Inf, quantile(cox_dt$Connections, probs = c(.25,.5,.75)), Inf), labels = c("1","2","3","4")))
cox_data$home_quartile = as.numeric(cut(cox_dt$Median_Home_Value[cox_dt$Event==1], breaks = c(-Inf, quantile(cox_dt$Median_Home_Value, probs = c(.25,.5,.75)), Inf), labels = c("1","2","3","4")))
cox_data$age_quartile = as.numeric(cut(cox_dt$Median_Structure_Age[cox_dt$Event==1], breaks = c(-Inf, quantile(cox_dt$Median_Structure_Age, probs = c(.25,.5,.75)), Inf), labels = c("1","2","3","4")))
cox_dt$Storage_Per_Connection_G[is.na(cox_dt$Storage_Per_Connection_G)] <- 0
cox_data$storage_quartile = as.numeric(cut(cox_dt$Storage_Per_Connection_G[cox_dt$Event==1], breaks = c(-Inf, quantile(cox_dt$Storage_Per_Connection_G, probs = c(.25,.5,.75)), Inf), labels = c("1","2","3","4")))

cox_data$y_private_iou = as.numeric(cox_data$owner_type  %in% c('IOU/Private'))
cox_data$y_municipal = as.numeric(cox_data$owner_type  %in% c('Municipality'))
cox_data$y_wsc = as.numeric(cox_data$owner_type  %in% c('Water Supply Corporation'))


cox_data$y_fwsd = as.numeric(cox_data$district_type  %in% c('FWSD'))
cox_data$y_wcid = as.numeric(cox_data$district_type  %in% c('WCID'))
cox_data$y_sud = as.numeric(cox_data$district_type  %in% c('SUD'))

cov_table_all = summary(cox_dt[,.(Connections,Groundwater,Emergency_Interconnect = as.numeric(Interconnections>0),Wholesale,Storage_Per_Connection_G,Median_Structure_Age,Median_Home_Value)])
cov_table_district = summary(cox_dt[!is.na(DOC_ID)&District_Type%in%c('FWSD','MUD','SUD','WCID'),.(Connections,Groundwater,as.numeric(Interconnections>0),Wholesale,Storage_Per_Connection_G,Median_Structure_Age,Median_Home_Value,Total_Revenue_Per_Connection,Debt_Per_Connection)])
htmlTable(t(cov_table_district))

cov_table_district$sample = 'Districts'
summary(joint.district.mod)
grep('^y_',grep('fwsd|wcid|sud',names(cox_data),value = T,invert = T),value = T)


p = inla.coxph(as.formula(paste0("inla.surv(time,event) ~ -1 + intercept.y + f(county.ids.y,model = 'iid')+",paste(grep('^y_',grep('fwsd|wcid|sud',names(cox_data),value = T,invert = T),value = T),collapse='+'))),
                data = cox_data,control.hazard = list(n.intervals = 16))

cox_model = inla(p$formula,verbose = F,
                 family = p$family,control.inla = list(int.strategy = "eb"),
                 control.fixed = list(expand.factor.strategy = 'model.matrix'),
                 data=c(as.list(p$data), p$data.list),control.compute = list(waic=T,dic=T),
                 E = p$E)

items_to_stratify = c('y_interconnect','y_groundwater','y_wholesale','owner_type','size_quartile','home_quartile','age_quartile','storage_quartile')
temp_data = cox_data
base_form = as.formula(paste0("inla.surv(time,event) ~ -1 + f(county.ids.y,model = 'iid')+",paste(grep('^y_|intercept.y',grep('fwsd|wcid|sud',names(cox_data),value = T,invert = T),value = T),collapse='+')))

nintervals = 16

stratified_cox_models = pblapply(items_to_stratify,function(x) {print(x)
  if(all(temp_data[[x]] %in%c(0,1))){temp_data$strata <-   temp_data[[x]] + 1}else{temp_data$strata <- as.numeric(as.factor(temp_data[[x]]))}
  p_temp = inla(base_form, data  = temp_data,control.hazard = list(n.intervals = nintervals ,strata.name = 'strata'),family = 'coxph',control.compute = list(waic=T,dic=T),
           control.inla = list(int.strategy = 'eb'))
  list(hazard = data.table(p_temp$summary.random$baseline.hazard,model = x,group = rep(sort(unique(temp_data$strata)),each = nintervals +1)),WAIC = p_temp$waic$waic,DIC = p_temp$dic$dic)},cl = 4)

require(htmlTable)
full_stratified = data.table(cbind(c('---',items_to_stratify),rbind(cbind(round(cox_model$waic$waic),round(cox_model$dic$dic)),
                                                                     cbind(round(sapply(stratified_cox_models,function(x) x$WAIC)),
                                                                           round(sapply(stratified_cox_models,function(x) x$DIC)))))
)

names(full_stratified) <- c('Stratified var.','WAIC','DIC')
require(htmlTable)
htmlTable(full_stratified,
caption = 'GOF scores for stratified hazard curves')


cox_district_data = lapply(cox_data,function(x) x[cox_data$d_index])
p_district = inla.coxph(as.formula(paste0("inla.surv(time,event) ~ -1 + intercept.y + f(county.ids.y,model = 'iid')+ ",paste(grep('^y_|intercept.y',grep('muni|wsc|iou',names(cox_data),value = T,invert = T),value = T),collapse='+'))),
               data = cox_district_data,control.hazard = list(n.intervals = 16))

cox_district_model = inla(p_district$formula,verbose = F,
                 family = p_district$family,control.inla = list(int.strategy = "eb"),
                 data=c(as.list(p_district$data), p_district$data.list),control.compute = list(waic=T,dic=T),
                 E = p_district$E)

temp_data = cox_district_data
district_items_to_stratify = c('y_interconnect','y_groundwater','y_wholesale','district_type','size_quartile','home_quartile','storage_quartile','age_quartile')
stratified_cox_district_models = pblapply(district_items_to_stratify,function(x) {print(x)
  if(all(temp_data[[x]] %in%c(0,1))){temp_data$strata <-   temp_data[[x]] + 1}else{temp_data$strata <- as.numeric(as.factor(temp_data[[x]]))}
  p_temp = inla(base_form, data  = temp_data,control.hazard = list(n.intervals = nintervals ,strata.name = 'strata'),family = 'coxph',control.compute = list(waic=T,dic=T),
           control.inla = list(int.strategy = 'eb'))
  list(hazard = data.table(p_temp$summary.random$baseline.hazard,model = x,group = rep(sort(unique(temp_data$strata)),each = nintervals +1)),WAIC = p_temp$waic$waic,DIC = p_temp$dic$dic)},cl = 4)

require(htmlTable)
full_district_stratified = data.table(cbind(c('---',district_items_to_stratify),rbind(cbind(round(cox_district_model$waic$waic),round(cox_district_model$dic$dic)),
                                                                    cbind(round(sapply(stratified_cox_district_models,function(x) x$WAIC)),
                                                                          round(sapply(stratified_cox_district_models,function(x) x$DIC)))))
)

names(full_district_stratified) <- c('Stratified var.','WAIC','DIC')

quantile(log(cox_dt$Median_Home_Value),seq(0.1,1,0.1))
summary(cox_dt$Median_Home_Value[cox_dt$`County Served`=='Harris'])
tapply(cox_dt$Median_Home_Value,cox_dt$`County Served`,mean)

cox_dt$`CURRENT ASSESSED VALUATION`[cox_dt$District_Type %in% c('MUD','FWSD','SUD','WCID')&!is.na(cox_dt$DOC_ID)]


cor(cox_dt[District_Type %in% c('MUD','FWSD','SUD','WCID')&!is.na(DOC_ID),.(std_ln_Median_Home_Value,std_ln_rev_per_connection,std_ln_debt_per_connection)],use = 'pairwise.complete.obs')
htmlTable(full_district_stratified,
          caption = 'GOF scores for stratified district-only hazard curves')


haz_dt = rbindlist(lapply(stratified_cox_models,function(x) x$hazard))
haz_dt$dv = 'CWS'
haz_dt2 = rbindlist(lapply(stratified_cox_district_models,function(x) x$hazard))
haz_dt2$dv = 'District'
haz_dt = rbind(haz_dt,haz_dt2,use.names = T)
haz_dt$name = NA
haz_dt$var = NA
haz_dt$name[haz_dt$model == 'y_interconnect'] <- ifelse(haz_dt$group[haz_dt$model == 'y_interconnect']==1,'No','Yes')
haz_dt$var[haz_dt$model == 'y_interconnect'] <- 'Emergency interconnect'

haz_dt$name[haz_dt$model == 'y_groundwater'] <- ifelse(haz_dt$group[haz_dt$model == 'y_groundwater']==1,'Surface','Groundwater')
haz_dt$var[haz_dt$model == 'y_groundwater'] <- 'Water source'

haz_dt$name[haz_dt$model == 'y_wholesale'] <- ifelse(haz_dt$group[haz_dt$model == 'y_wholesale']==1,'No','Yes')
haz_dt$var[haz_dt$model == 'y_wholesale'] <- 'Wholesaler'

haz_dt$name[haz_dt$model == 'home_quartile'] <- paste0('Q',haz_dt$group[haz_dt$model == 'home_quartile'])
haz_dt$var[haz_dt$model == 'home_quartile'] <- 'Median home value'

haz_dt$name[haz_dt$model == 'size_quartile'] <- paste0('Q',haz_dt$group[haz_dt$model == 'size_quartile'])
haz_dt$var[haz_dt$model == 'size_quartile'] <- 'Service connections'

haz_dt$name[haz_dt$model == 'storage_quartile'] <- paste0('Q',haz_dt$group[haz_dt$model == 'storage_quartile'])
haz_dt$var[haz_dt$model == 'storage_quartile'] <- 'Water storage'

haz_dt$name[haz_dt$model == 'owner_type'] <- c('SD','IOU/private','City','WSC')[haz_dt$group[haz_dt$model=='owner_type']]
haz_dt$var[haz_dt$model == 'owner_type'] <- 'Owner'

haz_dt$name[haz_dt$model == "age_quartile" ] <- paste0('Q',haz_dt$group[haz_dt$model == 'age_quartile'])
haz_dt$var[haz_dt$model == "age_quartile" ] <- 'Median structure age'


haz_dt$name[haz_dt$model == 'district_type'] <- c('FWSD','MUD','SUD','WCID')[haz_dt$group[haz_dt$model=='district_type']]
haz_dt$var[haz_dt$model == 'district_type'] <- 'District'

uq_mods = unique(haz_dt$var[haz_dt$dv=='CWS'])
min_val = min(haz_dt$`0.025quant`[haz_dt$dv=='CWS'])
max_val = max(haz_dt$`0.975quant`[haz_dt$dv=='CWS'])
strata_grobs = lapply(uq_mods,function(u) {
  ggplot(haz_dt[var==u&dv=='CWS',],aes(x = ID,y = mean,ymin = `0.025quant`,ymax = `0.975quant`,
                    fill = name,col = name,group = paste(model,group))) + 
    geom_ribbon(alpha = 0.3,col = NA) + geom_path() + 
    scale_y_continuous(limits =c(min_val,max_val))+
    ggtitle(label = print(u)) + scale_x_continuous(name = 'Year of drought')+
    scale_fill_colorblind() + scale_color_colorblind() + theme_bw() +
    theme(legend.position = c(0.6,0.2),legend.direction = 'horizontal',
          axis.title = element_blank(),legend.title = element_blank()) +
    NULL
})

require(gridExtra)

big_grob = grid.arrange(grobs = strata_grobs,ncol = 2,
top="Stratified hazards, all CWSs in Texas",left = '95% credible interval',
     bottom ="May 4, 2010 to July 7, 2015")
ggsave(plot = big_grob,filename = 'output/proj5/v2/figureA2.png',height=10,width = 9,units = 'in',dpi = 500)


uq_mods = unique(haz_dt$var[haz_dt$dv=='District'])
min_val = min(haz_dt$`0.025quant`[haz_dt$dv=='District'])
max_val = max(haz_dt$`0.975quant`[haz_dt$dv=='District'])
strata_grobs = lapply(uq_mods,function(u) {
  ggplot(haz_dt[var==u&dv=='District',],aes(x = ID,y = mean,ymin = `0.025quant`,ymax = `0.975quant`,
                                       fill = name,col = name,group = paste(model,group))) + 
    geom_ribbon(alpha = 0.3,col = NA) + geom_path() + 
    scale_y_continuous(limits =c(min_val,max_val))+
    ggtitle(label = print(u)) + scale_x_continuous(name = 'Year of drought')+
    scale_fill_colorblind() + scale_color_colorblind() + theme_bw() +
    theme(legend.position = c(0.6,0.2),legend.direction = 'horizontal',
          axis.title = element_blank(),legend.title = element_blank()) +
    NULL
})

require(gridExtra)
big_grob = grid.arrange(grobs = strata_grobs,ncol = 2,
                        top="Stratified hazards, water districts in Texas",left = '95% credible interval',
                        bottom ="May 4, 2010 to July 7, 2015")
ggsave(plot = big_grob,filename = 'output/proj5/v2/figureA3.png',height=10,width = 9,units = 'in',dpi = 500)


(gg_district_hazards = ggplot(haz_dt[var=='District'&dv=='District',],
       aes(x = ID,y = mean,ymin = `0.025quant`,ymax = `0.975quant`,
                                     fill = name,col = name,group = paste(model,group))) + 
  geom_ribbon(alpha = 0.3,col = NA) + geom_path() + 
  scale_y_continuous(limits =c(min_val,max_val),name = 'mean estimate + 95% credible interval')+
  ggtitle(label = 'Hazard stratified by district type') + scale_x_continuous(name = 'Year of drought')+
  scale_fill_colorblind() + scale_color_colorblind() + theme_bw() +
  theme(legend.position = c(0.6,0.2),legend.direction = 'horizontal',
     legend.title = element_blank()) +
  NULL )
  
ggsave(plot = gg_district_hazards,filename = 'output/proj5/v2/figureA4.png',height=6,width = 5,units = 'in',dpi = 500)


binomial_id = cox_dt$PWS_ID
u_std_ln_connections = as.numeric(cox_dt$std_ln_Connection)
u_std_ln_home_value = as.numeric(cox_dt$std_ln_Median_Home_Value)
u_std_structure_age = as.numeric(cox_dt$std_Median_Structure_Age)
u_std_ln_storage = as.numeric(cox_dt$std_ln_Storage_Per_Connection_G)
u_std_ln_storage[is.na(u_std_ln_storage)]<-0
u_wholesale = cox_dt$Wholesale
u_groundwater = cox_dt$Groundwater
u_interconnect = as.integer(cox_dt$Interconnections>0)
u_district_type = ifelse(cox_dt$District_Type %in% c('FWSD','SUD','MUD','WCID'),cox_dt$District_Type,NA)
u_owner_type = ifelse(cox_dt$Owner_Type %in% c('Private','Investor Owned'),'IOU/Private',as.character(cox_dt$Owner_Type))
u_private_iou = as.numeric(cox_dt$Owner_Type  %in% c('Private','Investor Owned'))
u_municipal = as.numeric(cox_dt$Owner_Type  %in% c('Municipality'))
u_wsc = as.numeric(cox_dt$Owner_Type  %in% c('Water Supply Corporation'))

u_fwsd = as.numeric(cox_dt$District_Type  %in% c('FWSD'))
u_wcid = as.numeric(cox_dt$District_Type  %in% c('WCID'))
u_sud = as.numeric(cox_dt$District_Type  %in% c('SUD'))


n.l = nrow(cox_dt)
#intercept.u = rep(1, n.l)
county.ids.u = cox_dt$CFIPS_ID
county.spatial.u = cox_dt$CFIPS_ID


df = data.frame(intercept.u, county.ids.u, county.spatial.u,binomial_id,do.call(cbind,sapply(grep('owner|district',ls(pattern = '^u_'),value = T, invert = T),function(x) as.numeric(get(x)),simplify = F)) ,failure = cox_dt$Event)
df.joint = c(as.list(inla.rbind.data.frames(p$data, df)), p$data.list)
df.joint$Y = cbind(df.joint$y..coxph, df.joint$failure)

formula_duo = update(p$formula, as.formula(paste0('Y ~ . -1 + intercept.y + intercept.u + f(county.ids.u,model = "iid") + ',
                                                  paste(grep('^u_',names(df.joint)[!names(df.joint)%in%c('u_wcid','u_fwsd','u_sud','u_id')],value = T),collapse = '+'))))

joint.mod = inla(formula_duo,control.inla = list( int.strategy = "eb"),control.fixed = list(expand.factor.strategy = 'model.matrix'),
                 family = c( p$family,"binomial"),Ntrials = 1,verbose = F,
                 data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                 E = df.joint$E..coxph)

df.district = data.frame(intercept.u,county.ids.u, doc_id = cox_dt$DOC_ID,county.spatial.u,binomial_id,do.call(cbind,sapply(grep('owner|district',ls(pattern = '^u_'),value = T, invert = T),function(x) as.numeric(get(x)),simplify = F)),failure = cox_dt$Event)
df.district = df.district[!is.na(u_district_type)&!is.na(cox_dt$DOC_ID),]

df.district.joint = c(as.list(inla.rbind.data.frames(p_district$data, df.district)), p_district$data.list)
df.district.joint$Y = cbind(df.district.joint$y..coxph, df.district.joint$failure)

formula_district_duo = update(p_district$formula, as.formula(paste0('Y ~ . -1 + intercept.y + intercept.u + f(county.ids.u,model = "iid") +',
                                                  paste(grep('^u_',names(df.district.joint)[!names(df.district.joint)%in%c('u_wsc','u_private_iou','u_municipal')],value = T),collapse = '+'))))

joint.district.mod = inla(formula_district_duo,control.inla = list( int.strategy = "eb"),control.fixed = list(expand.factor.strategy = 'model.matrix'),
                 family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                 control.update = list(result = joint.mod),
                 data = df.district.joint,control.compute = list(cpo = T,waic = T,dic = T),
                 E = df.district.joint$E..coxph)


cox_dt[Debt_Per_Connection>150000&!District_Type%in%c('Not a District','Other'),][,.(Connections,Debt_Per_Connection,Total_Debt_Service_Outstanding)]
plot(cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')],
     cox_dt$Total_Revenue_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')])
cox_dt[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other'),][order(-Debt_Per_Connection),.(Debt_Per_Connection,District_Name)]
cox_dt[,.(Total_Revenue_Per_Connection,District_Type)][order(-Total_Revenue_Per_Connection),]
summary(cox_dt$Total_Revenue_Per_Connection[{!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')}])


par(mfrow = c(1,2))
plot(quantile(cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')],seq(0.01,0.99,0.01)),ylab = 'Debt/connection',xlab = 'Percentile',main='Debt/connection percentiles')
plot(quantile(cox_dt$Total_Revenue_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')],seq(0.01,0.99,0.01)),ylab = 'Revenue/connection',xlab = 'Percentile',main = 'Revenue/connection percentiles')

ggplot() + geom_density(aes(y = cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')]))
quantile(cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')],seq(0.1,0.9,0.025))
quantile(cox_dt$Total_Revenue_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')],seq(0.1,0.9,0.05))



cox_dt$std_ln_debt_per_connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')] = scale(ifelse(cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')]==0,NA,log(cox_dt$Debt_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')])))
cox_dt$std_ln_rev_per_connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')] = scale(ifelse(cox_dt$Total_Revenue_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')]==0,NA,log(cox_dt$Total_Revenue_Per_Connection[!is.na(cox_dt$DOC_ID)&!cox_dt$District_Type%in%c('Not a district','Other')])))


df.district.joint$y_std_sqrt_debt_per_connection = cox_dt$std_sqrt_debt_per_connection[match(df.district.joint$id,cox_dt$PWS_ID)]
df.district.joint$u_std_sqrt_debt_per_connection = cox_dt$std_sqrt_debt_per_connection[match(df.district.joint$binomial_id,cox_dt$PWS_ID)]

df.district.joint$y_nonzero_debt_per_connection = as.numeric(cox_dt$Debt_Per_Connection[match(df.district.joint$id,cox_dt$PWS_ID)]>0)
df.district.joint$u_nonzero_debt_per_connection = as.numeric(cox_dt$Debt_Per_Connection[match(df.district.joint$binomial_id,cox_dt$PWS_ID)]>0)
df.district.joint$y_std_ln_debt_per_connection = cox_dt$std_ln_debt_per_connection[match(df.district.joint$id,cox_dt$PWS_ID)]
df.district.joint$u_std_ln_debt_per_connection = cox_dt$std_ln_debt_per_connection[match(df.district.joint$binomial_id,cox_dt$PWS_ID)]

formula_district_debt = update.formula(formula_district_duo, Y ~ . + u_std_sqrt_debt_per_connection+y_std_sqrt_debt_per_connection)

cox_dt[Total_Revenue_Per_Connection<1&!is.na(DOC_ID)&District_Type%in%c('FWSD','MUD','WCID','SUD'),.(Connections,Debt_Per_Connection,Total_Revenue_Per_Connection,
                                                                                                     Total_Debt_Service_Outstanding)]

length(unique(cox_dt$PWS_ID))


formula_district_debt = update.formula(formula_district_duo, Y ~ . + u_nonzero_debt_per_connection + y_nonzero_debt_per_connection + 
                                         u_nonzero_debt_per_connection:u_std_ln_debt_per_connection + 
                                         y_nonzero_debt_per_connection:y_std_ln_debt_per_connection)

joint.district.mod.debt = inla(formula_district_debt,control.inla = list( int.strategy = "eb"),
                               control.fixed = list(expand.factor.strategy = 'model.matrix'),
                               family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                               data = df.district.joint,  control.compute = list(cpo = T,waic = T,dic = T),
                               E = df.district.joint$E..coxph)

df.district.joint$y_rev_per_connection = cox_dt$std_ln_rev_per_connection[match(df.district.joint$id,cox_dt$PWS_ID)]
df.district.joint$u_rev_per_connection = cox_dt$std_ln_rev_per_connection[match(df.district.joint$binomial_id,cox_dt$PWS_ID)]

# med_rev = median(cox_dt[District_Type%in%c('FWSD','WCID','SUD','MUD')&!is.na(cox_dt$DOC_ID),.(Total_Revenue_Per_Connection)]$Total_Revenue_Per_Connection,na.rm = T)
# rev_cuts = quantile(cox_dt[District_Type%in%c('FWSD','WCID','SUD','MUD')&!is.na(DOC_ID),.(Total_Revenue_Per_Connection)]$Total_Revenue_Per_Connection,c(0.25,0.50),na.rm = T)
# df.district.joint$y_rev_q2q3 = as.integer(df.district.joint$y_rev_per_connection>=rev_cuts[1]&df.district.joint$y_rev_per_connection<rev_cuts[2])
# df.district.joint$y_rev_q4 = as.integer(df.district.joint$y_rev_per_connection>rev_cuts[2])
# df.district.joint$u_rev_q2q3 = as.integer(df.district.joint$u_rev_per_connection>=rev_cuts[1]&df.district.joint$u_rev_per_connection<rev_cuts[2])
# df.district.joint$u_rev_q4 = as.integer(df.district.joint$u_rev_per_connection>rev_cuts[2])
# formula_district_rev = update.formula(formula_district_duo, Y ~ . + y_rev_q2q3 + y_rev_q4 + u_rev_q2q3 + u_rev_q4)

formula_district_rev = update.formula(formula_district_duo, Y ~ . + u_rev_per_connection + y_rev_per_connection)

joint.district.mod.rev = inla(formula_district_rev,control.inla = list( int.strategy = "eb"),#control.fixed = list(expand.factor.strategy = 'model.matrix'),
                              family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                              data = df.district.joint,  control.compute = list(cpo = T,waic = T,dic = T),
                              E = df.district.joint$E..coxph)

formula_district_revdebt = update.formula(formula_district_duo, Y~.+ u_rev_per_connection + y_rev_per_connection + u_nonzero_debt_per_connection + y_nonzero_debt_per_connection + 
                                            u_nonzero_debt_per_connection:u_std_ln_debt_per_connection + 
                                            y_nonzero_debt_per_connection:y_std_ln_debt_per_connection)

joint.district.mod.revdebt = inla(formula_district_revdebt,control.inla = list( int.strategy = "eb"),#control.fixed = list(expand.factor.strategy = 'model.matrix'),
                                  family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                                  data = df.district.joint,  control.compute = list(cpo = T,waic = T,dic = T),
                                  E = df.district.joint$E..coxph)







summary(joint.district.mod.revdebt)
dim(as.matrix(joint.district.mod.revdebt$model.matrix))

ggcorrplot::ggcorrplot(round(cor(as.matrix(joint.district.mod.revdebt$model.matrix)),2))
?INLA::inla.as.dgTMatrix()
str(joint.district.mod.revdebt$model.matrix)
cor(joint.district.mod.revdebt$model.matrix@x)
summary(joint.district.mod.revdebt)

quantile(cox_dt[District_Type%in%c('FWSD','WCID','SUD','MUD')&!is.na(cox_dt$DOC_ID),.(Debt_Per_Connection)]$Debt_Per_Connection,0.5,na.rm = T)


qts = seq(0.35,0.95,0.05)
test = pblapply(qts,function(q){
med_debt = quantile(cox_dt[District_Type%in%c('FWSD','WCID','SUD','MUD')&!is.na(cox_dt$DOC_ID),.(Debt_Per_Connection)]$Debt_Per_Connection,q,na.rm = T)
df.district.joint$y_debt_low = as.integer(df.district.joint$y_debt_per_connection<=med_debt & df.district.joint$y_debt_per_connection>100)
df.district.joint$u_debt_low = as.integer(df.district.joint$u_debt_per_connection<=med_debt & df.district.joint$u_debt_per_connection>100)
df.district.joint$y_debt_high = as.integer(df.district.joint$y_debt_per_connection>1e3)
df.district.joint$u_debt_high = as.integer(df.district.joint$u_debt_per_connection>1e3)


test = dcast(data.table(id  = ifelse(!is.na(df.district.joint$id),df.district.joint$id,df.district.joint$binomial_id),
                 V1 = cut(df.district.joint$u_debt_per_connection,breaks = c(-Inf,seq(1,1e4,2.5e3),Inf))),id~V1)
test[,`NA`:=NULL]
df.district.joint$debt_u_groups = cut(df.district.joint$u_debt_per_connection,breaks = c(-Inf,seq(1,1e4,2.5e3),Inf))
df.district.joint$debt_y_groups = cut(df.district.joint$y_debt_per_connection,breaks = c(-Inf,seq(1,1e4,2.5e3),Inf))
formula_district_debt = update.formula(formula_district_duo, Y ~ . + debt_u_groups + debt_y_groups)

formula_district_debt = update.formula(formula_district_duo, Y ~ . + as.integer(u_debt_per_connection>0) + as.integer(u_debt_per_connection>0):scale(asinh(u_debt_per_connection))+
                                         as.integer(y_debt_per_connection>0) + as.integer(y_debt_per_connection>0):scale(asinh(y_debt_per_connection)))


quantile(cox_dt$Debt_Per_Connection,0.8)
formula_district_debt = update.formula(formula_district_duo, Y ~ . + as.integer(y_debt_per_connection==0) + as.integer(y_debt_per_connection>2427)+
                                         as.integer(u_debt_per_connection==0) + as.integer(u_debt_per_connection>2427))



joint.district.mod.test = inla(formula_district_debtn,control.inla = list( int.strategy = "eb"),#control.fixed = list(expand.factor.strategy = 'model.matrix'),
                                  family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                                  data = df.district.joint,  control.compute = list(cpo = T,waic = T,dic = T),
                                  E = df.district.joint$E..coxph)


joint.district.mod.test$waic$waic
joint.district.mod$waic$waic
joint.district.mod.debt$waic$waic
joint.district.mod.rev$waic$waic 
joint.district.mod.revdebt$waic$waic 

summary(joint.district.mod.debt )
summary(joint.district.mod.rev)
summary(joint.district.mod.debt)
summary(joint.district.mod.revdebt)
rm(u_rev_q4)
joint.district.mod$summary.random$baseline.hazard


table(df.district.joint$failure,df.district.joint$u_rev_q4)
cor(df.district.joint$u_rev_per_connection,df.district.joint$u_debt_per_connection,use = 'pairwise.complete.obs')
u_rev_q4 = df.district.joint$u_rev_q4
length(u_rev_q4)

df.district.joint$u_rev_q4

names(df.district.joint)
formula_district_rev


df.district.joint$u_rev_q2q3



joint.district.mod.debt = inla(formula_district_duo,control.inla = list( int.strategy = "eb"),control.fixed = list(expand.factor.strategy = 'model.matrix'),
                              family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                              data = df.district.joint,control.compute = list(cpo = T,waic = T,dic = T),
                              E = df.district.joint$E..coxph)

joint.district.mod.full = inla(formula_district_duo,control.inla = list( int.strategy = "eb"),control.fixed = list(expand.factor.strategy = 'model.matrix'),
                               family = c( p_district$family,"binomial"),Ntrials = 1,verbose = F,
                               data = df.district.joint,control.compute = list(cpo = T,waic = T,dic = T),
                               E = df.district.joint$E..coxph)
















# test_binomials = c('binomial','zeroinflatedbinomial0','zeroinflatedbinomial1','zeroinflatedbinomial2')
# binomial_options = lapply(test_binomials,function(x) {  
# inla(formula_duo,
#           family = c( p$family,x),control.inla = list(int.strategy = 'eb'),
#           data = df.joint,control.compute = list(waic = T,dic = T),
#           E = df.joint$E..coxph)})

joint.modA = inla(formula_duo,
                  control.inla = list(diagonal = 1000,  int.strategy = "eb"),
          family = c( p$family,"binomial"),num.threads = 8,Ntrials = 1,
          data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
          E = df.joint$E..coxph)
joint.modB = inla(formula_duo, control.inla = list(diagonal = 100,  int.strategy = "eb"),
                  family = c( p$family,"zeroinflatedbinomial0"),num.threads = 8,
                  control.mode = list(result = joint.modA, restart = TRUE),
                  data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                  E = df.joint$E..coxph)
joint.modC = inla(formula_duo, control.inla = list(diagonal = 10,  int.strategy = "eb"),
                  family = c( p$family,"zeroinflatedbinomial0"),num.threads = 8,
                  control.mode = list(result = joint.modB, restart = TRUE),
                  data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                  E = df.joint$E..coxph)
joint.modD = inla(formula_duo, control.inla = list(diagonal = 1,  int.strategy = "eb"),
                  family = c( p$family,"zeroinflatedbinomial0"),num.threads = 8,
                  control.mode = list(result = joint.modC, restart = TRUE),
                  data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                  E = df.joint$E..coxph)
joint.modE = inla(formula_duo,control.inla = list( int.strategy = "eb"),
                  control.mode = list(result = joint.modD, restart = TRUE),
                  family = c( p$family,"zeroinflatedbinomial0"),num.threads = 8,
                  data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                  E = df.joint$E..coxph)



summary(df.joint$Y)
summary(joint.mod)
table(is.na(df.joint$u_interconnect))
table(is.na(df.joint$y_interconnect))


dim(cox_dt)
summary(df.joint$u_interconnect)
summary(df.joint$u_groundwater)

summary(joint.mod)



joint.mod = inla(formula_duo,
                 family = c( p$family,"zeroinflatedbinomial0"),
                 control.mode = list(result = joint.modE, restart = TRUE),
                 data = df.joint,control.compute = list(cpo = T,waic = T,dic = T),
                 E = df.joint$E..coxph)

saveRDS(joint.mod,'scratch/joint_drought_restriction_all_texas_model.RDS')
ggplot(joint.mod$summary.random$baseline.hazard) + 
  geom_ribbon((aes(x = ID,ymin = `0.025quant`,ymax = `0.975quant`)),fill = 'grey70') + 
  geom_point(aes(x = ID,y = mean)) + 
  geom_path(aes(x = ID,y = mean)) + 
  theme_bw()





