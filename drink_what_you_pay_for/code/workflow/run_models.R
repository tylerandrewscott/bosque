rm(list=ls())
library(spdep)
library(maptools)
library(dplyr)
library(tidyverse)
library(lubridate)

#### MODEL SETUP ###
#master_df = read.csv('input/scratch/ready_model.csv')# %>% filter(DOC_ID != 256506)
model_df = read_csv('input/scratch/master_data.csv') %>% filter(TOTAL_REVENUE_LAG1 < 28000000) %>%
  filter(!is.na(TOTAL_REVENUE_LAG1)) %>% filter(SERVICE_CONNECTIONS_LAG1!=0) %>%
  filter(month(`FISCAL YEAR ENDED`) == MONTH) %>% filter(DISTRICT_AGE>0) %>% 
  filter(TOTAL_REVENUE_LAG1 != 0) %>%filter(FYEAR >= 2008) %>%
  mutate(LOG_SERVICE_CONNECTIONS = log(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`))
colnames(model_df)[1] = 'PWS_ID'

#model_df <- model_df %>% filter(IS_FWSD==0&IS_WCID==0)


# 
# master_df %>% filter(PWS_NAME == 'FORT BEND COUNTY MUD 2') %>% 
#   mutate(TOTAL_EXP = `GENERAL FUND - TOTAL EXPENDITURES` + `ENTERPRISE FUND - OPERATING EXPENSES`) %>%
#   filter(MONTH == month(`FISCAL YEAR ENDED`)) %>%
#   dplyr::select(FYEAR,TOTAL_EXP,`BONDS OUTSTANDING`,TOTAL_REVENUE,DOC_ID,NewMoneySize,TotDebtServiceOutstanding)
# 
# model_df %>% filter(PWS_NAME == 'FORT BEND COUNTY MUD 2') %>% 
#   mutate(TOTAL_EXP = `GENERAL FUND - TOTAL EXPENDITURES` + `ENTERPRISE FUND - OPERATING EXPENSES`) %>%
#   filter(MONTH == month(`FISCAL YEAR ENDED`)) %>%
#   dplyr::select(FYEAR,TOTAL_EXP,`BONDS OUTSTANDING`,TOTAL_REVENUE,DOC_ID,NewMoneySize,TotDebtPrincipalOutstanding)

district_shapes = readOGR('input/scratch','district_shapes')
district_shapes@data$PWS_ID = model_df$PWS_ID[match(district_shapes@data$NAME,model_df$PWS_NAME)]
district_shapes = district_shapes[!is.na(district_shapes@data$PWS_ID),]
district_mat<-poly2nb(district_shapes,queen=F, row.names=district_shapes$PWS_ID)
#Convert the adjacency matrix into a file in the INLA format
nb2INLA("tx_district.adj", district_mat)
poly_center = gCentroid(district_shapes)



n = nrow(model_df)
u <- (model_df$NewMoneySize>0) + 0
y <- ifelse(model_df$NewMoneySize>0,model_df$NewMoneySize/1000000,NA)
center_continuous_cov = TRUE
idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
idat$u_regionid = c(model_df$uq_sysid,model_df$uq_sysid)
idat$y_regionid = c(model_df$uq_sysid,model_df$uq_sysid)

idat$u_hviols_p5 <- c((model_df$hviol_p5_count_l1>0+0), rep(0,n))
idat$y_hviols_p5 <- c(rep(0,n),(model_df$hviol_p5_count_l1>0+0))
idat$u_mviols_p5 <- c((model_df$mviol_p5_count_l1>0+0), rep(0,n))
idat$y_mviols_p5 <- c(rep(0,n), (model_df$mviol_p5_count_l1>0+0))
idat$u_hviols_p4 <- c((model_df$hviol_p4_count_l1>0+0), rep(0,n))
idat$y_hviols_p4 <- c(rep(0,n),(model_df$hviol_p4_count_l1>0+0))
idat$u_mviols_p4 <- c((model_df$mviol_p4_count_l1>0+0), rep(0,n))
idat$y_mviols_p4 <- c(rep(0,n), (model_df$mviol_p4_count_l1>0+0))
idat$u_hviols_p3 <- c((model_df$hviol_p3_count_l1>0+0), rep(0,n))
idat$y_hviols_p3 <- c(rep(0,n),(model_df$hviol_p3_count_l1>0+0))
idat$u_mviols_p3 <- c((model_df$mviol_p3_count_l1>0+0), rep(0,n))
idat$y_mviols_p3 <- c(rep(0,n),(model_df$mviol_p3_count_l1>0+0))
idat$u_hviols_p2 <- c((model_df$hviol_p2_count_l1>0+0), rep(0,n))
idat$y_hviols_p2 <- c(rep(0,n),(model_df$hviol_p2_count_l1>0+0))
idat$u_mviols_p2 <- c((model_df$mviol_p2_count_l1>0+0), rep(0,n))
idat$y_mviols_p2 <- c(rep(0,n), (model_df$mviol_p2_count_l1>0+0))
idat$u_hviols_p1 <- c((model_df$hviol_p1_count_l1>0+0), rep(0,n))
idat$y_hviols_p1 <- c(rep(0,n),(model_df$hviol_p1_count_l1>0+0))
idat$u_mviols_p1 <- c((model_df$mviol_p1_count_l1>0+0), rep(0,n))
idat$y_mviols_p1 <- c(rep(0,n), (model_df$mviol_p1_count_l1>0+0))

idat$u_log_service_pop <- c(scale(model_df$LOG_SERVICE_CONNECTIONS,center = center_continuous_cov,scale=F), rep(0,n))
idat$y_log_service_pop <- c(rep(0,n), scale(model_df$LOG_SERVICE_CONNECTIONS,center = center_continuous_cov,scale=F))
idat$u_num_facilities <- c(scale(model_df$`# of Facilities`,center=F,scale=F), rep(0,n))
idat$y_num_facilities <- c(rep(0,n), scale(model_df$`# of Facilities`,center=F,scale=F))
idat$u_system_age <-c(model_df$DISTRICT_AGE, rep(0,n))
idat$y_system_age <-c(rep(0,n),model_df$DISTRICT_AGE)
idat$u_debt_issued_p1 <- c((model_df$NEW_DEBT_ISSUED_P1>0)+0, rep(0,n))
idat$y_debt_issued_p1 <- c(rep(0,n), (model_df$NEW_DEBT_ISSUED_P1>0)+0)
idat$u_total_revenue_1m_p1 <- c(scale(model_df$TOTAL_REVENUE_LAG1/1000000,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_total_revenue_1m_p1 <- c(rep(0,n), scale(model_df$TOTAL_REVENUE_LAG1/1000000,center=center_continuous_cov,scale=F))
idat$u_log_total_revenue_p1 <- c(scale(log(model_df$TOTAL_REVENUE_LAG1),center=center_continuous_cov,scale=F), rep(0,n))
idat$y_log_total_revenue_p1 <- c(rep(0,n), scale(log(model_df$TOTAL_REVENUE_LAG1),center=center_continuous_cov,scale=F))
idat$u_total_revenue_per_capita_1k_p1 <- c(scale(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1/1000,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_total_revenue_per_capita_1k_p1 <- c(rep(0,n), scale(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1/1000,center=center_continuous_cov,scale=F))
idat$u_log_total_revenue_per_capita_p1 <- c(scale(log(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1),center=center_continuous_cov,scale=F), rep(0,n))
idat$y_log_total_revenue_per_capita_p1 <- c(rep(0,n), scale(log(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1),center=center_continuous_cov,scale=F))



idat$y_fund_balance_1m_p1 <- c(rep(0,n), scale(model_df$TOTAL_FUND_BALANCE_LAG1/1000000,center=center_continuous_cov,scale=F))
idat$u_fund_balance_1m_p1 <- c( scale(model_df$TOTAL_FUND_BALANCE_LAG1/1000000,center=center_continuous_cov,scale=F), rep(0,n))
idat$y_fund_balance_per_capita_10k_p1 <- c(rep(0,n), scale(model_df$TOTAL_FUND_BALANCE_PER_CAPITA_LAG1/10000,center=center_continuous_cov,scale=F))
idat$u_fund_balance_per_capita_10k_p1 <- c( scale(model_df$TOTAL_FUND_BALANCE_PER_CAPITA_LAG1/10000,center=center_continuous_cov,scale=F), rep(0,n))

idat$u_no_debt_outstanding_p1 <- c(model_df$TOTAL_DEBT_SERVICE_LAG1==0 + 0, rep(0,n))
idat$y_no_debt_outstanding_p1 <- c(rep(0,n),model_df$TOTAL_DEBT_SERVICE_LAG1==0 + 0)
idat$u_debt_outstanding_1m_p1 <- c(scale(model_df$TOTAL_DEBT_SERVICE_LAG1/1000000,center = F,scale=F), rep(0,n))
idat$y_debt_outstanding_1m_p1 <- c(rep(0,n), scale(model_df$TOTAL_DEBT_SERVICE_LAG1/1000000,center = F,scale=F))
idat$u_log_nonzero_debt_outstanding_p1 <- c(scale(ifelse(model_df$TOTAL_DEBT_SERVICE_LAG1>0,log(model_df$TOTAL_DEBT_SERVICE_LAG1),0),center=center_continuous_cov,scale=F), rep(0,n))
idat$y_log_nonzero_debt_outstanding_p1 <- c(rep(0,n), scale(ifelse(model_df$TOTAL_DEBT_SERVICE_LAG1>0,log(model_df$TOTAL_DEBT_SERVICE_LAG1),0),center=center_continuous_cov,scale=F))
idat$u_debt_outstanding_per_capita_10k_p1 <- c(scale(model_df$TOTAL_DEBT_SERVICE_PER_CAPITA_LAG1/10000,center = center_continuous_cov,scale=F), rep(0,n))
idat$y_debt_outstanding_per_capita_10k_p1 <- c(rep(0,n), scale(model_df$TOTAL_DEBT_SERVICE_PER_CAPITA_LAG1/10000,center = center_continuous_cov,scale=F))
idat$u_log_debt_outstanding_per_capita_p1 <- c(scale(log(model_df$TOTAL_DEBT_SERVICE_PER_CAPITA_LAG1+1),center = center_continuous_cov,scale=F), rep(0,n))
idat$y_log_debt_outstanding_per_capita_p1 <- c(rep(0,n), scale(log(model_df$TOTAL_DEBT_SERVICE_PER_CAPITA_LAG1+1),center = center_continuous_cov,scale=F))


idat$u_primary_ground <- c(model_df$GROUNDWATER, rep(0,n))
idat$y_primary_ground <- c(rep(0,n),model_df$GROUNDWATER)
idat$u_primary_purch <- c(model_df$PURCHASED, rep(0,n))
idat$y_primary_purch <- c(rep(0,n),model_df$PURCHASED)
idat$u_wholesaler <- c(model_df$WHOLESALER, rep(0,n))
idat$y_wholesaler <- c(rep(0,n),model_df$WHOLESALER)
idat$u_fwsd <- c(model_df$IS_FWSD, rep(0,n))
idat$y_fwsd <- c(rep(0,n),model_df$IS_FWSD)
idat$u_wcid <- c(model_df$IS_WCID, rep(0,n))
idat$y_wcid <- c(rep(0,n),model_df$IS_WCID)
idat$u_PWS_ID <- c(as.character(model_df$PWS_ID), rep(NA,n))
idat$y_PWS_ID <- c(rep(NA,n), as.character(model_df$PWS_ID))
idat$u_fyear <- c(model_df$FYEAR, rep(NA,n))
idat$y_fyear <- c(rep(NA,n), model_df$FYEAR)
idat$u_fyear2 <- c(model_df$FYEAR, rep(0,n))
idat$y_fyear2 <- c(rep(0,n), model_df$FYEAR)
### census variables

#idat$u_log_med_home <- c(scale(log(model_df$MED_HOME_VALUE),center=F,scale=F),rep(0,n))
#idat$y_log_med_home <- c(rep(0,n), scale(log(model_df$MED_HOME_VALUE),center=F,scale=F))
idat$u_med_home_10k <- c(scale(model_df$TRACT_WEIGHTED_MED_HOME_VALUE/10000, center=center_continuous_cov,scale=F),rep(0,n))
idat$y_med_home_10k <- c(rep(0,n), scale(model_df$TRACT_WEIGHTED_MED_HOME_VALUE/10000,center=center_continuous_cov,scale=F))
idat$u_log_med_home <- c(scale(log(model_df$TRACT_WEIGHTED_MED_HOME_VALUE), center=center_continuous_cov,scale=F),rep(0,n))
idat$y_log_med_home <- c(rep(0,n), scale(log(model_df$TRACT_WEIGHTED_MED_HOME_VALUE),center=center_continuous_cov,scale=F))

idat$y.i <- idat$u.i <- c(1:n, 1:n)

require(INLA)
require(tidyverse)
#u_med_income_10k + y_med_income_10k +
#u_debt_issued_p1 + y_debt_issued_p1 +


##########  run across violation windows #######
base_form = "Y ~ 0 + mu.u + mu.y +
u_med_home_10k  + y_med_home_10k +
u_log_service_pop + y_log_service_pop + 
u_system_age + y_system_age + 
u_fwsd + y_fwsd  + 
u_wcid + y_wcid + 
u_debt_outstanding_1m_p1 + y_debt_outstanding_1m_p1 +
u_fund_balance_1m_p1 + y_fund_balance_1m_p1 +
u_primary_ground + y_primary_ground + 
u_primary_purch + y_primary_purch + 
u_mviols_p3 + y_mviols_p3 + 
u_hviols_p3 + y_hviols_p3 +
u_log_total_revenue_p1 + 
y_log_total_revenue_p1"


interaction_vary = c("", "u_hviols_p3:u_log_total_revenue_p1 + y_hviols_p3:y_log_total_revenue_p1",
                     "u_hviols_p3:u_fund_balance_1m_p1 + y_hviols_p3:y_fund_balance_1m_p1",
                     "u_hviols_p3:u_med_home_10k + y_hviols_p3:y_med_home_10k")

random_form = "f(u_fyear, model='iid') + f(y_fyear,model='iid') + f(u_PWS_ID, model='iid') + f(y_PWS_ID,model='iid')"     

form_temps = expand.grid(gsub('\\\n','',base_form),interaction_vary,
                         gsub('\\\n','',random_form))

form_list = lapply(1:nrow(form_temps),
                   function(x) paste(form_temps[x,1],form_temps[x,2],form_temps[x,3],sep='+'))

main_results = lapply(form_list,function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                                    control.fixed = list(expand.factor.strategy = "inla"),
                                                    data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                    control.predictor=list(compute=TRUE),verbose=F))


library(stringr)
library(forcats)
names(main_results) = paste(str_extract(gsub('.*:','',interaction_vary),' |revenue|balance|home'),str_extract(interaction_vary,'viols_p[0-5]'),sep='_')
names(main_results)[[1]] = 'main_p3' 

coef_results = do.call(rbind,lapply(1:length(main_results),function(x) main_results[[x]]$summary.fix[,c(1,3,5)] %>% 
                                      mutate(COEF = rownames(.),MODEL = names(main_results)[x],
                                             LIK = ifelse(grepl('^y_|\\.y$',COEF),'1.Gamma','2.Binomial')) %>%
                                      mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% 
                                      mutate(COEF = gsub('_p[1-5]','',COEF)) %>%
                                      mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)))) %>%
  mutate(SIG_FILL = paste(SIG,LIK))


# coef_results$COEF = fct_recode(f = coef_results$COEF,
#                                `Management viol.` = 'mviols', 
#                                `Health viol.` = 'hviols', 
#                                `FWSD` = 'fwsd', 
#                                `WCID` = 'wcid', 
#                                `Fund balance ($1M)` = 'fund_balance_1m',
#                                expression(ln(Revenue)[t-1]) = 'log_total_revenue',
#                                `ln(Revenue):Health viol.` = 'hviols:log_total_revenue',
#                                `Fund balance ($1M):Health viol.` = 'fund_balance_1m:hviols',
#                                `Med. home value ($10k):Health viol.` = 'med_home_10k:hviols',
#                                `System age (y)` = 'system_age',
#                                `ln(Service con.)` = 'log_service_pop',
#                                `Purchaser` = 'primary_purch',
#                                `Groundwater user` = 'primary_ground',
#                                `Med. home value ($10k)` = 'med_home_10k',
#                                `Outstanding debt ($1M)` = 'debt_outstanding_1m')
# 
# coef_results$COEF = factor(coef_results$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty",#"% Bachelor's",
#                                                         "Med. home value ($10k)",
#                                                         '# facilities','System age (y)','Groundwater user','Purchaser',
#                                                         # 'Purchaser:Groundwater',
#                                                         #  'Wholesaler',
#                                                         'FWSD','WCID',
#                                                         "ln(Service con.)",
#                                                         'Outstanding debt ($1M)',
#                                                         'Fund balance ($1M)',
#                                                         'ln(Revenue)',
#                                                         'Management viol.','Health viol.',
#                                                         'ln(Revenue):Health viol.',"Fund balance ($1M):Health viol.",
#                                                         "Med. home value ($10k):Health viol."))


unique(coef_results$COEF)

library(ggthemes)
coef_results$COEF = factor(coef_results$COEF,levels = 
                             c("mu.u","mu.y"  ,"med_home_10k",    
                               "system_age"   , 
                               "primary_ground",          
                               "primary_purch", 
                               "fwsd"  , "wcid",
                               "log_service_pop", 
                               "mviols" , "hviols"  ,
                               "debt_outstanding_1m"  ,
                               "fund_balance_1m"   ,   
                               "log_total_revenue"    ,    
                                "hviols:log_total_revenue", "fund_balance_1m:hviols",  
                                "med_home_10k:hviols" ))

                          
                           
coef_results$COEF = fct_rev(coef_results$COEF)
p3_results = coef_results %>% filter(!grepl('^mu',COEF),grepl('p3',MODEL))
p3_results$MODEL = as.factor(p3_results$MODEL) 

levels(p3_results$MODEL) = c('Model 3','Model 4','Model 1','Model 2')
p3_results$MODEL = factor(p3_results$MODEL,levels = c('Model 1','Model 2','Model 3','Model 4'))
gg_main = ggplot(p3_results) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=2) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=.5) + 
  coord_flip() + facet_grid(~MODEL,scales='free_x') +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.86,0.15)) +
  scale_x_discrete(labels=
                     rev(c(
                       "Med. home value ($10k)",
                       'System age (y)','Groundwater user','Purchaser',
                       'FWSD','WCID',
                       "ln(Service con.)",
                       expression("Management viol."[t-1:4]),
                       expression("Health viol."[t-1:4]),
                       'Outstanding debt ($1M)',
                       expression(paste("Fund balance"[t-1],"($1M)")),
                       expression(paste("ln(Revenue)"[t-1])),
                       expression(paste("ln(Revenue)"[t-1],":Health viol."[t-1:4])),
                       expression(paste("Fund balance"[t-1],"($1M)",":Health viol."[t-1:4])),
                       expression(paste("Med. home value ($10k):Health viol."[t-1:4])))))+
  scale_fill_manual(name = 'Component',values=c('white','white','#E69F00','black')) +
  scale_color_manual(name = 'Component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=12),legend.title=element_text(size=12),strip.text=element_text(size=12),
        axis.text=element_text(size=10),
        axis.title=element_blank()) +guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))

ggplot2::ggsave('figure1',plot = gg_main,device = 'png',height=6,width=6,units = 'in',dpi = 450)


rev_lc_seq = c(c(10,50,100,250,500)*1000,c(1,2,5,10,15)*1000000)
log_mc_rev_lc_seq = log(rev_lc_seq) - mean(log(model_df$TOTAL_REVENUE_LAG1))
lc_revenue <- inla.make.lincombs(u_hviols_p3 = rep(1, length(rev_lc_seq)),
                                 "u_hviols_p3:u_log_total_revenue_p1" = 1 * log_mc_rev_lc_seq)
mod_lc_revenue = inla(as.formula(form_list[[2]]),c('binomial', 'gamma'),
                      control.fixed = list(expand.factor.strategy = "inla"),
                      data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                      control.inla = list(lincomb.derived.only=FALSE),
                      control.predictor=list(compute=TRUE),verbose=F,lincomb=lc_revenue)

gg_lc_rev = ggplot(mod_lc_revenue$summary.lincomb) + 
  geom_errorbar(aes(x=ID,ymax=`0.025quant`,ymin=`0.975quant`)) + 
  geom_point(aes(x=ID,y=mean,col='black'),pch=21,fill='white') +
  scale_x_continuous(name=expression(ln(Revenue[t-1])),
                     breaks=which(rev_lc_seq %in% c(10000,c(1,15)*1000000)),
                     labels=c("10k","$1M","$15M"),expand=c(0,0))  +
  theme_tufte(ticks=F) + 
 # scale_y_continuous(name = '95% Credible Interval') + 
  theme(axis.text=element_text(size=12),axis.title = element_text(size=12),
        legend.title = element_blank(),legend.text = element_text(size=12),
        legend.position = c(0.7,0.2),axis.title.y=element_blank()) + 
  geom_hline(yintercept = 0,lty=2,col='grey50') + 
  scale_color_manual(values='black', label=expression(beta[1] + beta[2]*ln(Revenue)[t-1]))


fb_lc_seq = c(-5,0,5,10,20,30,40,50)
mc_fb_lc_seq = fb_lc_seq - mean(model_df$TOTAL_FUND_BALANCE_LAG1/1000000)

lc_fund_balance <- inla.make.lincombs(u_hviols_p3 = rep(1, length(fb_lc_seq)),
                                      "u_fund_balance_1m_p1:u_hviols_p3" = 1 * mc_fb_lc_seq)
mod_lc_fund_balance = inla(as.formula(form_list[[3]]),c('binomial', 'gamma'),
                           control.fixed = list(expand.factor.strategy = "inla"),
                           data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                           control.inla = list(lincomb.derived.only=FALSE),
                           control.predictor=list(compute=TRUE),verbose=F,lincomb=lc_fund_balance)

gg_lc_fb = ggplot(mod_lc_fund_balance$summary.lincomb) + 
  geom_errorbar(aes(x=ID,ymax=`0.025quant`,ymin=`0.975quant`)) + 
  geom_point(aes(x=ID,y=mean,col='black'),pch=21,fill='white') +
  scale_x_continuous(name=expression(paste('Fund Balance')[t-1]),
                     breaks=which(fb_lc_seq %in% c(-5,20,50)),
                     labels=c("(-$5M)","$20M","$50M"),
                     expand=c(0,0))  +
  theme_tufte(ticks=F) + 
  #scale_y_continuous(name = '95% Credible Interval') + 
  theme(axis.text=element_text(size=12),axis.title = element_text(size=12),
        legend.title = element_blank(),legend.text = element_text(size=12),
        legend.position = c(0.65,0.1),axis.title.y=element_blank()) + 
  geom_hline(yintercept = 0,lty=2,col='grey50') + 
  scale_color_manual(values='black', label=expression(beta[1] + beta[3]*paste('Fund Balance'[t-1])))



mh_lc_seq = seq(5,75,5)
mc_mh_lc_seq = mh_lc_seq - mean(model_df$TRACT_WEIGHTED_MED_HOME_VALUE/10000)

lc_median_home <- inla.make.lincombs(u_hviols_p3 = rep(1, length(mh_lc_seq)),
                                     "u_med_home_10k:u_hviols_p3" = 1 * mc_mh_lc_seq)
mod_lc_median_home = inla(as.formula(form_list[[4]]),c('binomial', 'gamma'),
                          control.fixed = list(expand.factor.strategy = "inla"),
                          data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                          control.inla = list(lincomb.derived.only=FALSE),
                          control.predictor=list(compute=TRUE),verbose=F,lincomb=lc_median_home)

gg_lc_mh = ggplot(mod_lc_median_home$summary.lincomb) + 
  geom_errorbar(aes(x=ID,ymax=`0.025quant`,ymin=`0.975quant`)) + 
  geom_point(aes(x=ID,y=mean,col='black'),pch=21,fill='white') +
  scale_x_continuous(name=expression(paste('Median Home Value')[t-1]),
                     breaks=which(mh_lc_seq %in% c(10,40,70)),
                     labels=c("$100k","$400k","$700k"),
                     expand=c(0,0))+
  theme_tufte(ticks=F) + 
 # scale_y_continuous(name = '95% Credible Interval') + 
  theme(axis.text=element_text(size=12),axis.title = element_text(size=12),
        legend.title = element_blank(),legend.text = element_text(size=12),
        legend.position = c(0.42,0.1),axis.title.y=element_blank()) + 
  geom_hline(yintercept = 0,lty=2,col='grey50') + 
  scale_color_manual(values='black', label=expression(beta[1] + beta[4]*paste('Med. Home ($10k)')))
library(gridExtra)
library(grid)
gg_lc_arrange = arrangeGrob(gg_lc_rev,gg_lc_fb,gg_lc_mh,ncol=2,
                            left=textGrob("95% credible interval (log-odds)", rot=90,gp = gpar(fontsize=14, fontfamily="Times New Roman"), vjust=1))


ggsave('figure2',plot = gg_lc_arrange,device = 'png',height=4.5,width=6,units = 'in',dpi = 450)



######## robustness check: ratio statistics ######
base_ratio_form = "Y ~ 0 + mu.u + mu.y +
u_med_home_10k  + y_med_home_10k +
u_log_service_pop + y_log_service_pop + 
u_system_age + y_system_age + 
u_debt_outstanding_per_capita_10k_p1 + y_debt_outstanding_per_capita_10k_p1 +
u_fund_balance_per_capita_10k_p1 + y_fund_balance_per_capita_10k_p1 +
u_primary_ground + y_primary_ground + 
u_primary_purch + y_primary_purch + 
u_fwsd + y_fwsd  + 
u_wcid + y_wcid + 
u_hviols_p3 + y_hviols_p3+
u_log_total_revenue_per_capita_p1 + 
y_log_total_revenue_per_capita_p1"


test = inla(Y ~ 0 + mu.u + mu.y +
              u_med_home_10k  + y_med_home_10k +
              u_log_service_pop + y_log_service_pop + 
              u_system_age + y_system_age + 
              u_debt_outstanding_per_capita_10k_p1 + y_debt_outstanding_per_capita_10k_p1 +
              u_fund_balance_per_capita_10k_p1 + y_fund_balance_per_capita_10k_p1 +
              u_primary_ground + y_primary_ground + 
              u_primary_purch + y_primary_purch + 
              u_fwsd + y_fwsd  + 
              u_wcid + y_wcid + 
              u_hviols_p3 + y_hviols_p3+
              u_log_total_revenue_per_capita_p1 + 
              y_log_total_revenue_per_capita_p1 + 
              u_hviols_p3:u_log_total_revenue_per_capita_p1 + y_hviols_p3:y_log_total_revenue_per_capita_p1 +  
              f(u_fyear, model='iid') + f(y_fyear,model='iid') +
              f(u_PWS_ID, model='iid') + f(y_PWS_ID,model='iid'),
            c('binomial', 'gamma'),
            control.fixed = list(expand.factor.strategy = "inla"),
            data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
            control.predictor=list(compute=TRUE),verbose=T)

summary(test)
random_form


interaction_ratio_vary = c("", "u_hviols_p3:u_log_total_revenue_per_capita_p1 + y_hviols_p3:y_log_total_revenue_per_capita_p1",
                           "u_hviols_p3:u_fund_balance_per_capita_10k_p1 + y_hviols_p3:y_fund_balance_per_capita_10k_p1",
                           "u_hviols_p3:u_med_home_10k + y_hviols_p3:y_med_home_10k")

form_ratio_temps = expand.grid(gsub('\\\n','',base_ratio_form),interaction_ratio_vary,
                               gsub('\\\n','',random_form))

form_ratio_list = lapply(1:nrow(form_ratio_temps),
                         function(x) paste(form_ratio_temps[x,1],form_ratio_temps[x,2],form_ratio_temps[x,3],sep='+'))
as.formula(base_ratio_form)
test = inla(as.formula(paste(base_ratio_form,random_form,sep='+')),c('binomial', 'gamma'),data = idat)
summary(test)

ratio_results = lapply(form_ratio_list[[1]],function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                                                control.fixed = list(expand.factor.strategy = "inla"),
                                                                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                                control.predictor=list(compute=TRUE),verbose=F))


names(ratio_results) = paste(str_extract(gsub('.*:','',interaction_vary),' |revenue|balance|home'),str_extract(interaction_vary,'viols_p[0-5]'),sep='_')
names(ratio_results)[[1]] = 'main_p3' 

coef_results = do.call(rbind,lapply(1:length(ratio_results),function(x) ratio_results[[x]]$summary.fix[,c(1,3,5)] %>% 
                                      mutate(COEF = rownames(.),MODEL = names(ratio_results)[x],
                                             LIK = ifelse(grepl('^y_|\\.y$',COEF),'1.Gamma','2.Binomial')) %>%
                                      mutate(COEF =  gsub(':y_|:u_',':',gsub('^y_|^u_','',COEF))) %>% 
                                      mutate(COEF = gsub('_p[1-5]','',COEF)) %>%
                                      mutate(SIG = ifelse(`0.025quant`<0&`0.975quant`>0,0,1)))) %>%
  mutate(SIG_FILL = paste(SIG,LIK))


coef_results$COEF = fct_recode(f = coef_results$COEF,
                               `Management viol.` = 'mviols', 
                               `Health viol.` = 'hviols', 
                               `FWSD` = 'fwsd', 
                               `WCID` = 'wcid', 
                               `Fund balance ($1M)` = 'fund_balance_per_capita',
                               `ln(Revenue) per capita` = 'log_total_revenue_per_capita',
                               `ln(Revenue) per capita:Health viol.` = 'hviols:log_total_revenue_per_capita',
                               `Fund balance ($1M) per capita:Health viol.` = 'fund_balance_per_capita:hviols',
                               `Med. home value ($10k):Health viol.` = 'med_home_10k:hviols',
                               `System age (y)` = 'system_age',
                               `ln(Service con.)` = 'log_service_pop',
                               Purchaser = 'primary_purch',
                               Groundwater = 'primary_ground',
                               `Med. home value ($10k)` = 'med_home_10k',
                               `Outstanding debt ($1M) per capita` = 'per_capita_1k')


coef_results$COEF = factor(coef_results$COEF,levels = c("mu.y" ,"mu.u",  "% Poverty",#"% Bachelor's",
                                                        "Med. home value ($10k)",
                                                        '# facilities','System age (y)','Groundwater','Purchaser',
                                                        # 'Purchaser:Groundwater',
                                                        #  'Wholesaler',
                                                        'FWSD','WCID',
                                                        "ln(Service con.)",
                                                        'Outstanding debt ($1M)',
                                                        'Fund balance ($1M)',
                                                        'ln(Revenue)',
                                                        'Management viol.','Health viol.',
                                                        'ln(Revenue):Health viol.',"Fund balance ($1M):Health viol.",
                                                        "Med. home value ($10k):Health viol."))


coef_results$COEF = fct_rev(coef_results$COEF)
p3_results = coef_results %>% filter(!grepl('^mu',COEF),grepl('p3',MODEL))
p3_results$MODEL = as.factor(p3_results$MODEL) 

levels(p3_results$MODEL) = c('Model 3','Model 4','Model 1','Model 2')
p3_results$MODEL = factor(p3_results$MODEL,levels = c('Model 1','Model 2','Model 3','Model 4'))

gg_ratio = ggplot(p3_results) + 
  geom_hline(aes(yintercept = 0),lty=2,col='grey50') +
  geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=COEF,colour = LIK),position = position_dodge(width = .8),lwd=2) + 
  geom_pointrange(aes(y=mean,ymin = mean,ymax = mean,x=COEF,colour = LIK,fill=SIG_FILL),position = position_dodge(width = .8),shape=21,size=.5) + 
  coord_flip() + facet_grid(~MODEL,scales='free_x') +  
  theme_tufte(ticks=F) + theme(legend.position = c(0.825,0.15)) +
  #scale_shape_manual(values=c(19)) +
  scale_fill_manual(name = 'Component',values=c('white','white','#E69F00','black')) +
  scale_color_manual(name = 'Component',values=c('#E69F00','black'),labels=c('Gamma','Binomial')) + #,'#E69F00','black')) +
  theme(legend.text=element_text(size=12),legend.title=element_text(size=12),strip.text=element_text(size=12),axis.text=element_text(size=12),
        axis.title=element_blank()) +guides(fill='none',colour = guide_legend(override.aes = list(shape = 19)))

gg_ratio

######### TEST alternative economic capacity specifications ######
##########  run across violation windows #######
# reduced_form = "Y ~ 0 + mu.u + mu.y +
# u_log_service_pop + y_log_service_pop + 
# u_system_age + y_system_age + 
# u_debt_outstanding_1m_p1 + y_debt_outstanding_1m_p1 +
# u_fund_balance_1m_p1 + y_fund_balance_1m_p1 +
# u_primary_ground + y_primary_ground + 
# u_primary_purch + y_primary_purch + 
# u_fwsd + y_fwsd  + 
# u_wcid + y_wcid + 
# u_mviols_p3 + y_mviols_p3 + 
# u_hviols_p3 + y_hviols_p3 +
# u_log_total_revenue_p1+ 
# y_log_total_revenue_p1"
# 
# economic_capacity_specs =c("u_med_home_10k  + y_med_home_10k",
#                            "u_log_med_home  + y_log_med_home",
#                            "u_med_income_10k + y_med_income_10k",
#                            "u_log_med_income + y_log_med_income")
#    
# 
# econ_spec_form_temps = expand.grid(gsub('\\\n','',reduced_form),economic_capacity_specs,
#                          gsub('\\\n','',random_form))
# 
# econ_spec_form_list = lapply(1:nrow(econ_spec_form_temps),
#                    function(x) paste(econ_spec_form_temps[x,1],econ_spec_form_temps[x,2],econ_spec_form_temps[x,3],sep='+'))
# 
# econ_spec_results = lapply(econ_spec_form_list,function(form) inla(as.formula(form),c('binomial', 'gamma'),
#                                                     control.fixed = list(expand.factor.strategy = "inla"),
#                                                     data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                                                     control.predictor=list(compute=TRUE),verbose=F))
# 
# 
# names(econ_spec_results) = c('home','log home','income','log income')
# 
# Reduce(full_join,lapply(1:length(econ_spec_results),function(x) econ_spec_results[[x]]$summary.fixed[,c(1,3,5)] %>% 
#                           mutate(coef = rownames(.),model = names(econ_spec_results)[x]))) %>% filter(grepl('home|income',coef))

###################




##########  run across violation windows #######
##########  run across violation windows #######
base_noviol_form = "Y ~ 0 + mu.u + mu.y +
u_med_home_10k  + y_med_home_10k +
u_log_service_pop + y_log_service_pop + 
u_system_age + y_system_age + 
u_debt_outstanding_1m_p1 + y_debt_outstanding_1m_p1 +
u_fund_balance_1m_p1 + y_fund_balance_1m_p1 +
u_primary_ground + y_primary_ground + 
u_primary_purch + y_primary_purch + 
u_fwsd + y_fwsd  + 
u_wcid + y_wcid + 
u_log_total_revenue_p1 + 
y_log_total_revenue_p1"

capacity_interaction_template = c("u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp",
                                  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_log_total_revenue_p1 +  y_hviols_pp:y_log_total_revenue_p1" ,
                                  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_fund_balance_1m_p1 +  y_hviols_pp:y_fund_balance_1m_p1",
                                  "u_mviols_pp + y_mviols_pp+ u_hviols_pp + y_hviols_pp+u_hviols_pp:u_med_home_10k +  y_hviols_pp:y_med_home_10k")

capacity_interactions = unlist(lapply(as.list(paste0('p',1:5)),function(x) gsub('pp',x,capacity_interaction_template)))

varyviol_form_temps = expand.grid(gsub('\\\n','',base_noviol_form),capacity_interactions,gsub('\\\n','',random_form))

varyviol_form_list = lapply(1:nrow(varyviol_form_temps),
                            function(x) paste(varyviol_form_temps[x,1],varyviol_form_temps[x,2],
                                              varyviol_form_temps[x,3],sep='+'))

varyviol_results = lapply(varyviol_form_list,function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                                                 data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                                 control.predictor=list(compute=TRUE),verbose=F))

viol_robust = Reduce(full_join,lapply(1:length(varyviol_results),function(x) varyviol_results[[x]]$summary.fixed[,c(1,3,5)] %>% 
                                        mutate(coef = rownames(.)) %>% filter(grepl('viol',coef))))


test = data.frame(cbind(rep(1:5,each=4),rep(c('main','rev','bal','home'),5),sapply(varyviol_results,function(x) x$waic$waic)))
test$X3 = as.numeric(as.character(test$X3))
ggplot() + geom_path(aes(x=X1,y=X3,col=X2,group=X2),test)


test = data.frame(cbind(rep(1:5,each=4),rep(c('main','rev','bal','home'),5),sapply(varyviol_results,function(x) x$dic$dic)))
test$X3 = as.numeric(as.character(test$X3))
ggplot() + geom_path(aes(x=X1,y=X3,col=X2,group=X2),test)


system_df = read_csv('input/epa_sdwis/system_base_master.csv')
system_type = rep(NA,nrow(system_df))
system_type[grepl('WCID',system_df$PWS_NAME)] = 'WCID'
system_type[grepl('MWA',system_df$PWS_NAME)] = 'MWA'
system_type[grepl('PUD$| PUD ',system_df$PWS_NAME)] = 'MUD'
system_type[grepl('MWD',system_df$PWS_NAME)] = 'MWD'
system_type[grepl(' MUD | MUD$',system_df$PWS_NAME)] = 'MUD'
system_type[grepl('WSC',system_df$PWS_NAME)] = 'WSC'
system_type[grepl('SUD$| SUD ',system_df$PWS_NAME)] = 'SUD'
system_type[grepl('FWSD',system_df$PWS_NAME)] = 'FWSD'

system_type[system_df$PWS_NAME=="GENERATION PARK MANAGEMENT DISTRICT"] = 'MMD'
system_type[system_df$PWS_NAME=="SIENNA PLANTATION MANAGEMENT DISTRICT"] = 'MMD'
system_type[system_df$PWS_NAME=="LOWER NECHES VALLEY AUTHORITY"] = 'Other District'
system_type[system_df$PWS_NAME %in% c("LAZY RIVER IMPROVEMENT DISTRICT"  , "INVERNESS FOREST IMPROVEMENT DISTRICT" ,
                                      "COMMODORE COVE IMPROVEMENT DISTRICT","CLEAR LAKE CITY WATER AUTHORITY" )] = 'WCID'
system_type[system_df$PWS_NAME=="HARRIS COUNTY IMPROVEMENT DISTRICT 18"] = 'MMD'
system_type[system_df$PWS_NAME=="SEQUOIA IMPROVEMENT DISTRICT"|system_df$PWS_NAME=="TIMBERLAKE IMPROVEMENT DISTRICT"] = 'MUD'
system_type[grepl(' UD | UD$',system_df$PWS_NAME)] = 'MUD'
system_type[is.na(system_type)&grepl('WATER AUTHORITY',system_df$PWS_NAME)] = 'Other District'
system_type[grepl('CITY OF|VILLAGE OF',system_df$PWS_NAME)] = 'City'
system_type[grepl('SUBDIVISION|MOBILE HOME|MHP',system_df$PWS_NAME)] = 'Private'
system_type[grepl('TBCD',system_df$PWS_NAME)] = 'Other District'
system_type[grepl("SIENNA PLANTATION THE WOODS",system_df$PWS_NAME)] = 'Other District'
system_type[system_df$`Owner Type`=='Private'] = 'Private'
system_type[system_df$PWS_NAME=="SUN RANCH WATER SYSTEM"] = 'Private'
system_type[system_df$PWS_NAME=="5TH STREET WATER SYSTEM"] = 'Other District'
system_type[system_df$PWS_NAME %in% c("HOOP N HOLLER LAKE ESTATES","OLD SNAKE RIVER ESTATES EAST", "BIG THICKET LAKE ESTATES 1","PATTON LAKE CLUB" ,"HUNTERS COVE SEC 1")] = 'WSC'
system_type[system_df$PWS_NAME %in% c("COE INDUSTRIAL PARK","ALICE ACRES MOBILE HOME SUBDIVISION","2920 WEST SUBDIVISION" ,"BRANDYWINE OAKS","BRANDYWINE PINES" ,"CYPRESS CROSSING",
                                      "KICKAPOO FARMS SUBDIVISION","CYPRESS PASS ESTATES" ,"WILLOW OAKS MOBILE HOME SUBDIVISION" ,"VILLAGE OF NEW KENTUCKY","RED OAK TERRACE" ,
                                      "HOUSE CORRAL STREET WATER SYSTEM" ,"TRAILWOOD SUBDIVISION"  ,"TIMBERWILDE MH SUBDIVISION" ,"GRANT ROAD ESTATES MOBILE HOME SUB",
                                      "TOWERING OAKS AND ROSEWOOD HILLS SUBDIVI","COE COUNTRY", "ALLENWOOD SUBDIVISION", "ARMADILLO WOODS SUBDIVISION" ,"RIMWICK FOREST" , "RUSTIC OAKS SUBDIVISION" , "MINK BRANCH VALLEY"  ,
                                      "KIPLING OAKS 1", "KIPLING OAKS AND TIMBERGREEN"  ,"ESTATES OF HOLLY LAKES","PLEASANT FOREST SUBDIVISION"  ,"DEER RIDGE SUBDIVISION", 
                                      "SHADY ACRES" , "HUNTERS RETREAT","TREICHEL WOODS ESTATES"  )] = 'SUD'

system_df$system_type = system_type

temp = system_df %>% filter(system_type %in% c('MUD','WCID','FWSD'))
table(temp$system_type,temp$`Activity Status`)



















summary(varyviol_results[[20]])
viol_robust %>% filter(grepl('^u_hv',coef)) %>% arrange(coef)


gginla = function(mod){
  ggplot(mod$summary.fixed) + geom_linerange(aes(ymin = `0.025quant`,ymax=`0.975quant`,x=rownames(mod$summary.fixed))) + coord_flip()
}

gginla(binary_results[[1]])

round(binary_results[[1]]$summary.fixed[,c(1,3,5)],2)
summary(idat$u_total_revenue_per_capita_p1)
summary(binary_results[[1]])
cor(model_df$TOTAL_REVENUE_LAG1,model_df$Pop_Served)
ggplot() + 
  geom_histogram(aes(x=model_df$TOTAL_REVENUE_PER_CAPITA_LAG1))

summary(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1)

model_df[model_df$TOTAL_REVENUE_PER_CAPITA_LAG1>1000000,] %>% 
  dplyr::select(PWS_NAME,Pop_Served,TOTAL_REVENUE_LAG1,TOTAL_REVENUE_PER_CAPITA_LAG1,TOTAL_DEBT_SERVICE_LAG1) 


hist(log(model_df$TOTAL_REVENUE_PER_CAPITA_LAG1[model_df$TOTAL_REVENUE_PER_CAPITA_LAG1<1000000]/1000))
library(tidyverse)
summary(idat$u_log_total_revenue_per_capita_p1)
round(binary_results[[1]]$summary.fixed[,c(1,3,5)],3)


table(ifelse(model_df$hviol_count_p3>0,'Violation','No viol.'),ifelse(model_df$NewMoneySize>0,'Issue Debt','No Issue'))

cor(model_df$TOTAL_DEBT_SERVICE_LAG1,model_df$DebtIssuedInFYEAR)

spec_results[[1]]$summary.fixed

summary(log(model_df$TOTAL_REVENUE_LAG1/100+1))

ggplot() + scale_x_continuous(limits=c(-5,250))+
  # geom_point(aes(y = spec_results[[1]]$summary.fitted.values[0:n,'mean'],
  #                         x  = exp(idat$y_log_total_revenue_p1[1:n])/1000000,
  #                         col = as.factor(idat$y_hviols_p3[1:n]))) +
  geom_point(aes(y = spec_results[[4]]$summary.fitted.values[(1:n),'mean'],
                 x  = idat$u_log_total_revenue_1m_p1))+
  stat_smooth(method='lm',aes(y = spec_results[[4]]$summary.fitted.values[(1:n),'mean'],
                              x  = idat$u_total_revenue_per_capita_1k_p1[1:n]^2,
                              col = as.factor(idat$u_hviols_p3[1:n])),se = F)



stat_smooth(method='lm',aes(y = spec_results[[1]]$summary.fitted.values[(n+1):(n*2),'0.025quant'],
                            x  = exp(idat$y_log_total_revenue_p1[(n+1):(n*2)])/1000000,
                            col = as.factor(idat$y_hviols_p3[(n+1):(n*2)])),se = F,lty=2)+
  stat_smooth(method='lm',aes(y = spec_results[[1]]$summary.fitted.values[(n+1):(n*2),'0.975quant'],
                              x  = exp(idat$y_log_total_revenue_p1[(n+1):(n*2)])/1000000,
                              col = as.factor(idat$y_hviols_p3[(n+1):(n*2)])),se = F,lty=2)+
  scale_color_colorblind(name ='',labels=c('No healh violation','Violation w/in past 3 years'))+
  theme(legend.position = c(0.2,0.8),legend.title=element_blank(),
        legend.background = element_rect(colour=NA,fill=NA))



table(model_df$NewMoneySize>0)
613/(2639+613)
102/(3150+102)

r = binary_results[[1]]
summary(r)
lc = inla.make.lincombs( 
  u_total_revenue_1m =  r$model.matrix[, "u_total_revenue_1m_p1"], 
  `u_total_revenue_1m:u_hviols_p3` =  r$model.matrix[, "u_total_revenue_1m_p1:u_hviols_p3"]) 

binary_results_test = lapply(form_list[[1]],function(form) inla(as.formula(form),c('binomial', 'gamma'),
                                                                data=idat, control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
                                                                control.predictor=list(compute=TRUE),verbose=F,lincomb = lc))

save.image('scratch_results_violation_binary.RData')


