library(tidyverse)
library(INLA)
library(ggridges)
mod_set = readRDS('scratch/proj6/mod_results.RDS')

marge_density = do.call(rbind,lapply(names(mod_set$mod_health$marginals.fixed),function(name){
  mod_set$mod_health$marginals.fixed[[name]] %>% data.frame(.,stringsAsFactors = F) %>% mutate(FE = name)}))

fe_df = do.call(rbind,lapply(seq_along(mod_set),function(x) {df = mod_set[[x]]$summary.fixed[,c(1,3,5)] %>% mutate(Model = names(mod_set)[x])
        df$FE = rownames(mod_set[[x]]$summary.fixed); df}))

fe_df$SIG = abs((fe_df$`0.025quant`<0 & fe_df$`0.975quant`>0) - 1)
library(ggthemes)
library(scales)
library(forcats)
fe_df$FE = as.factor(fe_df$FE)

fe_df$FE = fct_recode(fe_df$FE,
"scaled(Health violations in prior 3 years)" = "rescale(HEALTH_VIOL_COUNT_PERIOD_Prior3Years)",
"scaled(Management violations in prior 3 years)" = "rescale(MANAGEMENT_VIOL_COUNT_PERIOD_Prior3Years)",
"scaled(Total Revenue (t-1))" = "rescale(TOTAL_REVENUE_L1)",
"scaled(Total tax rate (t-1))" = "rescale(TOTAL_TAX_RATE_L1)",
"scaled(Revenue per connection (t-1))" = 'rescale(REV_PER_CONNECTION_L1)',
"Elected board" = "BOARD_SELECTIONElected",
"Special law district" = "SPECIAL_LAW",
"scaled(CBSA_HHI)" = "rescale(CBSA_HHI)",
"scaled(# districts in county)" = "rescale(county_district_count)",
"scaled(Service_Connections)" = "rescale(Service_Connections)",
"scaled(Median home price)" = "rescale(MEDIAN_HOME_PRICE)",
"scaled(% population change)" = 'rescale(Prop_Population_Change)', 
"scaled(Wholesale customers)" = "rescale(WHOLESALE_REGULAR_POP)",
"Wastewater treatment" = "RETAIL_WASTEWATER",
"scaled(District age)" = "rescale(DISTRICT_AGE)",
'scaled(% owner occupied homes)' = "rescale(Prop_Owner_Occupied_Household)",
'scaled(Median structure age)' = "rescale(Median_Structure_Age)",
'scaled(Total debt outstanding (t-1))' = 'rescale(TotDebtServiceOutstanding_P1)',
'New water debt (past 5 years)' = 'Any_Debt_Issued_P5',
'New water debt (past 5 years) * scaled(Issue size)' = 'Any_Debt_Issued_P5:rescale(Debt_Issued_P5)',
'scaled(Service_Connections):scaled(TOTAL_REVENUE_L1)' = 'rescale(Service_Connections):rescale(TOTAL_REVENUE_L1)',
'scaled(Service_Connections):scaled(CBSA_HHI)'  = 'rescale(Service_Connections):rescale(CBSA_HHI)')


fe_df$FE = fct_relevel(fe_df$FE,
                       "(Intercept)",
                      "scaled(Health violations in prior 3 years)",
                      "scaled(Management violations in prior 3 years)" ,
                      "scaled(Total Revenue (t-1))",
                      "scaled(Revenue per connection (t-1))",
                      "scaled(Total tax rate (t-1))",
                      'New water debt (past 5 years)',
                      'New water debt (past 5 years) * scaled(Issue size)',
                      "scaled(CBSA_HHI)" ,
                      "scaled(# districts in county)" ,
                      "scaled(Median income",
                      "scaled(Median home price)",
                      'scaled(% population change)',
                      "scaled(Wholesale customers)",
                      "Wastewater treatment",
                      "Elected board" ,
                      "Special law district" ,
                      "scaled(Service_Connections)" ,
                      "scaled(District age)",
                      "scaled(Median structure age)",
                      'scaled(% owner occupied homes)',
                      'scaled(Service_Connections):scaled(TOTAL_REVENUE_L1)',
                      'scaled(Service_Connections):scaled(CBSA_HHI)')
fe_df$FE = fct_rev(fe_df$FE)


#### grouping indicators


figure1 = ggplot(data = fe_df[(!grepl('Intercept',fe_df$FE)) & grepl('0$',fe_df$Model),]) + 
  geom_errorbar(aes(x = FE,ymax = `0.975quant`,ymin = `0.025quant` ),position=position_dodge(width = 0.5),width=0.5) + 
  geom_point(aes(x = FE,y = mean,fill=paste0(Model,SIG)),position=position_dodge(width = 0.5),pch=21) + 
  coord_flip() + theme_bw() + scale_fill_manual(values= c('white','black')) + #,'white','#FF7F0E')) + 
  scale_y_continuous(name = '95% credible interval and posterior mean') +# ,limits=c(-1.2,1.2)) +
  #scale_x_discrete(name = 'Standarized regression coefficients')
  #scale_colour_tableau(name='Violation type') + #,labels=expression(paste("standardized(", beta,")"))) + 
  guides(fill=FALSE) + 
  theme(axis.title = element_text(size=12),legend.position= c(0.8,0.2),axis.title.y = element_blank(),
        legend.background = element_rect(fill = alpha('white',0.25)),legend.title = element_blank(),
        legend.text = element_blank()) + 
  ggtitle('Health violation occurence by fiscal year') +
  geom_hline(yintercept=0,lty=2,col = 'grey50') +
  NULL

figure1

figure2 = ggplot(data = fe_df[(!grepl('Intercept',fe_df$FE)) & grepl('1$',fe_df$Model),]) + 
  geom_errorbar(aes(x = FE,ymax = `0.975quant`,ymin = `0.025quant` ),position=position_dodge(width = 0.5),width=0.5) + 
  geom_point(aes(x = FE,y = mean,fill=paste0(Model,SIG)),position=position_dodge(width = 0.5),pch=21) + 
  coord_flip() + theme_bw() + scale_fill_manual(values= c('white','black')) + #,'white','#FF7F0E')) + 
  scale_y_continuous(name = '95% credible interval and posterior mean') +#,limits=c(-1.2,1.2)) +
  #scale_x_discrete(name = 'Standarized regression coefficients')
  #scale_colour_tableau(name='Violation type') + #,labels=expression(paste("standardized(", beta,")"))) + 
  guides(fill=FALSE) + 
  theme(axis.title = element_text(size=12),legend.position= c(0.8,0.2),axis.title.y = element_blank(),
        legend.background = element_rect(fill = alpha('white',0.25)),legend.title = element_blank(),
        legend.text = element_blank()) + 
  ggtitle('Health violation occurence by fiscal year') +
  geom_hline(yintercept=0,lty=2,col = 'grey50') +
  NULL

figure2
fiscal_RE = rbind(mod_set$mod_health0$summary.random$FISCAL_YEAR_START_YEAR %>% mutate(Model = 'Health (restricted)'))
             #   mod_set$mod_health1$summary.random$FISCAL_YEAR_START_YEAR %>% mutate(Model = 'Health (unrestricted)'))
fiscal_RE$ID = fiscal_RE$ID + 1

figureA1 = ggplot(data=fiscal_RE,aes(x = ID,y = mean,ymin = `0.025quant`,ymax = `0.975quant`,group = Model)) + #, colour = Model)) +
  geom_point(position = position_dodge(width = 0.5)) + geom_path(position = position_dodge(width=0.5)) + 
  geom_errorbar(position = position_dodge(width=0.5)) + 
  theme_bw() + scale_color_tableau() + theme(legend.position = c(0.3,0.2),
                                             legend.text = element_text(size = 12),legend.title = element_blank(),
                                             axis.title = element_text(size = 12))+
  ggtitle('Random walk (first order) term for fiscal year') + 
  scale_y_continuous(name = '95% credible interval') + scale_x_continuous(name = 'Fiscal year',
                                                                          breaks=seq(2008,2018,2)) + geom_hline(yintercept = 0,lty=2,col = 'grey50')

figureA1
cbsa_all = tigris::core_based_statistical_areas()
cbsa_tx = cbsa_all[grepl('TX',cbsa_all$NAME),]
states  = tigris::states()
tx = states[states@data$NAME=='Texas',]

cbsa_RE = rbind(mod_set$mod_health0$summary.random$CBSA %>% mutate(Model = 'Health (restricted)'))
#                  mod_set$mod_health1$summary.random$CBSA %>% mutate(Model = 'Health (unrestricted)'))
cbsa_tx_df = fortify(cbsa_tx,region = 'CBSAFP')

cbsa_tx_df$NAMELSAD = cbsa_tx@data$NAMELSAD[match(cbsa_tx_df$id,cbsa_tx@data$CBSAFP)]

cbsa_tx_df$mean = cbsa_RE$mean[match(cbsa_tx_df$NAMELSAD,cbsa_RE$ID)]
cbsa_tx_df$sd = cbsa_RE$sd[match(cbsa_tx_df$NAMELSAD,cbsa_RE$ID)]

tx_df = fortify(tx)

figureA2a = ggplot() + geom_path(data = tx_df,aes(x = long,y = lat,group = group)) + 
  geom_polygon(data = cbsa_tx_df[!is.na(cbsa_tx_df$mean),],aes(x = long,y=lat,group = group,fill = mean),col = 'grey50') + 
  scale_fill_viridis_c(name = 'Mean',option = 'A') + theme_map() + ggtitle('CBSA random intercepts')+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),title = element_text(size = 14))

figureA2b = ggplot() + geom_path(data = tx_df,aes(x = long,y = lat,group = group)) + 
  geom_polygon(data = cbsa_tx_df[!is.na(cbsa_tx_df$sd),],aes(x = long,y=lat,group = group,fill = sd),col = 'grey50') + 
  scale_fill_viridis_c('Standard\n deviation',option = 'D',breaks=c(0.40,0.50)) + theme_map() + ggtitle('')+
  theme(legend.text = element_text(size = 12),legend.title = element_text(size = 12),title = element_text(size = 14))
library(gridExtra)
grid.arrange(figureA2a,figureA2b,ncol = 2)

plot(x = log(full_data$county_district_count[match(mod_set$mod_health$summary.random$COUNTY_FIPS$ID,full_data$COUNTY_FIPS)]),
y = mod_set$mod_health$summary.random$COUNTY_FIPS$mean)


ggB = ggplot() + 
  geom_errorbar(data= fe_df[!grepl('Intercept',fe_df$FE) & fe_df$Model=='Management',],aes(x = FE,ymax = `0.975quant`,ymin = `0.025quant`,group = FE )) + 
  geom_point(data= fe_df[!grepl('Intercept',fe_df$FE)& fe_df$Model=='Management',],aes(x = FE,y = mean,group = FE,
                                                            fill=as.factor(SIG)),pch=21) + 
  coord_flip() + theme_bw() + scale_fill_manual(values= c('white','black')) +
  scale_y_continuous(name = '95% credible interval and posterior mean') +
  guides(fill=FALSE) + theme(axis.title.y = element_blank(),axis.text.y = element_blank()) + ggtitle('Management Violations Per Year')

library(gridExtra)
grid.arrange(ggA,ggB,ncol=2)
fe_df$mean = round(fe_df$mean,2)
fe_df$`0.025quant` = round(fe_df$`0.025quant`,2)
fe_df$`0.975quant` = round(fe_df$`0.975quant`,2)
fe_df$Interval = paste(formatC(fe_df[,2],digits = 2,format = 'g'),formatC(fe_df[,3],digits = 2,format = 'g'),sep=',')

levels(fe_df$FE)

htmlTable(data.frame(c(fe_df[fe_df$Model == 'mod_health0',7],rep(NA,5)),fe_df[fe_df$Model == 'mod_health1',7]),
          rnames = fe_df$FE,
          rgroup = c('Organizational performance','Financial resources','Management resources',
                     'Context complexity','Context turbulence','Context munificence','Service features'),
          c(2,3,)
          header = c('Restricted model','Unrestricted model'))
