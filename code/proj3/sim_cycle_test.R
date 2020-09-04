rm(list=ls())
library(broom)
library(statnet)
library(tidyverse)
library(ergm.terms.contrib)
library(ergm.userterms)
library(pbapply)
load('scratch/ergm_mcmc_psock_results_unconst.RData')

table((net_list$FINANCIAL %v% 'Make_Buy')[1:net_list$FINANCIAL$gal$bipartite])

sim_m1 = simulate(m1B,nsim=1000,sequential = F,monitor=~cycle(4),
                   control = control.simulate.ergm(MCMC.burnin=10000,statsonly = T,
                                                    MCMC.interval=1500,verbose = T,
                                                    set.seed = 24))

sim_m2 = simulate(m2B,nsim=1000,sequential = F,monitor=~cycle(4),
                  control = control.simulate.ergm(MCMC.burnin=10000,statsonly = T,
                                                  MCMC.interval=1500,verbose = T,
                                                  set.seed = 24))

sim_m3 = simulate(m3B,nsim=1000,sequential = F,monitor=~cycle(4),
                  control = control.simulate.ergm(MCMC.burnin=10000,statsonly = T,
                                                  MCMC.interval=1500,verbose = T,
                                                  set.seed = 24))


sr1 = do.call(rbind,pblapply(sim_m1,function(n) summary(n ~ edges + cycle(4)),cl = 10))
sr2 = do.call(rbind,pblapply(sim_m2,function(n) summary(n ~ edges + cycle(4)),cl = 10))
sr3 = do.call(rbind,pblapply(sim_m3,function(n) summary(n ~ edges + cycle(4)),cl = 10))


net_list$FINANCIAL
net_list$MANAGERIAL
net_list$TECHNICAL
summary(m1)
net_list$FINANCIAL

do.call(rbind,list(summary(exp((net_list$FINANCIAL %v% 'Log_Acreage')[1:net_list$FINANCIAL$gal$bipartite])),
summary((net_list$FINANCIAL %v% 'AGE')[1:net_list$FINANCIAL$gal$bipartite]),
summary(exp((net_list$FINANCIAL %v% 'Log_Service_Pop')[1:net_list$FINANCIAL$gal$bipartite])),
summary(exp((net_list$FINANCIAL %v% 'Log_Bonds_Outstanding_Per_Connection')[1:net_list$FINANCIAL$gal$bipartite])),
summary(exp((net_list$FINANCIAL %v% 'Log_Revenue_Per_Connection')[1:net_list$FINANCIAL$gal$bipartite])),
summary((net_list$FINANCIAL %v% 'Viol_Points_5yr')[1:net_list$FINANCIAL$gal$bipartite])))[,c(1,4,3,6)]


711-532
624-532
summary(sr[,2])
summary(sr2[,2])
summary(sr3[,2])

summary(net_list$MANAGERIAL~cycle(4))
summary(net_list$TECHNICAL~cycle(4))




# save.image('scratch_bip5.RData')
# bt_gof <- btergm::gof(mod_main,nsim= 1000)
# save.image('scratch_bip5.RData')

#  
# library(broom)
# 
# results = tidy(mod_main,conf.int = T) %>% select(-mcmc.error,-std.error,-p.value)
# 
# results$term <- fct_recode(results$term, `GWD System (a=0.5)`="gwb1deg0.5",`Edges` = "edges",
#            `GWD Personnel (a=0.5)`="gwb2deg0.5",`GWNSP (a=0.5)` =  "gwnsp.fixed.0.5"  ,
#            `Groundwater`="b1factor.Water_Type.GW",`Size: >10,000` = "b1factor.System_4_Category.>10,000" ,
#            `Size: 3,301-10,000` = "b1factor.System_4_Category.3,301-10,000",
#            `Size: 501-3,300` = "b1factor.System_4_Category.501-3,300",
#            `Private` ="b1factor.Owner_Type.Private" ,`Public Corp.`= "b1factor.Owner_Type.Public_Corp"  ,
#            `SD` = "b1factor.Owner_Type.SD" ,`Works for buyer` = "edgecov.works_for_buyer",
#            `Works for seller` = "edgecov.works_for_seller" ,`Works for co-customer`= "edgecov.works_for_co_customer"  ,
#            `Groundwater, same authority` = "edgecov.works_for_gw_same_gcd")
# results$term <-  fct_relevel(results$term, 'Edges',"GWD Personnel (a=0.5)","GWD System (a=0.5)","GWNSP (a=0.5)",
#                             "Size: 501-3,300","Size: 3,301-10,000","Size: >10,000","Private",'Public Corp.',"SD",
#                             'Works for co-customer', 'Works for seller','Works for buyer','Groundwater, same authority')
# 
# 
# knitr::kable(results)
# 
# ggplot(results[results$term!='edges',],aes(y=estimate,ymin=conf.low,ymax=conf.high,x=term)) + 
#   geom_errorbar(lwd=.5,width=0.2) + coord_flip() + theme_bw() + 
#   geom_hline(yintercept=0,lty=2,col='grey50')
# 
# 
# probs <- eprobs %>% gather(Param,Change,-tie,-i,-j,-t,-probability)
# 
# 
#                      
# 
# 
# 
# 
# eprobs <- edgeprob(mod_main)
# 
# 
# library(GGally)
# plot(net)
# ggnet2(net)
# 
# net
# summary(net)
# 
# norm_bet <- betweenness(net,gmode='graph',diag = T,rescale = T)
# 
# temp = sna::kcycle.census(net, maxlen = 4, mode = "graph",  
#                    tabulate.by.vertex = TRUE, cycle.comembership = c( "bylength"))
# cycle_4_count <- as.data.frame(temp$cycle.count[3,]) %>% mutate(PWS_ID = rownames(.)) %>% filter(PWS_ID != 'Agg') %>%
#        filter(grepl("^TX",PWS_ID)) %>% rename(cycle_4 = `temp$cycle.count[3, ]`)
# cycle_4_count <- left_join(cycle_4_count,system_df)
# cycle_4_count$Owner_Type <- (net %v% 'Owner_Type')[match(cycle_4_count$PWS_ID,network.vertex.names(net))]
# cycle_4_observed <- cycle_4_count %>% group_by(Primary_Source_Type) %>% summarise(sum_4_cycle = sum(cycle_4))
# 


# library(tidyverse)
cycle_sim_counts <- lapply(1:length(sim_4cycle),function(x)
  as.data.frame(sim_4cycle[[x]]$cycle.count[3,-1]) %>% mutate(PWS_ID = rownames(.)) %>% filter(grepl('^TX[0-9]',PWS_ID)) %>%
    mutate(sim = x) %>% rename(cycle4vertex =`sim_4cycle[[x]]$cycle.count[3, -1]`))

cycle_sims <- do.call(rbind,cycle_sim_counts)
cycle_sims$Size <- (net %v% 'System_4_Category')[match(cycle_sims$PWS_ID,network.vertex.names(net))]
cycle_sims$Type <- (net %v% 'Owner_Type')[match(cycle_sims$PWS_ID,network.vertex.names(net))]
cycle_sims$GW <- (net %v% 'Water_Type')[match(cycle_sims$PWS_ID,network.vertex.names(net))]
cycle_sims$MakeBuy <- (net %v% 'Make_Buy')[match(cycle_sims$PWS_ID,network.vertex.names(net))]
cycle_sims$SelfGround <- ifelse(cycle_sims$GW =='GW' & cycle_sims$MakeBuy=='Make','GW_Self','Not')

ground_4cycle_dist <- cycle_sims %>% group_by(MakeBuy,GW,sim) %>% summarise(total_4cycles = sum(cycle4vertex)) %>%
  group_by(MakeBuy,GW) %>% summarise(mean = mean(total_4cycles),sd = sd(total_4cycles),max = max(total_4cycles),
                                     min = min(total_4cycles))

head(sim_stats)
summary(as.data.frame(sim_stats)['gwnsp.fixed.0.75'])
summary(net ~ gwnsp(0.75,fixed=T))
sna::kcycle.census(net, maxlen = 4, mode = "graph",  
                   tabulate.by.vertex = F, cycle.comembership = c( "sum"))

temp = sna::kcycle.census(net, maxlen = 4, mode = "graph",  
                          tabulate.by.vertex = TRUE, cycle.comembership = c( "sum"))
cycle_4_observed <- as.data.frame(temp$cycle.count[3,]) %>% mutate(PWS_ID = rownames(.)) %>% filter(PWS_ID != 'Agg') %>%
  filter(grepl("^TX",PWS_ID)) %>% rename(cycle_4 = `temp$cycle.count[3, ]`)

cycle_4_observed <- left_join(cycle_4_observed,system_df)
cycle_4_observed$Owner_Type <- (net %v% 'Owner_Type')[match(cycle_4_observed$PWS_ID,network.vertex.names(net))]
cycle_4_observed$MakeBuy <- ifelse(grepl('P',cycle_4_observed$Primary_Source_Type),'Buy','Make')
cycle_4_observed$GW <- ifelse(grepl('GW',cycle_4_observed$Primary_Source_Type),'GW','SW') 
cycle_4_observed <- cycle_4_observed %>% group_by(MakeBuy,GW) %>% summarise(sum_4_cycle = sum(cycle_4))

cycle_4_observed

#   
# sna::kcycle.census(net, maxlen = 6, mode = "graph",  
#           tabulate.by.vertex = F, cycle.comembership = c( "none"))
# 
# left_join(ground_4cycle_dist,cycle_4_observed) %>% mutate((sum_4_cycle - mean)/sd)


# 
# sna::kcycle.census(net, maxlen = 4, mode = "graph",  
#                    tabulate.by.vertex = F, cycle.comembership = c( "none"))
# 
# summary(net ~ gwnsp(0))
# 
# 
# sna::kpath.census(net,maxlen = 2,mode = 'graph',path.comembership = 'none')
# 
# warnings()
# stopCluster(cl)
# save.image('scratch_mle.RData')
# 
# 
# 
# 
# 
# 