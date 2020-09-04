
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