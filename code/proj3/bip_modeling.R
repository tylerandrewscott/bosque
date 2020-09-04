library(tidyverse)
library(statnet)

system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
  #filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','MONTGOMERY','FORT BEND')) %>%
  #        'WASHINGTON','SAN JACINTO','POLK','TYLER','HARDIN','JEFFERSON','ORANGE','JASPER','NEWTON','WALKER','GRIMES')) %>%
  #filter(County %in% c('ATASCOSA','BANDERA','BEXAR','COMAL','GUADALUPE','KENDALL','MEDINA','WILSON')) %>%
  #filter(County %in% c("BISHOP",'CALDWELL','HAYS','TRAVIS','WILLIAMSON')) %>%
  #filter(County %in% c("WISE",'COLLIN','DALLAS','ELLIS','HOOD','HUNT','JOHNSON','KAUFMAN','PARKER','ROCKWALL','SOMERVELL','TARRANT')) %>%
  rename(PWS_NAME = Name)

system_df$PWS_NAME <- gsub(' Fact.*','',system_df$PWS_NAME)
#epa_sdwis = read_csv('input/epa_sdwis/texas_cws_sdwis_summary_master.csv') %>% filter(!grepl('DETENTIO|ACADEMY',`PWS Name`))
epa_systems = read_csv('input/epa_sdwis/water_system_detail.csv') %>% rename(PWS_ID = `PWS ID`) %>%
  filter(`Owner Type` != 'State government') %>% filter(!grepl('DETENTIO|ACADEMY',`PWS Name`))
system_df = system_df %>% filter(PWS_ID %in% epa_systems$PWS_ID)
system_df$GCD_ZONE = NA
system_df$GCD_ZONE[system_df$County=='BRAZORIA'] <- 'Brazoria County GCD'
system_df$GCD_ZONE[system_df$County %in% c('HARRIS','GALVESTON')] <- 'Harris-Galveston Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('FORT BEND')] <- 'Fort Bend Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('AUSTIN','WALLER')] <- 'Bluebonnet GCD'
system_df$GCD_ZONE[system_df$County %in% c('MONTGOMERY')] <- 'Lone Star GCD'
system_df$GCD_ZONE[is.na(system_df$GCD_ZONE)]<-'NONE'


#sum(epa_systems$`Population Served Count`[match(system_df$PWS_ID,epa_systems$PWS_ID)])/(6.5*1000000)

operators = read_csv('input/texas_dww/licensed_operators_4_19_2017.csv') %>%
  dplyr::select(-X1) %>% rename(STATUS = X1_1) %>% filter(!duplicated(.)) %>%
  filter(STATUS!='EXPIRED') %>% 
  rename(PWS_ID = SYSTEM)  %>% filter(PWS_ID %in% system_df$PWS_ID)

ownership = read_csv('input/texas_dww/personnel_records.csv') %>%
  filter(Position =='OW- Owner') %>% 
  rename(PWS_ID = System,OWNER = NAME) %>% 
  filter(PWS_ID %in% system_df$PWS_ID) %>% dplyr::select(-Position)

poc = read_csv('input/texas_dww/personnel_records.csv') %>% rename(PWS_ID = System) %>%
  filter(PWS_ID %in% system_df$PWS_ID) %>% filter(Position != 'OW- Owner')

all_reps = full_join(poc %>% dplyr::select(PWS_ID,NAME),
                     operators %>% rename(NAME = LICENSE_HOLDER) %>% dplyr::select(PWS_ID,NAME))  %>%
  filter(!grepl(' INC| LLC| LP|OPERATIONS|CORPORATION|UTILIT',NAME)) %>%
  mutate(NAME = gsub(' [0-9].*| PO BOX.*','',NAME)) %>% mutate(NAME = gsub(' {1,}$','',NAME)) %>%
  mutate(NAME = gsub(' JR\\,| JR| SR\\,| SR| II$| III$| IV$','',NAME)) %>% mutate(NAME = gsub('\\,','',NAME))

all_reps$NAME <- ifelse(grepl(' [A-Z]$',all_reps$NAME) &
                          gsub(' [A-Z]$','',all_reps$NAME) %in%
                          all_reps$NAME,gsub(' [A-Z]$','',all_reps$NAME),all_reps$NAME)
all_reps = all_reps %>%
  filter(!duplicated(paste(PWS_ID,NAME)))

for (i in 1:nrow(all_reps))
{if(nrow(temp <- all_reps[agrepl(all_reps$NAME[i],x = all_reps$NAME,
                                 max.distance = list(cost = 0.01,deletions = 1,insertions = 0,substitutions = 0)),])>1 &
    length(unique(temp$PWS_ID)) == 1)
{if(all(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)) ==
        max(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)))))
{all_reps$NAME[i] <- temp$NAME[1]}}
  {if(!all(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)) ==
           max(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)))))
  {all_reps$NAME[i] <- names(which(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)) == max(sapply(temp$NAME,function(x) sum(x == all_reps$NAME)))))[1]}}}

all_reps = all_reps %>%
  filter(!duplicated(paste(PWS_ID,NAME)))

system_reps = full_join(system_df,all_reps)
#system_reps <- system_reps[system_reps$PWS_ID %in% epa_systems$PWS_ID[epa_systems$`Owner Type` !='Private'],]

sys_reps_mat = as.matrix(table(system_reps$NAME,system_reps$PWS_ID))
#system_matrix = crossprod(sys_reps_mat)
system_matrix = t(sys_reps_mat)
#diag(system_matrix) <- 0
net = as.network(x = system_matrix,directed = F,bipartite = T,
                 matrix.type = 'incidence',ignore.eval = F,names.eval = 'personnel_overlap')
net %v% 'Primary_Source_Type' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Water_Type' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Water_Type' <- ifelse(is.na(net %v% 'Water_Type'),NA,ifelse(grepl('GW',net %v% 'Water_Type'),'GW','SW'))

net %v% 'Make_Buy' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Make_Buy' <- ifelse(is.na(net %v% 'Make_Buy'),NA,ifelse(grepl('GWP|SWP',net %v% 'Water_Type'),'Buy','Make'))

net %v% 'Wholesaler' <- epa_systems$`Is Wholesaler`[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'Pop_Served_1k' <- epa_systems$`Population Served Count`[match(network.vertex.names(net),epa_systems$PWS_ID)]/1000
net %v% 'Owner_Sector' <- epa_systems$`Owner Type`[match(network.vertex.names(net),epa_systems$PWS_ID)]
epa_systems$`Is Source Water Protected`[epa_systems$`Is Source Water Protected` == '-'] <- 'Unknown'
net %v% 'Protected_Source' <- epa_systems$`Owner Type`[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'System_Major_Category' <- ifelse(net %v% 'Pop_Served_1k' * 1000 < 3300,"Small",ifelse(net %v% 'Pop_Served_1k' * 1000 < 10000,'Medium','Large'))
epa_systems$Pop_Cat_4 <- ifelse(epa_systems$`Pop Cat 5` %in% c("10,001-100,000",">100,000"),">10,000", epa_systems$`Pop Cat 5`)
epa_systems$Pop_Cat_3 <- ifelse(epa_systems$`Pop Cat 5` %in% c("10,001-100,000",">100,000"),">10,000", ifelse(epa_systems$`Pop Cat 5` %in% c("501-3,300","<=500"),'<3,300',epa_systems$`Pop Cat 5`))

net %v% 'System_4_Category' <- epa_systems$Pop_Cat_4[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'System_3_Category' <- epa_systems$Pop_Cat_3[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'GCD' <- system_df$GCD_ZONE[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'COUNTY' <- system_df$County[match(network.vertex.names(net),system_df$PWS_ID)]

sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
                  read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type %in% c('P','S'))
sales$Water_Condition = ifelse(sales$Water_Type %in% c('Partially Treated Water','Not Treated Water'),'Not/Partially Treated','Treated and/or Filtered')
sales = sales %>% dplyr::select(-Water_Type) %>% filter(!duplicated(paste(Buyer,Seller))) %>%
  filter(Buyer %in% network.vertex.names(net) & Seller %in% network.vertex.names(net))

seller_mat = as.sociomatrix(net)
seller_mat[TRUE] <- 0
purchaser_mat_volume <- purchase_mat_type <- seller_mat

for (i in 1:nrow(sales))
{seller_mat[which(rownames(seller_mat)==sales$Buyer[i]),which(colnames(seller_mat)==sales$Seller[i])] <- 1
purchaser_mat_volume[which(rownames(seller_mat)==sales$Buyer[i]),which(colnames(seller_mat)==sales$Seller[i])] <- 
  sales$Buyer_Service_Pop[i]/1000
purchase_mat_type[which(rownames(seller_mat)==sales$Buyer[i]),which(colnames(seller_mat)==sales$Seller[i])] <- sales$Water_Condition[i]
}
buyer_mat = t(seller_mat)
buyer_mat_volume = t(purchaser_mat_volume)

library(forcats)

sales$Buyer <- fct_expand(as.factor(sales$Buyer),network.vertex.names(net))
shared_seller <- tcrossprod(as.matrix(table(sales$Buyer,sales$Seller)))
shared_seller <- tcrossprod(as.matrix(table(sales$Buyer,sales$Seller)))

net %v% 'SYSTEM_NAME' <- epa_systems$`PWS Name`[match(network.vertex.names(net),epa_systems$PWS_ID)]

owner_type = NA
owner_type[(net %v% 'Owner_Sector') == 'Private'] <- 'Private'
owner_type[grepl('CITY OF|TOWN OF',net %v% 'SYSTEM_NAME')] <- 'City'
owner_type[grepl('WSC|SUPPLY CORPORATION',net %v% 'SYSTEM_NAME')] <- 'Public_Corp'
owner_type[grepl('MUD|SUD|PUD| UD|FWSD|WCID|MWD|REGIONAL|DISTRICT|AUTHORITY',net %v% 'SYSTEM_NAME')] <- 'SD'
owner_type[is.na(owner_type) & network.vertex.names(net) %in% system_df$PWS_ID] <- 'SD'
owner_type[is.na(owner_type)] <- 'Person'

net %v% "Owner_Type" <- owner_type

rep_count_df = system_reps %>% group_by(PWS_ID) %>% summarise(rep_count = n())

net %v% 'Num_Personnel' <- rep_count_df$rep_count[match(network.vertex.names(net),rep_count_df$PWS_ID)]

library(lubridate)

viols_df = read_csv('input/epa_echo/echo_compliance_6-23-17.csv',na=c("-")) %>% rename(PWS_ID = Pwsid) %>%
  filter(PWS_ID %in% network.vertex.names(net))
net %v% 'Viol_Points_5yr' <- viols_df$Viopaccr[match(network.vertex.names(net),viols_df$PWS_ID)]

net %v% 'GCD_Type' <- paste(net %v% 'GCD', net %v% 'Water_Type',sep = '_')

#three edgecov terms: 
#(1) person works for someone you buy from; 
#(2) person works for someone you sell to;
#(3) person who works for someone who buys from the same wholesaler
works_for_seller <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_seller[i, which(colnames(works_for_seller) %in% all_reps$NAME[all_reps$PWS_ID %in% sales$Seller[i]])] <<- 1))

works_for_buyer <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_buyer[i, which(colnames(works_for_buyer) %in% all_reps$NAME[all_reps$PWS_ID %in% sales$Buyer[i]])] <<- 1))

works_for_co_customer <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_co_customer[i, which(colnames(works_for_co_customer) %in% all_reps$NAME[all_reps$PWS_ID %in% sales$Buyer[sales$Seller==sales$Seller[i]][-i]])] <<- 1))

library(magrittr)
library(tidyverse)

works_for_gw_same_gcd <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(system_df),function(i) works_for_gw_same_gcd[i,which(colnames(works_for_gw_same_gcd) %in% all_reps$NAME[all_reps$PWS_ID %in% system_df$PWS_ID[system_df$GCD_ZONE == system_df$GCD_ZONE[i] &   system_df$Primary_Source_Type=='GW' & system_df$PWS_ID != system_df$PWS_ID[i]]])] <<- 1))


# same_gcd <- as.matrix(table(system_df$PWS_ID,system_df$GCD_ZONE))
# if(any(system_df$County=='LIBERTY'))
# {same_gcd[,'NONE'] <- 0}
# same_gcd_mat <- tcrossprod(same_gcd)
# both_ground <- as.matrix(table(system_df$PWS_ID,ifelse(system_df$Primary_Source_Type=='GW',1,0)))
# both_ground_mat <- tcrossprod(both_ground)
# same_gcd_both_ground = same_gcd_mat * both_ground_mat

library(ergm.count)
library(pbapply)
library(broom)

library(parallel)

num_occur = all_reps  %>% group_by(NAME) %>% summarise(co = n()) %>% arrange(-co)
major_nodes = all_reps %>% mutate(PWS_ID = as.factor(PWS_ID)) %>% 
  filter(NAME %in% num_occur$NAME[1:4])
major_net = as.network(tcrossprod(as.matrix(table(major_nodes$PWS_ID,major_nodes$NAME))),matrix.type = 'adjacency',directed = F,hyper = F,loops = F)
net %v% 'Person' <- c(rep(NA,nrow(as.sociomatrix(net))),rep('Person',ncol(as.sociomatrix(net))))
net %v% 'GCD_Type' <- paste(net %v% 'GCD', net %v% 'Water_Type')

np <- detectCores()/2

gwd_sim_reps = 1000
gwd_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)

net %v% 'System' <- c(rep('System',nrow(as.sociomatrix(net))),rep(NA,ncol(as.sociomatrix(net))))


net %v% 'Owner_Type' <- ifelse(is.na(net %v% 'Owner_Type'),'Person',net %v% 'Owner_Type')


temp_owner_type = net %v% 'Owner_Type'
net %v% 'Works_For_City' = sapply(network.vertex.names(net),function(i){
  if(i %in% system_df$PWS_ID){NA} 
  else if(any((net %v% 'Owner_Type')[match(all_reps$PWS_ID[all_reps$NAME==i],network.vertex.names(net))] == 'City')){'Works_For_City'}
  else {'Not_For_City'}})

temp_system_size = net %v% 'System_4_Category'
net %v% 'Works_For_Large_System' = sapply(network.vertex.names(net),function(i){
  if(i %in% system_df$PWS_ID){NA} 
  else if(any((net %v% 'System_4_Category')[match(all_reps$PWS_ID[all_reps$NAME==i],
                                                  network.vertex.names(net))]  %in% c(">10,000","3,301-10,000"))){'Works_For_Large_System'}
  else {'Not_For_Large_System'}})
# 
# mod_bin = ergm(net ~ edges,
#                eval.loglik = T,#estimate = 'MPLE',
#                control = control.ergm(MCMLE.maxit = 50,MCMC.interval = 1500,MCMC.burnin = 15000,MCMC.samplesize = 50000,
#                                       parallel = 8,MCMLE.check.degeneracy = TRUE,
#                                       parallel.type = 'PSOCK'),verbose=T)
# save.image('scratch_bip5.RData')
# # 
# mod_exog = ergm(net ~ edges +
#                   b1nodematch('COUNTY') +
#                   b1factor('Water_Type',base=2) +
#                   b1factor('System_4_Category') +
#                   b1factor('Owner_Type') +
#                   edgecov(works_for_buyer) +
#                   edgecov(works_for_seller) +
#                   edgecov(works_for_co_customer) +
#                   edgecov(works_for_gw_same_gcd),
#                 eval.loglik = F,#estimate = 'MPLE',
#                 control = control.ergm(MCMLE.maxit = 100,MCMC.interval = 1500,MCMC.burnin = 10000,MCMC.samplesize = 50000,
#                                        parallel = 8,MCMLE.check.degeneracy = TRUE,
#                                        parallel.type = 'PSOCK'),verbose=T)
# save.image('scratch_bip5.RData')


bmod = bergm(net ~ edges + gwb1degree(0,fixed=T) + gwb2degree(1,fixed=T) +
               b1nodematch('COUNTY') +
               b1factor('System_4_Category') + 
               b1factor('Owner_Type')+
               edgecov(works_for_buyer) + 
               edgecov(works_for_seller) + 
               edgecov(works_for_gw_same_gcd),burn.in=500,main.iters=2000,aux.iters=20000,nchains = 8
             gamma=0.6,verbose=T)


# 
# mod_main = ergm(net ~ edges + 
#                   gwb1degree(3,fixed=T) + 
#                  # b1mindegree(5) +
#                   gwb2degree(1,fixed=T) +
#                   gwnsp(.25,fixed=T) +
#                   b1nodematch('COUNTY') +
#                   b1factor('System_4_Category') + 
#                   b1factor('Owner_Type') + 
#                   edgecov(works_for_buyer) + 
#                   edgecov(works_for_seller) + 
# #                  edgecov(works_for_co_customer) +
#                   edgecov(works_for_gw_same_gcd),
#                 eval.loglik = T,#estimate = 'MPLE',
#                 control = control.ergm(#MCMLE.maxit = 100,#MCMC.samplesize = 10000,
#                                        main.method = 'Stepping',
#                                        MCMC.runtime.traceplot=F,
#                                        parallel = 16,#MCMLE.check.degeneracy = F,
#                                        parallel.type = 'PSOCK'),verbose=F)

save.image('scratch_bip_bayes.RData')

# mod_endog = ergm(net ~ edges + 
#                    gwb1degree(1,fixed=T) + gwb2degree(1,fixed=T) +
#                    gwnsp(1,fixed=T),
#                  eval.loglik = T,#estimate = 'MPLE',
#                  control = control.ergm(MCMLE.maxit = 100,MCMC.interval = 1500,MCMC.burnin = 10000,MCMC.samplesize = 80000,
#                                         parallel = 8,
#                                         parallel.type = 'PSOCK'),verbose=T)
# save.image('scratch_bip5.RData')
# library(texreg)
# library(btergm)
# 
# 
# 
# sim_bip = simulate(mod_main,nsim=1000,sequential=T,monitor=~b2star(2) + gwnsp(0,fixed=T),
#                    control = control.simulate.ergm(MCMC.burnin=10000,statsonly = F,
#                                                    MCMC.interval=1500,verbose =T,
#                                                    parallel=10,set.seed = 24,
#                                                    parallel.type="PSOCK"))
# 
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

