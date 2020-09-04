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


library(ggmap)
temp <- paste(epa_systems$`Address Line1`[match(system_df$PWS_ID,epa_systems$PWS_ID)],
              epa_systems$`City Name`[match(system_df$PWS_ID,epa_systems$PWS_ID)],
              'TX',epa_systems$`Zip Code`[match(system_df$PWS_ID,epa_systems$PWS_ID)],sep=', ')
library(pbapply)
library(parallel)
# 
# locs <- invisible(mclapply(temp,function(x) ggmap::geocode(location = x,source = 'dsk'),mc.cores = 8,mc.cleanup=T,mc.preschedule = T))
# 
# geol <- do.call(rbind,locs) %>% mutate(PWS_ID = epa_systems$PWS_ID[match(system_df$PWS_ID,epa_systems$PWS_ID)])
# system_df <- left_join(system_df,geol)
# system_dist_matrix <- geosphere::distm(system_df[,c('lon','lat')])
# colnames(system_dist_matrix) = rownames(system_dist_matrix) = system_df$PWS_ID
# 
#  library(geosphere)



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
net = as.network(x = system_matrix,directed = F,bipartite = T,nonedges=F,
                 matrix.type = 'incidence',ignore.eval = F,names.eval = 'personnel_overlap')
net %v% 'Primary_Source_Type' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Water_Type' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Water_Type' <- ifelse(is.na(net %v% 'Water_Type'),NA,ifelse(grepl('GW',net %v% 'Water_Type'),'GW','SW'))

net %v% 'Make_Buy' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Make_Buy' <- ifelse(is.na(net %v% 'Make_Buy'),(net %v% 'Make_Buy'),ifelse(grepl('P',(net %v% 'Make_Buy')),'Buy','Make'))

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

# num_occur = all_reps  %>% group_by(NAME) %>% summarise(co = n()) %>% arrange(-co)
# major_nodes = all_reps %>% mutate(PWS_ID = as.factor(PWS_ID)) %>% 
#   filter(NAME %in% num_occur$NAME[1:4])
# major_net = as.network(tcrossprod(as.matrix(table(major_nodes$PWS_ID,major_nodes$NAME))),matrix.type = 'adjacency',directed = F,hyper = F,loops = F)

net %v% 'Person' <- c(rep(NA,nrow(as.sociomatrix(net))),rep('Person',ncol(as.sociomatrix(net))))
net %v% 'GCD_Type' <- paste(net %v% 'GCD', net %v% 'Water_Type')


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
# distance_to <- (as.data.frame(system_dist_matrix) %>% mutate(PWS_ID = rownames(.)) %>% gather(Other,Distance,-PWS_ID))
# net_soc <- as.sociomatrix(net)
# nearest_other_system <- as.sociomatrix(net)/-Inf
# edl <- as.data.frame(net_soc) %>% mutate(PWS_ID = rownames(.)) %>% gather(NAME,TIE,-PWS_ID)

# as.numeric(distance_to[distance_to$PWS_ID %in% rownames(net_soc)[i] & !distance_to$Other %in% rownames(net_soc)[i],] %>%
#              filter(Other %in% edl$PWS_ID[edl$NAME==colnames(net_soc)[j] & edl$TIE==1]) %>% summarise(nearest_other_employer <- min(Distance)) / 1000)),
# 
# 
# nearest_other_employer_list <- mclapply(1:nrow(net_soc),function(i) sapply(1:ncol(net_soc),function(j) as.numeric(distance_to[distance_to$PWS_ID %in% rownames(net_soc)[i] & !distance_to$Other %in% rownames(net_soc)[i],] %>%
#                                                                                                                     filter(Other %in% edl$PWS_ID[edl$NAME==colnames(net_soc)[j] & edl$TIE==1]) %>% summarise(nearest_other_employer <- min(Distance)) / 1000)),
#                                         mc.cores=16,mc.cleanup = T,mc.preschedule = T)
# 

# # nearest_other_employer_list
# # 
# mat <- matrix(0,ncol=30,nrow=10)
# for (i in 1:10)
# {
#   for (j in 1:30)
#   {
#     mat[i,j] <- as.numeric(distance_to[distance_to$PWS_ID %in% rownames(net_soc)[i] & !distance_to$Other %in% rownames(net_soc)[i],] %>%
#                              filter(Other %in% edl$PWS_ID[edl$NAME==colnames(net_soc)[j] & edl$TIE==1]) %>% summarise(nearest_other_employer <- min(Distance)) / 1000)
#   }
# }

# mat == do.call(rbind,test)


mod_mle = ergm(net ~ edges + gwb1degree(1) +
                 gwb2degree(1,fixed=T) +
                 gwnsp(1,fixed=T) + 
                 # b1nodematch('COUNTY',alpha =0.25,beta = 0.25) +
                 # b1factor('COUNTY') + 
                 b1factor('System_4_Category') + 
                 b1factor('Owner_Type')+
                 edgecov(works_for_buyer) + 
                 edgecov(works_for_seller) + 
                 edgecov(works_for_gw_same_gcd),
               control = control.ergm(main.method = 'Stepping',#MCMC.burnin = 15000,
                                      Step.MCMC.samplesize=10000,MCMC.burnin = 15000,
                                      MCMC.interval = 2000,
                                      MCMC.runtime.traceplot=F,#MCMLE.steplength="adaptive",
                                      parallel = 8,#MCMLE.check.degeneracy = F,
                                      parallel.type = 'PSOCK'),verbose=F)

save.image('scratch_mle3.RData')


