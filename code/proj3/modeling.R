library(tidyverse)
library(statnet)

system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
  rename(PWS_NAME = Name)

system_df$PWS_NAME <- gsub(' Fact.*','',system_df$PWS_NAME)


epa_systems = read_csv('input/epa_sdwis/water_system_detail.csv') %>% rename(PWS_ID = `PWS ID`) %>%
  filter(`Owner Type` != 'State government') %>% filter(!grepl('DETENTIO|ACADEMY',`PWS Name`))
system_df = system_df %>% filter(PWS_ID %in% epa_systems$PWS_ID)



operators = read_csv('input/texas_dww/licensed_operators_4_19_2017.csv') %>%
  select(-X1) %>% rename(STATUS = X1_1) %>% filter(!duplicated(.)) %>%
  filter(STATUS!='EXPIRED') %>% 
  rename(PWS_ID = SYSTEM) %>% filter(PWS_ID %in% system_df$PWS_ID)
ownership = read_csv('input/texas_dww/personnel_records.csv') %>%
  filter(Position =='OW- Owner') %>% 
  rename(PWS_ID = System,OWNER = NAME) %>% 
  filter(PWS_ID %in% system_df$PWS_ID) %>% select(-Position)
poc = read_csv('input/texas_dww/personnel_records.csv') %>% rename(PWS_ID = System) %>%
  filter(PWS_ID %in% system_df$PWS_ID) %>% filter(Position != 'OW- Owner')

all_reps = full_join(poc %>% select(PWS_ID,NAME),
                     operators %>% rename(NAME = LICENSE_HOLDER) %>% select(PWS_ID,NAME))

system_reps = full_join(system_df,all_reps)
sys_reps_mat = as.matrix(table(system_reps$NAME,system_reps$PWS_ID))
system_matrix = crossprod(sys_reps_mat)
net = as.network(x = system_matrix,directed = F,matrix.type = 'adjacency',ignore.eval = F,names.eval = 'personnel_overlap')


net %v% 'Primary_Source_Type' <- system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'Water_Type' <- ifelse(system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)] %in% c('GWP','GW'),"GW",'SW')
net %v% 'Make_Buy' <- ifelse(system_df$Primary_Source_Type[match(network.vertex.names(net),system_df$PWS_ID)] %in% c('GWP','SWP'),"Buy",'Make')
net %v% 'Wholesaler' <- epa_systems$`Is Wholesaler`[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'Pop_Served_1k' <- epa_systems$`Population Served Count`[match(network.vertex.names(net),epa_systems$PWS_ID)]/1000
net %v% 'Owner_Sector' <- epa_systems$`Owner Type`[match(network.vertex.names(net),epa_systems$PWS_ID)]
epa_systems$`Is Source Water Protected`[epa_systems$`Is Source Water Protected` == '-'] <- 'Unknown'
net %v% 'Protected_Source' <- epa_systems$`Owner Type`[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'System_Major_Category' <- ifelse(net %v% 'Pop_Served_1k' * 1000 < 3300,"Small",ifelse(net %v% 'Pop_Served_1k' * 1000 < 10000,'Medium','Large'))
epa_systems$Pop_Cat_4 <- ifelse(epa_systems$`Pop Cat 5` %in% c("10,001-100,000",">100,000"),">10,000", epa_systems$`Pop Cat 5`)

net %v% 'System_4_Category' <- epa_systems$Pop_Cat_4[match(network.vertex.names(net),epa_systems$PWS_ID)]

sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
                  read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type == 'P')
sales$Water_Condition = ifelse(sales$Water_Type %in% c('Partially Treated Water','Not Treated Water'),'Not/Partially Treated','Treated and/or Filtered')
sales = sales %>% select(-Water_Type) %>%
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

net %v% 'SYSTEM_NAME' <- epa_systems$`PWS Name`[match(network.vertex.names(net),epa_systems$PWS_ID)]

owner_type = NA
owner_type[(net %v% 'Owner_Sector') == 'Private'] <- 'Private'
owner_type[grepl('CITY OF|TOWN OF',net %v% 'SYSTEM_NAME')] <- 'City'
owner_type[grepl('WSC|SUPPLY CORPORATION',net %v% 'SYSTEM_NAME')] <- 'Public_Corp'
owner_type[grepl('MUD|SUD|PUD| UD|FWSD|WCID|MWD|REGIONAL|DISTRICT|AUTHORITY',net %v% 'SYSTEM_NAME')] <- 'SD'
owner_type[is.na(owner_type)] <- 'SD'

net %v% "Owner_Type" <- owner_type

rep_count_df = system_reps %>% group_by(PWS_ID) %>% summarise(rep_count = n())

net %v% 'Num_Personnel' <- rep_count_df$rep_count[match(network.vertex.names(net),rep_count_df$PWS_ID)]

library(lubridate)

viols_df = read_csv('input/epa_echo/echo_compliance_6-23-17.csv',na=c("-")) %>% rename(PWS_ID = Pwsid) %>%
  filter(PWS_ID %in% network.vertex.names(net))
net %v% 'Viol_Points_5yr' <- viols_df$Viopaccr[match(network.vertex.names(net),viols_df$PWS_ID)]


library(ergm.count)
library(snow)
library(Rmpi)
form = "net ~ edges +   gwdegree(gwd_decay_sim,fixed=T,cutoff=15) + 
gwesp(gwesp_decay_sim,fixed=T,cutoff=15) +  + nodefactor('Wholesaler') + 
nodefactor('System_4_Category') + nodecov('Viol_Points_5yr') + 
nodefactor('Owner_Type') + 
nodefactor('Make_Buy',base=2) + nodefactor('Water_Type',base=2)"

npar_min1 <- length(summary(as.formula(form)))-1


np <- 8
cluster <- makeMPIcluster(np)
clusterEvalQ(cluster, library(ergm.count))
clusterEvalQ(cluster, library(pbapply))
g <- sum(net %e% "personnel_overlap")/network.dyadcount(net)
geo.init = log(1 - 1/(g+1))
library(pbapply)
library(broom)
gwd_sim_reps = 1000
gwd_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)


mod_main = ergm(net ~ edges +   gwdegree(0.5,fixed=T,cutoff=15) + 
                  gwesp(0.5,fixed=T,cutoff=15) +  + nodefactor('Wholesaler') + 
                  nodefactor('System_4_Category') + nodecov('Viol_Points_5yr') + 
                  nodefactor('Owner_Type') + 
                  nodefactor('Make_Buy',base=2) + nodefactor('Water_Type',base=2),
                control = control.ergm(init = c(geo.init,rep(0,npar_min1)),
                                       main.method = "Stochastic-Approximation",
                                       parallel = cluster,parallel.type = 'MPI'),
                eval.loglik = F,verbose=T)

stopCluster(cluster)
mpi.exit()


cluster <- makeMPIcluster(np)
clusterEvalQ(cluster, library(ergm.count))
clusterEvalQ(cluster, library(pbapply))

x = 10
t0 <- proc.time()
test = try(tidy(ergm(as.formula(
  gsub('gwesp_decay_sim',as.character(gwesp_decay_sim[x]),
       gsub('gwd_decay_sim',as.character(gwd_decay_sim[x]),form))),
  control = control.ergm(init = c(geo.init,rep(0,npar_min1)),
                         main.method = "Stochastic-Approximation",
                         parallel = cluster,parallel.type = 'MPI'),
  eval.loglik = F,verbose=T)))
proc.time() - t0

t1 <- proc.time()
test = try(tidy(ergm(as.formula(
  gsub('gwesp_decay_sim',as.character(gwesp_decay_sim[x]),
       gsub('gwd_decay_sim',as.character(gwd_decay_sim[x]),form))),
  control = control.ergm(main.method = "Stochastic-Approximation"),
  #parallel = cluster,parallel.type = 'MPI'),
  eval.loglik = F,verbose=T)))
proc.time() - t1






mod_sensitivity = pblapply(1,function(x) try(tidy(ergm(as.formula(
  gsub('gwesp_decay_sim',as.character(gwesp_decay_sim[x]),
       gsub('gwd_decay_sim',as.character(gwd_decay_sim[x]),form))),
  verbose=F,control = control.ergm(init = c(geo.init,rep(0,npar_min1)),MPLE.type = 'penalized',
                                   #main.method = "Stochastic-Approximation",
                                   #main.method=c('Robbins-Monro'),
                                   MCMLE.trustregion = 100,
                                   parallel = cluster,parallel.type = 'MPI'),
  eval.loglik = F,estimate = 'MPLE'))))



save.image('personnel_ergms_alphsasim.RData')

stopCluster(cluster)

mpi.exit()

