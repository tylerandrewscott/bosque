library(tidyverse)
library(statnet)

system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
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
  mutate(NAME = gsub(' JR\\,| JR| SR\\,| SR','',NAME)) %>% mutate(NAME = gsub('\\,','',NAME)) %>%
  filter(!duplicated(paste(PWS_ID,NAME)))


system_reps = full_join(system_df,all_reps)
sys_reps_mat = as.matrix(table(system_reps$NAME,system_reps$PWS_ID))
system_matrix = crossprod(sys_reps_mat)
diag(system_matrix) <- 0
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
epa_systems$Pop_Cat_3 <- ifelse(epa_systems$`Pop Cat 5` %in% c("10,001-100,000",">100,000"),">10,000", ifelse(epa_systems$`Pop Cat 5` %in% c("501-3,300","<=500"),'<3,300',epa_systems$`Pop Cat 5`))

net %v% 'System_4_Category' <- epa_systems$Pop_Cat_4[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'System_3_Category' <- epa_systems$Pop_Cat_3[match(network.vertex.names(net),epa_systems$PWS_ID)]
net %v% 'GCD' <- system_df$GCD_ZONE[match(network.vertex.names(net),system_df$PWS_ID)]
net %v% 'COUNTY' <- system_df$County[match(network.vertex.names(net),system_df$PWS_ID)]

sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
                  read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type == 'P')
sales$Water_Condition = ifelse(sales$Water_Type %in% c('Partially Treated Water','Not Treated Water'),'Not/Partially Treated','Treated and/or Filtered')
sales = sales %>% dplyr::select(-Water_Type) %>%
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

same_gcd <- as.matrix(table(system_df$PWS_ID,system_df$GCD_ZONE))
same_gcd[,'NONE'] <- 0
same_gcd_mat <- tcrossprod(same_gcd)

both_ground <- as.matrix(table(system_df$PWS_ID,ifelse(system_df$Primary_Source_Type=='GW',1,0)))
both_ground_mat <- tcrossprod(both_ground)

same_gcd_both_ground = same_gcd_mat * both_ground_mat


library(ergm.count)
library(snow)
library(Rmpi)

library(pbapply)
library(broom)
gwd_sim_reps = 1000
gwd_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)

form = "net ~ edges +   gwdegree(gwd_decay_sim,fixed=T,cutoff=15) + 
gwesp(gwesp_decay_sim,fixed=T,cutoff=15) +  + nodefactor('Wholesaler') + 
nodefactor('System_4_Category') + nodecov('Viol_Points_5yr') + 
nodefactor('Owner_Type') + 
nodefactor('Make_Buy',base=2) + nodefactor('Water_Type',base=2)"

npar_min1 <- length(summary(as.formula(form)))-1
library(parallel)

num_occur = all_reps  %>% group_by(NAME) %>% summarise(co = n()) %>% arrange(-co)
major_nodes = all_reps %>% mutate(PWS_ID = as.factor(PWS_ID)) %>% 
  filter(NAME %in% num_occur$NAME[1:4])
major_net = as.network(tcrossprod(as.matrix(table(major_nodes$PWS_ID,major_nodes$NAME))),matrix.type = 'adjacency',directed = F,hyper = F,loops = F)

np <- detectCores()/2
# cluster <- makeMPIcluster(np)
# clusterEvalQ(cluster, library(ergm.count))
# clusterEvalQ(cluster, library(pbapply))
#g <- sum(net %e% "personnel_overlap")/network.dyadcount(net)
#geo.init = log(1 - 1/(g+1))
#gwdegree(0.5,fixed=T,cutoff=15) + 
#  gwesp(0.5,fixed=T,cutoff=15) +

obs_values = summary(net ~ edges + #kstar(2:4) + 
          gwdegree(2,fixed=T,cutoff=20) + gwesp(0.5,fixed=T,cutoff=20) +
          nodefactor('System_4_Category') + nodecov('Viol_Points_5yr') + 
          nodecov('Num_Personnel') + 
          nodefactor('Owner_Type') + nodefactor('COUNTY') + 
          edgecov(seller_mat) + edgecov(buyer_mat) + edgecov(same_gcd_both_ground) + 
          edgecov(shared_seller) + 
          nodefactor('Water_Type',base=2))

sum(net %v% 'COUNTY' %in% c('HARRIS','MONTGOMERY','FORT BEND','BRAZORIA','GALVESTON'))


test = data.frame(co<-(net %v% 'COUNTY'),deg = degree(net,gmode = 'graph'))
tapply(test$deg,test$co,median)

mod_endog =  ergm(net ~ edges + isolates + gwdegree(4,fixed=T) + gwesp(4,fixed=T),eval.loglik = F,verbose=T,
                  estimate = 'MPLE')
test_gof = btergm::gof(mod_endog,nsim=50)

plot(test_gof)
isolates + gwdegree(4,fixed=T) + gwesp(4,fixed=T)

mod_mle = ergm(net ~ edges + twopath + gwdegree(4,fixed=T) + gwesp(4,fixed=T),eval.loglik = F,verbose=T,
               control = control.ergm(MCMC.samplesize = 5000,MCMC.interval = 500,
                                      MCMC.max.maxedges=as.numeric(summary(net~edges)) * 2,
                                      parallel = 10,parallel.type = 'PSOCK',MCMLE.maxit = 10,Step.maxit = 5))
               
               
                  gwdegree(2,fixed=T,cutoff=20) + gwesp(0.5,fixed=T,cutoff=20) +
                  nodefactor('System_4_Category') + nodecov('Viol_Points_5yr') + 
                  nodecov('Num_Personnel') + 
                  nodefactor('Owner_Type') + nodefactor('COUNTY') + 
                  edgecov(seller_mat) + edgecov(buyer_mat) + edgecov(same_gcd_both_ground) + 
                  edgecov(shared_seller) + 
                  nodefactor('Water_Type',base=2),eval.loglik = F,estimate = 'MLE',
                control = control.ergm(parallel = np,parallel.type = 'PSOCK',MCMLE.maxit = 10))
# screenreg(list(mod_bern,mod_exog,mod_main,mod_endog))

# constraints =~bd(maxout=ifelse(degree(net,gmode = 'graph')>100,degree(net,gmode = 'graph'),NA),
#                  minin=ifelse(degree(net,gmode = 'graph')>100,degree(net,gmode = 'graph'),NA)))

save.image('mle_result.RData')

