library(tidyverse)
library(statnet)
library(parallel)
library(pbapply)
library(btergm)
library(ergm.terms.contrib)
library(ergm.userterms)

# system_df = read_csv('input/texas_dww/texas_master_pws.csv') %>%
#   filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER')) %>%
#   #filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','MONTGOMERY','FORT BEND')) %>%
#   #        'WASHINGTON','SAN JACINTO','POLK','TYLER','HARDIN','JEFFERSON','ORANGE','JASPER','NEWTON','WALKER','GRIMES')) %>%
#   #filter(County %in% c('ATASCOSA','BANDERA','BEXAR','COMAL','GUADALUPE','KENDALL','MEDINA','WILSON')) %>%
#   #filter(County %in% c("BISHOP",'CALDWELL','HAYS','TRAVIS','WILLIAMSON')) %>%
#   #filter(County %in% c("WISE",'COLLIN','DALLAS','ELLIS','HOOD','HUNT','JOHNSON','KAUFMAN','PARKER','ROCKWALL','SOMERVELL','TARRANT')) %>%
#   rename(PWS_NAME = Name)

#### Load system summary from EPA
system_df =  read_csv('input/epa_sdwis/texas_cws_sdwis_summary_master.csv') %>% filter(!grepl('DETENTIO|ACADEMY',`PWS Name`)) %>% rename(PWS_ID = `PWS ID`,PWS_Type = `PWS Type`,
                                                                                                                                         PWS_NAME = `PWS Name`,PWS_ID = `PWS ID`) %>%
  filter(`Owner Type` != 'State government') %>% 
  mutate(County = toupper(`Counties Served`)) %>% 
  #dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) %>%
  filter(`Population Served Count`!=0) %>%
  filter(!grepl('SHERIFF|ACADEMY|MONTESSORI',PWS_NAME)) %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER'))

#system_df  = system_df %>% filter(!grepl('CITY|TOWN ',system_df$PWS_NAME))
system_df$PWS_NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('UTILITY DISTRICT',"UD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('MUNICIPAL MANAGEMENT DIST ',"MMD ",system_df$PWS_NAME)
system_df$PWS_NAME[grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME)] = 
  str_extract(grep('^FORT BEND COUNTY MUD',system_df$PWS_NAME,value=T),'FORT BEND COUNTY MUD [0-9]{1,}[A-Z]{0,2}')
system_df$PWS_NAME = gsub('HCO',"HARRIS COUNTY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' NO '," ",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 24 COUNTRY COLONY","MONTGOMERY COUNTY MUD 24",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 6 CARRIAGE LANE","HARRIS COUNTY MUD 6",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 119 SPRING TRAILS","MONTGOMERY COUNTY MUD 119",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 400   WEST","HARRIS COUNTY MUD 400",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 55 HERITAGE PARK","HARRIS COUNTY MUD 55",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 148 KINGSLAKE","HARRIS COUNTY MUD 148",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 374 CYPRESS CREEK LAKE","HARRIS COUNTY MUD 374",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("WEST HARRIS COUNTY MUD 2 CHASE","WEST HARRIS COUNTY MUD 2",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('WATER SUPPLY CORPORATION','WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('REGIONAL WATER AUTHOR$|REGIONAL WATER AUTHORITY','RWA',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('HARRIS COUNTY MUD 400 - WEST','HARRIS COUNTY MUD 400',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 113 ENCHANTED VILLAGE","HARRIS COUNTY WCID 113",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THE WOODLANDS",'WOODLANDS',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('BRAZORIA COUNTY FWSD 1 DAMON','BRAZORIA COUNTY FWSD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD1",'MUD 1',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MUD3",'MUD 3',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("THUNDERBIRD UD 1","THUNDERBIRD UD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CYPRESS KLEIN UD WIMBLETON","CYPRESS KLEIN MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" TOWNE LAKE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GULF COAST WATER AUTHORITY TX CITY","GULF COAST WATER AUTHORITY",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("LIBERTY COUNTY FWSD 1 HULL","HULL FWSD",system_df$PWS_NAME)

system_df$PWS_NAME = gsub("BROOK HOLLOW WEST S",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(" FAIRFIELD VILLAGE",'',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('TOWN OF','CITY OF',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND" ,"GALVESTON COUNTY FWSD 6" ,system_df$PWS_NAME)

system_df$PWS_NAME = gsub("CINCO SOUTHWEST MUD 3 DAYCARE","CINCO SOUTHWEST MUD 3",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY WCID 50 EL LAGO","HARRIS COUNTY WCID 50",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('DOBBIN PLANTERSVILLE WSC 1','DOBBIN PLANTERSVILLE WSC',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY MUD 200 CRANBROOK","HARRIS COUNTY MUD 200",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('CENTRAL HARRIS COUNTY REGIONAL WATER AUT$','CENTRAL HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub('NORTH HARRIS COUNTY REGIONAL WATER AUTHO$','NORTH HARRIS COUNTY REGIONAL WATER AUTHORITY',system_df$PWS_NAME)
system_df$PWS_NAME = gsub("MONTGOMERY COUNTY MUD 16 WHITE OAK PLANT","MONTGOMERY COUNTY MUD 16",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("CITY OF WOOD BRANCH VILLAGE","CITY OF WOODBRANCH",system_df$PWS_NAME)
system_df$PWS_NAME = gsub("HARRIS COUNTY FWSD 52$","CHAMPIONS MUD",system_df$PWS_NAME)
system_df$PWS_NAME = gsub('-',' ',system_df$PWS_NAME)
system_df$PWS_NAME = gsub(' $','',system_df$PWS_NAME)

system_df$GCD_ZONE = NA
system_df$GCD_ZONE[system_df$County=='BRAZORIA'] <- 'Brazoria County GCD'
system_df$GCD_ZONE[system_df$County %in% c('HARRIS','GALVESTON')] <- 'Harris-Galveston Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('FORT BEND')] <- 'Fort Bend Subsidence District'
system_df$GCD_ZONE[system_df$County %in% c('AUSTIN','WALLER')] <- 'Bluebonnet GCD'
system_df$GCD_ZONE[system_df$County %in% c('MONTGOMERY')] <- 'Lone Star GCD'
system_df$GCD_ZONE[is.na(system_df$GCD_ZONE)]<-'NONE'

dinfo = read_csv('input/tceq_audits/district_info.csv')  %>% filter(`Activity Status:`=='ACTIVE') %>% 
  filter(`Primary County:` %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER'))
dinfo$NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",dinfo$NAME)
dinfo$NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",dinfo$NAME)
dinfo$NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",dinfo$NAME)
dinfo$NAME = gsub('UTILITY DISTRICT',"UD",dinfo$NAME)
dinfo$NAME = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",dinfo$NAME)


system_df = system_df %>% filter(PWS_NAME %in% dinfo$NAME)
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
ownership = read_csv('input/texas_dww/personnel_records_4_19_2017.csv') %>%
  filter(grepl('OW- Owner|OW - ',Position)) %>%
  rename(PWS_ID = System,OWNER = NAME) %>% 
  filter(PWS_ID %in% system_df$PWS_ID) %>% dplyr::select(-Position)
poc = read_csv('input/texas_dww/personnel_records_4_19_2017.csv') %>% rename(PWS_ID = System) %>%
  filter(PWS_ID %in% system_df$PWS_ID) %>% filter(Position != 'OW- Owner')


all_reps = full_join(poc %>% dplyr::select(PWS_ID,NAME),
                     operators %>% rename(NAME = LICENSE_HOLDER) %>% dplyr::select(PWS_ID,NAME))  %>%
  filter(!grepl(' INC| LLC| LP|OPERATIONS|CORPORATION|UTILIT|SYSTEM',NAME)) %>%
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

system_df <- system_df %>% left_join(.,ownership) %>% left_join(.,epa_systems)
system_reps <- system_reps %>% left_join(.,ownership)
all_reps$OWNER <- ownership$OWNER[match(all_reps$PWS_ID,ownership$PWS_ID)]

#system_reps <- system_reps[system_reps$PWS_ID %in% epa_systems$PWS_ID[epa_systems$`Owner Type` !='Private'],]

system_matrix = (as.matrix(table(system_reps$OWNER,system_reps$NAME)))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
func$Status

func = read_csv('input/tceq_audits/district_functions.csv') %>% filter(Status == 'ACTIVE') %>% select(-Status,-SYSTEM_NAME)
funcs = data.frame(SYSTEM_ID = func$SYSTEM_ID,(!is.na(func[,!colnames(func)%in%'SYSTEM_ID'])) + 0)

net = as.network(x = system_matrix,directed = F,bipartite = T,nonedges=F,
                 matrix.type = 'incidence',ignore.eval = F,names.eval = 'personnel_overlap')

get.inducedSubgraph(net$)



system_df$Primary_Source_Type = system_df$`Primary Source Code`
net %v% 'Primary_Source_Type' <- sapply(network.vertex.names(net),function(x) getmode(system_df$Primary_Source_Type[system_df$OWNER %in% x]))
net %v% 'Water_Type' <- sapply(network.vertex.names(net),function(x) getmode(system_df$Primary_Source_Type[system_df$OWNER %in% x]))
net %v% 'Water_Type' <- ifelse(is.na(net %v% 'Water_Type'),NA,ifelse(grepl('GW',net %v% 'Water_Type'),'GW','SW'))

net %v% 'Make_Buy' <- sapply(network.vertex.names(net),function(x) getmode(system_df$Primary_Source_Type[system_df$OWNER %in% x]))
net %v% 'Make_Buy' <- ifelse(is.na(net %v% 'Make_Buy'),(net %v% 'Make_Buy'),ifelse(grepl('P',(net %v% 'Make_Buy')),'Buy','Make'))
net %v% 'Wholesaler' <- sapply(network.vertex.names(net),function(x) any(system_df$`Is Wholesaler`[system_df$OWNER %in% x] =='Y') + 0)


net %v% 'Pop_Served' <- sapply(network.vertex.names(net),function(x) sum(system_df$`Population Served Count`[system_df$OWNER %in% x]))
net %v% 'Owner_Sector' <- sapply(network.vertex.names(net),function(x) getmode(system_df$`Owner Type`[system_df$OWNER %in% x]))
net %v% 'GCD' <- sapply(network.vertex.names(net),function(x) getmode(system_df$GCD_ZONE[system_df$OWNER %in% x]))
net %v% 'COUNTY' <- sapply(network.vertex.names(net),function(x) getmode(system_df$County[system_df$OWNER %in% x]))

cws_count = system_df %>% group_by(OWNER) %>% summarise(cws_count = n())
net %v% 'CWS_COUNT' <- cws_count$cws_count[match(network.vertex.names(net),cws_count$OWNER)]


sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
                  read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type %in% c('P','S'))
sales$Water_Condition = ifelse(sales$Water_Type %in% c('Partially Treated Water','Not Treated Water'),'Not/Partially Treated','Treated and/or Filtered')

sales$Buyer_Owner <- system_df$OWNER[match(sales$Buyer,system_df$PWS_ID)]
sales$Seller_Owner <- system_df$OWNER[match(sales$Seller,system_df$PWS_ID)]

sales = sales %>% dplyr::select(-Water_Type) %>% filter(!duplicated(paste(Buyer,Seller))) %>%
  filter(Buyer_Owner %in% network.vertex.names(net) & Seller_Owner %in% network.vertex.names(net))


seller_mat = as.sociomatrix(net)
seller_mat[TRUE] <- 0
purchaser_mat_volume <- purchase_mat_type <- seller_mat

for (i in 1:nrow(sales))
{seller_mat[which(rownames(seller_mat)==sales$Buyer_Owner[i]),which(colnames(seller_mat)==sales$Seller_Owner[i])] <- 1
purchaser_mat_volume[which(rownames(seller_mat)==sales$Buyer_Owner[i]),which(colnames(seller_mat)==sales$Seller_Owner[i])] <- 
  sales$Buyer_Service_Pop[i]/1000
purchase_mat_type[which(rownames(seller_mat)==sales$Buyer_Owner[i]),which(colnames(seller_mat)==sales$Seller[i])] <- sales$Water_Condition[i]
}
buyer_mat = t(seller_mat)
buyer_mat_volume = t(purchaser_mat_volume)

library(forcats)

sales$Buyer_Owner <- fct_expand(as.factor(sales$Buyer_Owner),network.vertex.names(net))
shared_seller <- tcrossprod(as.matrix(table(sales$Buyer,sales$Seller)))
shared_seller <- tcrossprod(as.matrix(table(sales$Buyer,sales$Seller)))

system_df$`Owner Type`[grepl('CITY OF|TOWN OF|VILLAGE OF',system_df$`PWS Name`)] <- 'City'
system_df$`Owner Type`[grepl('WSC|SUPPLY CORPORATION|HUNTERS COVE SEC 1|HOOP N HOLLER LAKE ESTATES|OLD SNAKE RIVER ESTATES EAST|BIG THICKET LAKE ESTATES 1|PATTON LAKE CLUB',system_df$`PWS Name`)] <- 'Public_Corp'
system_df$`Owner Type`[grepl('TBCD|SJRA|MUD|SUD|PUD| UD|FWSD|WCID|MWD|REGIONAL|DISTRICT|AUTHORITY|5TH STREET',system_df$`PWS Name`)] <- 'SD'
system_df$`Owner Type`[grepl('H-M-W|SIENNA PLANTATION',system_df$OWNER)] <-'SD'
system_df$`Owner Type`[grepl('SUN RANCH WATER SYSTEM|BAUER RANCH SUBDIVISION',system_df$PWS_NAME)] <-'Private'

net %v% 'SYSTEM_TYPE' <- sapply(network.vertex.names(net),function(x) getmode(system_df$`Owner Type`[system_df$OWNER %in% x]))

sales <- sales[!duplicated(paste(sales$Buyer_Owner,sales$Seller_Owner)),]
sales_df <- as.data.frame(table(sales$Buyer_Owner,sales$Seller_Owner))
sales_df <- right_join(sales_df,tidyr::expand(sales_df,Var1 = network.vertex.names(net)[1:net$gal$bipartite],
                                  Var2 = network.vertex.names(net)[1:net$gal$bipartite])) %>% mutate(Freq = ifelse(is.na(Freq),0,Freq))
sales_matrix <- spread(sales_df,key = Var2,value=Freq)
rownames(sales_matrix) <- sales_matrix$Var1
sales_net <- as.network(sales_matrix[,-1],matrix.type = 'adjacency')
sales_graph <- intergraph::asIgraph(sales_net)
exchange_cluster <- data.frame(cbind(network.vertex.names(net)[1:net$gal$bipartite],igraph::clusters(sales_graph)$membership)) %>%
  rename(VERTEX = X1,CLUSTER = X2)

net %v% 'Exchange_Cluster' <- as.character(exchange_cluster$CLUSTER[match(network.vertex.names(net),exchange_cluster$VERTEX)])


#rep_count_df = system_reps %>% group_by(PWS_ID) %>% summarise(rep_count = n())
#net %v% 'Num_Personnel' <- rep_count_df$rep_count[match(network.vertex.names(net),rep_count_df$PWS_ID)]

library(lubridate)

viols_df = read_csv('input/epa_echo/echo_compliance_6-23-17.csv',na=c("-")) %>% rename(PWS_ID = Pwsid)
viols_df$OWNER <- system_df$OWNER[match(viols_df$PWS_ID,system_df$PWS_ID)]
viols_df <- viols_df %>% group_by(OWNER) %>% summarise(tot_viol_points = sum(Viopaccr))
net %v% 'Viol_Points_5yr' <- viols_df$tot_viol_points[match(network.vertex.names(net),viols_df$OWNER)]

net %v% 'GCD_Type' <- paste(net %v% 'GCD', net %v% 'Water_Type',sep = '_')

#three edgecov terms: 
#(1) person works for someone you buy from; 
#(2) person works for someone you sell to;
#(3) person who works for someone who buys from the same wholesaler
works_for_seller <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_seller[i, which(colnames(works_for_seller) %in% all_reps$NAME[all_reps$OWNER %in% sales$Seller_Owner[i]])] <<- 1))

works_for_buyer <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_buyer[i, which(colnames(works_for_buyer) %in% all_reps$NAME[all_reps$OWNER %in% sales$Buyer_Owner[i]])] <<- 1))

works_for_co_customer <- as.sociomatrix(net)/-Inf
invisible(lapply(1:nrow(sales),function(i)
  works_for_co_customer[i, which(colnames(works_for_co_customer) %in% all_reps$NAME[all_reps$OWNER %in% sales$Buyer_Owner[sales$Seller_Owner==sales$Seller_Owner[i]][-i]])] <<- 1))

works_for_common_gcdmember <- as.sociomatrix(net)/-Inf
invisible(lapply(1:net$gal$bipartite,function(i) {
  if(!grepl('District_GW|GCD_GW',(net %v% 'GCD_TYPE')[i])){works_for_common_gcdmember[i,]<<-0}
  else {works_for_common_gcdmember[i, colnames(works_for_common_gcdmember)  %in% all_reps$NAME[all_reps$OWNER != network.vertex.names(net)[i] & all_reps$OWNER %in% 
                                                                                                network.vertex.names(net)[which((net %v% 'GCD_TYPE') == (net %v% 'GCD_TYPE')[i])]]] <<- 1}}))

library(magrittr)
library(tidyverse)

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

gwd_sim_reps = 1000
gwd_decay_sim <- runif(gwd_sim_reps,0.01,3)
gwesp_decay_sim <- runif(gwd_sim_reps,0.01,3)

net %v% 'System' <- c(rep('System',nrow(as.sociomatrix(net))),rep(NA,ncol(as.sociomatrix(net))))
net %v% 'SYSTEM_TYPE' <- ifelse(is.na(net %v% 'SYSTEM_TYPE'),'Person',net %v% 'SYSTEM_TYPE')
temp_owner_type = net %v% 'SYSTEM_TYPE'


net %v% 'Works_For_City' = sapply(network.vertex.names(net),function(i){
  if(i %in% system_df$OWNER){NA} 
  else if(any((net %v%  'SYSTEM_TYPE')[network.vertex.names(net) %in% all_reps$OWNER[all_reps$NAME==i]] == 'City')){'Works_For_City'}
  else {'Not_For_City'}})

net %v% 'Works_For_Large_System' = sapply(network.vertex.names(net),function(i){
  if(i %in% system_df$PWS_ID){NA} 
  else if(any((net %v%  'Pop_Served')[network.vertex.names(net) %in% all_reps$OWNER[all_reps$NAME==i]] >= 10000)){'Works_For_Large_System'}
  else {'No_Large_System'}})


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

source('code/proj3/boot_MPLE.R')
net_sub <- get.inducedSubgraph(net,(1:network.size(net))[degree(net,gmode='graph')>1 | !is.na(net %v% 'Owner_Sector')])
net_sub %v% 'Log_Pop' = log((net_sub %v% 'Pop_Served') + 1)
net_sub %v% 'Same_GCD_GWuser' <- ifelse(!grepl('NA|SW|NONE',net_sub %v% 'GCD_Type'),net_sub %v% 'GCD_Type',runif(network.size(net_sub)))
net_sub %v% 'Log_CWS_COUNT' <- log(net_sub %v% 'CWS_COUNT')

bn <- 50
cores <- 5
form <- net_sub ~ edges  + # offset(b2degree(0:1))+
  #b1degree(0) + 
  #b1star(2) + b2star(2) +
  gwb1degree(0.25,fixed=T) +
  gwb2degree(0.25,fixed=T) +
  gwnsp(0.25,fixed=T) 
  
#gwb1nsp(0, fixed=T)+
  #gwb2nsp(0.5,fixed=T) +
  b1factor('COUNTY') +
  b1nodematch('COUNTY',alpha=0.25) +
  b1factor('SYSTEM_TYPE')+
  b1cov('Log_Pop') +
  b1cov('Log_CWS_COUNT') +
  b1factor('Water_Type') 

tnet
form = tnet ~ edges  + # offset(b2degree(0:1))+
    #b1degree(0) + 
    #b1star(2) + b2star(2) +
    gwdegree(0.25,fixed=T) +
    gwesp(0.25,fixed=T) 
  
  
boot_mod <- boot.MPLE(form,#offset_coef = c(-Inf,-Inf),
                        boot.rep=100,calc_loglik=F,number.cores = 10)
  

install.packages('hergm')
library(hergm)
sort(degree(tnet,gmode='graph'))
plot(net_sub)
tmat = tcrossprod(as.sociomatrix(net))
dimnames(tmat) <- replicate(2,network.vertex.names(net)[1:net$gal$bipartite],simplify = F)
tnet = as.network(tmat,matrix.type = 'adjacency',directed=F)
plot(tnet,isolates=FALSE)




tmod = hergm(tnet ~ edges_ij + triangle_ijk)




hergm(tnet ~ edges_ij + triangle_ijk,parametric = FALSE, parallel = 10,scaling = TRUE,simulate = FALSE,verbose = 0)



net_sub %v% 'Water_Type'
  b1nodematch('Exchange_Cluster') +
  b1nodematch('Same_GCD_GWuser')  

boot_mod <- boot.MPLE(form,offset_coef = c(-Inf,-Inf),
                      boot.rep=1000,calc_loglik=F,number.cores = 10)

mo = ifelse(1:network.size(net_sub) %in% which(degree(net_sub,gmode='graph')>50),degree(net_sub,gmode='graph'),NA)

test = ergm(net_sub ~ edges ,  constraints = ~bd(maxout = mo,minout=mo),estimate = 'MPLE',
            control = control.ergm(init.method = 'zeros'))


              b1factor('COUNTY') +
              b1nodematch('COUNTY',alpha=0.25) +
              b1factor('SYSTEM_TYPE')+
              b1cov('Log_Pop') +
              b1cov('Log_CWS_COUNT') +
              b1factor('Water_Type'),
            control = control.ergm(MCMLE.maxit = 40,MCMC.samplesize = 50000,MCMC.interval = 1500,
                                   MCMC.burnin = 10000),
            constraints = ~bd(maxout = mo,minout=mo),eval.loglik = F)






network.vertex.names(net_sub)[which(degree(net_sub,gmode='graph')>50)]


sort(degree(net_sub),decreasing = T)[1:10]
summary(net_sub ~ gwb1nsp(0.2,fixed=T))

boot_mod$bootstrapped_MPLE_results[,1] > boot_mod$bootstrapped_MPLE_results[,2] &
  boot_mod$bootstrapped_MPLE_results[,1] < boot_mod$bootstrapped_MPLE_results[,3]


boot_mod



boot_mod1



test = ergm(form, constraints = ~ bd(maxout = 93), eval.loglik = F,
control = control.ergm( MCMLE.check.degeneracy = FALSE,MCMC.max.maxedges = 5000),verbose=T)
      
                      

form <- net_sub ~ edges  + b2degree(0:1)+
  b1degree(0) + 
  #b1star(2) + b2star(2) +
  #gwb1degree(0,fixed=T) +
  gwb2degree(0.2,fixed=T) +
  gwb1nsp(0.2, fixed=T)+
  #gwb2nsp(0.5,fixed=T) +
  b1factor('SYSTEM_TYPE')+
  b1cov('Log_Pop') +
  b1cov('Log_CWS_COUNT') +
  b1factor('Water_Type') +
#edgecov('works_for_seller') + 
#edgecov('works_for_buyer') 
b1nodematch('Exchange_Cluster') +
  b1nodematch('Same_GCD_GWuser')

test = ergm(form, eval.loglik = FALSE,
     #offset.coef = c(-Inf,-Inf),
     constraints = ~edges,
     control = control.ergm(MCMC.burnin = 10000,MCMC.samplesize = 50000,
                            MPLE.samplesize = 100000,parallel = 8))

test2 = ergm(form, eval.loglik = FALSE,
            #offset.coef = c(-Inf,-Inf),
            constraints = ~edges,
            control = control.ergm(MCMC.burnin = 10000,MCMC.samplesize = 50000,MCMLE.maxit = 30,
                                   MPLE.samplesize = 100000,parallel = 8,init = c(test$coef)))


form2 <- net_sub ~ edges  +  #offset(b2degree(0:1))+
  b1degree(0) + 
  #b1star(2) + b2star(2) +
  #gwb1degree(0,fixed=T) +
  gwb2degree(0.2,fixed=T) +
  gwb1nsp(0.2, fixed=T)+
  #gwb2nsp(0.5,fixed=T) +
  b1factor('SYSTEM_TYPE')+
  b1cov('Log_Pop') +
  b1cov('Log_CWS_COUNT') +
  b1factor('Water_Type') +
#edgecov('works_for_seller') + 
#edgecov('works_for_buyer') 
b1nodematch('Exchange_Cluster') +
  b1nodematch('Same_GCD_GWuser')

test2 = ergm(form2, eval.loglik = FALSE,
            #offset.coef = c(-Inf,-Inf),
            constraints = ~edges,,parallel = 8,
            control = control.ergm(MCMC.burnin = 10000,MCMC.samplesize = 50000,
                                   MPLE.samplesize = 100000))




test <- ergm(form,#offset.coef = c(-Inf,-Inf),
             eval.loglik = F,estimate = 'CD',
                 constraints = ~bd(maxout = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
                                    maxin = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
                                    minout = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
                                    minin = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA)),
             control = control.ergm(MCMC.burnin = 10000,MCMC.interval = 1000,
                                                    MCMLE.maxit = 10,
                                                    Step.maxit = 100,Step.MCMC.samplesize = 500,Step.gridsize = 100))




#,offset_coef = c(-Inf,-Inf))

save.image('test_scratch.RData')
# 
# boot_mod2 <- boot.MPLE(form,
#                       boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod3 <- boot.MPLE(form,
#                       boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod4 <- boot.MPLE(form,
#                       boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod5 <- boot.MPLE(form,
#                       boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod6 <- boot.MPLE(form,
#                        boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod7 <- boot.MPLE(form,
#                        boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod8 <- boot.MPLE(form,
#                        boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod9 <- boot.MPLE(form,
#                       boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# boot_mod10 <- boot.MPLE(form,
#                        boot.rep=bn,calc_loglik=F,number.cores = cores,offset_coef = c(-Inf,-Inf))
# 
# #mod_step = ergm(form,control = control.ergm(main.method = 'Stepping',Step.MCMC.samplesize = 500,Step.gridsize = 100,
# #                                            Step.maxit = 100,MCMC.burnin = 15000,MCMC.interval = 1500),offset.coef = c(-Inf,-Inf))
# 
# save.image('scratch_mple_test_ten_boot_results2.RData')
# degree(net_sub,gmode = 'graph')[(net_sub %v% 'SYSTEM_TYPE') == 'Person']
# 
# 
# 
# 
# table(degree(net_sub,gmode = 'graph')>=48)
# 
# sort(degree(net_sub,gmode = 'graph')[(net_sub %v% 'SYSTEM_TYPE') == 'Person'])
# bd(attribs=attribs,
#    maxout=maxout,
#    maxin=maxin,
#    minout=minout,
#    minin=minin)
# summary(net_sub ~ twopath)
# summary(net_sub ~ b2star(2))
# 


test <- ergm(form,offset.coef = c(-Inf,-Inf),
              eval.loglik = F,control = control.ergm(MCMC.burnin = 15000,MCMC.interval = 2000,
                                                     MCMLE.maxit = 120))
# 
# 
#                                                   
# 
#      control = control.ergm(MCMC.interval = 1500,MCMC.burnin = 15000,
#                            # MCMC.init.maxedges=20000*0.2,MCMC.max.maxedges = 10000,
#                             MCMC.samplesize = 50000,parallel = 5,init.method = 'zeros'),verbose=T)
# 
# 
# 
# 
# 
#      constraints = ~bd(maxout = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
#                       maxin = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
#                       minout = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA),
#                       minin = ifelse(degree(net_sub,gmode = 'graph')>=50,degree(net_sub,gmode = 'graph'),NA)))
# 
# table(ifelse(degree(net_sub,gmode = 'graph')>=30,degree(net_sub,gmode = 'graph'),NA))
# #11945
# #25787 (v2)
# #4963
# #10591
# 
#   