#29392
library(tidyverse)
library(statnet)
library(parallel)
library(pbapply)
library(btergm)
library(ergm.terms.contrib)
library(ergm.userterms)
library(ggmap)
library(lubridate)
require(doParallel)
require(foreach)
require(ergm)
require(Rmpi)

system_df <- read_csv('scratch/system_with_lat_lon.csv') %>% select(-X1) %>% filter(!is.na(x)) %>%
  filter(County %in% c('HARRIS','GALVESTON','BRAZORIA','MONTGOMERY','FORT BEND'))

dinfo = read_csv('input/tceq_audits/district_info.csv')  %>% filter(`Activity Status:`=='ACTIVE') %>%
  filter(`Primary County:` %in% c('HARRIS','GALVESTON','BRAZORIA','AUSTIN','LIBERTY','CHAMBERS','MONTGOMERY','FORT BEND','WALLER'))

dinfo$NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",dinfo$NAME)
dinfo$NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",dinfo$NAME)
dinfo$NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",dinfo$NAME)
dinfo$NAME = gsub('UTILITY DISTRICT',"UD",dinfo$NAME)
dinfo$NAME = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",dinfo$NAME)
dfunc = read_csv('input/tceq_audits/district_functions.csv')
dfunc$NUM_FUNCTIONS <- rowSums(is.na(dfunc %>% select(-Status,-SYSTEM_ID,-SYSTEM_NAME)))
dinfo = left_join(dinfo,dfunc %>% select(-SYSTEM_NAME))
dinfo = dinfo[year(mdy(dinfo$`Creation Date:`))<2016,]
dinfo$SYSTEM_ID = as.character(dinfo$SYSTEM_ID)

system_df$YEAR_CREATED = decimal_date(mdy(dinfo$`Creation Date:`))[match(system_df$PWS_NAME,dinfo$NAME)]
system_df$AGE = 2017 - system_df$YEAR_CREATED
system_df$ACREAGE = dinfo$`Acre Size:`[match(system_df$PWS_NAME,dinfo$NAME)]
system_df$ACREAGE[is.na(system_df$ACREAGE)] <- median(system_df$ACREAGE,na.rm = T)
system_df$NUM_FUNCTIONS = dinfo$NUM_FUNCTIONS[match(system_df$PWS_NAME,dinfo$NAME)]
system_df$WASTEWATER = (!is.na(dinfo$RETAIL.WASTEWATER[match(system_df$PWS_NAME,dinfo$NAME)])) + 0
system_df$SOLID_WASTE = (!is.na(dinfo$SOLID.WASTE.GARBAGE[match(system_df$PWS_NAME,dinfo$NAME)])) + 0
system_df$TAX_POWERS = (!is.na(dinfo$TAX.BOND.AUTHORITY[match(system_df$PWS_NAME,dinfo$NAME)])) + 0


library(stringr)
bd = read_csv('input/texas_iwdd/infopage_long.csv',col_names = F) %>% as.data.frame()
colnames(bd) <- c('SYSTEM_ID','POSITION','BOARD_NAME','X4','DNAME','ADDRESS','ZIP','PHONE','FAX','TERM','CHOSEN')
bd$BOARD_FULL = bd$BOARD_NAME
bd$BOARD_NAME = gsub('\\,.*','',bd$BOARD_NAME)
bd = bd[!grepl('^[0-9]|^PO BOX',bd$BOARD_NAME),]
bd$SYSTEM_ID = as.character(bd$SYSTEM_ID)
bd = bd %>% filter(!duplicated(paste(SYSTEM_ID,BOARD_NAME)))
bd$TITLE = gsub('-',' ', gsub('^\\, ','',str_extract(bd$BOARD_FULL,'\\,.*')))
bd$TITLE = gsub('ASST ','ASSISTANT ',bd$TITLE)
bd$TITLE = gsub('N0TICE','NOTICE',bd$TITLE)
bd$TITLE = gsub('DAN','DISTRICT AGENT FOR NOTICE',bd$TITLE)
bd$TITLE = gsub('ASST\\. ','ASSISTANT ',bd$TITLE)
bd$TITLE = gsub('SECT$','SECRETARY',bd$TITLE)
bd$TITLE = gsub('SEC$','SECRETARY',bd$TITLE)
bd$TITLE = gsub('BT','TREASURER',bd$TITLE)
bd$TITLE = gsub('TREAS$','TREASURER',bd$TITLE)
bd$TITLE = gsub('TRES$','TREASURER',bd$TITLE)
bd$TITLE = gsub('ATTONREY|ATTORNRY','ATTORNEY',bd$TITLE)
bd$TITLE = gsub('ATTONREY|ATTORNRY','ATTORNEY',bd$TITLE)
bd$TITLE = gsub('DIRECTIR','DIRECTOR',bd$TITLE)
bd$TITLE = gsub('PPRESIDENT','PRESIDENT',bd$TITLE)
bd$TITLE = gsub('BOOK KEEPER|BOOKKEPPER|BOOKEEPER|BOOKKEPER','BOOKKEEPER',bd$TITLE)
bd$TITLE = gsub('VISE','VICE',bd$TITLE)
bd$TITLE = gsub('AGNT','AGENT',bd$TITLE)
bd$TITLE = gsub('^FIN ','FINANCIAL ',bd$TITLE)
bd$TITLE = gsub('SYSTM MNGR','SYSTEM MANAGER',bd$TITLE)


bd$TITLE = gsub('^GEN ','GENERAL ',bd$TITLE)
bd$TITLE = gsub('^DIST ','DISTRICT ',bd$TITLE)
bd$TITLE = gsub('^SECT','SECRETARY',bd$TITLE)
bd$TITLE = gsub('^SEC ','SECRETARY ',bd$TITLE)
bd$TITLE = gsub('ENGINEERS|ENGINGEER|EINGINEER','ENGINEER',bd$TITLE)
bd$TITLE[grep("^POSITION |^POS ",bd$TITLE)] = 'MEMBER'
bd$TITLE[grepl('^GM',bd$TITLE)] <- 'GENERAL MANAGER'
bd$TITLE[grep("PROFESSIONAL ENGINER",bd$TITLE)] = 'ENGINEER'
bd$TITLE = gsub('TAXC COLLECTOR|^TAC COLLECTOR','TAX COLLECTOR',bd$TITLE)
bd$TITLE[grep("^TAX COLL",bd$TITLE)] = 'TAX COLLECTOR'
bd$TITLE[grepl("SECRETARY",bd$TITLE) & grepl("TREASURER",bd$TITLE)] <- 'SECRETARY/TREASURER'

bd$TITLE = gsub('^BOARD |^DISTRICT |^DISTIRCT ','',bd$TITLE)

bd$TYPE = NA
bd$TYPE[grepl('PRESIDENT|CHAIRMAN|REPRESENT|DIRECTOR|MEMBER|COMMISSIONER|JUDGE|RESPONSIBLE',bd$TITLE)] <- 'POLITICAL'
bd$TYPE[grepl('ENGINEER|OPERAT|TECHN|CONSULT|SUPERVISOR|SUPERINTENDENT',bd$TITLE)] <- 'TECHNICAL'
bd$TYPE[grepl('MANAG|AGENT FOR NOTICE|SECRETARY|TREASURER|ATTORNEY|ADMINIST|COUNSEL|TRUSTEE|OWNER',bd$TITLE)] <- 'MANAGERIAL'
bd$TYPE[grepl('FINANCIAL|INVEST|CPA|TAX|BOND|ACCOUNT|AUDITOR|BOOKKEEPER|INV OFF',bd$TITLE)] <- 'FINANCIAL'

bd = bd[!is.na(bd$TYPE),]

dinfo = left_join(bd,dinfo)
system_df = system_df %>% filter(PWS_NAME %in% dinfo$NAME)
system_df$SYSTEM_ID = dinfo$SYSTEM_ID[match(system_df$PWS_NAME,dinfo$NAME)]


library(readxl)
debt = read_excel('input/fiscal_data/16WD_debtissuance.xlsx')
fiscal = read_excel('input/fiscal_data/16WDTR_clean.xls')
audits = read_csv('input/tceq_audits/yearly_district_audits.csv')
audits$SYSTEM_NAME = gsub('MUNICIPAL UTILITY DISTRICT',"MUD",audits$SYSTEM_NAME)
audits$SYSTEM_NAME = gsub('SPECIAL UTILITY DISTRICT',"SUD",audits$SYSTEM_NAME)
audits$SYSTEM_NAME = gsub('PUBLIC UTILITY DISTRICT',"PUD",audits$SYSTEM_NAME)
audits$SYSTEM_NAME = gsub('UTILITY DISTRICT',"UD",audits$SYSTEM_NAME)
audits$SYSTEM_NAME = gsub('MUNICIPAL MANAGEMENT DISTRICT',"MMD",audits$SYSTEM_NAME)

library(lubridate)
audits$FYEND = mdy(audits$`FISCAL YEAR ENDED`)
audits$FY = year(audits$FYEND)
audits = audits %>% arrange(SYSTEM_NAME,-FY) %>% filter(!duplicated(SYSTEM_NAME))
system_df$TAX_RATE <- audits$`TOTAL TAX RATE`[match(system_df$PWS_NAME,audits$SYSTEM_NAME)]
system_df$GEN_REV <- audits$`GENERAL FUND - TOTAL REVENUES`[match(system_df$PWS_NAME,audits$SYSTEM_NAME)]
system_df$EXP_REV <- audits$`ENTERPRISE FUND - OPERATING REVENUES`[match(system_df$PWS_NAME,audits$SYSTEM_NAME)]
system_df$BONDS_OUTSTANDING <- audits$`BONDS OUTSTANDING`[match(system_df$PWS_NAME,audits$SYSTEM_NAME)]


library(ggmap)

library(pbapply)
library(parallel)


viols_df = read_csv('input/epa_echo/echo_compliance_6-23-17.csv',na=c("-")) %>% rename(PWS_ID = Pwsid)
viols_df$OWNER <- system_df$OWNER[match(viols_df$PWS_ID,system_df$PWS_ID)]
viols_df <- viols_df %>% group_by(PWS_ID) %>% summarise(tot_viol_points = sum(Viopaccr))


system_reps = left_join(bd %>% select(SYSTEM_ID,POSITION,BOARD_NAME,TYPE) %>% filter(SYSTEM_ID %in% system_df$SYSTEM_ID),
                        system_df %>% select(PWS_NAME,PWS_ID,SYSTEM_ID))



how_many_reps = as.data.frame(table(system_reps$PWS_ID,system_reps$TYPE)) %>% rename(PWS_ID = Var1,TYPE = Var2) %>%
  spread(TYPE,Freq) %>% mutate(PWS_ID = as.character(PWS_ID))


system_matrix = matrix(0,ncol = length(unique(system_reps$BOARD_NAME)),nrow=length(unique(system_reps$PWS_ID)))
colnames(system_matrix) <- sort(unique(system_reps$BOARD_NAME))
rownames(system_matrix) <- sort(unique(system_reps$PWS_ID))




p = as.data.frame(table(system_reps$PWS_ID,system_reps$BOARD_NAME,system_reps$TYPE)) %>%
  filter(Freq>0) %>% rename(PWS_ID = Var1,BOARD_NAME = Var2,TYPE = Var3)

system_financial_matrix = matrix(0,ncol = length(unique(system_reps$BOARD_NAME[system_reps$TYPE=='FINANCIAL'])),nrow=length(unique(system_reps$PWS_ID)))
system_managerial_matrix = matrix(0,ncol = length(unique(system_reps$BOARD_NAME[system_reps$TYPE=='MANAGERIAL'])),nrow=length(unique(system_reps$PWS_ID)))
system_technical_matrix = matrix(0,ncol = length(unique(system_reps$BOARD_NAME[system_reps$TYPE=='TECHNICAL'])),nrow=length(unique(system_reps$PWS_ID)))

colnames(system_financial_matrix) <- sort(unique(system_reps$BOARD_NAME[system_reps$TYPE=='FINANCIAL']))
colnames(system_managerial_matrix) <- sort(unique(system_reps$BOARD_NAME[system_reps$TYPE=='MANAGERIAL']))
colnames(system_technical_matrix) <- sort(unique(system_reps$BOARD_NAME[system_reps$TYPE=='TECHNICAL']))
rownames(system_financial_matrix) = rownames(system_managerial_matrix)  = rownames(system_technical_matrix) <- sort(unique(system_reps$PWS_ID))


system_matrix1 = system_matrix2 = system_matrix
invisible(lapply(which(p$TYPE == 'MANAGERIAL'),function(i) system_managerial_matrix[match(p$PWS_ID[i],rownames(system_managerial_matrix)),
                                                                                    match(p$BOARD_NAME[i],colnames(system_managerial_matrix))] <<- p$Freq[i]))
invisible(lapply(which(p$TYPE == 'FINANCIAL'),function(i) system_financial_matrix[match(p$PWS_ID[i],rownames(system_financial_matrix)),
                                                                                  match(p$BOARD_NAME[i],colnames(system_financial_matrix))] <<- p$Freq[i]))
invisible(lapply(which(p$TYPE == 'TECHNICAL'),function(i) system_technical_matrix[match(p$PWS_ID[i],rownames(system_technical_matrix)),
                                                                                  match(p$BOARD_NAME[i],colnames(system_technical_matrix))] <<- p$Freq[i]))

invisible(lapply(1:nrow(p),function(i) system_matrix[match(p$PWS_ID[i],rownames(system_matrix)),
                                                     match(p$BOARD_NAME[i],colnames(system_matrix))] <<- p$Freq[i]))



mat = tcrossprod(system_matrix)

mat_list = lapply(grep('system_.*_matrix',ls(),value=T),get)
names(mat_list) <- grep('system_.*_matrix',ls(),value=T)
mat1 = tcrossprod(system_matrix1)
mat2 = tcrossprod(system_matrix2)


n = as.network(mat,matrix.type = 'adjacency',directed = F)
n1 = as.network(mat1,matrix.type = 'adjacency',directed = F)
n2 = as.network(mat2,matrix.type = 'adjacency',directed = F)
net_list = lapply(mat_list,function(x) as.network(x,matrix.type='incidence',directed=F,bipartite=T))
names(net_list) <- names(mat_list)


lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Water_Type' ,c(system_df$`GW or SW Code`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep('M',network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Service_Pop' ,c(log(system_df$`Population Served Count`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Pop_Per_Service_Connection' , c(log(system_df$`Population Served Count`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]/system_df$`Service Connections Count`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))

lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Wholesaler' ,c(system_df$`Is Wholesaler`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep('M',network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Owner_Type' ,c(system_df$`Owner Type Code`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep('M',network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))

lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]],'Facilities' ,system_df$`# of Facilities`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Make_Buy' ,c(ifelse(grepl('P',system_df$`Primary Source Code`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),'Purchase',"Self-Source"))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'GCD' ,c(system_df$GCD_ZONE[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep('M',network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))

lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'AGE' ,c(system_df$AGE[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))

lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'County' ,c(system_df$County[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)],rep("M",network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Bonds_Outstanding' ,c(log(as.numeric(gsub('$','',gsub(",",'',system_df$BONDS_OUTSTANDING[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],
                                                                                                                                                                                                 system_df$PWS_ID)]),fixed=T))+0.00001),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Revenue' ,c(log(as.numeric(gsub('$','',gsub(",",'',system_df$GEN_REV[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))+  as.numeric(gsub('$','',gsub(",",'',system_df$EXP_REV[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))+ 0.00001),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Bonds_Outstanding' ,ifelse(is.na(net_list[[x]] %v% 'Log_Bonds_Outstanding'), log(0 + 0.00001),net_list[[x]] %v% 'Log_Bonds_Outstanding')))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Revenue' ,ifelse(is.na(net_list[[x]] %v% 'Log_Revenue'), log(0 + 0.00001),net_list[[x]] %v% 'Log_Revenue')))

lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Bonds_Outstanding_Per_Connection' ,c(log((as.numeric(gsub('$','',gsub(",",'',system_df$BONDS_OUTSTANDING[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))/system_df$`Service Connections Count`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)])+0.00001),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))


lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Revenue_Per_Connection' ,c(log(((as.numeric(gsub('$','',gsub(",",'',system_df$GEN_REV[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))+ as.numeric(gsub('$','',gsub(",",'',system_df$EXP_REV[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))) / system_df$`Service Connections Count`[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)])+ +0.00001),rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Bonds_Outstanding_Per_Connection' ,ifelse(is.na(net_list[[x]] %v% 'Log_Bonds_Outstanding_Per_Connection'), log(0 + 0.00001),net_list[[x]] %v% 'Log_Bonds_Outstanding_Per_Connection')))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Revenue_Per_Connection' ,ifelse(is.na(net_list[[x]] %v% 'Log_Revenue_Per_Connection'), log(0 + 0.00001),net_list[[x]] %v% 'Log_Revenue_Per_Connection')))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Viol_Points_5yr' , c(viols_df$tot_viol_points[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],viols_df$PWS_ID)],rep(0,network.size(net_list[[x]]) - net_list[[x]]$gal$bipartite))))


lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Acreage' ,c(log(as.numeric(gsub('$','',gsub(",",'',system_df$ACREAGE[match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],system_df$PWS_ID)]),fixed=T))))))
lapply(1:length(net_list), function(x) net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'Log_Acreage' ,ifelse(is.na(net_list[[x]] %v% 'Log_Acreage'), log(0 + 0.00001),net_list[[x]] %v% 'Log_Acreage')))


names(net_list) <- toupper(gsub('_','',str_extract(names(net_list),'_.*_')))

lapply(1:length(net_list), function(x)
  net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'People_Observed' , how_many_reps[names(net_list)[x]][match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],how_many_reps$PWS_ID),]))

lapply(1:length(net_list), function(x)
  net_list[[x]] <<- set.vertex.attribute(net_list[[x]], 'No_People_Observed' ,
                                         (how_many_reps[names(net_list)[x]][match(network.vertex.names(net_list[[x]])[1:net_list[[x]]$gal$bipartite],how_many_reps$PWS_ID),]==0)+0))


n %v% 'Water_Type' = n1 %v% 'Water_Type' = n2 %v% 'Water_Type'  = system_df$`GW or SW Code`[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'Log_Service_Pop' = n1 %v% 'Log_Service_Pop' = n2 %v% 'Log_Service_Pop'  = log(system_df$`Population Served Count`[match(network.vertex.names(n),system_df$PWS_ID)])
n %v% 'Log_Pop_Per_Service_Connection'  = n1 %v% 'Log_Pop_Per_Service_Connection' = n2 %v% 'Log_Pop_Per_Service_Connection'  = log(system_df$`Population Served Count`[match(network.vertex.names(n),system_df$PWS_ID)]/system_df$`Service Connections Count`[match(network.vertex.names(n),system_df$PWS_ID)][match(network.vertex.names(n),system_df$PWS_ID)])
n %v% 'Wholesaler' = n1 %v% 'Wholesaler' = n2 %v% 'Wholesaler'  = system_df$`Is Wholesaler`[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'Owner_Type' = n1 %v% 'Owner_Type' = n2 %v% 'Owner_Type'  = system_df$`Owner Type Code`[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'Facilities' = n1 %v% 'Facilities' = n2 %v%  'Facilities' = system_df$`# of Facilities`[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'Make_Buy' = n1 %v% 'Make_Buy' = n2 %v%  'Make_Buy' = ifelse(grepl('P',system_df$`Primary Source Code`[match(network.vertex.names(n),system_df$PWS_ID)]),'Purchase',"Self-Source")
n %v% 'GCD'  = n1 %v% 'GCD' = n2 %v%  'GCD' = system_df$GCD_ZONE[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'County' = n1 %v% 'County' = n2 %v%  'County' = system_df$County[match(network.vertex.names(n),system_df$PWS_ID)]
n %v% 'Log_Bonds_Outstanding' = n1 %v% 'Log_Bonds_Outstanding' = n2 %v%  'Log_Bonds_Outstanding' = log(as.numeric(gsub('$','',gsub(",",'',system_df$BONDS_OUTSTANDING[match(network.vertex.names(n),system_df$PWS_ID)]),fixed=T))+0.00001)
n %v% 'Log_Revenue' = n1 %v% 'Log_Revenue' = n2 %v%  'Log_Revenue' = log(as.numeric(gsub('$','',gsub(",",'',system_df$GEN_REV[match(network.vertex.names(n),system_df$PWS_ID)]),fixed=T))+
                                                                           as.numeric(gsub('$','',gsub(",",'',system_df$EXP_REV[match(network.vertex.names(n),system_df$PWS_ID)]),fixed=T))+ +0.00001)
n %v% 'Log_Bonds_Outstanding' = ifelse(is.na(n %v% 'Log_Bonds_Outstanding'), log(0 + 0.00001),n1 %v% 'Log_Bonds_Outstanding')
n %v% 'Log_Revenue' = ifelse(is.na(n %v% 'Log_Revenue'), log(0 + 0.00001),n1 %v% 'Log_Revenue')


library(sp)
library(rgdal)
library(rgeos)

system_df = system_df %>% arrange(PWS_ID)
space_mat <- sp::spDists(cbind(system_df$x,system_df$y),longlat = FALSE)
colnames(space_mat) = rownames(space_mat) = system_df$PWS_ID

sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
                  read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type %in% c('P','S')) %>%
  filter(!duplicated(paste0(Buyer,Seller)))

empty_mat = as.sociomatrix(n)
empty_mat[TRUE] <- 0
sales_mat = buy_mat =  empty_mat

sell_fin_mat = buy_fin_mat = as.sociomatrix(net_list$FINANCIAL)
sell_fin_mat[TRUE] <- 0
buy_fin_mat[TRUE] <- 0
for (i in 1:nrow(sell_fin_mat))
{sell_fin_mat[i,colnames(sell_fin_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Seller[sales$Buyer==rownames(sell_fin_mat)[i]]]] <- 1
buy_fin_mat[i,colnames(buy_fin_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Buyer[sales$Seller==rownames(buy_fin_mat)[i]]]] <- 1}

sell_man_mat = buy_man_mat = as.sociomatrix(net_list$MANAGERIAL)
sell_man_mat[TRUE] <- 0
buy_man_mat[TRUE] <- 0
for (i in 1:nrow(sell_man_mat))
{sell_man_mat[i,colnames(sell_man_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Seller[sales$Buyer==rownames(sell_man_mat)[i]]]] <- 1
buy_man_mat[i,colnames(buy_man_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Buyer[sales$Seller==rownames(buy_man_mat)[i]]]] <- 1}

sell_tech_mat = buy_tech_mat = as.sociomatrix(net_list$TECHNICAL)
sell_tech_mat[TRUE] <- 0
buy_tech_mat[TRUE] <- 0
for (i in 1:nrow(sell_tech_mat))
{sell_tech_mat[i,colnames(sell_tech_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Seller[sales$Buyer==rownames(sell_tech_mat)[i]]]] <- 1
buy_tech_mat[i,colnames(buy_tech_mat) %in% system_reps$BOARD_NAME[system_reps$PWS_ID %in% sales$Buyer[sales$Seller==rownames(buy_tech_mat)[i]]]] <- 1}

trans_tech_mat = sell_tech_mat + buy_tech_mat
trans_fin_mat = sell_fin_mat + buy_fin_mat
trans_man_mat = sell_man_mat + buy_man_mat


dist_tech_mat = as.sociomatrix(net_list$TECHNICAL)
tech_tie_df = dist_tech_mat %>% as.data.frame() %>% mutate(PWS_ID = rownames(.)) %>% gather(BOARD_NAME,TIE,-PWS_ID)
tech_tie_df$CLOSEST_OTHER_EMPLOYER <- NA
tech_tie_df$CLOSEST_OTHER_EMPLOYER <- pbsapply(1:nrow(tech_tie_df),function(i)
  min(space_mat[rownames(space_mat)==tech_tie_df$PWS_ID[i],colnames(space_mat) %in% tech_tie_df$PWS_ID[tech_tie_df$BOARD_NAME == tech_tie_df$BOARD_NAME[i]  & tech_tie_df$PWS_ID != tech_tie_df$PWS_ID[i]]])
  ,cl = 10,simplify=T)
stopImplicitCluster()
mat_tech_nearest_other = tech_tie_df %>% select(-TIE) %>% spread(key = BOARD_NAME,value = CLOSEST_OTHER_EMPLOYER)
rownames(mat_tech_nearest_other) <- mat_tech_nearest_other[,1]
mat_tech_nearest_other = mat_tech_nearest_other[,-1]

dist_fin_mat = as.sociomatrix(net_list$FINANCIAL)
fin_tie_df = dist_fin_mat %>% as.data.frame() %>% mutate(PWS_ID = rownames(.)) %>% gather(BOARD_NAME,TIE,-PWS_ID)
fin_tie_df$CLOSEST_OTHER_EMPLOYER <- NA
fin_tie_df$CLOSEST_OTHER_EMPLOYER <- pbsapply(1:nrow(fin_tie_df),function(i)
  min(space_mat[rownames(space_mat)==fin_tie_df$PWS_ID[i],colnames(space_mat) %in% fin_tie_df$PWS_ID[fin_tie_df$BOARD_NAME == fin_tie_df$BOARD_NAME[i]  & fin_tie_df$PWS_ID != fin_tie_df$PWS_ID[i]]])
  ,cl = 16,simplify=T)
stopImplicitCluster()
mat_fin_nearest_other = fin_tie_df %>% select(-TIE) %>% spread(key = BOARD_NAME,value = CLOSEST_OTHER_EMPLOYER)
rownames(mat_fin_nearest_other) <- mat_fin_nearest_other[,1]
mat_fin_nearest_other = mat_fin_nearest_other[,-1]


dist_man_mat = as.sociomatrix(net_list$MANAGERIAL)
man_tie_df = dist_man_mat %>% as.data.frame() %>% mutate(PWS_ID = rownames(.)) %>% gather(BOARD_NAME,TIE,-PWS_ID)
man_tie_df$CLOSEST_OTHER_EMPLOYER <- NA
man_tie_df$CLOSEST_OTHER_EMPLOYER <- pbsapply(1:nrow(man_tie_df),function(i)
  min(space_mat[rownames(space_mat)==man_tie_df$PWS_ID[i],colnames(space_mat) %in% man_tie_df$PWS_ID[man_tie_df$BOARD_NAME == man_tie_df$BOARD_NAME[i]  & man_tie_df$PWS_ID != man_tie_df$PWS_ID[i]]])
  ,cl = 20,simplify=T)
stopImplicitCluster()
dist_man_mat[TRUE] <- 0
mat_man_nearest_other = man_tie_df %>% select(-TIE) %>% spread(key = BOARD_NAME,value = CLOSEST_OTHER_EMPLOYER)
rownames(mat_man_nearest_other) <- mat_man_nearest_other[,1]
mat_man_nearest_other = mat_man_nearest_other[,-1]

require(Rmpi)

invisible(sapply(1:nrow(empty_mat),function(i){
  sales_mat[sales$Seller[i] == rownames(sales_mat),sales$Buyer[i] == colnames(sales_mat)] <<- 1;
  buy_mat[sales$Buyer[i] == rownames(sales_mat),sales$Seller[i] == colnames(sales_mat)] <<- 1}))
transaction_mat = (sales_mat + buy_mat)


library(btergm)
library(Rmpi)


gw_same_gcd = ifelse(tcrossprod(as.matrix(table(network.vertex.names(n1),n1 %v% 'GCD')))+
                       tcrossprod(as.matrix(table(network.vertex.names(n1),n1 %v% 'Water_Type')))==2,1,0)
gw_same_gcd_n = ifelse(tcrossprod(as.matrix(table(network.vertex.names(n),n %v% 'GCD')))+
                         tcrossprod(as.matrix(table(network.vertex.names(n),n %v% 'Water_Type')))==2,1,0)

net_list[[1]] %v% 'Source_Cat' = paste(net_list[[1]] %v% 'Water_Type' ,net_list[[1]] %v% 'Make_Buy',sep='_')
net_list[[2]] %v% 'Source_Cat' = paste(net_list[[2]] %v% 'Water_Type' ,net_list[[2]] %v% 'Make_Buy',sep='_')
net_list[[3]] %v% 'Source_Cat' = paste(net_list[[3]] %v% 'Water_Type' ,net_list[[3]] %v% 'Make_Buy',sep='_')

source('code/proj3/boot_MPLE.R')

net_list$FINANCIAL %v% 'Same_GCD_GW_User' = paste(net_list$FINANCIAL %v% 'GCD',net_list$FINANCIAL %v% 'Source_Cat',sep='_')
net_list$MANAGERIAL %v% 'Same_GCD_GW_User' = paste(net_list$MANAGERIAL %v% 'GCD',net_list$MANAGERIAL %v% 'Source_Cat',sep='_')
net_list$TECHNICAL %v% 'Same_GCD_GW_User' = paste(net_list$TECHNICAL %v% 'GCD',net_list$TECHNICAL %v% 'Source_Cat',sep='_')
# 
# form_fin = net_list$FINANCIAL ~ edges + b1factor('Water_Type') + b1factor('Make_Buy') +
#   b1cov('Log_Acreage') + b1absdiff('Log_Acreage') +
#   b1cov('AGE') + b1absdiff('AGE')+
#   b1cov('Log_Service_Pop') + b1absdiff('Log_Service_Pop') +
#   b1cov('Log_Bonds_Outstanding_Per_Connection') + b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
#   b1cov('Log_Revenue_Per_Connection') + b1absdiff('Log_Revenue_Per_Connection') +
#   b1cov('Viol_Points_5yr') + b1absdiff('Viol_Points_5yr') +
#   b1nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(net_list$FINANCIAL %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(net_list$FINANCIAL %v% 'Same_GCD_GW_User')))) )+
#   edgecov(trans_fin_mat) + edgecov(as.matrix(mat_fin_nearest_other))
# 
# boot_mod_fin <- boot.MPLE(form_fin,boot.rep=1000,calc_loglik=F,number.cores = 10)
# stopImplicitCluster()
# save.image('scratch/final_boot_runs.RData')

load('scratch/final_boot_runs.RData')

form_fin = net_list$FINANCIAL ~ edges + 
gwb1degree(0.25,fixed=T) + 
gwb2degree(0.25,fixed=T) + 
gwb1nsp(0.5,fixed=T) + gwb2nsp(0.5,fixed=T) + 
offset(b1mindegree(max(degree(net_list$FINANCIAL,gmode='graph')[1:net_list$FINANCIAL$gal$bipartite]) + 1)) + offset(b2mindegree(max(degree(net_list$FINANCIAL,gmode='graph')[(net_list$FINANCIAL$gal$bipartite+1):network.size(net_list$FINANCIAL)]) + 1)) + 
b1factor('Water_Type') + b1factor('Make_Buy') +
b1cov('Log_Acreage') + 
b1cov('AGE') + 
b1cov('Log_Service_Pop') + 
b1cov('Log_Bonds_Outstanding_Per_Connection') + 
b1cov('Log_Revenue_Per_Connection') + 
b1cov('Viol_Points_5yr') + 
#b1absdiff('Log_Service_Pop') +
#b1absdiff('Log_Revenue_Per_Connection') +
#b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
#b1absdiff('Viol_Points_5yr') +
b1nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(net_list$FINANCIAL %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(net_list$FINANCIAL %v% 'Same_GCD_GW_User')))) )+
edgecov(trans_fin_mat) + edgecov(as.matrix(mat_fin_nearest_other))

#m1 = ergm(form_fin,eval.loglik = F,verbose=F,constraints = ~edges,control = control.ergm(MCMC.interval = 2000,MCMC.burnin = 40000,
#MCMLE.maxit = 20,MCMC.samplesize = 5000,
#Step.MCMC.samplesize = 1000),offset.coef = c(-Inf,-Inf))
#stopImplicitCluster()

#save.image('scratch/ergm_mcmc_runs.RData')


form_man = net_list$MANAGERIAL ~ edges + cycle(4) + 
gwb1degree(.5,fixed=T) + 
gwb2degree(.5,fixed=T) + 
gwb1nsp(.5,fixed=T) + gwb2nsp(.5,fixed=T) + 
offset(b1mindegree(max(degree(net_list$MANAGERIAL,gmode='graph')[1:net_list$MANAGERIAL$gal$bipartite]) + 1)) + 
offset(b2mindegree(max(degree(net_list$MANAGERIAL,gmode='graph')[(net_list$MANAGERIAL$gal$bipartite+1):network.size(net_list$MANAGERIAL)]) + 1)) + 
b1factor('Water_Type') + b1factor('Make_Buy') +
b1cov('Log_Acreage') + 
b1cov('AGE') + 
b1cov('Log_Service_Pop') + 
b1cov('Log_Bonds_Outstanding_Per_Connection') + 
b1cov('Log_Revenue_Per_Connection') + 
b1cov('Viol_Points_5yr') + 
#b1absdiff('Log_Service_Pop') +
#b1absdiff('Log_Revenue_Per_Connection') +
#b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
#b1absdiff('Viol_Points_5yr') +
b1nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(net_list$MANAGERIAL %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(net_list$MANAGERIAL %v% 'Same_GCD_GW_User')))) )+
edgecov(trans_man_mat) + edgecov(as.matrix(mat_man_nearest_other))

m2 = ergm(form_man,eval.loglik = F,verbose=F,
          constraints = ~edges,
          control = control.ergm(MCMC.interval = 2000,MCMC.burnin = 40000,
           MCMLE.maxit = 20,MCMC.samplesize = 10000,
Step.MCMC.samplesize = 1000),offset.coef = c(-Inf,-Inf))

#stopImplicitCluster()
save.image('scratch/ergm_mcmc_runs.RData')
# 
# form_tech = net_list$TECHNICAL ~ edges + gwb1degree(0.25,fixed=T) + 
#   gwb2degree(0.25,fixed=T) + 
#   gwb1nsp(0.5,fixed=T) + gwb2nsp(0.5,fixed=T) + 
# offset(b1mindegree(max(degree(net_list$TECHNICAL,gmode='graph')[1:net_list$TECHNICAL$gal$bipartite]) + 1)) + offset(b2mindegree(max(degree(net_list$TECHNICAL,gmode='graph')[(net_list$TECHNICAL$gal$bipartite+1):network.size(net_list$TECHNICAL)]) + 1)) + 
# b1factor('Water_Type') + b1factor('Make_Buy') +
# b1cov('Log_Acreage') + 
# b1cov('AGE') + 
# b1cov('Log_Service_Pop') + 
# b1cov('Log_Bonds_Outstanding_Per_Connection') + 
# b1cov('Log_Revenue_Per_Connection') + 
# b1cov('Viol_Points_5yr') + 
# #b1absdiff('Log_Service_Pop') +
# #b1absdiff('Log_Revenue_Per_Connection') +
# #b1absdiff('Log_Bonds_Outstanding_Per_Connection') +
# #b1absdiff('Viol_Points_5yr') +
# b1nodematch('Same_GCD_GW_User',diff=F,keep = which(grepl('GW',sort(unique(net_list$TECHNICAL %v% 'Same_GCD_GW_User')))&grepl('Self',sort(unique(net_list$TECHNICAL %v% 'Same_GCD_GW_User')))) )+
# edgecov(trans_tech_mat) + edgecov(as.matrix(mat_tech_nearest_other))
# 
# m3 = ergm(form_tech,eval.loglik = F,verbose=F,
#           constraints = ~edges,
#           control = control.ergm(MCMC.interval = 2000,MCMC.burnin = 40000,
#            MCMLE.maxit = 20,MCMC.samplesize = 5000,
# Step.MCMC.samplesize = 1000),offset.coef = c(-Inf,-Inf))
# 
# #stopImplicitCluster()
# save.image('scratch/ergm_mcmc_runs.RData')
