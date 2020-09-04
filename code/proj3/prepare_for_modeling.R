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
system_df$TYPE = dinfo$`District Type:`[match(system_df$PWS_NAME,dinfo$NAME)]


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

table(system_df$TYPE[system_df$PWS_ID %in% rownames(system_matrix)])

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
keep_nodes = lapply(net_list,function(x) which(degree(x,gmode='graph')>1 | 1:network.size(x) <= x$gal$bipartite))
nms = names(net_list)
net_list = lapply(1:length(net_list),function(x) get.inducedSubgraph(net_list[[x]], v = keep_nodes[[x]]))
names(net_list) <- nms



twd_geo = readOGR('spatial_inputs/government_units','TCEQ_WATER_DISTRICTS')
twd_geo = twd_geo[twd_geo$COUNTY %in% houston_counties,]
twd_geo$NAME = str_to_upper(twd_geo$NAME)
twd_geo$NAME = gsub('PUBLIC UTILITY DISTRICT','PUD',twd_geo$NAME)
twd_geo$NAME = gsub('UTILITY DISTRICT|UTILITY DRISTRICT','UD',twd_geo$NAME)
twd_geo$NAME = gsub('134 A','134A',twd_geo$NAME)
twd_geo$NAME = gsub('134 C','134C',twd_geo$NAME)
twd_geo$NAME = gsub('THE WOODLANDS','WOODLANDS',twd_geo$NAME)
twd_geo$NAME = gsub('POST WOOD','POSTWOOD',twd_geo$NAME)
twd_geo$NAME = gsub('CLOVERCREEK','CLOVER CREEK',twd_geo$NAME)
twd_geo$NAME = gsub('  ',' ',twd_geo$NAME)
twd_geo$NAME = gsub("'","",twd_geo$NAME)
twd_geo$NAME = gsub('OF BRAZORIA COUNTY','',twd_geo$NAME)
twd_geo$NAME = gsub('VILLIAGES','VILLAGES',twd_geo$NAME)
twd_geo$NAME = gsub('HARRIS COUNTY FWSD 1-B','HARRIS COUNTY FWSD 1B',twd_geo$NAME)
twd_geo$NAME = gsub(' $','',twd_geo$NAME)
#twd_geo$NAME = gsub('HARRIS FORT BEND COUNTIES MUD 1','HARRIS-FORT BEND COUNTIES MUD 1',twd_geo$NAME)
#twd_geo$NAME = gsub('HARRIS FORT BEND COUNTIES MUD 3','HARRIS-FORT BEND COUNTIES MUD 3',twd_geo$NAME)
twd_geo$NAME = gsub('THUNDERBIRD UD','THUNDERBIRD UD 1',twd_geo$NAME)
twd_geo$NAME = gsub('-',' ',twd_geo$NAME)
twd_geo$NAME = gsub("CYPRESS KLEIN UTILTIY DISTRICT","CYPRESS KLEIN MUD",twd_geo$NAME)
twd_geo$NAME = gsub("^HARRIS COUNTY MUD 18$","HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS",twd_geo$NAME)
twd_geo$NAME[twd_geo$NAME=="THUNDERBIRD UD 1"] = "THUNDERBIRD UD"
twd_geo = spTransform(twd_geo,CRS(proj4string(hout_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo = twd_geo[!twd_geo@data$NAME %in% service1@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% service2@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% hout_places@data$NAME,]
twd_geo = twd_geo[!twd_geo@data$NAME %in% hout_utils@data$UTILITY,]
twd_geo <- twd_geo[twd_geo@data$NAME %in% system_df$PWS_NAME,]

twd_geo = spTransform(twd_geo,CRS(proj4string(hout_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)
twd_geo.df$MASTER_TYPE = 'SD'

ids = system_df$SYSTEM_ID[system_df$PWS_ID %in% network.vertex.names(net_list$FINANCIAL)[1:net_list$FINANCIAL$gal$bipartite]]
twd_geo = twd_geo[twd_geo$DISTRICT_I %in% ids,]
twd_geo = spTransform(twd_geo,CRS(proj4string(hout_counties)))
twd_geo@data$id = rownames(twd_geo@data)
twd_geo.df     <- fortify(twd_geo)
twd_geo.df = left_join(twd_geo.df, twd_geo@data)

houston_counties = c('Harris','Montgomery','Fort Bend','Brazoria',"Galveston")
tx_counties = readOGR('spatial_inputs/government_units','county_nrcs_a_tx')
hout_counties = tx_counties[tx_counties$COUNTYNAME %in% houston_counties,]



hout_counties@data$id = hout_counties@data$COUNTYNAME
hout_counties.points = fortify(hout_counties, region="id")
hout_counties.df = plyr::join(hout_counties.points, hout_counties@data, by="id")

gg_hous = ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group),data=hout_counties.df,col = 'grey80',fill='grey80') + 
  geom_polygon(aes(x=long,y=lat,group=group),col='grey50',data =twd_geo.df,lwd=0.2)+
  #geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(!MASTER_TYPE%in% c('Municipal','Other SD','Private')),col='grey50') +
  #geom_polygon(aes(x=long,y=lat,group=group,fill=MASTER_TYPE),data =service_df %>% filter(MASTER_TYPE%in% c('Private')),col='grey50') +
  theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title = element_blank(),legend.position = c(0.9,0.15),legend.title=element_text(size=18),legend.text=element_text(size=18)) +
  scale_fill_colorblind(name = "Provider type") 
gg_hous
library(ggthemes)


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

save.image('scratch/model_prep.RData')
