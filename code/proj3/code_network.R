library(tidyverse)

#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(RSelenium)
dww_data =  read_csv('input/texas_dww/texas_master_pws.csv')  %>%
  select(-X1,-Status) %>%
  mutate(Name = gsub(' Fact.*','',Name)) %>% rename(SYSTEM_NAME = Name)
search_ids = read_csv('input/tceq_audits/district_info.csv') %>%
  rename(SYSTEM_NAME = NAME)

temp = left_join(dww_data,search_ids)

district_reports = read_csv('code/upwork/WIDE (1).csv') %>% 
  rename(SYSTEM_ID = `District:`,SYSTEM_NAME = X2)
district_reports$HOW_CHOSEN = district_reports$`Number of Directors:`
district_reports$ACTIVITY = ifelse(grepl('[0-9]',district_reports$`Business Phone:`),district_reports$`Activity Status:`, district_reports$`Business Phone:` )
district_reports$TYPE = ifelse(grepl('[0-9]',district_reports$`Business Phone:`),district_reports$`Type:`,  district_reports$`Activity Status:` )
district_reports = district_reports %>%
  select(-`Activity Status:`,-`Financial Status:`)




temp = left_join(temp,district_reports)
temp = temp %>% filter(ACTIVITY == 'ACTIVE')

table(temp$Primary_Source_Type)

functions_df = read_csv('input/tceq_audits/district_functions.csv')
table(is.na(functions_df$SUPPLY.TREATED.OR.RETAIL.WATER),is.na(functions_df$RETAIL.WASTEWATER))

functions_df$

table(is.na(functions_df$RETAIL.WASTEWATER))


temp = left_join(temp,functions_df %>% dplyr::select(-GROUNDWATER))

temp = temp %>% mutate(TAX_AUTHORITY = ifelse(is.na(temp$TAX.BOND.AUTHORITY),0,1))
temp$WASTEWATER = ifelse(!is.na(temp$RETAIL.WASTEWATER),1,0)
temp$ROADS = ifelse(!is.na(temp$ROAD.POWERS),1,0)
temp$ROADS_PLUS_WASTEWATER = temp$WASTEWATER + temp$ROADS
temp = temp %>% filter(!is.na(SUPPLY.TREATED.OR.RETAIL.WATER))

temp = left_join(search_ids,temp %>% dplyr::select(-`Financial Status:`,-`Activity Status:`))
temp = temp %>% 
  filter(`Primary County:` %in% c('HARRIS','FORT BEND','BRAZORIA','MONTGOMERY','LIBERTY','WALLER','CHAMBERS','AUSTIN','GALVESTON')) 

temp = temp %>% filter(TYPE %in% c("SPECIAL UTILITY DISTRICT","WATER CONTROL AND IMPROVEMENT DISTR","MUNICIPAL UTILITY DISTRICT",
                                   "FRESH WATER SUPPLY DISTRICT")) %>% 
  mutate(TAX.BOND.AUTHORITY = ifelse(!is.na(TAX.BOND.AUTHORITY),'TAX.BOND','NO.TAX')) %>%
  mutate(RETAIL.WASTEWATER = ifelse(!is.na(RETAIL.WASTEWATER),'WASTEWATER','NO.WASTEWATER'))

library(knitr)


test = read_csv('input/texas_dww/texas_master_pws.csv') %>% select(PWS_ID,Name)

read_csv('input/texas_dww/')

officials = left_join(read_csv('input/texas_dww/personnel_records.csv')  %>% rename(PWS_ID = System),
                      read_csv('input/texas_dww/texas_master_pws.csv') %>% select(PWS_ID,Name) %>%
                        mutate(Name = gsub(' Fact.*','',Name)) %>% rename(PWS_NAME = Name))
#officials = officials %>% filter(PWS_NAME %in% temp$NAME)
off_mat = as.matrix(table(officials$PWS_ID,officials$NAME))
off_df = read_csv('code/upwork/LONG (1).csv',col_names = FALSE) %>%
  mutate(X3 = gsub('\\,.*','',X3)) %>% rename(SYSTEM_ID = X1) %>%
  filter(SYSTEM_ID %in% temp$SYSTEM_ID)
officials$PWS_NAME

grep('HOUSTON',officials$PWS_NAME,value=T)
officials[grepl('SREERAMA',officials$NAME),]
officials[!is.na(officials$PWS_NAME)&officials$PWS_NAME=='CITY OF HOUSTON',] %>% dplyr::select(NAME)





library(statnet)
aj.mat = as.matrix(table(off_df$SYSTEM_ID,off_df$X3))
aj.socio = tcrossprod(aj.mat)
off_net = as.network(aj.socio,ignore.eval = F,names.eval = 'Overlap',directed=F)


off_net  %v%  'water_source' <- temp$Primary_Source_Type[match(network.vertex.names(off_net),temp$SYSTEM_ID)]
off_net  %v%  'wastewater' <- temp$RETAIL.WASTEWATER[match(network.vertex.names(off_net),temp$SYSTEM_ID)]


sales = full_join(read_csv('input/texas_dww/purchasing_connections.csv'),
read_csv('input/texas_dww/sales_connections.csv')) %>% filter(Sale_Type == 'P')






dim(read_csv('input/texas_dww/purchasing_connections.csv'))
dim(read_csv('input/texas_dww/sales_connections.csv'))





network.vertex.names(off_net)

head(temp)
temp$GROUNDWATER

off_net %vattr% attrname <- value



table(aj.socio[upper.tri(aj.socio)]>0)

off_graph = igraph::graph_from_adjacency_matrix(aj.socio,mode = 'undirected')
off_pagerank = igraph::page.rank(off_graph)

bridging_scores = influenceR::bridging(off_graph)

bridging_df = data.frame(SYSTEM_ID = as.character(names(bridging_scores)),BRIDGING_SCORE = c(bridging_scores))

temp = left_join(temp %>% mutate(SYSTEM_ID = as.character(SYSTEM_ID)),bridging_df)

names = read_csv('input/texas_dww/texas_master_pws.csv') %>% 
  mutate(Name = gsub(' Fact.*','',Name)) %>% rename(PWS_NAME = Name)

names$SYSTEM_ID = twd_geo@data$DISTRICT_I[match(names$PWS_NAME,twd_geo@data$NAME)]


temp = temp %>% filter(SYSTEM_ID %in% names$SYSTEM_ID)
table(temp$HOW_CHOSEN,temp$TAX.BOND.AUTHORITY,temp$TYPE)
table(temp$TAX.BOND.AUTHORITY,temp$TYPE)
table(temp$HOW_CHOSEN)





table(is.na(temp$SUPPLY.TREATED.OR.RETAIL.WATER))


temp$NAME[!temp$NAME %in% names$PWS_NAME]







library(lubridate)



#### MODEL SETUP ###
#master_df = read.csv('input/scratch/ready_model.csv')# %>% filter(DOC_ID != 256506)
model_df = read_csv('input/scratch/master_data.csv') %>% filter(TOTAL_REVENUE_LAG1 < 28000000) %>%
  filter(!is.na(TOTAL_REVENUE_LAG1)) %>% filter(SERVICE_CONNECTIONS_LAG1!=0) %>%
  filter(month(`FISCAL YEAR ENDED`) == MONTH) %>% filter(DISTRICT_AGE>0) %>% 
  filter(TOTAL_REVENUE_LAG1 != 0) %>%filter(FYEAR >= 2008) %>%
  mutate(LOG_SERVICE_CONNECTIONS = log(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`))
colnames(model_df)[1] = 'PWS_ID'

temp = read_csv('code/upwork/WIDE (1).csv') %>% 
  rename(SYSTEM_ID = `District:`,SYSTEM_NAME = X2)%>% filter(!is.na(HOW_CHOSEN))
temp$HOW_CHOSEN = temp$`Number of Directors:`
temp$ACTIVITY = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Activity Status:`, temp$`Business Phone:` )
temp$TYPE = ifelse(grepl('[0-9]',temp$`Business Phone:`),temp$`Type:`,  temp$`Activity Status:` )
temp$ELECTED_BOARD = ifelse(temp$HOW_CHOSEN%in% c('Elected',"Elected by Precinct"),1,0)
temp = temp %>% filter(ACTIVITY=='ACTIVE') 

temp = full_join(model_df %>% arrange(-YEAR) %>% filter(!duplicated(PWS_ID)),temp %>% dplyr::select(-`Activity Status:`))

functions_df = read_csv('input/tceq_audits/district_functions.csv')
temp = left_join(temp,functions_df %>% dplyr::select(-GROUNDWATER))
temp = temp %>% mutate(TAX_AUTHORITY = ifelse(is.na(temp$TAX.BOND.AUTHORITY),0,1))
temp$WASTEWATER = ifelse(!is.na(temp$RETAIL.WASTEWATER),1,0)
temp$ROADS = ifelse(!is.na(temp$ROAD.POWERS),1,0)
temp$ROADS_PLUS_WASTEWATER = temp$WASTEWATER + temp$ROADS






