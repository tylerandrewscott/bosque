library(data.table)
library(tidyverse)
library(sf)
start_date = mdy('5/4/2010')
end_date = mdy('7/7/2015')
weekly <- seq(start_date,end_date,by='weeks')
start_date_dec = decimal_date(start_date)

albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
twd_boundaries <- st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile/PWS_Export.shp')
twd_boundaries <- twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries <- st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries <- st_make_valid(twd_boundaries)
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()


tx_restrictions <- readRDS('drought_and_debt/input/combined_restriction_records.RDS')
tx_restrictions <- tx_restrictions[!duplicated(tx_restrictions[,.(`PWS ID`,Priority,Stage,Population,Connections,Notified)]),]
tx_restrictions$PWS_ID = paste0('TX',tx_restrictions$`PWS ID`)
tx_restrictions[,`PWS ID`:=NULL]
tx_restrictions = tx_restrictions[!is.na(tx_restrictions$PWS_ID),]
tx_restrictions$Restriction = NA
tx_restrictions$Restriction <- tx_restrictions$Mandatory
tx_restrictions$Date_Notified  = ymd(tx_restrictions$Notified)
tx_rest = tx_restrictions
tx_rest = tx_rest[!duplicated(paste(tx_rest$PWS_ID,tx_rest$Date_Notified,tx_rest$STAGE)),]
#tx_rest = tx_rest[tx_rest$Restriction==1,]
tx_rest = tx_rest[tx_rest$Date_Notified>=start_date,]
tx_rest = tx_rest %>% arrange(PWS_ID,Date_Notified)# %>% filter(!duplicated(PWS_ID))
tx_rest = tx_rest[!is.na(tx_rest$PWS_ID),]
tx_rest = tx_rest[tx_rest$Date_Notified < end_date,]
tx_rest$Restriction_Time = decimal_date(tx_rest$Date_Notified) - start_date_dec


drought_dt <- fread('drought_and_debt/input/MR_drought_scores.csv')
drought_dt <- drought_dt[order(Week),]
drought_dt$Week <- ymd(drought_dt$Week)
drought_dt <- drought_dt[Week>=start_date,]
tx_rest <- tx_rest[,.(PWS_ID,Restriction,Date_Notified,Restriction_Time,Priority_Numeric)]
tx_rest$join_time <- tx_rest$Restriction_Time
drought_dt$join_time <- decimal_date(drought_dt$Week) - start_date_dec
setkey(tx_rest,'PWS_ID','join_time')
setkey(drought_dt,'PWS_ID','join_time')
#### drop things we don't have boundaries for
tx_rest <- tx_rest[tx_rest$PWS_ID %in% twd_boundaries$PWS_ID,]

tx_cox <- tx_rest[drought_dt,roll=T]
tx_cox$Restriction[is.na(tx_cox$Restriction)]<-0
tx_cox[order(PWS_ID,join_time),week_prior:=lag(join_time),by=.(PWS_ID)]
#tx_cox <- tx_cox[!is.na(week_prior),]


demos <- readRDS('drought_and_debt/input/pws_demos_MR.RDS')
tx_cox <- left_join(tx_cox,demos)

population <- readRDS('drought_and_debt/input/pws_population.RDS')
setnames(population,'# I/Cw/other PWS','Interconnections')
population$Interconnections<-as.numeric(population$Interconnections)
inters <- population[,sum(Interconnections),by=.(PWS_ID)]
setnames(inters,'V1','Interconnections')
tx_cox <- left_join(tx_cox,inters)


population$PopulationServed <- as.numeric(population$PopulationServed)
pop <- dcast(population[,.(PopulationType,PopulationServed,PWS_ID)],
      PWS_ID ~ PopulationType,value.var = 'PopulationServed')
pop$Wholesale[is.na(pop$Wholesale)]<-0
pop$Total <- ifelse(is.na(pop$Total),pop$Residential + pop$Wholesale,pop$Total)

pop_df <- pop[,list(max(Total),100 * Residential/Total),by=.(PWS_ID)]

setnames(pop_df,c('V1','V2'),c('Population_Served','Perc_Residential'))

tx_cox <- left_join(tx_cox,pop_df)

storage <- readRDS('drought_and_debt/input/pws_storage.RDS')
storage$unit <- str_extract(storage$`TotalStorage(MG)`,'[A-Z]{1,}')
storage$number <- as.numeric(str_extract(storage$`TotalStorage(MG)`,'[0-9\\.\\,]{2,}'))
storage$Storage_MG <- ifelse(storage$unit=='GAL',storage$number/1e6,storage$number)
storage$consumption_units <- str_extract(storage$AverageDailyConsump.,'[A-Z]{1,}')
storage$consumption_number <- as.numeric(str_extract(storage$AverageDailyConsump.,'[0-9\\.\\,]{2,}'))
storage$Avg_Daily_Consumption <- ifelse(storage$consumption_units!='MGD',storage$consumption_number/1e6,
                                        storage$consumption_number)
storage$Avg_Daily_Consumption<- storage$Avg_Daily_Consumption*1e6

tx_cox <- left_join(tx_cox,storage[,.(PWS_ID,Avg_Daily_Consumption,Storage_MG)])

pwsd <- fread('input/SDWA_latest_downloads/SDWA_PUB_WATER_SYSTEMS.csv')
pwsd <- pwsd[,.(PWSID,GW_SW_CODE,SERVICE_CONNECTIONS_COUNT,PWS_TYPE_CODE)]
pwsd$Groundwater <- (pwsd$GW_SW_CODE=='GW') + 0
setnames(pwsd,'PWSID','PWS_ID')
tx_cox <- left_join(tx_cox,pwsd[,.(PWS_ID,Groundwater,SERVICE_CONNECTIONS_COUNT,PWS_TYPE_CODE)])
tx_cox$Groundwater[tx_cox$PWS_ID=='TX1290056'] <- 0

owners <- readRDS('input/texas_dww/pws_details_2023-03-21.RDS')
owners$PWS_ID <- owners$PWS_ID_EX
owners <- data.table(owners)[,.(PWS_ID,Owner_Type)]
tx_cox <- left_join(tx_cox,owners)

tx_cox$Avg_Daily_Consumption_1kPeople <- tx_cox$Avg_Daily_Consumption/{tx_cox$Population_Served/1e3}
#tx_cox[,Priority_Numeric:=NULL]
tx_cox$SERVICE_CONNECTIONS_COUNT[tx_cox$PWS_ID=='TX1290056']<-1801
tx_cox$Mandatory_Plan_Filer <- (tx_cox$SERVICE_CONNECTIONS_COUNT>=3300)+0

library(survival)

tx_cox[order(join_time),restriction_sum:=cumsum(Restriction),by=.(PWS_ID)]
tx_cox <- tx_cox[restriction_sum<=1,]

tx_cox <- tx_cox[Owner_Type %in% c('Municipality','Private','Water Supply Corporation','District','Investor Owned'),]
tx_cox$Restriction_Time[tx_cox$Restriction==0] <- tx_cox$join_time[tx_cox$Restriction==0]

#### https://casetext.com/regulation/texas-administrative-code/title-30-environmental-quality/part-1-texas-commission-on-environmental-quality/chapter-290-public-drinking-water/subchapter-d-rules-and-regulations-for-public-water-systems/section-29045-minimum-water-system-capacity-requirements
### texas code specifies a CWS generally must have minimum of 200G per connection storage
### so for missing storage (not all systems bother to log this in TCEQ system apparently)
### we impute 200G x # connections
### these values are generally supported by testing observed storage values
summary((tx_cox$Storage_MG*1e6)/tx_cox$SERVICE_CONNECTIONS_COUNT)
tx_cox$Storage_MG[is.na(tx_cox$Storage_MG)] <- (tx_cox$SERVICE_CONNECTIONS_COUNT[is.na(tx_cox$Storage_MG)] * 200)/1e6
tx_cox$Total_Storage_MG_1kPeople <- tx_cox$Storage_MG / {tx_cox$Population_Served/1e3}

tx_cox[is.na(Groundwater),]
#### daily consumption also not well reported (~30% missing)
#### but, from observables, we know that: 
# (1) daily consumption and service connections are highly correlated, particularly when
# factoring in the % of residential customers (i.e., residential service connections)
cor(tx_cox$Avg_Daily_Consumption,tx_cox$SERVICE_CONNECTIONS_COUNT*tx_cox$Perc_Residential,use = 'pairwise.complete.obs')
# (2) daily consumption PER 1k PEOPLE is basically completely uncorrelated with residential service connections
cor(tx_cox$Avg_Daily_Consumption_1kPeople,tx_cox$SERVICE_CONNECTIONS_COUNT*tx_cox$Perc_Residential,use = 'pairwise.complete.obs')
# so, we can impute this value pretty well by fitting a simple linear model that predicts
# daily consumption using residential service connections, be pretty confident that we aren't
# biased for high or low use areas (i.e., we might be _wrong_, but at random)
tx_cox$residential_connections <- tx_cox$SERVICE_CONNECTIONS_COUNT*tx_cox$Perc_Residential
simple_consumption <- lm(Avg_Daily_Consumption ~ residential_connections,tx_cox[!duplicated(PWS_ID),])
pdata <- data.table(residential_connections = {tx_cox$SERVICE_CONNECTIONS_COUNT*tx_cox$Perc_Residential}[is.na(tx_cox$Avg_Daily_Consumption)])
tx_cox$Avg_Daily_Consumption[is.na(tx_cox$Avg_Daily_Consumption)]<-as.numeric(predict.lm(simple_consumption,pdata))
index <- is.na(tx_cox$Avg_Daily_Consumption_1kPeople)
tx_cox$Avg_Daily_Consumption_1kPeople[index] <- tx_cox$Avg_Daily_Consumption[index]/{tx_cox$Population_Served[index]/1e3}
### double check new correlation -- a little larger, but still negligible
cor(tx_cox$Avg_Daily_Consumption_1kPeople,tx_cox$SERVICE_CONNECTIONS_COUNT*tx_cox$Perc_Residential,use = 'pairwise.complete.obs')


tx_cox$Municipality <- (tx_cox$Owner_Type=='Municipality') + 0
filers <- unique(tx_cox[(Restriction==0&!is.na(Date_Notified))|Restriction==1,]$PWS_ID)

tx_cox$filers <- (tx_cox$PWS_ID %in% filers) + 0

cox_MR_duplicate <- coxph(Surv(time = Restriction_Time,
                               event = Restriction) ~ DM +
                            log(Med_Household_Income) + Perc_Bachelors +
                            Perc_Hispanic + Perc_Black + Perc_Dem_Vote_Share + 
                            Perc_Rural + Perc_Houses_Since1980 + 
                            Municipality + #Priority_Numeric + 
                            log(Population_Served) + Perc_Residential + 
                            Avg_Daily_Consumption_1kPeople + 
                            Interconnections +
                            Total_Storage_MG_1kPeople +
                            Groundwater ,data = tx_cox[tx_cox$SERVICE_CONNECTIONS_COUNT>=3300 & tx_cox$filers==1,])

stargazer(cox_MR_duplicate,notes = '*Only systems with >3,300 connections + a drought filing with TCEQ',
            type = 'html',out = 'drought_and_debt/output/mullin_rubado_almost_repliation.html',style = 'ajps',
          title = 'Replicating Mullin and Rubado',omit.stat = c('ll','rsq'),)

cox_mod_drought <- coxph(Surv(time = Restriction_Time,
                              event = Restriction) ~ DM +
                        Mandatory_Plan_Filer + Owner_Type,
                        data = tx_cox,cluster = PWS_ID)

cox_mod_covs <- coxph(Surv(time = Restriction_Time,
                           event = Restriction) ~ DM +
                   log(Med_Household_Income) + Perc_Bachelors +
                   Perc_Hispanic + Perc_Black + Perc_Dem_Vote_Share + 
                   Perc_Rural + Perc_Houses_Since1980 + 
                   #Mandatory_Plan_Filer + 
                   Owner_Type + 
                   log(Population_Served) + Perc_Residential + 
                   Avg_Daily_Consumption_1kPeople + 
                   Interconnections +
                   Total_Storage_MG_1kPeople +
                   Groundwater ,data = tx_cox)

id_crosswalk <- readRDS('drought_and_debt/input/id_crosswalk.RDS')
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='8492000'] <- 'TX2490016'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='5952250'] <- 'TX0430053'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='2312250'] <- 'TX0420034'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='7585150'] <- 'TX1900009'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='7492500'] <- 'TX2040033'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='8070000'] <- 'TX2290037'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='7634575'] <- 'TX2360010'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='995951'] <- 'TX1290010'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='2412188'] <- 'TX0940015'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='5846750'] <- 'TX1650133'
id_crosswalk$PWS_ID[id_crosswalk$District_ID=='1636654'] <- 'TX0200706'
tx_cox$District_ID <- id_crosswalk$District_ID[match(tx_cox$PWS_ID,id_crosswalk$PWS_ID)]


audits <- readRDS('drought_and_debt/input/district_audits.RDS')
audits$join_time <- decimal_date(ymd(audits$`FISCAL YEAR ENDED`)) - start_date_dec




#summary(cox_mod_covs)
#tapply(tx_cox$Population_Served,tx_cox$Mandatory_Plan_Filer,summary)





