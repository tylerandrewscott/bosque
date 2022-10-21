library(tidyverse)
library(lubridate)
library(INLA)

#write_csv(sdw_date,'scratch/scratch_file.csv')

co = read_csv('input/census/tx_county_census.csv') %>% arrange(Name,Year4)
library(zoo)
co_full = expand.grid(1965:2016,unique(co$Name)) %>% rename(Year4 = Var1,Name = Var2)
co_full <- right_join(co,co_full)
co_full <- co_full %>% arrange(Name,Year)
co_full = co_full %>% group_by(Name) %>% fill(Population,TotalRevenue)


###### DISTRICT INFO ########
info = read_csv('input/texas_iwdd/infopage_wide.csv') %>% rename(SYSTEM_NAME = X2,ID = `District:`) %>% select(-X3,-X4)
info$STATUS = ifelse(grepl('[A-Z]',info$`Business Phone:`),info$`Business Phone:`,info$`Activity Status:`)
info$DISTRICT_TYPE <- NA
info$DISTRICT_TYPE[is.na(info$`Type:`)] <- info$`Activity Status:`[is.na(info$`Type:`)]
info$DISTRICT_TYPE[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Activity Status:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$DISTRICT_TYPE[is.na(info$DISTRICT_TYPE)] <- info$`Type:`[is.na(info$DISTRICT_TYPE)]
info$STATUS <- NA
info$STATUS[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Business Phone:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$STATUS[is.na(info$STATUS)] <- info$`Activity Status:`[is.na(info$STATUS)] 
info$FORMED_BY <- info$`Registration Received:`
info$BOARD_SELECTION <- NA
info$BOARD_SELECTION <- info$`Number of Directors:`
info$BOARD_SELECTION <- ifelse(info$BOARD_SELECTION ==  "Elected by Precinct",'Elected',info$BOARD_SELECTION)
info$BOARD_MEMBERS <- info$`Way Created:`
info$COUNTY <- NA
info$COUNTY <- info$`Way Chosen:`
info$TAX_RATE <- NA
info$`Actual Date of Notice:` <- gsub('\\$ |,','',info$`Actual Date of Notice:`)
info$TAX_RATE <- as.numeric(info$`Actual Date of Notice:`)
info$LEVY_TAX <- (!is.na(info$TAX_RATE)) + 0
library(stringr)
info$NUM_COUNTIES <- 1 + sapply(ifelse(is.na(info$`Main County:`),list(NULL),str_split(info$`Main County:`,"\\s{2,}")),length)
info$MULTI_COUNTY <- (info$NUM_COUNTIES >1) + 0
temp = read_csv('input/tceq_audits/district_info.csv')



temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='DORMANCY AFFIDAVIT'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='DORMANT'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&is.na(temp$`Activity Reason:`)] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='CONFIRMATION REQUIRED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='CONVERSION'] <- 'ACTIVE'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='ISSUED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Activity Reason:`=='ELECTION CONFIRMED'] <- 'NOT STARTED'
temp$`Activity Status:`[temp$`Activity Status:` == 'INACTIVE'&temp$`Financial Status:`=='DORMANT AFFIDAVIT FILED'] <- 'DORMANT'
temp$`Activity Status:`[temp$`Activity Status:` %in% c('DORMANT','NOT STARTED','INACTIVE','UNKNOWN-NO ACTIVIT')] = 'OUT OF SAMPLE'
temp$SYSTEM_NAME <- info$SYSTEM_NAME[match(temp$SYSTEM_ID,info$ID)]
temp$`District Type:`[grepl('RIVER AUTHORITY',temp$SYSTEM_NAME) & temp$`District Type:` == 'OTHER'] <- 'RIVER AUTHORITY'
event_df <- data.frame(creation = mdy(temp$`Creation Date:`),
                       start = decimal_date(mdy(temp$`Creation Date:`)),
                       event = ifelse(temp$`Activity Status:` == 'ACTIVE',0,1),
                       end = mdy(temp$`Activity Date:`),
                       end2 = mdy(temp$`Dissolved Date:`),
                       id = temp$SYSTEM_ID,
                       status = temp$`Activity Status:`,
                       reason = temp$`Activity Reason:`) 
event_df <- event_df %>% mutate(fail_date = ifelse(event==0,NA,ifelse(!is.na(end2),as.character(end2),as.character(end))))
event_df$creation_year = year(event_df$creation)
event_df$deactivation_year = year(ymd(event_df$fail_date))


#time: for failed districts, age at failure; for active districts, age at 12/31/2016

event_df$end = decimal_date(ymd(event_df$fail_date))
event_df$event = ifelse(is.na(event_df$fail_date),0,1)
event_df$time = ifelse(is.na(event_df$fail_date), decimal_date(mdy('12/31/2016')) - event_df$start,  decimal_date(ymd(event_df$fail_date)) - event_df$start)
event_df$SYSTEM_NAME <- info$SYSTEM_NAME[match(event_df$id,info$ID)]
event_df$COUNTY <- info$COUNTY[match(event_df$id,info$ID)]
event_df$MULTI_COUNTY <- info$MULTI_COUNTY[match(event_df$id,info$ID)]
event_df$NUM_COUNTIES <- info$NUM_COUNTIES[match(event_df$id,info$ID)]
event_df$Acre_Size <- temp$`Acre Size:`[match(event_df$id,temp$SYSTEM_ID)]
event_df$DISTRICT_TYPE <- info$DISTRICT_TYPE[match(event_df$id,info$ID)]
event_df$FORMED_BY <- info$FORMED_BY[match(event_df$id,info$ID)]
event_df$HAS_BOARD <- (info$BOARD_MEMBERS[match(event_df$id,info$ID)]>0) + 0
event_df$BOARD_MEMBERS <- info$BOARD_MEMBERS[match(event_df$id,info$ID)]
event_df$LEVY_TAX <- info$LEVY_TAX[match(event_df$id,info$ID)] 
event_df$TAX_RATE <- info$TAX_RATE[match(event_df$id,info$ID)] 
event_df$Acre_Size[is.na(event_df$Acre_Size)] <- median(event_df$Acre_Size,na.rm=T)
event_df$Log_Acre_Size <- log(event_df$Acre_Size)

event_df$SYSTEM_NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('UTILITY DISTRICT','UD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('COUNTIES','COUNTY',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('WATER SEWER IRRIGATION AND DRAINAGE DIST','WSID',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('-',' ',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub(' $','',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('THE WOODLANDS','WOODLANDS',event_df$SYSTEM_NAME)
#event_df$SYSTEM_NAME = gsub('PEARLAND MUNICIPAL MANAGEMENT DIST 1','PEARLAND MUD 1',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('CLOVERCREEK','CLOVER CREEK',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('POST WOOD','POSTWOOD',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("GALVESTON COUNTY FWSD 6 TIKI ISLAND","GALVESTON COUNTY FWSD 6",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("BRAZORIA COUNTY FWSD 1 DAMON","BRAZORIA COUNTY FWSD 1",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("TATTOR ROAD MUNICIPAL DISTRICT","TATTOR ROAD MUD",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub('OF BRAZORIA COUNTY$','',event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("HARRIS COUNTY FWSD 1 B","HARRIS COUNTY FWSD 1B",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("HARRIS COUNTY FWSD 1 A","HARRIS COUNTY FWSD 1A",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME = gsub("TANGLEWOOD FOREST LIMITED DISTRICT","TANGLEWOOD FOREST MUD",event_df$SYSTEM_NAME)
event_df$SYSTEM_NAME[event_df$SYSTEM_NAME=="HARRIS COUNTY MUD 18"] = "HARRIS COUNTY MUD 18 HEATHERWOOD HUNTERS"
event_df$SYSTEM_NAME[event_df$SYSTEM_NAME=="ABLES SPRINGS SUD OF HUNT, KAUFMAN, AND VAN ZANDT COUNTY"] = "ABLES SPRINGS SUD"
event_df$SYSTEM_NAME <- gsub(' $','',event_df$SYSTEM_NAME)
event_df = event_df %>% rename(DISTRICT_NAME = SYSTEM_NAME)
event_df$TAX_RATE[is.na(event_df$TAX_RATE)]<-0

event_df = event_df %>% filter(!is.na(creation_year),!is.na(COUNTY))


# cyear = expand.grid(1965:2016,unique(event_df$COUNTY)) %>% rename(Year = Var1,COUNTY = Var2)
# library(pbapply)
# cyear$county_districts = pbsapply(1:nrow(cyear), function(i)
# sum(event_df$creation_year[event_df$COUNTY==cyear$COUNTY[i]]<=cyear$Year[i] &
#       (is.na(event_df$deactivation_year[event_df$COUNTY==cyear$COUNTY[i]])|
#          event_df$deactivation_year[event_df$COUNTY==cyear$COUNTY[i]]>cyear$Year[i])))
# cyear$county_muds = pbsapply(1:nrow(cyear), function(i)
#   sum(event_df$DISTRICT_TYPE[event_df$COUNTY==cyear$COUNTY[i]]=='MUNICIPAL UTILITY DISTRICT' &event_df$creation_year[event_df$COUNTY==cyear$COUNTY[i]]<=cyear$Year[i] &
#         (is.na(event_df$deactivation_year[event_df$COUNTY==cyear$COUNTY[i]])|
#            event_df$deactivation_year[event_df$COUNTY==cyear$COUNTY[i]]>cyear$Year[i])))
# cyear$state_districts = pbsapply(1:nrow(cyear), function(i)
#   sum(event_df$creation_year<=cyear$Year[i] &
#         (is.na(event_df$deactivation_year)|
#            event_df$deactivation_year>cyear$Year[i])))
# cyear$state_muds = pbsapply(1:nrow(cyear), function(i)
#   sum(event_df$DISTRICT_TYPE=='MUNICIPAL UTILITY DISTRICT' &event_df$creation_year<=cyear$Year[i] &
#         (is.na(event_df$deactivation_year)|
#            event_df$deactivation_year>cyear$Year[i])))

event_df <- event_df %>% filter(creation_year>=1965) %>% 
  filter(is.na(deactivation_year)|creation_year != deactivation_year) %>%
  filter(creation_year < 2016) %>% filter(is.na(deactivation_year)|deactivation_year<2017)
event_df$DISTRICT_TYPE[event_df$DISTRICT_TYPE %in% c('IRRIGATION DISTRICT','NAVIGATION DISTRICT','WATER IMPROVEMENT DISTRICT','RIVER AUTHORITY',
                                                     'SOIL AND WATER CONSERVATION DISTRIC','STORMWATER CONTROL DISTRICT','REGIONAL DISTRICT')] <- 'OTHER'

event_df <- event_df %>% filter(!status %in% c('OUT OF SAMPLE','TRACKING')) %>% filter(!grepl("NORTHGATE CROSSING ROAD UD",DISTRICT_NAME)) 
event_df$status[event_df$status == 'MERGED/ANNEXED'] <- 'DELETED/DISSOLVED'

event_df1 = event_df %>% filter(status!='OUT OF SAMPLE') %>% group_by(creation_year) %>% summarise(create_by_year = n()) %>% rename(year = creation_year )
event_df2 = event_df %>% filter(status!='OUT OF SAMPLE') %>% group_by(deactivation_year) %>% summarise(delete_by_year = n()) %>% rename(year = deactivation_year)
event_df3 = event_df %>% filter(status=='OUT OF SAMPLE') %>% group_by(creation_year) %>% summarise(create_by_year_out = n()) %>% rename(year = creation_year)
event_df_count = Reduce(full_join,list(event_df1,event_df2,event_df3))
event_df_count = gather(event_df_count,action,count,-year)
event_df_count$count[is.na(event_df_count$count)] <- 0
event_df_count = event_df_count %>% filter(!is.na(year)) %>% filter(!is.na(COUNTY))
event_df$FORMED_BY[event_df$FORMED_BY=='BOARD RESOLUTION TO DIVIDE'] <- 'COUNTY'
event_df = event_df %>% filter(!is.na(COUNTY))


event_df = event_df %>% filter(DISTRICT_TYPE == 'MUNICIPAL UTILITY DISTRICT')

temp = event_df %>% select(creation_year,deactivation_year) %>% gather() %>%
  mutate(key = ifelse(key == 'creation_year','Year created','Year deactivated'))
library(ggthemes)
ggplot(temp,aes(x = value)) + geom_bar()  + facet_wrap(~key,nrow=2) + theme_tufte(ticks=F) +
  theme(axis.title.y=element_text(size=14),axis.title.x = element_blank(),legend.title=element_blank(),strip.text = element_text(size=14),
        axis.text = element_text(size = 14)) + ylab('# districts') +
scale_x_continuous(breaks = seq(1970,2010,10))


gg1 = ggplot(aes(y=count,x=year,fill = action),data=event_df_count) + geom_bar(stat='identity',position ='dodge') +
  #geom_density(stat='identity',aes(y=count,x=year,fill = action),data=temp,position ='dodge') +
  theme_bw() + scale_fill_brewer(name = '',labels=c('# created (in sample)','# dissolved (in sample)'),
                                 type='qual',palette = 1) + theme(legend.position = c(0.2,0.85),axis.text=element_text(size=12),
                                                                  axis.title=element_text(size=12),legend.title=element_blank(),
                                                                  legend.text=element_text(size = 12))  +
  scale_y_continuous(expand=c(0,0),limits = c(0,150),name = '# of districts') + scale_x_continuous(name = 'Year',expand=c(0,0)) 


event_df = event_df %>% mutate(Max_Age_Obs = ifelse(event==1,deactivation_year - creation_year,
                                                    2016 - creation_year))

district_age_df = do.call(rbind,pblapply(1:nrow(event_df),function(i) data.frame(DISTRICT_NAME = event_df$DISTRICT_NAME[i],
                                                                                 AGE = 0:event_df$Max_Age_Obs[i]) %>%
                                           mutate(Year = (0:event_df$Max_Age_Obs[i]) + event_df$creation_year[i])))



# event_df = event_df %>% mutate(creation_period = ifelse(creation_year < 1975,'1965-1974',
#                                                         ifelse(creation_year < 1985,'1975-1984',
#                                                                ifelse(creation_year < 1995,'1985-1994',
#                                                                       ifelse(creation_year < 2005,'1995-2004','2005-2015')))))

library(survival)
library(coxme)
event_df = event_df %>% mutate(status = ifelse(event == 0 | (event==1&Year!=deactivation_year),0,1))
event_df$time0 <- 0











split_df <- survSplit(Surv(time,event)~.,data = event_df,cut = 0:50,
                      episode = 'timegroup')
split_df <- split_df %>% mutate(Year = creation_year + tstart)
split_df = left_join(split_df,district_age_df) 
split_df = left_join(split_df,cyear)
split_df = split_df %>% mutate(time0 = time-1)
last <- split_df$id[which.max(split_df$time)]
intervals <- split_df[split_df$id == last, c("time0", "time", "event")]

split_df = split_df %>% mutate(count_muds_over_total_muds = 100 * county_muds/state_muds,
                               county_districts_over_total_districts = 100 * county_districts/state_districts)
split_df$county_districts = as.vector(scale(split_df$county_districts))
split_df$county_muds = as.vector(scale(split_df$county_muds))


mod1 = coxph(Surv(time, event) ~ HAS_BOARD + LEVY_TAX + LEVY_TAX:TAX_RATE + FORMED_BY + 
               Log_Acre_Size +  cluster(COUNTY), event_df)
mod2 = coxph(Surv(time0,time, event) ~ HAS_BOARD + LEVY_TAX + LEVY_TAX:TAX_RATE + FORMED_BY + 
               Log_Acre_Size +  cluster(id) + 
               county_muds + count_muds_over_total_muds + 
               county_districts +
               county_districts_over_total_districts, data = split_df)

screenreg(list(mod1,mod2))
mod2 = coxph(Surv(time0, time, event) ~ HAS_BOARD + LEVY_TAX + 
               LEVY_TAX:TAX_RATE +       
               FORMED_BY + 
               Log_Acre_Size + county_muds + count_muds_over_total_muds + 
               county_districts +
               county_districts_over_total_districts + COUNTY +
               + cluster(id), split_df)




screenreg(list(test,test2))

coxme(Surv(time0, time, event) ~ HAS_BOARD + LEVY_TAX + FORMED_BY + 
        Log_Acre_Size + county_muds + count_muds_over_total_muds + 
        county_districts +
        county_districts_over_total_districts + 
        cluster(I)
      (1|COUNTY) + (1|creation_year), split_df)

?cluster


model <- coxph(Surv(time0, time, event) ~ HAS_BOARD + LEVY_TAX + county_muds + state_muds,
               data = split_df)




surv_base <- Surv(time = event_df$time,event = event_df$event)
cut.points <- unique(event_df$end[event_df$event == 1])

survSplit(Surv())

survSplit(data = event_df, cut = cut.points, end = "time",
          start = 'creation', event = "event")
?survSplit
event_df[(is.na(event_df$end)+is.na(event_df$end2) == 1),]

sob = Surv(time = event_df$Year, time2 = event_df$Year+1,event =  event_df$status)
library(coxinterval)
fit <- coxdual(sob ~ cluster(id) + )

coxph(sob ~ 1 + cluster(DISTRICT_NAME),data = event_df)


head(event_df,100)



test = coxme::coxme(formula = sob ~ 1 + Log_Acre_Size + HAS_BOARD + 
                      MULTI_COUNTY + LEVY_TAX + LEVY_TAX:TAX_RATE + 
                      county_muds + state_muds +  county_districts + state_districts + 
                      (1|COUNTY),data = event_df)

table(is.na(event_df$Log_Acre_Size))


FORMED_BY + (1|COUNTY) +
  (1|creation_period),data = event_df)



sob <- Surv(time = event_df$time,event = event_df$event)


event_df$time

table(event_df$MULTI_COUNTY[!duplicated(event_df$DISTRICT_NAME)])



save.image('scratch/survival_results.RData')
