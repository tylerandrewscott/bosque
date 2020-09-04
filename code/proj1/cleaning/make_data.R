library(tidyverse)
library(lubridate)
library(stringr)

mud_df = read_csv('Input/tceq_audits/mud_master.csv')

mud_df = dcast(melt(mud_df, id.vars = "X1"), variable ~ X1) %>% 
  mutate(PWS_NAME = gsub(' \\(.*','',gsub('District Name: ','',variable)))

tex_dww_df = read_csv('Input/texas_dww/texas_master_pws.csv')
dww_name = gsub(' Fact  Sheet  Summary Sheet','',tex_dww_df$Name)
dww_muds = grep('MUD',dww_name,value=T)

grep("HARRIS",mud_df$PWS_NAME,value=T)





mud_df$PWS_NAME[!mud_df$PWS_NAME %in% dww_muds]

grep('WALLER',dww_muds,value=T)

nms = grep('MUD',system_df$`PWS Name`,value=T)


mud_df[is.na(mud_df$`CR Regulated Entity Number:`)&mud_df$`Activity Status:`=='ACTIVE',]

mud_df[c(50,195),]
tex_dww_df$PWS_ID

nms[496]
grep('WILLIAMSON',mud_df$PWS_NAME,value=T)




tex_wdd_df = read_csv('Input/tceq_audits/system_df.csv')




system_df = read_csv('Input/epa_sdwis/water_system_detail.csv',na = c("-")) %>% rename(PWS_Type = `PWS Type`,City = `City Name`) %>%
filter(PWS_Type == 'Community water system'&!`Owner Type` %in% c('State government','Federal government','Private')) %>%
  mutate(First_Report_Date = dmy(`First Reported Date`),Year_Start = year(First_Report_Date),
         City = str_to_title(City)) %>%
  dplyr::select(-`EPA Region`,-PWS_Type,-`Primacy Agency`,-`First Reported Date`) 
  

muds = system_df$`PWS Name`[grepl('MUD',system_df$`PWS Name`)]


table(mud_df$Name %in% system_df$`PWS Name`)
table(system_df$`PWS Name` %in% mud_df$Name)
table(grepl('MUD',system_df$`PWS Name`))

system_df$`PWS Name`[grepl('MUD',system_df$`PWS Name`) & !(system_df$`PWS Name` %in% mud_df$Name)]



viols_df = read_csv('Input/epa_sdwis/violation_report.csv',na=c("-")) %>% filter(`PWS ID` %in% system_df$`PWS ID`) %>% filter(!duplicated(.)) %>% select(-`EPA Region`,-`Primacy Agency`,-`Primacy Type`,-`Deactivation Date`,-`Activity Status`,-`Population Served Count`,-`Primary Source`,-`PWS Type`) %>%
  mutate(Period_Begin = dmy(`Compliance Period Begin Date`),Period_End = dmy(`Compliance Period End Date`),Period_Begin_ddate = decimal_date(Period_Begin),Begin_Month = month(Period_Begin),Begin_Year = year(Period_Begin)) %>% 
  select(-`Compliance Period Begin Date`,-`Compliance Period End Date`) %>% filter(year(Period_Begin)>=2002 & year(Period_Begin)<=2014) %>%
  mutate(Period = ifelse(Begin_Month <=3,1,ifelse(Begin_Month >3 & Begin_Month <=6, 2,ifelse(Begin_Month > 6 & Begin_Month <=9,3,4)))) %>%
  mutate(Period_Factor = paste(Begin_Year,Period,sep='_'))

temp = viols_df %>% filter(Begin_Year >=2005) %>% group_by(Period_Factor,`Is Health Based`) %>% summarise(vcount = n())

temp2 = viols_df %>% filter(Begin_Year >=2005) %>% group_by(Period_Factor,`Is Health Based`) %>% summarise(unique_violators = length(unique(`PWS ID`)))

temp = left_join(temp,temp2) %>%gather(type,vc,-`Is Health Based`,-Period_Factor)



viols_df$Points = NA
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`=='Nitrate') | (viols_df$`Violation Code` %in% c('21','43','44','41','13'))] = 10
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`!='Nitrate') | 
                  (viols_df$`Violation Code` %in% c('2','11','12','22','23','24','37','40','42','46','47','57','58','59','63','64','65'))|
                  (viols_df$`Violation Code`=='03' & viols_df$`Contaminant Name`=='Nitrate')] = 5
viols_df$Points[is.na(viols_df$Points)] = 1


viol_grid = expand.grid(system_df$`PWS ID`,seq(2005,2016,0.25)) %>% rename(PWS_ID = Var1, Quarter = Var2)

viols_df$RTC_Date_Dec = decimal_date(dmy(viols_df$`RTC Date`))
viols_df$RTC_Date_Dec[is.na(viols_df$RTC_Date_Dec)] = 3000  

viol_ett_sum =  do.call(rbind,pblapply(1:nrow(viol_grid),function(i) viols_df %>% filter(`PWS ID` == viol_grid$PWS_ID[i]) %>% 
                                        filter(Period_Begin_ddate<viol_grid$Quarter[i]) %>% 
                                        filter((viol_grid$Quarter[i]+0.25)<RTC_Date_Dec) %>%
           mutate(Quarter_Dec = viol_grid$Quarter[i]) %>% group_by(Quarter_Dec,`PWS ID`) %>% summarise(ETT_Score = sum(Points,na.rm=T))))


viol_ett_length = do.call(rbind,pblapply(1:nrow(viol_grid),function(i) viols_df %>% 
                                           #only viols for PWS
                                           filter(`PWS ID` == viol_grid$PWS_ID[i]) %>% 
                                           # viol happened before end of quarter
                                           filter(Period_Begin_ddate < (viol_grid$Quarter[i] + 0.25)) %>%
                                           # viol not RTC by end of quarter
                                           filter(RTC_Date_Dec > (viol_grid$Quarter[i] + 0.25)) %>%
                                            group_by(`PWS ID`) %>%
                                           # max distance between end of quarter and violation start
                                           summarise(longest_viol = max((viol_grid$Quarter[i]+0.25) - Period_Begin_ddate)) %>% 
                                           mutate(longest_viol_trunc = trunc(longest_viol),Quarter_Dec = viol_grid$Quarter[i])))


viol_ett_length$longest_viol_trunc[viol_ett_length$longest_viol_trunc>5] = 5
viol_ett = full_join(viol_ett_sum,viol_ett_length) %>% mutate(ett_score = ETT_Score + longest_viol_trunc)
viol_ett$ett_score[is.na(viol_ett$ett_score)] = 0


library(ggthemes)
library(viridis)
library(scales)
ggplot(viol_ett) + geom_jitter(aes(x=Quarter_Dec,y=ett_score,colour=(ett_score>10)),height=0,pch=1) + 
  scale_color_colorblind(labels=c('Below enforcement threshold','Above enforcement targeting threshold')) +
  theme_tufte(ticks=F) + scale_x_continuous(name = 'Quarter',expand=c(0,0),breaks=c(2006:2015)) + scale_y_continuous('Enforcement Targeting Tool Score',expand=c(0,0)) + 
  theme(axis.text = element_text(size=16),axis.title=element_text(size=18),legend.text = element_text(size=18),legend.title=element_blank(),
        legend.position = c(0.3,0.8))


kable(table(viol_ett$Quarter_Dec,viol_ett$ett_score>10))


grep(' MUD | MUD$',system_df$`PWS Name`,value=T)

#   geom_point(aes(x=Period_Factor,y=vc,colour=paste(`Is Health Based`,type)),pch=19) + theme_tufte(ticks=T) + 
#  scale_x_discrete(breaks=c(paste(2005:2015,'1',sep='_')),labels=c(paste(2005:2015,'Q1',sep=' '))) +
#   scale_y_continuous(name='# by quarter',limits=c(0,1500)) + 
#   scale_color_manual(labels=c('Unique management violaters','Total management violations','Unique health violators','Total health violations'),values=c(col1,col1,col2,col2)) +
#   scale_linetype_manual(labels=c('Unique management violaters','Total management violations','Unique health violators','Total health violations'),values=c(3,1,3,1)) +
#   theme(legend.title=element_blank(),legend.text=element_text(size=16),axis.text=element_text(size=16),axis.title.y=element_text(size=18),axis.title.x=element_blank(),legend.position = c(0.8,0.75))



# col1 = colorblind_pal()(8)[1]
# col2 = colorblind_pal()(8)[2]
# ggplot(temp) + geom_path(aes(x=Period_Factor,y=vc,colour=paste(`Is Health Based`,type),linetype=paste(`Is Health Based`,type),group=paste(`Is Health Based`,type)),lwd=1) + 
#   geom_point(aes(x=Period_Factor,y=vc,colour=paste(`Is Health Based`,type)),pch=19) + theme_tufte(ticks=T) + 
#  scale_x_discrete(breaks=c(paste(2005:2015,'1',sep='_')),labels=c(paste(2005:2015,'Q1',sep=' '))) +
#   scale_y_continuous(name='# by quarter',limits=c(0,1500)) + 
#   scale_color_manual(labels=c('Unique management violaters','Total management violations','Unique health violators','Total health violations'),values=c(col1,col1,col2,col2)) +
#   scale_linetype_manual(labels=c('Unique management violaters','Total management violations','Unique health violators','Total health violations'),values=c(3,1,3,1)) +
#   theme(legend.title=element_blank(),legend.text=element_text(size=16),axis.text=element_text(size=16),axis.title.y=element_text(size=18),axis.title.x=element_blank(),legend.position = c(0.8,0.75))


system_quarter = expand.grid(unique(system_df$`PWS ID`),seq(2005,2016,0.25)) %>% rename(PWS_ID = Var1, Quarter = Var2)

library(pbapply)      



viols_df = read_csv('Input/epa_sdwis/violation_report.csv',na=c("-")) %>% filter(`PWS ID` %in% system_df$`PWS ID`) %>% filter(!duplicated(.)) %>% select(-`EPA Region`,-`Primacy Agency`,-`Primacy Type`,-`Deactivation Date`,-`Activity Status`,-`Population Served Count`,-`Primary Source`,-`PWS Type`) %>%
  mutate(Period_Begin = dmy(`Compliance Period Begin Date`),Period_End = dmy(`Compliance Period End Date`),Period_Begin_ddate = decimal_date(Period_Begin),Begin_Month = month(Period_Begin),Begin_Year = year(Period_Begin)) %>% 
  select(-`Compliance Period Begin Date`,-`Compliance Period End Date`) %>% filter(year(Period_Begin)>=2002 & year(Period_Begin)<=2014) %>%
  mutate(Period = ifelse(Begin_Month <=3,1,ifelse(Begin_Month >3 & Begin_Month <=6, 2,ifelse(Begin_Month > 6 & Begin_Month <=9,3,4)))) %>%
  mutate(Period_Factor = paste(Begin_Year,Period,sep='_'))

temp = viols_df %>% filter(Begin_Year >=2005) %>% group_by(Period_Factor,`Is Health Based`) %>% summarise(vcount = n())

temp2 = viols_df %>% filter(Begin_Year >=2005) %>% group_by(Period_Factor,`Is Health Based`) %>% summarise(unique_violators = length(unique(`PWS ID`)))

temp = left_join(temp,temp2) %>%gather(type,vc,-`Is Health Based`,-Period_Factor)



viols_df$Points = NA
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`=='Nitrate') | (viols_df$`Violation Code` %in% c('21','43','44','41','13'))] = 10
viols_df$Points[(viols_df$`Violation Code`=='01' & viols_df$`Contaminant Name`!='Nitrate') | 
                  (viols_df$`Violation Code` %in% c('2','11','12','22','23','24','37','40','42','46','47','57','58','59','63','64','65'))|
                  (viols_df$`Violation Code`=='03' & viols_df$`Contaminant Name`=='Nitrate')] = 5
viols_df$Points[is.na(viols_df$Points)] = 1


viol_grid = expand.grid(system_df$`PWS ID`,seq(2005,2016,0.25)) %>% rename(PWS_ID = Var1, Quarter = Var2)

viols_df$RTC_Date_Dec = decimal_date(dmy(viols_df$`RTC Date`))
viols_df$RTC_Date_Dec[is.na(viols_df$RTC_Date_Dec)] = 3000  

viol_ett_sum =  do.call(rbind,pblapply(1:nrow(viol_grid),function(i) viols_df %>% filter(`PWS ID` == viol_grid$PWS_ID[i]) %>% 
                                         filter(Period_Begin_ddate<viol_grid$Quarter[i]) %>% 
                                         filter((viol_grid$Quarter[i]+0.25)<RTC_Date_Dec) %>%
                                         mutate(Quarter_Dec = viol_grid$Quarter[i]) %>% group_by(Quarter_Dec,`PWS ID`) %>% summarise(ETT_Score = sum(Points,na.rm=T))))


viol_ett_length = do.call(rbind,pblapply(1:nrow(viol_grid),function(i) viols_df %>% 
                                           #only viols for PWS
                                           filter(`PWS ID` == viol_grid$PWS_ID[i]) %>% 
                                           # viol happened before end of quarter
                                           filter(Period_Begin_ddate < (viol_grid$Quarter[i] + 0.25)) %>%
                                           # viol not RTC by end of quarter
                                           filter(RTC_Date_Dec > (viol_grid$Quarter[i] + 0.25)) %>%
                                           group_by(`PWS ID`) %>%
                                           # max distance between end of quarter and violation start
                                           summarise(longest_viol = max((viol_grid$Quarter[i]+0.25) - Period_Begin_ddate)) %>% 
                                           mutate(longest_viol_trunc = trunc(longest_viol),Quarter_Dec = viol_grid$Quarter[i])))


viol_ett_length$longest_viol_trunc[viol_ett_length$longest_viol_trunc>5] = 5
viol_ett = full_join(viol_ett_sum,viol_ett_length) %>% mutate(ett_score = ETT_Score + longest_viol_trunc)
viol_ett$ett_score[is.na(viol_ett$ett_score)] = 0


viol_p3count = do.call(rbind,pblapply(1:nrow(system_quarter), function(i) viols_df %>% filter(Period_Begin_ddate<system_quarter$Quarter[i] & `PWS ID` == system_quarter$PWS_ID[i] &
                                                                                                          Period_Begin_ddate>=(system_quarter$Quarter[i]-3)) %>% 
                         group_by(`Is Health Based`) %>% summarise(p3_vcount = n()) %>%
                         mutate(PWS_ID = system_quarter$PWS_ID[i],Quarter = system_quarter$Quarter[i]))) %>%
  mutate(PWS_ID = as.character(PWS_ID)) %>% spread(`Is Health Based`,p3_vcount) %>% complete(PWS_ID,Quarter,fill=list(N = 0,Y = 0))

system_quarter = left_join(system_quarter,viol_p3count) %>% complete(PWS_ID,Quarter,fill=list(N = 0,Y = 0))

sysdf = left_join(system_quarter,viol_p3count) %>% complete(PWS_ID,Quarter,fill=list(N = 0,Y = 0))
sysdf$Q = as.factor(ifelse(gsub('^[0-9]{4}','',as.character(sysdf$Quarter)) == '','1',gsub('^[0-9]{4}','',as.character(sysdf$Quarter))))
sysdf$Year = gsub('\\..*','',sysdf$Quarter)


ggplot(sysdf[sysdf$Q==1&sysdf$Year<2016,],aes(x=Quarter,y=Y)) + geom_jitter(height=F,pch=1) +
  theme_tufte(ticks=F) +
  scale_x_continuous(name = '',breaks=c(2005:2015),
                   labels=paste(2005:2015,'Q1',sep=' '),expand=c(0,0))+
  scale_y_continuous(name='# health violations in prior 3 years') +
  theme(axis.title.x=element_blank(),axis.title.y= element_text(size=18),axis.text=element_text(size=16))


system_type = rep(NA,nrow(system_df))
system_type[grepl('WCID',system_df$`PWS Name`)] = 'WCID'
system_type[grepl('CITY|TOWN',system_df$`PWS Name`)] = 'CITY'
system_type[grepl('MWA',system_df$`PWS Name`)] = 'MWA'
system_type[grepl('PUD$| PUD ',system_df$`PWS Name`)] = 'PUD'
system_type[grepl('MWD',system_df$`PWS Name`)] = 'MWD'
system_type[grepl(' MUD | MUD$',system_df$`PWS Name`)] = 'MUD'
system_type[grepl('WSC',system_df$`PWS Name`)] = 'WSC'
system_type[grepl('SUD$| SUD ',system_df$`PWS Name`)] = 'SUD'
system_type[grepl('FWSD',system_df$`PWS Name`)] = 'FWSD'
system_type[grepl('MUNICIPAL WATER SYSTEM',system_df$`PWS Name`)] = 'MWS'
system_type[grepl('REGIONAL WATER',system_df$`PWS Name`)] = 'RWA'
system_type[grepl('UTILITY DISTRICT|UTILITIES DISTRICT',system_df$`PWS Name`)] = 'PUD'
system_type[grepl('IMPROVEMENT DISTRICT',system_df$`PWS Name`)] = 'ID'
system_type[grepl('WATER SYSTEM$',system_df$`PWS Name`)] = 'MWS'

#test = read_csv('Input/tceq_audits/scraped_audits.csv')





head(master_set)








viols_df$`RTC Date`
viols_df$Period_Begin
library(knitr)
kable(viols_df$`Enforcement Action Description`)
as.data.frame(table(viols_df$`Enforcement Action Description`))
###
# State administrative penalty assessed
- 




head(test)

test$System_ID = rownames(test)




system_df$`PWS Name`[is.na(system_type)]

grep('REGIONAL',system_df$`PWS Name`[is.na(system_type)],value=T)


grep('MUD',system_df$`PWS Name`,value=T) 

system_df$`PWS Name`
viol_p3count %>% filter(PWS_ID == 'TX2530002')



do.call(rbind,pblapply((nrow(system_quarter)-400):nrow(system_quarter), function(i) 
  
  
  table(viols_df$`PWS ID` %in% system_df$`PWS ID`)    
  
  table('TX2530002' %in% system_df$`PWS ID`)
  
  system_quarter[which(system_quarter$PWS_ID=='TX2530002'&system_quarter$Quarter==2005.25),]
  
  
  viols_df %>% filter(Period_Begin_ddate<2005.25 & `PWS ID` == "TX2530002" & Period_Begin_ddate>=(2005.25-3)) %>% 
    group_by(`Is Health Based`) %>% summarise(p3_vcount = n()) %>%
    mutate(PWS_ID = "TX2530002",Quarter = 2005.25) %>%
    mutate(PWS_ID = as.character(PWS_ID)) %>% spread(`Is Health Based`,p3_vcount) %>% complete(PWS_ID,Quarter,fill=list(N = 0,Y = 0))
  
  
  

         




tdf_list = lapply(1:nrow(viols_df), function(i) if (viols_df$Begin_Year[i]>=2005) {
viols_df %>% filter(`PWS ID` == viols_df$`PWS ID`[i] & viols_df$Period_Begin_ddate >= (viols_df$Period_Begin_ddate[i] - 3) & viols_df$Period_Begin_ddate <= viols_df$Period_Begin_ddate[i]) %>% 
  group_by(`PWS ID`,`Is Health Based`) %>% summarise(vcount_p3 = n()) %>% mutate(Period_Begin_ddate =viols_df$Period_Begin_ddate[i])}
)

tdf = do.call(rbind,tdf_list)

ggplot(tdf) + geom_density(aes(x=vcount_p3,colour=`Is Health Based`))


read_csv('Input/tceq_audits/system_df.csv')




