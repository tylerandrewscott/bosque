


require(rvest)
require(data.table)
require(RCurl)
require(stringr)
require(ggthemes)
require(viridis)
require(forcats)
require(lubridate)
require(tidyverse)
require(jsonlite)
require(pbapply)
viol_pull_base = 'https://data.epa.gov/efservice/VIOLATION/PWSID/=/'

system_find <- 'https://ofmpub.epa.gov/echo/sdw_rest_services.get_systems?output=JSON&p_st=TX'
res <- fromJSON(system_find)
qid <- res$Results$QueryID
return_systems <- fread(paste0('https://ofmpub.epa.gov/echo/sdw_rest_services.get_download?qid=',qid,'&qcolumns=6,9,10,11,14'))
setnames(return_systems,'PWSId','PWS_ID')
setnames(return_systems,'PWSName','PWS_NAME')
tx_sdwis = return_systems
tx_sdwis = tx_sdwis[grepl('^TX',PWS_ID),]


# viol_pulls = pblapply(tx_sdwis$`PWS ID`,function(x) {
# fromJSON(paste0(viol_pull_base,x,'/JSON'))
# },cl = 4)
# 
# while(any(sapply(viol_pulls,class)=='try-error')){
#   bads = which(sapply(viol_pulls,class)=='try-error')
#   for(b in bads){
#     id = tx_sdwis$`PWS ID`[b]
#     viol_pulls[[b]]<-fromJSON(paste0(viol_pull_base,id,'/JSON'))
#   }
# }
# 
# viol_dt = rbindlist(viol_pulls,use.names = T,fill = T)
#viol_dt = saveRDS(viol_dt,paste0('scratch/viol_list_',Sys.Date(),'.rds'))

viol_dt = readRDS(paste0('scratch/viol_list_2021-03-02.rds'))

dets = readRDS('scratch/pws_details_2020-11-15.RDS')
dets = dets[dets$Owner_Type=='District',]



dww_dets = data.table(readRDS('scratch/pws_details_2020-11-15.RDS'))

setkey(dww_dets,'PWS_ID')
setkey(tx_sdwis,'PWS_ID')

system_dt = merge(tx_sdwis,dww_dets,all = T)

setnames(viol_dt,'PWSID','PWS_ID')

viol_dt$UQID = paste0(viol_dt$PWS_ID,'_',viol_dt$VIOLATION_ID)
viol_dt$COMPL_PER_YEAR = year(dmy(viol_dt$COMPL_PER_BEGIN_DATE))

temp = viol_dt[,.N,by=.(IS_HEALTH_BASED_IND,PWS_ID,COMPL_PER_YEAR)]
cws = system_dt[system_dt$`PWSTypeDesc` == 'Community water system',]
temp = temp[PWS_ID %in% cws$PWS_ID,]

districts = fread('input/twdd_records/district_list_2019-03-08.csv')
districts = districts[Created!=''&year(mdy(Created))<2007,]
districts = districts[year(mdy(Ended))>2007|Ended=='',]
districts = districts[Type %in% c("SPECIAL UTILITY DISTRICT" ,
"FRESH WATER SUPPLY DISTRICT" , 
"MUNICIPAL UTILITY DISTRICT"   ,   
"WATER CONTROL AND IMPROVEMENT DISTR"),]



temp = temp[PWS_ID %in% unlist(str_split(districts$PWS_ID,'\\|')),]

temp$IS_HEALTH_BASED_IND = ifelse(temp$IS_HEALTH_BASED_IND=='Y','Health viol.','Management viol.')

base = expand.grid(PWS_ID = tx_sdwis$PWS_ID[tx_sdwis$PWS_ID %in% unlist(str_split(districts$PWS_ID,'\\|'))],COMPL_PER_YEAR=2006:2019,IS_HEALTH_BASED_IND = c('Health viol.','Management viol.'))
full_counts = merge(base,temp,all = T)
full_counts$N[is.na(full_counts$N)]<-0
full_counts = data.table(full_counts)
full_counts$PWS_ID = fct_reorder(full_counts$PWS_ID,full_counts$N,sum)

g1 = ggplot(data = full_counts[COMPL_PER_YEAR%in%c(2007:2018),]) + 
  geom_tile(aes(x = COMPL_PER_YEAR,y = PWS_ID,fill = N>0)) + 
  theme_bw() + theme(axis.text.y= element_blank(),panel.background = element_rect(fill = 'white'),
                    # axis.text.x = element_blank(),axis.title.x = element_blank(),
                     axis.ticks = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal') + 
  facet_wrap(~IS_HEALTH_BASED_IND)+
  scale_fill_manual(values = c('white','black'))+
  scale_y_discrete(name = 'Violation occurence by system (1 or more)',expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),name = 'Compliance period (year)')+ guides(fill = F)+
  ggtitle('Special district-owned CWS SDWA violations, 2007-2018')
g1


g2 = ggplot(full_counts[COMPL_PER_YEAR%in%c(2007:2018),sum(N),by=.(COMPL_PER_YEAR,IS_HEALTH_BASED_IND)]) + geom_bar(aes(x = COMPL_PER_YEAR,y = V1),stat = 'identity') + 
  facet_wrap(~IS_HEALTH_BASED_IND) + theme_bw() + theme(axis.ticks = element_blank(),axis.text.y=element_blank()) + 
  scale_y_continuous(name = 'total viols.') + 
  scale_x_continuous(name = 'Compliance year',expand = c(0,0))+
  ggtitle('Total CWS SDWA violations, 2007-2018')
g2





library(sf)
wd = st_read('spatial_inputs/water_districts_shp/TCEQ_WaterDistricts.shp')
albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
wd = st_transform(wd,crs = 3083)

wd <- wd[wd$DISTRICT_I %in% master_dt$District_ID,]
wd$District_ID <- as.character(wd$DISTRICT_I)
sb = st_read('spatial_inputs/Service_Area_Boundaries/PWS_Export.shp')
sb = st_transform(sb,crs = 3083)
sb = st_make_valid(sb)
sb$INSAMPLE = sapply(sb$PWSId,function(s) any(grepl(s,districts$PWS_ID)))

totviols = viol_dt[IS_HEALTH_BASED_IND=='Y'&COMPL_PER_YEAR %in% 2007:2018,.N,by = .(PWS_ID)]
sb$violations = totviols$N[match(sb$PWSId,totviols$PWS_ID)]
sb$violations[is.na(sb$violations)]<-0
sb$violations[sb$violations>50]<-50

tx_counties = tigris::counties(state = 'TX',cb = T)
tx_counties = st_transform(tx_counties,crs = 3083)

  
ggplot() + geom_sf(data = sb,aes(fill = violations),colour = NA,lwd = 0.05) + 
  geom_sf(data = tx_counties,fill = NA,col = 'grey70',lwd=0.1)+
  theme_map() + 
  geom_sf(data = sb[sb$INSAMPLE,],fill = NA,aes(color = 'black'),lwd = 0.05) +
  scale_fill_viridis_c(option = 'D',direction = -1,name = '# health violations',
                       breaks=seq(0,50,10),limits=c(0,50),labels=c(0,10,20,30,40,'50+')) + 
  theme(legend.position = c(0.2,0.1),title = element_text(size = 14),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12),
        legend.background = element_rect(fill = NA,colour=NA)) + 
  scale_colour_manual(name = '',values=c('black'),labels = c('District in sample'))+
  guides(color = guide_legend(override.aes = list(lwd = 1)))+
  ggtitle('Retail water district health violations, 2007 to 2018')



sb_district = sb[sb$PWSId %in% unlist(str_split(districts$PWS_ID,'\\|')),]
sb_other = sb[!sb$PWSId %in% unlist(str_split(districts$PWS_ID,'\\|')),]





match(sb_district$PWSId,str_split(districts$PWS_ID,'\\}'))


sb_district = st_union(sb_district,by_feature = T)
sb_combo <- sb %>% group_by(District_ID) %>% summarize()
wd_simple <- wd[!wd$District_ID %in% sb_combo$District_ID,c('District_ID')]
wd_simple <- wd_simple[!duplicated(wd_simple$District_ID),]
district_sf = rbind(sb_combo,wd_simple)

tot = viol_dt[COMPL_PER_YEAR%in%2007:2018,.N,by=.(PWS_ID)]

pws_dt
sb[]
sb = st_read('spatial_inputs/Service_Area_Boundaries/PWS_Export.shp')
sb = st_transform(sb,crs = 3083)
sb = st_make_valid(sb)
sb





tot$V1[match(PWSId,tot$PWS_ID)]


ggplot() + geom_sf(data = d
                   istrict_sf)
ggplot() + geom_sf(data = sb)
district_sf


full_counts$District_ID = districts$District_ID[unlist(sapply(full_counts$PWS_ID,function(x) grep(x,districts$PWS_ID)))]

full_counts$TYPE = districts$Type[match(full_counts$District_ID,districts$District_ID)]

full_counts[!duplicated(PWS_ID),.N,by=.(TYPE)]

"reatil waterlength(unique(full_counts$PWS_ID))
length(unique(full_counts$District_ID))

temp
require(gridExtra)

grid.arrange(g1,g2,ncol = 1,heights=c(0.8,0.2),widths = c(1),
             top = 'Special district SDWA violations, 2007-2018')

g1
g2
full_counts[COMPL_PER_YEAR%in%c(2007:2018),sum(N),by=.(COMPL_PER_YEAR,IS_HEALTH_BASED_IND)]

ggplot(full_counts[,sum(N>0),by=.(PWS_ID,IS_HEALTH_BASED_IND)]) + facet_wrap(~IS_HEALTH_BASED_IND) + 
  geom_bar(aes(x = V1)) + xlab('# years with one or more violations') + ggtitle('Violations from 2006 to 2019') + 
  ylab('# systems')+ theme_bw()


ggplot(data =test[,.N,by=.(PWS_ID)]) + geom_bar(aes(x = N))

test = temp[IS_HEALTH_BASED_IND=='Health viol.']
ggplot(data =test[,.N,by=.(PWS_ID)]) + geom_bar(aes(x = N))


test_cast = dcast(test,PWS_ID ~ COMPL_PER_YEAR,value.var = 'N',fill = 0)

tt = melt(test_cast)
tt = tt[order(PWS_ID,variable),]

tt[,prior_value:=lag(value),by=.(PWS_ID)]
tt = tt[as.numeric(as.character(tt$variable))>=2006,]
tt$status = NA
tt$status[tt$value>0&tt$prior_value>0] <- 'Repeat'
tt$status[tt$value>0&tt$prior_value==0]<- 'New problem'
tt$status[tt$value==0&tt$prior_value>0]<- 'Fixed'
tt$status[tt$value==0&tt$prior_value==0]<- 'Still good'

ggplot(data = tt[,sum(value>0),by=.(PWS_ID)]) + geom_bar(aes(x = V1)) + 
  ggtitle('# of years with a management violation, 2006-2019') + 
  ylab('# systems') + xlab('# years')




cor(test_cast[['2009']]>0,test_cast[['2010']]>0,method = 'spearman')


glm(value ~ prior_value,family = 'poisson',data = tt)


tes
R test_cast



ff = tt[,.N,by=.(variable,status)][variable%in%c(2010:2011),]



dj


tt

test



g1









ggplot(data = temp[COMPL_PER_YEAR>2006,]) + geom_tile(aes(x = COMPL_PER_YEAR,y = PWS_ID,fill = N>0)) + 
  theme_bw() + theme(axis.text.y= element_blank(),axis.ticks = element_blank(),legend.position = 'bottom',legend.direction = 'horizontal') + 
  facet_wrap(~IS_HEALTH_BASED_IND)+
  scale_fill_colorblind(name = '1 or more violations',labels = '') 



temp$IS_HEALTH_BASED_IND

viol_dt[PWS_ID=='TX1010212'&VIOLATION_ID=='100069044']
table(du)
require(lubridate)
table(duplicated(paste(viol_dt$VIOLATION_ID,year(dmy(viol_dt$COMPL_PER_BEGIN_DATE)))))

which(duplicated(viol_dt$VIOLATION_ID))



viol_dt[VIOLATION_ID == viol_dt$VIOLATION_ID[3933],]





