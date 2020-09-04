library(tidyverse)
library(lubridate)
library(INLA)

#write_csv(sdw_date,'scratch/scratch_file.csv')

co = read_csv('input/census/tx_county_census.csv') %>% arrange(Name,Year)
library(zoo)
co_full = expand.grid(1965:2016,unique(co$Name)) %>% rename(Year = Var1,Name = Var2)
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
event_df_count = event_df_count %>% filter(!is.na(year)) 
event_df$FORMED_BY[event_df$FORMED_BY=='BOARD RESOLUTION TO DIVIDE'] <- 'COUNTY'
event_df = event_df %>% filter(!is.na(COUNTY))

event_df = event_df %>% filter(DISTRICT_TYPE == 'MUNICIPAL UTILITY DISTRICT')

event_df <- event_df %>% filter(creation_year>=1965) %>% 
  filter(is.na(deactivation_year)|creation_year != deactivation_year) %>%
  filter(creation_year < 2016) %>% filter(is.na(deactivation_year)|deactivation_year<2017)

temp = event_df %>% select(creation_year,deactivation_year) %>% gather() %>%
  mutate(key = ifelse(key == 'creation_year','Year created','Year deactivated'))
library(ggthemes)
ggplot(temp,aes(x = value)) + geom_bar()  + facet_wrap(~key,nrow=2) + theme_tufte(ticks=F) +
  theme(axis.title.y=element_text(size=14),axis.title.x = element_blank(),legend.title=element_blank(),strip.text = element_text(size=14),
        axis.text = element_text(size = 14)) + ylab('# MUDs') +
  scale_x_continuous(breaks = seq(1970,2010,10))

event_df = event_df %>% mutate(Max_Age_Obs = ifelse(event==1,deactivation_year - creation_year,
                                                    2016 - creation_year))


cfreq = as.data.frame(table(event_df$COUNTY)) %>% arrange(Freq) %>% filter(Freq>1) %>% rename(COUNTY = Var1)
event_df = event_df[event_df$COUNTY %in% cfreq$COUNTY,]


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

event_df = event_df %>% mutate(Max_Age_Obs = ifelse(event==1,deactivation_year - creation_year,
                                                    2016 - creation_year))







district_age_df = do.call(rbind,pblapply(1:nrow(event_df),function(i) data.frame(DISTRICT_NAME = event_df$DISTRICT_NAME[i],
                                                                                 AGE = 0:event_df$Max_Age_Obs[i]) %>%
                                           mutate(Year = (0:event_df$Max_Age_Obs[i]) + event_df$creation_year[i])))






summary(cdate$total_muds)



# event_df = event_df %>% mutate(creation_period = ifelse(creation_year < 1975,'1965-1974',
#                                                         ifelse(creation_year < 1985,'1975-1984',
#                                                                ifelse(creation_year < 1995,'1985-1994',
#                                                                       ifelse(creation_year < 2005,'1995-2004','2005-2015')))))

library(survival)
library(coxme)
event_df = event_df %>% mutate(status = ifelse(event == 0 | (event==1&Year!=deactivation_year),0,1))
event_df$time0 <- 0





surv_ob <- inla.surv(time = event_df$time,event = event_df$event)
formula=surv_ob ~ 1 + Log_Acre_Size + HAS_BOARD + 
  MULTI_COUNTY + TAX_RATE  + FORMED_BY +
  # f(inla.group(creation_year), model='rw1',scale.model = TRUE) + 
  f(inla.group(creation_year), model='iid') + 
  f(COUNTY,model = 'iid',hyper = list(prec = list(param=c(1, 1)))) 
mod=inla(formula, family ="coxph", data = event_df,verbose=F,
         control.hazard=list(model="rw1", n.intervals=50,scale.model = TRUE),
         control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE))


require(grid)
basehaz <- mod$summary.random$baseline.hazard
ggplot(basehaz) + 
  geom_path(aes(x = ID,y = mean,colour = 'hazard',linetype = 'solid')) +  
  geom_path(aes(x = ID,y = `0.975quant`,colour = 'hazard_ci',linetype = 'dashed')) +
  geom_path(aes(x = ID,y = `0.025quant`,colour = 'hazard_ci',linetype = 'dashed')) +
  geom_smooth(se = F, aes(x = ID,y = mean,colour = 'loess_hazard',linetype = 'dotted')) +
  scale_x_continuous(name = 'Age of MUD') + scale_y_continuous(name = 'Hazard rate') +
  theme_bw() +theme(legend.position = c(0.7,0.2),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),legend.title=element_blank(),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm")) +
  scale_colour_manual(values = c('black','black','grey70'),labels = c('Baseline hazard','95% credible interval','Smoothed baseline hazard')) +  
  scale_linetype_identity() + guides(colour = guide_legend(override.aes = list(linetype = c('solid','dashed','dotted'),colour = c('black','black','grey70'))),linetype = FALSE)


#hyper = inla.hyperpar(mod,diff.logdens=10,dz=0.5,verbose=T)

fixed_vals <- mod$summary.fixed %>% mutate(ID = rownames(.))
#random_type_vals <- mod$summary.random$DISTRICT_TYPE
random_county_vals <- mod$summary.random$COUNTY

#ggplot(random_county_vals %>% arrange(mean) %>% mutate(print = 1:nrow(.)),aes(x = print,xend = print,y = `0.025quant`,yend = `0.975quant`)) + geom_segment() + coord_flip()

tabl <- rbind(fixed_vals) %>% 
  select(ID,mean,`0.025quant`,`0.975quant`) %>% mutate(`0.025quant` = round(`0.025quant`,3),
                                                       mean = round(mean,3),`0.975quant`=round(`0.975quant`,3))

library(stargazer)
stargazer(tabl,out = 'scratch/survival_table_out.html',type = 'html',summary = FALSE)


create_year_df = mod$summary.random$`inla.group(creation_year)`

ggplot(create_year_df) + geom_errorbar(aes(x = ID,ymin = `0.025quant`,ymax =`0.975quant` )) +
  scale_x_continuous(name = 'Year of creation') + scale_y_continuous(name = '95% credible interval') +
  theme_bw() +theme(legend.position = c(0.7,0.2),axis.text=element_text(size=14),
                    axis.title=element_text(size=14),legend.title=element_blank(),
                    legend.text=element_text(size = 14),legend.key.width = unit(2,units = "cm"))

library(rgdal)
tx_counties <- readOGR('spatial_inputs/government_units/','county_nrcs_a_tx')
tx_counties$COUNTYNAME <- toupper(tx_counties$COUNTYNAME)
random_county_vals$COUNTYNAME = random_county_vals$ID

tx_counties@data <- left_join(tx_counties@data,random_county_vals)

# Next the shapefile has to be converted to a dataframe for use in ggplot2
tx_df <- fortify(tx_counties,region="OBJECTID")
tx_counties@data$id = tx_counties@data$OBJECTID
tx_df <- left_join(tx_df,tx_counties@data)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
library(viridis)
library(ggthemes)
map1 <- ggplot() + ggtitle('Random intercept mean') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=mean)) + 
  scale_fill_viridis(option = 'D',name = 'mean') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))

map2 <- ggplot() + ggtitle('Random intercept standard deviation') +
  geom_polygon(data = tx_df, 
               aes(x = long, y = lat, group = group,fill=sd)) + 
  scale_fill_viridis(option = 'D',name = 'sd') + theme_map() + 
  theme(legend.position = c(0.1,0.05),
        title = element_text(size = 16),legend.title.align = .5,
        legend.title = element_text(size = 12),legend.text = element_text(size = 12))


library(gridExtra)

grid.arrange(map1,map2,ncol=2)



head(event_df)





# 
# tax_rate_seq = seq(0,5,0.25)
# lc_tax_rate <- inla.make.lincombs(LEVY_TAX = rep(1, length(tax_rate_seq)),
#                                   "LEVY_TAX:TAX_RATE" = 1 * tax_rate_seq)
# mod_lc_taxrate = inla(formula,family = 'coxph',data = event_df,verbose=F,
#                       control.hazard=list(model="rw1", n.intervals=50,scale.model = TRUE),
#                       #control.fixed = list(expand.factor.strategy = "inla"),
#                       control.compute = list(waic=TRUE,dic=TRUE,cpo=TRUE),
#                       control.inla = list(lincomb.derived.only=FALSE),
#                       control.predictor=list(compute=TRUE),lincomb=lc_tax_rate)
# 
# gg_lc_tax = ggplot(mod_lc_taxrate$summary.lincomb) + 
#   geom_errorbar(aes(x=tax_rate_seq,ymax=`0.025quant`,ymin=`0.975quant`)) + 
#   geom_point(aes(x=tax_rate_seq,y=mean,col='black'),pch=21,fill='white') +
#   scale_x_continuous(name='Tax rate (%)') +
#   # breaks=which(rev_lc_seq %in% c(10000,c(1,15)*1000000)),
#   # labels=c("10k","$1M","$15M"),expand=c(0,0))  +
#   theme_tufte(ticks=F) + 
#   scale_y_continuous(name = '95% credible interval for tax powers by rate',limits = c(-7,2)) + 
#   theme(axis.text=element_text(size=12),axis.title = element_text(size=12),
#         legend.title = element_blank(),legend.text = element_text(size=12),
#         legend.position = c(0.3,0.2)) + 
#   geom_hline(yintercept = 0,lty=2,col='grey50') + 
#   scale_color_manual(values='black', label=expression(beta[1] + beta[2]*paste('Rate',sep = ' ')))

library(gridExtra)








