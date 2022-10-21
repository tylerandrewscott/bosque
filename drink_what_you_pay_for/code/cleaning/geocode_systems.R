
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(RgoogleMaps)
library(ggmap)
library(rjson)
library(knitr)
library(ggplot2)
library(rgdal)
library(maptools)
library(rgeos)
library(httr)
library(ggthemes) 
library(viridis)
require(rgdal)


echo_viols  = fread('Input/epa_echo/echo_query_8-25-16.csv',)


texas_data = fread('input/epa_sdwis/txeq_wsp_base_reference.csv')
ws_base = fread('input/epa_sdwis/water_system_summary.csv',na.strings = "-")
ws_detail = fread('input/epa_sdwis/water_system_detail.csv',na.strings = "-")
ws_detail = ws_detail %>% 
  mutate(query_address = paste(`Address Line1`,paste0(`City Name`,", ",`State Code`),`Zip Code`))
ws_base = left_join(ws_base,ws_detail)
ws_cws = ws_base %>% filter(`PWS Type`=='Community water system'&`Owner Type`=='Local government') %>% 
  rename(Total_Violations = `# of Violations`,Total_Facilities = `# of Facilities`) %>% mutate(COUNTYNAME = `Counties Served`) %>%
  mutate(Total_Violations = as.numeric(Total_Violations),Total_Facilities=as.numeric(Total_Facilities))

ws_facs = fread('input/epa_sdwis/facility_report.csv',na.strings = "-")
#ws_viols = fread('input/epa_sdwis/violation_report.csv',na.strings='-')


# cred_url = '../../Desktop/client_secret_29813640204-4t8db4q0pbe1kkh7ln1mb8rv3tgr00a7.apps.googleusercontent.com.json'
# raw.data <- readLines(cred_url, warn = "F")
# rd <- fromJSON(raw.data)
# unique_addresses = ws_cws$query_address[!duplicated(ws_cws$query_address)]
# gcodes = sapply(unique_addresses,function(x) geocode(x,output='latlon',source='dsk'),simplify=F)
# gcodes = do.call('rbind',gcodes)
# ws_cws$lon = gcodes$lon[match(ws_cws$query_address,unique_addresses)]
# ws_cws$lat = gcodes$lat[match(ws_cws$query_address,unique_addresses)]

library(RCurl)


tx_county  = readOGR(dsn = 'spatial_inputs/government_units/','county_nrcs_a_tx')
county_totals = 
  ws_cws %>% group_by(COUNTYNAME) %>%
  summarize(county_viols = sum(Total_Violations),county_facilities = sum(Total_Facilities),county_providers = n())

tx_county@data = left_join(tx_county@data,county_totals)
tx_county@data$id = tx_county$FIPS_C
tx.points = fortify(tx_county, region="id")
tx.df = left_join(tx.points, tx_county@data, by="id")
county_pops  = read.csv('input/DEC_10_SF1_P1_with_ann.csv',stringsAsFactors = F)
county_pops$COUNTYNAME = gsub(' County.*','',county_pops$COUNTYNAME)
county_pops$FIPS_C = as.character(county_pops$FIPS_C)
county_pops$Total_Population = as.numeric(county_pops$Total_Population)
tx.df =  left_join(tx.df,county_pops)
tx.df$Viols_per_1000people = tx.df$county_viols / (tx.df$Total_Population/1000)


echo_viols[(matrix(rep(grepl('^vio',colnames(echo_viols)),each=nrow(echo_viols)),byrow = F,ncol=ncol(echo_viols)) & is.na(echo_viols))] = 0
echo_viols$ifea[is.na(echo_viols$ifea)] = 0
echo_viols$feas[is.na(echo_viols$feas)] = 0

link_base = "https://ofmpub.epa.gov/echo/dfr_rest_services.get_sdwis_compliance?output=json&p_id="
echo_viols$rest_query = paste0(link_base,echo_viols$pwsid)

library(pbapply)
library(parallel)

library(doMC)

doMC::registerDoMC(cores=4) # or however many cores you have access to

"Results" %in% names(fromJSON(getURL(i)))

empty_df = data.frame()
for (i in echo_viols$rest_query[echo_viols$owner_type_code=='L'])
{
  repeat {
    print(i)
    temp = fromJSON(getURL(i)[1]);Sys.sleep(5)
    if (temp$Results$Message=='Success')
    {break}
    }
empty_df <<- rbind(empty_df,as.data.frame(temp$Results$SDWISCompliance$Sources[[1]]$Status))
write.csv(x = empty_df,'Input/epa_echo/system_status_by_quarter.csv')
}

i = echo_viols$rest_query[echo_viols$owner_type_code=='L'][30]
temp = fromJSON(getURL(i)[1]);Sys.sleep(1)
temp$Results$Message


as.data.frame(temp$Results$SDWISCompliance$Sources[[1]]$Status)
empty_df
  repeat( temp = fromJSON(getURL(i)[1]); if(!temp$Results$Message=='Success'){break})
  
}

getURL(i)

repeat {
  statement
}


test = pblapply(echo_viols$rest_query[echo_viols$owner_type_code=='L'][1:50],
 function(wsys) {
  as.data.frame(temp$Results$SDWISCompliance$Sources[[1]]$Status)}
)





(unlist(lapply(system_status_return_list,nrow)))

system_status_return_list2 = pblapply(echo_viols$rest_query[echo_viols$owner_type_code=='L'][1001:2000],function(wsys) {
  as.data.frame(fromJSON(getURL(wsys)[1])$Results$SDWISCompliance$Sources[[1]]$Status)})

system_status_return_list3 = pblapply(echo_viols$rest_query[echo_viols$owner_type_code=='L'][2001:length(echo_viols$rest_query[echo_viols$owner_type_code=='L'])],function(wsys) {
  as.data.frame(fromJSON(getURL(wsys)[1])$Results$SDWISCompliance$Sources[[1]]$Status)})


system_status_df = do.call(rbind,system_status_return_list) %>% tidyr::gather(Quarter,Time,-SourceID)
write.csv(x = system_status_df,'Input/epa_echo/system_status_by_quarter.csv')



viol_type_return_list = plyr::l_ply(echo_viols$rest_query[echo_viols$owner_type_code=='L'],function(wsys) {
  do.call(rbind,lapply(fromJSON(getURL(wsys)[1])$Results$SDWISCompliance$Sources[[1]]$RulesViolated,as.data.frame))},
  .parallel = TRUE)




rule_status_df <<- rbind(rule_status_df,
                         
                                  

                         
                         



,.progress='text')

test

system
as.data.frame(fromJSON(getURL(system)[1])$Results$SDWISCompliance$Sources[[1]]$Status)



fromJSON(getURL(system)[1])$Results$SDWISCompliance$Sources[[1]]$Status
echo_viols$rest_query[echo_viols$owner_type_code=='L'][9]


?mclapply
system_status_df = data.frame()
rule_status_df = data.frame()
quarter_ref_df = data.frame()
plyr::l_ply(echo_viols$rest_query[echo_viols$owner_type_code=='L'],function(system) {
temp_df <- fromJSON(getURL(system)[1])$Results$SDWISCompliance
system_status_df <<- rbind(system_status_df,as.data.frame(temp_df$Sources[[1]]$Status))
rule_status_df <<- rbind(rule_status_df,do.call(rbind,lapply(temp_df$Sources[[1]]$RulesViolated,as.data.frame)))
quarter_ref_df <<- 
  rbind(quarter_ref_df,data.frame(SourceID = temp_df$Sources[[1]]$Status$SourceID,
                                  as.data.frame(temp_df[grepl('^Qtr',names(temp_df))])))
},.progress="text")









cws_viols = ws_viols %>% filter(`PWS ID` %in% ws_cws$`PWS ID`)
cws_viols$Year = year(dmy(cws_viols$`Violation First Reported Date`))
cws_viols$Month = month(dmy(cws_viols$`Violation First Reported Date`))
cws_viols$Report_Date = dmy(cws_viols$`Violation First Reported Date`)
cws_viols$Decimal_Date = decimal_date(dmy(cws_viols$`Violation First Reported Date`))


ggplot(cws_viols) + geom_density(aes(x=Decimal_Date)) + theme_bw()

kable(cws_viols %>% group_by(`Violation Type`) %>% summarise(Violations = n()) %>% arrange(-Violations))

ggplot(cws_viols %>% filter(`Violation Type`=="Maximum Contaminant Level Violation, Average")) + geom_density(aes(x=Decimal_Date)) + theme_bw()

kable(cws_viols %>% group_by(`Violation Type`) %>% filter(!is.na(`Is Major Violation`) & `Is Major Violation`=='Y') %>%
        summarise(Violations = n()) %>% arrange(-Violations))



temp_nonviol_sum = data.frame(PWS_ID = ws_cws$`PWS ID`[!ws_cws$`PWS ID` %in% cws_viols$`PWS ID`],Total_Violations = 0)
temp_viol_sum = cws_viols %>% group_by(`PWS ID`) %>% summarize(Total_Violations = n()) %>% arrange(Total_Violations) %>% rename(PWS_ID = `PWS ID`)
temp_sum = rbind(temp_nonviol_sum,temp_viol_sum)

ggplot(temp_sum,aes(x=Total_Violations)) + geom_histogram(bins=200) + scale_y_continuous(expand=c(0,0),name='# Systems') + 
  scale_x_continuous(name='Total Violations') + theme_bw()


ggplot(ws_cws,aes(x=as.numeric(Total_Facilities),y=as.numeric(Total_Violations))) + geom_point() + theme_bw() + 
  ylab('# Violations') + scale_x_continuous(name = '# Facilities',breaks=c(0,25,100,300,600,900)) + stat_smooth(method='loess')



gg_viol_by_county = ggplot(tx.df,aes(x=long,y=lat,group=id)) + geom_polygon(aes(fill=county_viols)) + theme_tufte(ticks=F) + 
  scale_fill_viridis()




gg_viol_per_1000 = ggplot(tx.df,aes(x=long,y=lat,group=id)) + geom_polygon(aes(fill=Viols_per_1000people)) + theme_tufte(ticks=F) + 
  scale_fill_viridis()

cws_viols$COUNTYNAME = ws_cws$COUNTYNAME[match(cws_viols$`PWS ID`,ws_cws$`PWS ID`)]
                                          
cws_viols_post2000 = cws_viols %>% filter(Year >= 2004) %>% group_by(Year,COUNTYNAME) %>% summarise(Year_Viols = n())
yearly_tx = merge(tx.df,data.frame(Year = 2004:2015))
tx.df.years = left_join(yearly_tx,cws_viols_post2000)
tx.df.years$Year_Viols[is.na(tx.df.years$Year_Viols)] = 0
tx.df.years$Viols_per_1000people = tx.df.years$Year_Viols/(tx.df.years$Total_Population/1000)

pq = qplot(x=long,y=lat,group=id,fill=Viols_per_1000people,data=tx.df.years,geom="polygon")
pq + scale_fill_viridis() + facet_wrap(~Year) + theme_tufte(ticks=F) + 
  theme(axis.text = element_blank(),axis.title=element_blank(),legend.position = 'bottom')


head(cws_viols[grepl('^APPLEBY',cws_viols$`PWS Name`),])

cws_viols$time_to_rectify = dmy(cws_viols$`RTC Date`) - 
  dmy(cws_viols$`Compliance Period Begin Date`)
cws_viols$time_to_rectify[is.na(cws_viols$`RTC Date`)] = 
  ymd(Sys.Date()) - dmy(cws_viols$`Compliance Period Begin Date`[is.na(cws_viols$`RTC Date`)])

cws_viols$health_and_fixed = paste(!is.na(cws_viols$`RTC Date`),cws_viols$`Is Health Based`)


cws_viols_post2004 = cws_viols %>% filter(Year>=2004)

cws_viols_post2004 %>% 

  
unique(cws_viols_post2004$`PWS Name`)
  


ggplot(cws_viols_post2004,aes(x=as.numeric(time_to_rectify),color=health_and_fixed,linetype=health_and_fixed)) + 
  geom_freqpoly(bins=200)  +facet_wrap(~health_and_fixed)+
  theme_tufte(ticks=F) +  theme(legend.position = c(0.8,0.24))+
  xlab('days from compliance period start to return to compliance')  + 
  scale_color_manual(name = 'Type and Status',labels=c('Non-health, Not fixed','Non-health, Fixed',
                                                       'Health, Not fixed','Health, Fixed'),
                     values=c('black','black','orange','orange'))+
  scale_linetype_manual(name = 'Type and Status',labels=c('Non-health, Not fixed','Non-health, Fixed',
                                                       'Health, Not fixed','Health, Fixed'),
                        values = c(1,2,1,2))





head(as.numeric(cws_viols$time_to_rectify))
hist(as.numeric(cws_viols$time_to_rectify))




      
      


