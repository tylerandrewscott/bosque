#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(stringr)
library(stringi)

library(lubridate)
library(magrittr)
#slackr_setup(config_file = '../proj3/.slackr')

base_site = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=4%2F19%2F2015&end_date=4%2F19%2F2017&action=Search+For+Water+Systems'
#length(system_summary_urls)
prefix = 'https://dww2.tceq.texas.gov/DWW/JSP/'
page_links = base_site %>% read_html() %>% html_nodes('a') 
data_sheet_urls = page_links[grepl('Summary',page_links %>% html_text(trim=T))] %>% html_attr('href')

facility_sheet_urls = gsub(' ','',gsub('DataSheet','WaterSystemFacilities',data_sheet_urls))

facility_table_css = 'table:nth-child(9) td , table:nth-child(9) tr+ tr th'
facility_connections_css = 'table:nth-child(12) td , table:nth-child(12) tr+ tr th'

library(data.table)
facility_lists = mclapply(facility_sheet_urls,function(x) {
url = gsub(' ','',paste0(prefix,x)) 
ht = url %>% read_html() 
gmaps = ht %>% html_nodes('a:contains(Google)') %>% html_attr('href')
facilities = ht %>% html_nodes('table') %>% .[[5]] %>% html_table(trim=T,fill=T) 
colnames(facilities) <- gsub('\\r|\\t|\\n','',facilities[1,])
facilities = facilities[-1,]
facilities = data.table::data.table(apply(facilities,2,function(x) gsub('\\r|\\t|\\n','',x)))
facilities$PWS_ID = str_extract(x,'TX[0-9]{7}')
facility_connections = ht %>% html_nodes('table') %>% .[[6]] %>% html_table(trim=T,fill=T) 
colnames(facility_connections) <- gsub('\\r|\\t|\\n','',facility_connections[1,])
facility_connections = facility_connections[-1,]
facility_connections = data.table::data.table(apply(facility_connections,2,function(x) gsub('\\r|\\t|\\n','',x)))
facility_connections$PWS_ID = str_extract(x,'TX[0-9]{7}')
facilities$Google_Maps[grepl('GoogleMaps',facilities$AerialView)] <- gmaps
list(facilities,facility_connections)
},mc.cores = 4,mc.cleanup = T,mc.preschedule = T)


faclist = lapply(facility_lists,function(x) x[[1]])
conlist = lapply(facility_lists,function(x) x[[2]])

facility_dt = rbindlist(faclist,fill = T)
facility_connection_dt = rbindlist(conlist,fill = T)
write_csv(facility_dt,paste('input/texas_dww/system_facilities',paste0(Sys.Date(),'.csv'),sep='_'))
write_csv(facility_connection_dt,paste('input/texas_dww/system_facility_connections',paste0(Sys.Date(),'.csv'),sep='_'))

