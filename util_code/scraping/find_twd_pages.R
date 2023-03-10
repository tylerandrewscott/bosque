iwdd_base = "https://www14.tceq.texas.gov/iwud/dist/index.cfm?fuseaction=ListDistricts&COMMAND=LIST"
i = 0
library(rvest)
library(tidyverse)
library(pbapply)
library(httr)


base_sess = html_session(iwdd_base)
page_html = base_sess %>% read_html()
dtable = page_html %>% html_nodes('table') %>% html_table(trim=T,fill=T)  
temp_table = dtable[[5]][-c(1,2),-1]
temp_table = temp_table[rowSums(temp_table=='') < ncol(temp_table),]
temp_table$URL = NA


url_list = lapply(seq_along(temp_table$X2),function(x) {
  if(temp_table$X2[x] %in% (page_html %>% html_nodes('table+ table .iwud a') %>% html_text(trim=T)))
    (page_html %>% html_nodes('table+ table .iwud a') %>% html_attr('href'))[which((page_html %>% html_nodes('table+ table .iwud a') %>% html_text(trim=T)) == temp_table$X2[x])]
  else{NA}
})

temp_table$URL = unlist(url_list)[!duplicated(unlist(url_list))]


img_nodes = base_sess %>% read_html() %>% html_nodes('img')
district_links = list()
i = 1
district_links[[i]] = unlist(base_sess %>% read_html() %>% html_nodes('table+ table .iwud a') %>% html_attr('href'))

continue_sess = base_sess
while(any(img_nodes %>% html_attr('name') == 'next') & all(img_nodes %>% html_attr('alt') != 'No next page')){
i = i + 1
next_page_link = which(img_nodes %>% html_attr('name') == 'next')
continue_sess = follow_link(x = continue_sess,i = next_page_link)
district_links[[i]] <- unlist(continue_sess %>% read_html() %>% html_nodes('table+ table .iwud a') %>% html_attr('href'))
img_nodes = continue_sess %>% read_html() %>% html_nodes('img')
}

id = page_html %>% html_nodes('.iwud+ .iwud:nth-child(3)') %>% html_text(trim=T)
status = base_sess %>% read_html() %>% html_nodes('.iwud~ .iwud+ .iwud') %>% html_text(trim=T)
name = page_html %>% html_nodes('.iwud+ .iwud:nth-child(3)') %>% html_text(trim=T)
district_links = unlist(district_links)

saveRDS(unlist(district_links),'input/texas_water_district_pages.RDS')

district_links = readRDS('input/texas_water_district_pages.RDS')
css_district_id = "table:nth-child(5) tr:nth-child(1) .iwud+ .iwuddata"
css_ccn = "table:nth-child(5) tr+ tr .iwud+ .iwuddata"
css_acres = "table:nth-child(11) tr:nth-child(1) .iwuddata"
css_created = "tr:nth-child(2) .iwuddata~ .iwud+ .iwuddata"
css_board = ".iwuddata b"
css_created = "table:nth-child(9) tr:nth-child(1) .iwuddata:nth-child(2)"
css_ended = "table:nth-child(9) tr:nth-child(1) .iwuddata~ .iwuddata"
css_num_directors = "table+ table tr:nth-child(3) .iwuddata:nth-child(2)"
css_name = "table+ table .iwuddata:nth-child(1)"
css_type = "table:nth-child(8) .iwuddata~ .iwuddata"
css_status = "table:nth-child(8) tr:nth-child(1) .iwuddata:nth-child(2)"
css_primary_county = "table+ table tr:nth-child(4) .iwuddata"


which(district_links == "index.cfm?fuseaction=DetailDistrict&ID=12694&command=list&name=RED%20RIVER%20AUTHORITY%20OF%20TEXAS")
info_list = pblapply(seq_along(district_links),function(x){
#print(x)
url = paste0('https://www14.tceq.texas.gov/iwud/dist/',district_links[x])
district_session = html_session(url)
link_urls = district_session  %>% read_html() %>% html_nodes('a') %>% html_attr('href')

i_link = which({district_session %>% read_html() %>% html_nodes('a') %>% html_text(trim=T)} == "Run District Information Report")       
district_info_session = follow_link(x = district_session,i = i_link)
district_info_html = district_info_session %>% read_html()

tdf = data.frame(PWS_ID = paste(str_extract(if(any(grepl('DWW',link_urls))){grep('DWW',link_urls,value=T)}else{NA},'TX[0-9]{1,}'),collapse = '|'),
Info_Link = i_link,
District_ID = district_info_html %>% html_node(css_district_id) %>% html_text(trim=T),
District_Name = district_info_html %>% html_node(css_name) %>% html_text(trim=T),
Status = district_info_html %>% html_node(css_status) %>% html_text(trim=T),
Created = district_info_html %>% html_node(css_created) %>% html_text(trim=T),
Ended = district_info_html %>% html_node(css_ended) %>% html_text(trim=T),
Type = district_info_html %>% html_node(css_type) %>% html_text(trim=T),
CCN = district_info_html %>% html_node(css_ccn) %>% html_text(trim=T),
Primary_County = district_info_html %>% html_node(css_primary_county) %>% html_text(trim=T),
BOARD_NUMBER = district_info_html %>% html_node(css_num_directors) %>% html_text(trim=T),
BOARD_SELECTION = district_info_html %>% html_node(css_board) %>% html_text(trim=T),
ACRES = district_info_html %>% html_node(css_acres) %>% html_text(trim=T),stringsAsFactors = F)
Sys.sleep(0.15)
tdf})

full_df = do.call(rbind,info_list)
#full_df$PWS_Page = full_df$PWS_ID
#full_df$PWS_ID = str_extract(full_df$PWS_ID,'TX[0-9]{1,}')
full_df$District_Link = district_links
write_csv(x = full_df,path = paste0('input/twdd_records/district_list_',Sys.Date(),'.csv'))



district_links = readRDS('scratch/texas_water_district_pages.RDS')
affiliates = pblapply(seq_along(district_links),function(x){
affil_url = paste0('http://www14.tceq.texas.gov/iwud/affiliation/',gsub("DetailDistrict",'listAffiliations',district_links[x]))
tdf = read_html(affil_url) %>% html_nodes('table') %>% html_table(trim=T,fill=T) 
tdf = data.frame(tdf[[3]],stringsAsFactors = F,fix.empty.names = TRUE)
tdf$district_link = district_links[x]
Sys.sleep(0.25)
tdf},cl = 5)
affiliates_df = do.call(rbind,affiliates)
affiliates_df$District_ID = full_df$District_ID[match(affiliates_df$district_link,full_df$District_Link)]
write_csv(x = affiliates_df,path = paste0('input/twdd_records/district_affiliates_',Sys.Date(),'.csv'))



service_list = pblapply(seq_along(district_links),function(x){
  #print(x)
  print(x)
  url = paste0('http://www14.tceq.texas.gov/iwud/dist/',district_links[x])
  htm = read_html(url)
  tnodes = html_nodes(htm,'table') 
  tables = lapply(tnodes,function(x) tryCatch(html_table(x), error = function(e) NULL))
  if(all(!is.null(tables)) & length(tables)>0 & any(grepl('Function',lapply(tables,colnames)))){
    service_df = data.frame(tables[[which(grepl('Function',lapply(tables,colnames)))]],stringsAsFactors = F,fix.empty.names = T)
    service_df$District_Link = district_links[x]
    Sys.sleep(0.25)
    service_df = service_df[,!grepl('^Var',colnames(service_df))]
    Sys.sleep(0.25)
    service_df[!grepl('Occurrences retrieved',service_df$Function),]}
  },cl = 10)

district_services_df = do.call(rbind,service_list)
district_services_df$District_ID = full_df$District_ID[match(district_services_df$District_Link,full_df$District_Link)]

write_csv(x = district_services_df,path = paste0('input/twdd_records/district_services_',Sys.Date(),'.csv'))
