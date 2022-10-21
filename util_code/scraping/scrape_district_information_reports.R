
library(rvest)
library(tidyverse)


district_set_base = "http://www14.tceq.texas.gov/iwud/dist/index.cfm?fuseaction=ListDistricts&Command=list"
dsess = html_session(district_set_base)

grab_districts = "table+ table .iwud"

page_results = dsess %>% read_html() %>% html_node(css = 'table:nth-of-type(2)') %>% html_table(trim=T,fill=T) %>%
  .[-c(1:3),]

while((dsess %>% read_html() %>% html_node("tr+ tr td a img") %>% html_attr('alt')) == 'Next page')
{
  
  dsess = dsess %>%  follow_link(i = which(grepl("Command=NEXT",dsess %>% read_html() %>% html_nodes('a') %>% html_attr('href') ))[1]) 
  page_results = rbind(page_results,
                       dsess %>% read_html() %>% html_node(css = 'table:nth-of-type(2)') %>% html_table(trim=T,fill=T) %>%
                         .[-c(1:3),])
  Sys.sleep(time = 1)
}

temp_df = page_results %>% select(-X1) %>% rename(STATUS = X4,DISTRICT_ID = X3,DISTRICT_NAME = X2)

base = 'http://www14.tceq.texas.gov/iwud/reports/index.cfm'
sess = html_session(base)
form = sess %>% rvest::html_form() %>% .[[1]]
form = set_values(form,fuseaction = 'ERPT0001')
Sys.sleep(0.5)
district_report_session = submit_form(sess,form)
search_district = district_report_session  %>% html_form() %>% .[[1]]
search_district = set_values(search_district,districtnumber = as.numeric(temp_df$DISTRICT_ID[1]))
Sys.sleep(0.5)
district_results = submit_form(district_report_session,search_district)


