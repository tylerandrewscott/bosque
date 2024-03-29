#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(stringr)
library(stringi)
#empty_df = read_csv('test.csv')

library(lubridate)
#slackr_setup(config_file = '../proj3/.slackr')

base_site = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=4%2F19%2F2015&end_date=4%2F19%2F2017&action=Search+For+Water+Systems'
#length(system_summary_urls)
prefix = 'https://dww2.tceq.texas.gov/DWW/JSP/'
page_links = base_site %>% read_html() %>% html_nodes('a') 
data_sheet_urls = page_links[grepl('Summary',page_links %>% html_text(trim=T))] %>% html_attr('href')


temp = mclapply(data_sheet_urls,function(x) 
{temp_tds =  gsub(' ','',paste0(prefix,x)) %>% read_html() %>% html_nodes('td');
data.frame(System = str_extract(x,'TX[0-9]{7}'),
           Position = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(trim=T)),
           NAME = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text(trim=T)))},
mc.cores=6,mc.cleanup=T)


while('try-error' %in% sapply(temp,class)){
  index = which(sapply(temp,class)=='try-error')
  for(i in index){
    print(i)
    temp_tds =  gsub(' ','',paste0(prefix,data_sheet_urls[i])) %>% read_html() %>% html_nodes('td');
    replacement = data.frame(System = str_extract(data_sheet_urls[i],'TX[0-9]{7}'),
               Position = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(trim=T)),
               NAME = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text(trim=T)))
    temp[[i]]<-replacement
  }
}



mutate_temp  = pblapply(temp,function(x){x%>% mutate(
                Position = stri_replace_all_regex(Position,"\n|\t|\r|&nbsp",""),
                NAME = stri_replace_all_regex(NAME,"\n|\t|\r|&nbsp","")) %>%
  filter(!is.na(NAME))})

poc = rbindlist(mutate_temp)

write_csv(poc,paste('input/texas_dww/personnel_records',paste0(Sys.Date(),'.csv'),sep='_'))

#slackr(print('dww scrape done'))


# 
# 
# head(temp)


operator_css_selector = 'table:nth-child(18)'
system_number_css = "#AutoNumber7 td:nth-child(1) a"
library(magrittr)


system_operators_table_list = pblapply(data_sheet_urls,function(x) {#print(x);
  Sys.sleep(0.05)
  gsub(' ','',paste0(prefix,x)) %>% read_html() %>% rvest::html_node(operator_css_selector) %>% 
    html_table() %>% mutate(System = str_extract(x,'TX[0-9]{7}'))},cl = 4)
  
system_operators_table_list <- system_operators_table_list[!sapply(system_operators_table_list,is.null)]
system_operators_df = do.call(rbind,system_operators_table_list[!sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])
 
 nosystem_operators_df = do.call(rbind,system_operators_table_list[sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])
 
 system_operators_df = cbind(system_operators_df[grepl('License',system_operators_df$X1),] %>% select(-X3,-X1) %>% rename(LICENSE_HOLDER = X2),
              system_operators_df[!grepl('License',system_operators_df$X1),] %>% select(-System) %>% rename(CLASSIFICATION = X2,LICENSE_NUM = X3))
 
system_operators_df = full_join(system_operators_df ,nosystem_operators_df %>% rename(LICENSE_HOLDER = X1))

write.csv(system_operators_df,paste('input/texas_dww/licensed_operators',paste0(Sys.Date(),'.csv'),sep='_'))
