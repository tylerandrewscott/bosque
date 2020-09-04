#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(stringr)
library(stringi)
#empty_df = read_csv('test.csv')
library(slackr)
library(lubridate)
#slackr_setup(config_file = '../proj3/.slackr')

base_site = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=4%2F19%2F2015&end_date=4%2F19%2F2017&action=Search+For+Water+Systems'
#length(system_summary_urls)
prefix = 'https://dww2.tceq.texas.gov/DWW/JSP/'
page_links = base_site %>% read_html() %>% html_nodes('a') 
data_sheet_urls = page_links[grepl('Summary',page_links %>% html_text(trim=T))] %>% html_attr('href')

temp = do.call(rbind,mclapply(data_sheet_urls,function(x) 
{temp_tds =  gsub(' ','',paste0(prefix,x)) %>% read_html() %>% html_nodes('td');
data.frame(System = str_extract(x,'TX[0-9]{7}'),
           Position = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '25%'] %>% html_text(trim=T)),
           NAME = as.character(temp_tds[!is.na(temp_tds %>% html_attr('width')) & (temp_tds %>% html_attr('width')) == '35%'] %>% html_text(trim=T)))},
mc.cores=4,mc.cleanup=T))

poc = temp %>% mutate(
                Position = stri_replace_all_regex(Position,"\n|\t|\r|&nbsp",""),
                NAME = stri_replace_all_regex(NAME,"\n|\t|\r|&nbsp","")) %>%
  filter(!is.na(NAME))


write_csv(poc,paste('input/texas_dww/personnel_records',paste0(Sys.Date(),'.csv'),sep='_'))

#slackr(print('dww scrape done'))


# 
# 
# head(temp)


operator_css_selector = 'table:nth-child(18)'
system_number_css = "#AutoNumber7 td:nth-child(1) a"
library(magrittr)

system_operators_table_list = mclapply(data_sheet_urls,function(x) 
  gsub(' ','',paste0(prefix,x)) %>% read_html() %>% rvest::html_node(operator_css_selector) %>% 
    html_table() %>% mutate(System = str_extract(x,'TX[0-9]{7}')),mc.cores=4,mc.cleanup=T)


 system_operators_df = do.call(rbind,system_operators_table_list[!sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])
 
 nosystem_operators_df = do.call(rbind,system_operators_table_list[sapply(system_operators_table_list,function(x) x$X1[1]=='No Licensing Data for this PWS')])
 
 system_operators_df = cbind(system_operators_df[grepl('License',system_operators_df$X1),] %>% select(-X3,-X1) %>% rename(LICENSE_HOLDER = X2),
              system_operators_df[!grepl('License',system_operators_df$X1),] %>% select(-System) %>% rename(CLASSIFICATION = X2,LICENSE_NUM = X3))
 
system_operators_df = full_join(system_operators_df ,nosystem_operators_df %>% rename(LICENSE_HOLDER = X1))

write.csv(system_operators_df,paste('input/texas_dww/licensed_operators',paste0(Sys.Date(),'.csv'),sep='_'))

drought_site = 'https://www.tceq.texas.gov/drinkingwater/trot/droughtw.html'
dtable = drought_site %>% read_html() %>% html_table(fill=T,header=T)
write_csv(dtable[[1]],paste('input/texas_dww/system_water_restrictions',paste0(Sys.Date(),'.csv'),sep='_'))
