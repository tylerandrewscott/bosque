#$ sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0
#$ sudo docker ps
library(parallel)
library(rvest)
library(tidyverse)
library(magrittr)
library(RSelenium)
#empty_df = read_csv('test.csv')
temp = read_csv('input/tceq_audits/private_entity_financial_report_reference_list.csv')

table_css = 'td.iwud'


test = Reduce(full_join,mclapply(1:nrow(temp),function(x) temp$DOC_URL[x] %>% read_html() %>% rvest::html_nodes(table_css) %>% html_text(trim = T) %>%
    matrix(.,nrow = length(.)/2,byrow = T) %>% as.data.frame() %>% spread(V1,V2) %>% 
      mutate(PWS_NAME = temp$`District Name`[x],PWS_ID = temp$SITE_ID[x],DOC_ID = temp$DOC_ID[x],DOC_URL = temp$DOC_URL[x]),
    mc.cores=8,mc.cleanup = T))

write_csv(test,'input/tceq_audits/private_entity_financial_reports_4-24-17.csv')
