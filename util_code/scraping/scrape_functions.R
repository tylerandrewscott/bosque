library(tidyverse)
function_css = 'table:nth-child(17) .iwud'
function_css2 = "table:nth-child(15) .iwud"

search_urls = read_csv('input/tceq_audits/master_district_reference.csv')

in_hout = read_csv('input/tceq_audits/district_info.csv') %>% 
  filter(`Activity Status:`=='ACTIVE') %>%
  filter(`Primary County:` %in% c('HARRIS','FORT BEND','BRAZORIA','MONTGOMERY','LIBERTY','WALLER','CHAMBERS','AUSTIN','GALVESTON'))

search_urls = search_urls %>% filter(SYSTEM_ID %in% in_hout$SYSTEM_ID)


library(rvest)
library(parallel)
functions_list = mclapply(search_urls$URL,function(url) {
url %>% read_html()  %>%  html_nodes(css=function_css) %>% html_text(trim=T)},mc.cores=8,mc.cleanup=T)
url_list = lapply(as.list(search_urls$URL),function(x) data.frame(URL = x))

function_df_list = lapply(which(sapply(functions_list,length)!=0 & sapply(functions_list,length) %% 2 ==0),function(x) {functions_list[[x]] %>% 
                matrix(.,ncol=2,byrow = T) %>% data.frame(.) %>% rename(Power = X1, Date = X2) %>%
    filter(!duplicated(Power)) %>%
         spread(Power,Date) %>% mutate(URL = url_list[[x]])})

temp = lapply(function_df_list,function(x) data.frame(x %>% select(-URL),URL = x$URL))

temp = invisible(Reduce(full_join,temp))

district_functions = left_join(temp,search_urls %>% select(-X1) %>% filter(!duplicated(.)))

write_csv(district_functions,path = 'input/tceq_audits/district_functions.csv')


