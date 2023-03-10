library(rvest)
library(parallel)
library(tidyverse)
library(stringr)
library(stringi)
all_systems_page = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=A&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=5%2F13%2F2015&end_date=5%2F13%2F2017&action=Search+For+Water+Systems'
all_links = all_systems_page %>% read_html() %>% html_nodes('a') 
system_summary_links = all_links[grepl('^TX[0-9]',all_links %>% html_text(trim=T))] %>% html_attr('href')


system_summary_links = system_summary_links
prefix = 'https://dww2.tceq.texas.gov/DWW/JSP/'


temp_results = do.call(rbind,mclapply(system_summary_links,function(x) {
  temp_nodes = URLencode(paste0(prefix,x)) %>% read_html() %>% html_nodes('td')
  cbind(temp_nodes[which({temp_nodes %>% html_attr('width')} == '100%' & !is.na(temp_nodes %>% html_attr('bgcolor')))] %>% html_text(trim=T),str_extract(x,'TX[0-9]{7}'))
},mc.cores = 5,mc.cleanup = T))


temp_df = as_tibble(temp_results)  %>% 
  rename(Transfer = V1, Head = V2) %>% filter(Transfer != 'No Buyers') %>%
  mutate(Transfer = stri_replace_all_regex(Transfer,"\n|\t|\r|&nbsp",""))

purchasing = temp_df %>% filter(grepl('buys from',Transfer))
selling = temp_df %>% filter(grepl('sells to',Transfer))
transfer_lists = lapply(selling$Transfer,function(x) str_extract_all(x,'(?:TX[0-9]{7}.*?)(TX[0-9]{7})[^TX]+'))
transfers = unlist(lapply(transfer_lists,unlist))
sale_df = tibble(Seller = str_extract(transfers,'^TX[0-9]{7}'),
           Buyer = str_extract(transfers, '(?!^TX[0-9]{7}.*)(TX[0-9]{7})'),
           Sale_Type = str_extract(gsub(' {1,}$','',transfers),'[A-Z]$'),
           Buyer_Service_Pop = str_extract(str_extract(transfers,'(?:\\/).*(?:\\/)'),'[0-9]{1,}'))
sale_df = sale_df[!duplicated(sale_df),]


purchase_lists = lapply(purchasing$Transfer,function(x) str_extract_all(x,'(TX[0-9]{7}.*?)(TX[0-9]{7})*Water'))
purchases = unlist(lapply(purchase_lists,unlist))

purchase_df = tibble(Buyer = str_extract(purchases,'^TX[0-9]{7}'),
       Seller = str_extract(purchases,'(?!^TX[0-9]{7}.*)(TX[0-9]{7})'),
       Water_Type = gsub('providing ','',str_extract(purchases,'providing.*')))
purchase_df = purchase_df[!duplicated(purchase_df)&!is.na(purchase_df$Seller),]

write_csv(purchase_df,file = paste('input/texas_dww/purchasing_connections',paste0(Sys.Date(),'.csv'),sep='_'))
write_csv(sale_df, file = paste('input/texas_dww/sales_connections',paste0(Sys.Date(),'.csv'),sep='_'))
