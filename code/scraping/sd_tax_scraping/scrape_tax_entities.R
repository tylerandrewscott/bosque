library(rvest)
tx_comp_county_dir = 'https://www.comptroller.texas.gov/taxes/property-tax/county-directory/' %>% 
  html_session() 
county_names = tx_comp_county_dir %>% html_nodes(css='.medium-2 a') %>% html_text()
entity_df = do.call(rbind,lapply(county_names,function(name) 
  data.frame(Gov_Entity = tx_comp_county_dir %>% follow_link(name) %>% html_nodes(css='.column .columns li') %>% html_text(),
             County_Loc = name)))

library(stringr)
library(tidyverse)
master_tax_entity = (entity_df %>% mutate(Gov_ID = str_extract(Gov_Entity,"^[0-9]{0,}\\-[0-9]{0,}\\-[0-9]{0,}"),
                          Gov_Entity = str_replace(Gov_Entity,"^[0-9]{0,}\\-[0-9]{0,}\\-[0-9]{0,} ",''),
                          County_Name = gsub('Dimmitt','Dimmit',paste0(str_replace(County_Loc,'^[0-9]{3} ',''),' County'))) %>% select(-County_Loc))


county_fips = read_csv('http://www2.census.gov/geo/docs/reference/codes/files/st48_tx_cou.txt',col_names = c('State','State_FIPS','County_FIPS','County_Name','X')) %>%
  select(-X) %>% mutate(FIPS = paste0(State_FIPS,County_FIPS)) %>% select(-State_FIPS,-County_FIPS)

master_tax_entity = left_join(master_tax_entity,county_fips)

write_csv(master_tax_entity,'input/texas_sd_data/tx_taxunits.csv')

tax_history_base = 'http://www.txcip.org/tac/census/districthist.php?FIPS='

temp = do.call(rbind,lapply(1:nrow(county_fips),function(x) paste0(tax_history_base,county_fips$FIPS[x]) %>% read_html() %>% html_node(css='table') %>% html_table(header = T) %>% 
         mutate(FIPS = county_fips$FIPS[x],County_Name = county_fips$County_Name[x])))

temp_long = temp %>% gather('Year','proptax_per100dollars',-`District ID`,-`Special District`,-County_Name,-FIPS) %>% mutate(proptax_per100dollars = as.numeric(gsub('\\$','',proptax_per100dollars)))

write_csv(temp_long,'input/texas_sd_data/tx_sd_tax_rates_2012_2017.csv')


