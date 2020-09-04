
library(rvest)
library(tidyverse)
library(pbapply)
library(lubridate)

old_page = list.files('input/tceq_drought_webpages',full.names = T)
old_restrictions = pblapply(old_page,function(p) {print(p);
  temp = read_html(p) %>% html_nodes('table') %>% html_table(trim=T,fill=T)
  if(length(temp)==1){tdf = temp[[1]]}
  if(length(temp)>1){tdf = temp[[4]]}
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(any(colnames(tdf)=='TCEQ Stage')){tdf = tdf %>% rename(Stage = `TCEQ Stage`)}
  if(any(colnames(tdf)=='Date Notified')){tdf = tdf %>% rename(Notified = `Date Notified`)}
  if(any(colnames(tdf)=='Last Updated')){tdf = tdf %>% rename(Notified = `Last Updated`)}
  tdf
},cl = 16)

old_restrictions <- lapply(seq_along(old_restrictions),function(x) old_restrictions[[x]] %>% 
                             mutate(file = old_page[x]))
rest_df = Reduce(plyr::rbind.fill,old_restrictions)

lapply(old_restrictions,colnames)
rest_df$Population = as.numeric(rest_df$Population)
rest_df$Connections = as.numeric(rest_df$Connections)
rest_df$`PWS ID` = formatC(rest_df$`PWS ID`,width=max(nchar(rest_df$`PWS ID`)),flag=0)
rest_df$Obs_Date = ymd(str_extract(rest_df$file,"[0-9]{8}"))


new_restrictions = lapply(list.files('input/texas_dww/',pattern = 'system_water_restrictions',full.names = T),function(x) read_csv(x) %>%
         mutate(file = x))
rest_df2 = Reduce(plyr::rbind.fill,new_restrictions)
rest_df2$Obs_Date = ymd(str_extract(rest_df2$file,'[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}'))
rest_df2$`PWS ID` = formatC(rest_df2$`PWS ID`,width=max(nchar(rest_df2$`PWS ID`)),flag=0)
rest_df2 = rest_df2 %>% rename(Stage = `TCEQ Stage`,Notified = `Date Notified`)

rest_all = plyr::rbind.fill(rest_df,rest_df2)
rest_all = rest_all %>% rename(PWS_ID = `PWS ID`,PWS_NAME = `PWS Name`)

write_csv(x = rest_all,path = 'input/combined_drought_records.csv')
