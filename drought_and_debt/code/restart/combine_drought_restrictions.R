### from MULLIN/RUBADO
#We also measure the adequacy of a water system’s existing supply by
#using its declaration—submitted to the TCEQ at the time of reporting usage
#restrictions—about its “level of concern,” or the number of days’ water supply the system has remaining. Reporting on water system supplies is incomplete, and the estimates only sometimes coincide with triggering criteria in
#drought contingency plans. The variable is a 5-point scale with values at
#resolved (all drought-related issues have been resolved),
#watch (greater than 180 days of water supply remaining), 
#concern (180 days or less), 
#priority (90 days or less),
#and emergency (could be out of water in 45 days or less).

library(rvest)
library(tidyverse)
library(pbapply)
library(lubridate)
library(data.table)
dr <- 'web.archive.org/web/'
fls <- list.files(dr,pattern = 'html',recursive = T,full.names = T)

old_restrictions = pblapply(fls,function(p) {#print(p);
  temp = read_html(p) %>% html_nodes('table') %>% html_table(trim=T,fill=T)
  if(length(temp)==1){tdf = temp[[1]]}
  if(length(temp)>1){tdf = temp[[4]]}
  if(!any(grepl('PWS ID',names(tdf)))){
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]}
  if(any(colnames(tdf)=='TCEQ Stage')){tdf = tdf %>% rename(Stage = `TCEQ Stage`)}
  if(any(colnames(tdf)=='Date Notified')){tdf = tdf %>% rename(Notified = `Date Notified`)}
  if(any(colnames(tdf)=='Last Updated')){tdf = tdf %>% rename(Notified = `Last Updated`)}
  tdf$file <- p
  tdf
},cl = 8)

rest_df <- rbindlist(old_restrictions,fill = T,use.names = T)
rest_df <- rest_df[order(-file),]
rest_df <- rest_df[!{rest_df %>% dplyr::select(-file) %>% duplicated(.)},]
library(lubridate)
rest_df$Notified <- mdy(rest_df$Notified)
rest_df <- rest_df[!is.na(Notified),]
rest_df$`PWS ID` <- as.integer(rest_df$`PWS ID`)
rest_df$`PWS ID` <- as.character(formatC(rest_df$`PWS ID`,width=7,flag = 0))
rest_df$Priority <- toupper(rest_df$Priority)

#https://www.tceq.texas.gov/drinkingwater/trot/droughtdic.html
rest_df <- rest_df %>% 
  mutate(Priority = case_when(
    Priority == 'W' ~ 'Watch',
    Priority == 'C' ~ 'Concern',
    Priority == 'P' ~ 'Priority',
    Priority == 'E' ~ 'Emergency',
    Priority == 'R' ~ 'Resolved',
    Priority == 'O' ~ 'Outage',
    .default = Priority))
rest_df$Priority_Numeric <- as.numeric(fct_relevel(rest_df$Priority,'Resolved','Watch','Concern','Priority','Emergency','Outage'))

rest_df$Stage<-toupper(rest_df$Stage)
rest_df <- rest_df %>% mutate(Stage = case_when(
  Stage == 'V' ~ 'Voluntary',
  Stage == '1' ~ 'Mild',
  Stage == '2' ~ 'Moderate',
  Stage == '3' ~ 'Severe',
  TRUE ~ 'Voluntary'
))
rest_df$Mandatory <- (rest_df$Stage != "Voluntary") + 0

saveRDS(rest_df,'drought_and_debt/input/combined_restriction_records.RDS')

