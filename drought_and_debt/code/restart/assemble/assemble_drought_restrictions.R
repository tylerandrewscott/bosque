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


library(reReg)
library(data.table)
library(readxl)
library(tidyverse)
tceq_file <- 'input/TCEQ_FOIA/PIR 98118_Copy_Drought_Database_Reported_MASTER.xlsx'
ntc <- read_excel(tceq_file,sheet = 'Master')
warnings()
ntc <- data.table(ntc)
ntc$`PWS ID` <- paste0('TX',formatC(ntc$`PWS ID`,width = 7,flag = "0",format = 'd'))
ntc$Notified <- str_remove(ntc$Notified,'\\s.*')
ntc$Notified_ymd <- ymd(ntc$Notified)
colnames(ntc) <- toupper(colnames(ntc))

library(stringr)
#https://github.com/tidyverse/readxl/issues/716
# a function that takes a character vector that may contain dates in various formats, and attempts to convert each format to a date value appropriately
convert_excel_dates <-
  function(x){
    case_when(
      str_detect(x, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$") ~ mdy(x),   # handles values imported as text values in the format "MM/DD/YYYY"
      str_detect(x, "^[0-9]{5}$")                         ~ x |> as.integer() |> as.Date(origin = as.Date("1899-12-30")),  # handles values imported as numbers expressed as days since 1899-12-30 (Microsoft's convention)
      TRUE                                                ~ NA_Date_  # default case, no applicable date format, returns a missing date value
    )
  }
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
sheets <- grep('[0-9]{4}',readxl::excel_sheets(tceq_file),value = T)
sheet_list <- lapply(sheets,function(x) {
  print(x)
  temp <- read_excel(tceq_file, col_types = "text",sheet = x)
  colnames(temp) <- toupper(colnames(temp))
  colnames(temp)[colnames(temp)%in%c('PUBLIC WATER SYSTEM SEVEN-DIGIT ID NUMBER')] <- "PWS ID"   
  colnames(temp)[colnames(temp)=="TODAY'S DATE"] <- "NOTIFIED"
  
  temp <- temp |>  mutate(NOTIFIED = as.character(round(as.numeric(NOTIFIED)))) |>
    mutate(NOTIFIED = convert_excel_dates(NOTIFIED))
  temp$NOTIFIED_YMD <- ymd(temp$NOTIFIED)
  temp <- temp |> select(-NOTIFIED)
  temp$`PWS ID` <- ifelse(grepl('TX',temp$`PWS ID`),temp$`PWS ID`,paste0('TX',formatC(as.numeric(temp$`PWS ID`),width = 7,format = 'd',flag = '0')))
  temp
})

sheet_dt <- rbindlist(sheet_list,fill = T,use.names = T)
sheet_dt <- sheet_dt[!is.na(`PWS ID`),]

ntc$YEAR <- as.character(ntc$YEAR)
ntc$ID <- paste(ntc$`PWS ID`,ntc$NOTIFIED_YMD,sep = '_')
ntc[,NOTIFIED:=NULL]
sheet_dt$ID <- paste(sheet_dt$`PWS ID`,sheet_dt$NOTIFIED_YMD,sep = '_')

sheet_dt <- sheet_dt[!is.na(NOTIFIED_YMD),]

ntc2 <- (full_join(ntc,sheet_dt[!sheet_dt$ID %in% ntc$ID,]))

ntc2 <- ntc2 |> filter(is.na(`IMPLEMENTING/CHANGING/RESCINDING`) | `IMPLEMENTING/CHANGING/RESCINDING` != 'Rescinding') |>
  filter(is.na(`DROUGHT/MECHANICAL/BOTH`) | `DROUGHT/MECHANICAL/BOTH` != 'Mechanical') |>
  filter(STAGE!='NA - Resolved')

ntcM <- ntc2[STAGE %in% c("M1","M2",'M3')]

saveRDS(ntcM,file = 'drought_and_debt/input/combined_restriction_records.RDS')
# library(rvest)
# library(tidyverse)
# library(pbapply)
# library(lubridate)
# library(data.table)
# ### older location
# #dr <- 'web.archive.org/web/'
# dr <- 'util_code/scraping/dol-wayback/'
# fls <- list.files(dr,pattern = 'html',recursive = T,full.names = T)
# #### skip empty files
# flsize <- file.size(fls)
# fls <- fls[flsize>0]
# 
# 
# old_restrictions = pblapply(fls,function(p) {
#   #print(p)
#   temp = read_html(p) %>% html_nodes('table') %>% html_table(trim=T,fill=T)
#   if(length(temp)==1){tdf = temp[[1]]}
#   if(length(temp)>1){tdf = temp[[4]]}
#   if(!any(grepl('PWS ID',names(tdf)))){
#   colnames(tdf) <- tdf[1,]
#   tdf = tdf[-1,]}
#   if(any(colnames(tdf)=='TCEQ Stage')){tdf = tdf %>% rename(Stage = `TCEQ Stage`)}
#   if(any(colnames(tdf)=='Date Notified')){tdf = tdf %>% rename(Notified = `Date Notified`)}
#   if(any(colnames(tdf)=='Last Updated')){tdf = tdf %>% rename(Notified = `Last Updated`)}
#   tdf$file <- p
#   tdf},cl = 8)
# 
# rest_df <- rbindlist(old_restrictions,fill = T,use.names = T)
# rest_df <- rest_df[order(-file),]
# rest_df <- rest_df[!{rest_df %>% dplyr::select(-file) %>% duplicated(.)},]
# library(lubridate)
# rest_df$Notified <- mdy(rest_df$Notified)
# rest_df <- rest_df[!is.na(Notified),]
# rest_df$`PWS ID` <- as.integer(rest_df$`PWS ID`)
# rest_df$`PWS ID` <- as.character(formatC(rest_df$`PWS ID`,width=7,flag = 0))
# rest_df$Priority <- toupper(rest_df$Priority)
# 
# #https://www.tceq.texas.gov/drinkingwater/trot/droughtdic.html
# rest_df <- rest_df %>% 
#   mutate(Priority = case_when(
#     Priority %in% c('W','GREATER THAN 180-DAY SUPPLY') ~ 'Watch',
#     Priority %in% c('C','LESS THAN 180-DAY SUPPLY') ~ 'Concern',
#     Priority == 'P' ~ 'Priority',
#     Priority == 'E' ~ 'Emergency',
#     Priority == 'R' ~ 'Resolved',
#     Priority == 'O' ~ 'Outage',
#     .default = Priority))
# 
# table(rest_df$Priority)
# rest_df$Priority_Numeric <- as.numeric(fct_relevel(rest_df$Priority,'Resolved','Watch','Concern','Priority','Emergency','Outage'))
# 
# rest_df$Stage<-toupper(rest_df$Stage)
# rest_df <- rest_df %>% mutate(Stage = case_when(
#   Stage == 'V' ~ 'Voluntary',
#   Stage == '1' ~ 'Mild',
#   Stage == '2' ~ 'Moderate',
#   Stage == '3' ~ 'Severe',
#   TRUE ~ 'Voluntary'
# ))
# rest_df$Mandatory <- (rest_df$Stage != "Voluntary") + 0
# 
# saveRDS(rest_df,'drought_and_debt/input/combined_restriction_records.RDS')
# 
# table(year(test$Notified))
