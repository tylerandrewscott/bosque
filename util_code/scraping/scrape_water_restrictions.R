library(parallel)
library(rvest)
library(tidyverse)
library(stringr)
library(stringi)
drought_site = 'https://www.tceq.texas.gov/drinkingwater/trot/droughtw.html'
dtable = drought_site %>% read_html() %>% html_table(fill=T,header=T)

table(year(mdy(dtable[[1]]$`Date Notified`)))
write_csv(dtable[[1]],paste('input/texas_dww/system_water_restrictions',paste0(Sys.Date(),'.csv'),sep='_'))
