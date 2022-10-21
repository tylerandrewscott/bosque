
remote  = TRUE
library(RCurl)
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(rjson)
library(knitr)

library(httr)
if(remote)
{echo_viols  = fread('../../Input/epa_echo/echo_query_8-25-16.csv',stringsAsFactors = F)}
if(!remote)
{echo_viols  = fread('Input/epa_echo/echo_query_8-25-16.csv',stringsAsFactors = F)}
echo_viols[(matrix(rep(grepl('^vio',colnames(echo_viols)),each=nrow(echo_viols)),byrow = F,ncol=ncol(echo_viols)) & is.na(echo_viols))] = 0
echo_viols$ifea[is.na(echo_viols$ifea)] = 0
echo_viols$feas[is.na(echo_viols$feas)] = 0
link_base = "https://ofmpub.epa.gov/echo/dfr_rest_services.get_sdwis_compliance?output=json&p_id="
echo_viols = echo_viols %>% filter(owner_desc == 'Local government')

status_code = list()
query_list = list()
system_list = list()

for (sys in unique(echo_viols$pwsid))
{
  print(sys)
  url <- paste0(link_base,sys)
  try(req <- httr::GET(url))
  status_code <- append(status_code,req$status_code)
  if(req$status_code==200)
  {
    json <- httr::content(req, as = "text")
    comp_data = fromJSON(json)
    query_list <<- append(query_list,comp_data)
  }
  else{query_list <<- append(query_list,'Error')}
  system_list <- append(system_list,sys)
  Sys.sleep(time=10)
}



##SAVE TEMPORARY IMAGE
# save.image('echo_query_api_results.RData')


###GO BACK THROUGH AND FIND PRIOR ERROR CODES, TRY AGAIN
# rm(list=ls())
# load('Code/scraping/echo_query_api_results.RData')
# 
# 
# for (i in 1:length(system_list))
# {
#   if(unlist(status_code)[i] == 503)
#   {
#     sys = unique(echo_viols$pwsid)[i]
#     url <- paste0(link_base,sys)
#     try(req <- httr::GET(url))
#     status_code[[i]] <- req$status_code
#     if(req$status_code==200)
#     {
#       json <- httr::content(req, as = "text")
#       comp_data = fromJSON(json)
#       query_list[[i]] <- comp_data
#     }
#     else{query_list <<- append(query_list,'Error')}
#     #system_list <- system_list,sys)
#     Sys.sleep(time=10)
#   }
# }

                  
                            
temp_df_l = lapply(query_list,function(x) as.data.frame(x$SDWISCompliance$Sources[[1]]$Status))

temp_df_l[lapply(temp_df_l,nrow)==0] = lapply(which(lapply(temp_df_l,nrow)==0),function(i)
  as.data.frame(t(as.data.frame(query_list[[i]]$Results$SDWISCompliance$Sources[[1]]$Status))))


temp_df = do.call(rbind,temp_df_l)


temp_list = lapply(query_list,function(x) do.call(rbind,lapply(x$SDWISCompliance$Sources[[1]]$RulesViolated,as.data.frame)))

indiv_viols = do.call(rbind,temp_list)

long_df = (temp_df %>% tidyr::gather(Quarter,Status,-SourceID))
long_df$Quarter = gsub('Status','',long_df$Quarter)

start_dates = as.data.frame(t(as.data.frame(query_list[[1]]$SDWISCompliance[grepl('Start$',names(query_list[[1]]$SDWISCompliance))])))
end_dates = as.data.frame(t(as.data.frame(query_list[[100]]$SDWISCompliance[grepl('End$',names(query_list[[1]]$SDWISCompliance))])))
names(end_dates) = 'Quarter_End'
names(start_dates) = 'Quarter_Start'
start_dates$Quarter = gsub('Start','',rownames(start_dates))
end_dates$Quarter = gsub('End','',rownames(end_dates))

long_df = left_join(long_df,left_join(start_dates,end_dates))
write.csv(x = long_df,'Input/epa_echo/quarter_compliance_l3yr.csv')


indiv_viols = (indiv_viols %>% gather(Quarter,Status,-DrinkingWaterRule,-SourceID) %>% mutate(Quarter = gsub('Status','',Quarter)))
indiv_viols = left_join(indiv_viols,left_join(start_dates,end_dates))

write.csv(x = indiv_viols,'Input/epa_echo/rule_compliance_l3yr.csv')
