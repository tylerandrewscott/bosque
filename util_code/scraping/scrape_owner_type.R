scratch_loc = 'scratch/'

starts = list( 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NTNC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems')


css_owner = "table:nth-child(21) tr+ tr td"
css_cn = "caption+ tbody td"
css_rn = "p+ table tr+ tr td"
css_flow = "table:nth-child(31) td"
css_inter = "table:nth-child(28) td"
library(rvest)
library(tidyverse)
library(pbapply)
quers = sapply(starts,function(x) {
  nodes = x[[1]] %>% read_html() %>% html_nodes('a')
sum_nodes = nodes[grep('DataSheet',nodes %>% html_attr('href'))] %>% html_attr('href')
pref = 'https://dww2.tceq.texas.gov/DWW/JSP/'
sum_nodes = gsub('\\s','',sum_nodes)
q = paste0(pref,sum_nodes)
q})

quers = unlist(quers)

owner_list = pblapply(seq_along(quers),function(x) {
  print(x)
rm(list = grep('temp',ls(),value=T))
tryCatch({
base_page = quers[x] %>% read_html() 
flow_temp = base_page %>% html_nodes(css_flow) %>% html_text(trim=T) %>% matrix(.,nrow=2,byrow = T)
colnames(flow_temp) <- flow_temp[1,]
flow_temp = flow_temp[-1,]
inter_temp = base_page %>% html_nodes(css_inter) %>% html_text(trim=T) #%>% matrix(.,nrow=2,byrow = T)
table_nodes = base_page %>% html_nodes('table')
table_nodes = table_nodes[!sapply(sapply(seq_along(table_nodes),function(t) tryCatch({html_table(table_nodes[[t]],fill=T,trim=T)},error = function(e) NULL)),is.null)]
table_list = html_table(table_nodes,trim=T,fill=T)
inter_temp = table_list[[which(grepl('w/other PWS',table_list,fixed = T ))[2]]][,-5]
colnames(inter_temp) <- inter_temp[1,]
inter_temp = inter_temp[-1,]
if(nrow(inter_temp)>0){pop_temp = inter_temp %>% dplyr::select(-`# ofConnect`,-`# I/Cw/other PWS`) %>% spread(PopulationType,PopulationServed,sep = '_Population_')
sc_temp = inter_temp %>% dplyr::select(-PopulationServed,-`# I/Cw/other PWS`) %>% spread( PopulationType,`# ofConnect`,sep = '_ServiceConnections_')
}else{pop_temp = NA;sc_temp=NA}
interconnects_temp = sum(as.numeric(inter_temp$`# I/Cw/other PWS`))
cn_temp = base_page %>% html_nodes(css_cn) %>% html_text(trim=T) %>% matrix(.,ncol=2,byrow=T) %>% .[-1,] %>% matrix(.,ncol=2) %>% data.frame(.,stringsAsFactors = F) %>% mutate(CN_ID = paste('CN',1:nrow(.),sep='_'))
org_cn_temp = cn_temp %>% dplyr::select(-X2) %>% spread(CN_ID,X1)
colnames(org_cn_temp) = gsub('CN_','CN_ORG_',colnames(org_cn_temp))
id_cn_temp = cn_temp %>% dplyr::select(-X1) %>% spread(CN_ID,X2)
cn_bind_temp = cbind(org_cn_temp,id_cn_temp)
df = data.frame(PWS_ID = str_extract(quers[x],'TX[0-9]{1,}'),
cbind(matrix(base_page %>% html_nodes(css_owner) %>% html_text(trim=T) ,nrow = 1,dimnames = list(x,'Owner_Type')),
matrix(base_page %>% html_nodes(css_rn) %>% html_text(trim=T) ,nrow=1,dimnames = list(x,c('PWS_ID_EX','PWS_NAME','RN'))),
rbind(flow_temp),
cn_bind_temp,
pop_temp,sc_temp),
Interconnections = interconnects_temp,
stringsAsFactors = F)
df})
},cl = 5)


while(any(sapply(owner_list,class)=='try-error')){
  index = which(sapply(owner_list,class)=='try-error')
  for(x in index){
  rm(list = grep('temp',ls(),value=T))
  tryCatch({
    base_page = quers[x] %>% read_html() 
    flow_temp = base_page %>% html_nodes(css_flow) %>% html_text(trim=T) %>% matrix(.,nrow=2,byrow = T)
    colnames(flow_temp) <- flow_temp[1,]
    flow_temp = flow_temp[-1,]
    inter_temp = base_page %>% html_nodes(css_inter) %>% html_text(trim=T) #%>% matrix(.,nrow=2,byrow = T)
    table_nodes = base_page %>% html_nodes('table')
    table_nodes = table_nodes[!sapply(sapply(seq_along(table_nodes),function(t) tryCatch({html_table(table_nodes[[t]],fill=T,trim=T)},error = function(e) NULL)),is.null)]
    table_list = html_table(table_nodes,trim=T,fill=T)
    inter_temp = table_list[[which(grepl('w/other PWS',table_list,fixed = T ))[2]]][,-5]
    colnames(inter_temp) <- inter_temp[1,]
    inter_temp = inter_temp[-1,]
    if(nrow(inter_temp)>0){pop_temp = inter_temp %>% dplyr::select(-`# ofConnect`,-`# I/Cw/other PWS`) %>% spread(PopulationType,PopulationServed,sep = '_Population_')
    sc_temp = inter_temp %>% dplyr::select(-PopulationServed,-`# I/Cw/other PWS`) %>% spread( PopulationType,`# ofConnect`,sep = '_ServiceConnections_')
    }else{pop_temp = NA;sc_temp=NA}
    interconnects_temp = sum(as.numeric(inter_temp$`# I/Cw/other PWS`))
    cn_temp = base_page %>% html_nodes(css_cn) %>% html_text(trim=T) %>% matrix(.,ncol=2,byrow=T) %>% .[-1,] %>% matrix(.,ncol=2) %>% data.frame(.,stringsAsFactors = F) %>% mutate(CN_ID = paste('CN',1:nrow(.),sep='_'))
    org_cn_temp = cn_temp %>% dplyr::select(-X2) %>% spread(CN_ID,X1)
    colnames(org_cn_temp) = gsub('CN_','CN_ORG_',colnames(org_cn_temp))
    id_cn_temp = cn_temp %>% dplyr::select(-X1) %>% spread(CN_ID,X2)
    cn_bind_temp = cbind(org_cn_temp,id_cn_temp)
    df = data.frame(PWS_ID = str_extract(quers[x],'TX[0-9]{1,}'),
                    cbind(matrix(base_page %>% html_nodes(css_owner) %>% html_text(trim=T) ,nrow = 1,dimnames = list(x,'Owner_Type')),
                          matrix(base_page %>% html_nodes(css_rn) %>% html_text(trim=T) ,nrow=1,dimnames = list(x,c('PWS_ID_EX','PWS_NAME','RN'))),
                          rbind(flow_temp),
                          cn_bind_temp,
                          pop_temp,sc_temp),
                    Interconnections = interconnects_temp,
                    stringsAsFactors = F)
    owner_list[[x]]<-df
    })
}}


owner_df = do.call(plyr::rbind.fill,owner_list[sapply(owner_list,class)!='try-error'])
owner_df$Total_Storage_Units = str_extract(owner_df$TotalStorage.MG.,'[A-Z]{1,}')
owner_df$Total_Storage_MG = as.numeric(gsub(' [A-Z]{1,}$','',owner_df$TotalStorage.MG.))
owner_df$Total_Storage_MG = ifelse(is.na(owner_df$Total_Storage_MG ),NA,ifelse(owner_df$Total_Storage_Units=='MG',owner_df$Total_Storage_MG ,owner_df$Total_Storage_MG/1000000))

owner_df$Total_Production_Units = str_extract(owner_df$TotalProduct.MGD.,'[A-Z]{1,}')
owner_df$Total_Product_MGD = as.numeric(gsub(' [A-Z]{1,}$','',owner_df$TotalProduct.MGD.))
owner_df$Total_Product_MGD = ifelse(is.na(owner_df$Total_Product_MGD),NA,ifelse(owner_df$Total_Production_Units=='MG',owner_df$Total_Product_MGD,owner_df$Total_Product_MGD/1000000))

owner_df$Avg_Daily_Consumption_Units = str_extract(owner_df$AverageDailyConsump.,'[A-Z]{1,}')
owner_df$Average_Daily_Consump_MGD = as.numeric(gsub(' [A-Z]{1,}$','',owner_df$AverageDailyConsump.))
owner_df$Avg_Daily_Consumption_Units[is.na(owner_df$Average_Daily_Consump_MGD)] <- NA
owner_df$Average_Daily_Consump_MGD[!is.na(owner_df$Average_Daily_Consump_MGD)&owner_df$Avg_Daily_Consumption_Units == 'GPM'] <-
owner_df$Average_Daily_Consump_MGD[!is.na(owner_df$Average_Daily_Consump_MGD)&owner_df$Avg_Daily_Consumption_Units == 'GPM']  * 1440  / 1000000 

# extra = lapply(which(sapply(owner_list,class)=='try-error'),function(x){
#   own = tryCatch({quers[x] %>% read_html() %>% html_nodes(css_owner) %>% html_text(trim=T)},error=function(e) NULL)
#   if(!is.null(own)){
#     data.frame(PWS_ID = str_extract(quers[x],'TX[0-9]{1,}'),owner_type = own,stringsAsFactors = F)}})
# owner_df = rbind(owner_df,do.call(rbind,extra))

saveRDS(owner_df,paste0(paste0('scraped_input/pws_details/pws_details_',Sys.Date(),'.RDS')))




