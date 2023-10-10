library(rvest)
library(pbapply)


starts = list( 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
               'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
               'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NTNC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems')

quers = sapply(starts,function(x) {
  nodes = x[[1]] %>% read_html() %>% html_nodes('a')
  sum_nodes = nodes[grep('DataSheet',nodes %>% html_attr('href'))] %>% html_attr('href')
  pref = 'https://dww2.tceq.texas.gov/DWW/JSP/'
  sum_nodes = gsub('\\s','',sum_nodes)
  q = paste0(pref,sum_nodes)
  q})

master_set <- data.table()
quers = unlist(quers)
qdt <- data.table(q = quers,grabbed = 0)
library(stringr)

albersNA = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
twd_boundaries <- st_read('spatial_inputs/Service_Area_Boundaries/PWS_shapefile/PWS_Export.shp')
twd_boundaries <- twd_boundaries %>% rename(PWS_ID = PWSId,PWS_NAME = pwsName)
twd_boundaries <- st_transform(twd_boundaries,st_crs(albersNA))
twd_boundaries <- st_make_valid(twd_boundaries)
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()

ids <- twd_boundaries$PWS_ID
storage <- data.table()
population <- data.table()

qdt <- qdt[str_extract(qdt$q,'TX[0-9]{7}') %in% ids,]
qdt <- qdt[!str_extract(qdt$q,'TX[0-9]{7}')  %in% population$PWS_ID,]
qdt <- qdt[!str_extract(qdt$q,'TX[0-9]{7}')  %in% storage$PWS_ID,]

list_result2 <- pblapply(qdt$q,function(q){
# index <- which(q==qdt$q)
nest <- list()
p <- str_extract(q,'TX[0-9]{7}')
tabs <- q %>% read_html() %>% html_nodes('td table')%>% html_table()
storage_i <- grep('Max Daily Demand',tabs)
if(length(storage_i)>0){
temp_storage <- as.data.table(tabs[[storage_i]][2:nrow(tabs[[storage_i]]),])
colnames(temp_storage) <- as.character(tabs[[storage_i]][1,])
temp_storage$PWS_ID <- p
#storage <- rbind(temp,storage,use.names = T,fill = T)
nest$storage <- temp_storage}

population_i <- grep('PopulationType',tabs)
if(length(population_i)>0){
  temp_population <- as.data.table(tabs[[population_i]][2:nrow(tabs[[population_i]]),])
  colnames(temp_population) <- as.character(tabs[[population_i]][1,])
  temp_population$PWS_ID <- p
  #population <- rbind(temp_population,population,use.names = T,fill = T)
  nest$population <- temp_population
}
nest
},cl = 6)

population <- rbindlist(lapply(list_result2,'[[','population'))
storage <- rbindlist(lapply(list_result2,'[[','storage'))
saveRDS(storage,file = 'drought_and_debt/input/pws_storage.RDS')
saveRDS(population,file = 'drought_and_debt/input/pws_population.RDS')

