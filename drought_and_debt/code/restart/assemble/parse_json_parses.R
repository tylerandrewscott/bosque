library(data.table)
library(stringr)
library(pbapply)
library(jsonlite)
mlist <- fread('input/texas_dww/district_master_list.csv')
mlist <- mlist[Type=='C']
dir <- 'input/texas_dww/html_responses/'
resp <- list.files(dir)
resp <- resp[str_remove(resp,'\\.json$') %in% mlist$`Water System No.`]

rlist <- pblapply(resp,function(r) {
  message <- read_json(paste0(dir,r))
  js <- message$choices[[1]]$message.content
  js_return <- tryCatch(fromJSON(js,flatten = T),error = function(e) NULL)
  return(js_return)
},cl = 5)
names(rlist) <- str_remove(resp,'\\.json$')
rlist <- rlist[!sapply(rlist,is.null)]

delist <- function(x) {
  x[sapply(x,is.vector)]
}

rlist2 <- lapply(rlist,delist)

spreadList <- function(x) {
  y <- unlist(x)
  y.names <- names(y)
  y.values <- as.vector(y)
  y.dt <- data.table(t(data.table(y)))
  colnames(y.dt) <- tolower(y.names)
  colnames(y.dt) <- str_replace_all(colnames(y.dt),'\\s','_')
  colnames(y.dt) <- str_extract(colnames(y.dt),'[^\\.]+$')
  return(y.dt)
}

dt.list <- lapply(rlist2,spreadList)
dt.list <- dt.list[sapply(dt.list,nrow)>0]
system.dt <- rbindlist(dt.list,use.names = T,fill = T)
if(nrow(system.dt)==length(dt.list)){
  system.dt$PWS_ID <- names(dt.list)
}else{
  stop(print('rows do not match up, something went wrong here'))
}

storage.dt <- system.dt[,intersect(grep('storage',colnames(system.dt),value = F),grep('elev',colnames(system.dt),invert = T)),with = F]
storage.dt$PWS_ID <- system.dt$PWS_ID
storage.melt <- melt(storage.dt,id.vars = 'PWS_ID')


storage.melt[grepl('GAL',toupper(storage.melt$value)),]

storage.melt[PWS_ID=='TX0030010',]
str_extract(storage.melt$value,'')

storage.melt$variable

[,sum(value,na.rm = T),by=.(PWS_ID,variable)][PWS_ID=='TX0010001'&V1>0,]



test <- storage.dt[, lapply(.SD, str_remove_all, '\\,'), .SDcols = names(storage.dt)]

storage.dt[which(system.dt$PWS_ID=='TX0030010'),]


storage.dt <- storage.dt[, lapply(.SD, function(x) as.numeric(str_remove_all(x, '[^0-9\\.]'))), .SDcols = names(storage.dt)]
gal_cols <- grep('gal',colnames(storage.dt),value=T)
storage.dt[, (gal_cols):=lapply(.SD, function(x) x / 1e6), .SDcols = gal_cols]
setnames(storage.dt, old = grep("gal", colnames(storage.dt), value = TRUE), new = gsub("gal", "mgd", grep("gal", colnames(storage.dt), value = TRUE)))
colnames(storage.dt) <- str_remove_all(colnames(storage.dt),"[^a-z]")




test <- storage.dt[, lapply(.SD, sum, na.rm = TRUE), by = PWS_ID, .SDcols = names(storage.dt)]



testdim(storage.dt)


library(tidyverse)
storage.dt <- tibble(storage.dt)

mill <- function(x) {x / 1e6}
storage.dt |> mutate(across(contains("gal"),~ mill(.x)))
?mutate_if
1000000 == 1e6
class(storage.dt)
grepl('\\,',storage.dt)

storage.dt

length(dt.list)
dim(system.dt)
length(rlist)
length(rlist2)
length(resp)



summary(as.numeric(storage.dt$`total_storage_(gal)`))
summary(as.numeric(storage.dt$`total_storage_gal`))

storage.dt$PWS_ID[!is.na(storage.dt$`total_storage_(gal)`)]
grep('total',colnames(storage.dt),value = T)
colMeans(is.na(storage.dt))[1:10]

dim(storage.dt)
dim(system.dt)

filter = c('codes','options','phone','fax','contacts','gps','depth','drill_date','treatment_sequence','treatment_plant','source_information\\.active_sources','entry_point','survey','source\\.sources','source_location','sources\\.sourcenumber','sources\\.active','active_sources\\.',
           'treatment','^sources\\.','_[0-9]$','code_explanations','contact',
           '^source_types\\.','^source_information','^sourcelocation')
rlist2[[1]]

sapply(rlist2,function(x) x[grepl('owner(_|)type$',na)])

sapply(rlist2,function(x) x[[c('Owner Type')]])
rlist2[[994]]

spreadList <- function(x) {
  y <- unlist(x)
  y.names <- names(y)
  y.values <- as.vector(y)
  y.dt <- data.table(t(data.table(y)))
  colnames(y.dt) <- tolower(y.names)
  colnames(y.dt) <- str_replace_all(colnames(y.dt),'\\s','_')
  colnames(y.dt) <- str_remove(colnames(y.dt),'tceq_summary_sheet\\.')
  colnames(y.dt) <- str_remove(colnames(y.dt),'^waterdistrict\\.')
  colnames(y.dt) <- str_remove(colnames(y.dt),'^water_district\\.')
  colnames(y.dt) <- str_remove(colnames(y.dt),'^district\\.')
  y.dt <- y.dt[,!grepl(paste(filter,collapse = '|'),colnames(y.dt)),with = F]
  colnames(y.dt) <- str_remove_all(colnames(y.dt),'[^a-z]')
  colnames(y.dt)[grepl('ownertype$',colnames(y.dt))] <- 'ownertype'
  return(y.dt)
}

dt.list <- lapply(rlist2,spreadList)
system.dt <- rbindlist(dt.list,use.names = T,fill = T)

summary(sapply(dt.list,ncol))
dt.list[sapply(dt.list,ncol)==max(sapply(dt.list,ncol))]



test <- system.dt[is.na(ownertype),]

table(is.na(system.dt$ownertype))
colMeans(!is.na(test[,grepl('owner',colnames(test)),with = F]))

system.dt[,.(owner_type)]

url = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=A&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=1%2F30%2F2023&end_date=1%2F30%2F2025&action=Search+For+Water+Systems'
library(rvest)
test = url |> read_html() |> html_nodes(css = '#AutoNumber7 td:nth-child(1) a') |> html_text(trim = T)

test2 <- test[!test %in% system.dt$pws_id]




system.dt$system_type <- tolower(system.dt$system_type)
system.dt <- system.dt[!is.na(system.dt$pws_id),]

system.dt[system_type %in% c('community','c - community','com'),]



table(system.dt$system_type)
grep('pws',colnames(system.dt),value = T)
dim(system.dt)
system.dt[,colMeans(!is.na(system.dt)) > 0.2,with = F]

table(test %in% str_remove(resp,'\\.json$'))


system.dt[is.na(population_served) & !is.na(population_served.population),1:10][1:10,]
grep('population',colnames(system.dt[is.na(population_served)&!is.na(pws_id),]),value = T)[1:10]
system.dt[!is.na(population.type),grepl('population',colnames(system.dt)),with = F]
head(system.dt)
dt.list[[1]]
rlist[[1]][sapply(rlist[[1]],is.vector)]
rlist[[1]] %>% select_if(.predicate = is.vector)
as.data.table(rlist[[1]])

sapply(rlist[[1]],is.vector)
rlist <- lapply(rlist,function(x){ifelse(is.data.frame(x),x,x[!{sapply(x,is.data.frame)|sapply(x,is.list)}])})

str(rlist)

