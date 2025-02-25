library(data.table)
library(stringr)
library(pbapply)
library(jsonlite)
library(rvest)
mlist <- fread('input/texas_dww/district_master_list.csv')
mlist <- mlist[Type=='C']
setnames(mlist,"Water System No.",'PWS_ID')
url = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=A&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=1%2F30%2F2023&end_date=1%2F30%2F2025&action=Search+For+Water+Systems'

base_url <- "https://dww2.tceq.texas.gov/DWW/JSP/"
hlinks <- url |> read_html() |> html_nodes(css = '#AutoNumber7 td:nth-child(1) a')
pws_ids = hlinks |> html_text(trim = T)

output_file <- 'drought_and_debt/input/storage_connections_data.txt'

if (file.exists(output_file)) {
  existing_data <- fread(output_file)
} else {
  existing_data <- data.table()
}

starting_n <- nrow(existing_data)
CLOBBER <- FALSE  # Set this variable as needed

if (CLOBBER) {
  filtered_pws_ids <- pws_ids
} else {
  filtered_pws_ids <- pws_ids[!pws_ids %in% existing_data$PWS_ID]
}

index <- which(pws_ids %in% mlist$PWS_ID & pws_ids %in% filtered_pws_ids)
hrefs <- hlinks[index] |> html_attr('href')

for(href in hrefs){
  print(href)
  pws_id <- str_extract(href,'TX[0-9]{1,}')
  href_encoded <- URLencode(paste0(base_url,href))
  html_string <- href_encoded |> read_html() 
  # IF TSTC exists, the three items here are TSTC, amount, and units
  tstc_txt <- html_string |> html_nodes('td td') |> html_text(trim = T)
  tstc_result <- as.list(tstc_txt[grep('TSTC',tstc_txt)+0:2])
  
    if(length(tstc_result) == 3){
    temp_dt <- as.data.frame(as.list(tstc_result),col.names = c('Var','Value','Unit'))
    temp_dt$PWS_ID <- pws_id
    temp_dt$Var <- str_extract(temp_dt$Var,'^[A-Z]+')
  }else{
    temp_dt <- data.table(PWS_ID = pws_id, Var = 'TSTC', Value = NA, Unit = NA)
  }

  connections_txt <- html_string |> html_nodes('th, td td') |> html_text(trim = T)
  sources_idx <- grep('Sources of Water', connections_txt)
  percentages_idx <- grep('Source Water Percentages', connections_txt)
  
  if(length(sources_idx) > 0 & length(percentages_idx) > 0){
    h0 <- sources_idx + 1
    h1 <- percentages_idx - 1
    hmat <- matrix(unlist(connections_txt[h0:h1]), byrow = T, ncol = 4)
    if(nrow(hmat)<3){
      hdf <- as.data.frame.list(hmat[-1,])}else{
        hdf <- as.data.frame(hmat[-1,])
        }
      colnames(hdf) <- hmat[1,]
      connections_result <- sum(hdf$Type == 'CC')
      }
  if (exists("connections_result")) {
    temp_dt$Num_Interconnections <- connections_result
  } else {
    temp_dt$Num_Interconnections <- NA
  }
  existing_data <- rbindlist(list(existing_data, temp_dt), use.names = TRUE, fill = TRUE)
}

if (starting_n < nrow(existing_data)) {
  fwrite(existing_data, output_file)
} else {
  print("no new data")
}