scratch_loc = 'scratch/'

starts = list( 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=C&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems',
 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=NTNC&SourceWaterType=All&SampleType=null&begin_date=2%2F9%2F2017&end_date=2%2F9%2F2019&action=Search+For+Water+Systems')

library(data.table)

library(rvest)
library(tidyverse)
library(pbapply)
library(xml2)
library(readr)
library(dplyr)

tab_list <- lapply(starts,function(x) {
  x |> read_html() |> html_nodes('table') |> html_table(trim=T,header = T)
})

main_record <- rbindlist(sapply(tab_list,function(x) x[[3]],simplify = F),fill = T,use.names = T)
write_csv(x = main_record,file = 'input/texas_dww/district_master_list.csv')

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
### work in chunks, record results
qdt
dir.create(file.path(scratch_loc, "html_reports"), showWarnings = FALSE, recursive = TRUE)

for(i in 1:nrow(qdt)) {
  tryCatch({
    # Extract PWS ID from URL
    pws_id <- str_extract(qdt$q[i], "TX[0-9]{7}")
    
    if(!is.na(pws_id)) {
      # Check if file already exists
      file_path <- file.path(scratch_loc, "html_reports", paste0(pws_id, ".html"))
      if(!file.exists(file_path)) {
        # Download and save HTML only if file doesn't exist
        html_content <- read_html(qdt$q[i])
        xml2::write_html(html_content, file_path)
        
        # Mark as successfully grabbed
        qdt$grabbed[i] <- 1
        
        # Small delay to be nice to the server
        Sys.sleep(0.5)
      } else {
        # Mark as grabbed if file already exists
        qdt$grabbed[i] <- 1
        cat(sprintf("Skipping %s - file already exists\n", pws_id))
      }
    }
    
  }, error = function(e) {
    warning(paste("Failed to process URL:", qdt$q[i]))
  })
  
  if(i %% 100 == 0) {
    cat(sprintf("Processed %d of %d URLs\n", i, nrow(qdt)))
  }
}
