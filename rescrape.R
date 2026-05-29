id_id <- readRDS('drought_and_debt/input/id_crosswalk.rds')
library(rvest)

base <- "https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county=All&WaterSystemType=All&SourceWaterType=All&SampleType=null&begin_date=2%2F19%2F2023&end_date=2%2F19%2F2025&action=Search+For+Water+Systems"

tb <- base |> read_html() |> html_nodes(css = '#AutoNumber7 td:nth-child(1) a')
hrefs <- tb |> html_attr('href')
ids <- tb |> html_text(trim = T)
hdt <- data.table(PWS_ID = ids,href = hrefs)
hdt <- hdt[PWS_ID %in% id_id$PWS_ID,]

baseurl <- 'https://dww2.tceq.texas.gov/DWW/JSP/'

tds <- paste0(baseurl,hdt$href[1]) |> URLencode() |> read_html() |> html_nodes('td')
tx <- tds |> html_text(trim = T)
tx
tx[grep('TSTC',tx)+0:1]


pws <- id_id[!is.na(id_id$PWS_ID),]
plinks <- paste0("https://dww2.tceq.texas.gov/DWW/JSP/DataSheet.jsp?wsnumber=",pws$PWS_ID,"%20%20%20&DWWState=TX&begin_date=&end_date=&counter=")



