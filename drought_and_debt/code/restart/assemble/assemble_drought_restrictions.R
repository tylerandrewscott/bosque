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
library(pbapply)
tceq_file <- 'input/TCEQ_FOIA/PIR 98118_Copy_Drought_Database_Reported_MASTER.xlsx'
ntc <- read_excel(tceq_file,sheet = 'Master')
warnings()
ntc <- data.table(ntc)

# Schema-drift check: warn if expected columns from the FOIA Master sheet are missing
expected_master_cols <- c('PWS ID','Notified','YEAR','STAGE',
                          'IMPLEMENTING/CHANGING/RESCINDING','DROUGHT/MECHANICAL/BOTH')
missing_master <- setdiff(expected_master_cols, names(ntc))
if (length(missing_master) > 0) {
  warning(sprintf("assemble_drought_restrictions.R: FOIA 'Master' sheet missing expected columns: %s",
                  paste(missing_master, collapse = ', ')))
}

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

library(rvest)
# library(tidyverse)
# library(pbapply)
# library(lubridate)
# library(data.table)
# ### older location
# #dr <- 'web.archive.org/web/'
dr <- 'util_code/scraping/dol-wayback/'
fls <- list.files(dr,pattern = 'html',recursive = T,full.names = T)
#### skip empty files
flsize <- file.size(fls)
fls <- fls[flsize>0]
# 
# 
parse_wayback_html <- function(p) {
  tryCatch({
    temp <- read_html(p) %>% html_nodes('table') %>% html_table(trim = TRUE, fill = TRUE)
    if (length(temp) == 0) return(NULL)
    tdf <- if (length(temp) == 1) temp[[1]] else temp[[4]]
    if (is.null(tdf) || nrow(tdf) == 0) return(NULL)
    if (!any(grepl('PWS ID', names(tdf)))) {
      colnames(tdf) <- as.character(unlist(tdf[1, ]))
      tdf <- tdf[-1, , drop = FALSE]
    }
    if (any(colnames(tdf) == 'TCEQ Stage'))    tdf <- tdf %>% rename(Stage    = `TCEQ Stage`)
    if (any(colnames(tdf) == 'Date Notified')) tdf <- tdf %>% rename(Notified = `Date Notified`)
    if (any(colnames(tdf) == 'Last Updated'))  tdf <- tdf %>% rename(Notified = `Last Updated`)
    # Some wayback snapshots have duplicate column headers (e.g. "Greater than
    # 180-day supply" repeated); disambiguate so downstream select()/dedup works.
    if (any(duplicated(colnames(tdf)))) {
      colnames(tdf) <- make.unique(colnames(tdf))
    }
    tdf$file <- p
    tdf
  }, error = function(e) {
    message(sprintf("parse_wayback_html failed for %s: %s", p, conditionMessage(e)))
    NULL
  })
}

# Serial pblapply: avoids parallel-worker library-loading issues that silently
# returned NULLs and produced an empty rest_df. ~295 files is fast enough serially.
old_restrictions <- pblapply(fls, parse_wayback_html)
old_restrictions <- old_restrictions[!sapply(old_restrictions, is.null)]
message(sprintf("Wayback: parsed %d/%d HTML files successfully",
                length(old_restrictions), length(fls)))

rest_df <- rbindlist(old_restrictions, fill = TRUE, use.names = TRUE)

# Guard against an empty parse result so downstream `-file` doesn't resolve
# to base::file and trip data.table's order().
if (nrow(rest_df) == 0 || !'file' %in% names(rest_df)) {
  warning("rest_df from wayback parse is empty; initializing with required columns.")
  rest_df <- data.table(`PWS ID` = character(), Notified = character(),
                        Stage = character(), Priority = character(),
                        file = character())
} else {
  # Final defensive dedup of column names across the combined data.table
  if (any(duplicated(names(rest_df)))) {
    setnames(rest_df, make.unique(names(rest_df)))
  }
  rest_df <- rest_df[order(-file),]
  # Use base-R column drop (works with duplicated names) for the dedup
  rest_df <- rest_df[!duplicated(rest_df[, !(names(rest_df) %in% 'file'), with = FALSE]), ]
}
# library(lubridate)
rest_df$Notified <- mdy(rest_df$Notified)
rest_df <- rest_df[!is.na(Notified),]
rest_df$`PWS ID` <- as.integer(rest_df$`PWS ID`)
rest_df$`PWS ID` <- as.character(formatC(rest_df$`PWS ID`,width=7,flag = 0))
rest_df$Priority <- toupper(rest_df$Priority)
# 
# #https://www.tceq.texas.gov/drinkingwater/trot/droughtdic.html
rest_df <- rest_df %>% 
   mutate(Priority = case_when(
     Priority %in% c('W','GREATER THAN 180-DAY SUPPLY') ~ 'Watch',
     Priority %in% c('C','LESS THAN 180-DAY SUPPLY') ~ 'Concern',
     Priority == 'P' ~ 'Priority',
     Priority == 'E' ~ 'Emergency',
     Priority == 'R' ~ 'Resolved',
     Priority == 'O' ~ 'Outage',
     .default = Priority))
# 
table(rest_df$Priority)
rest_df$Priority_Numeric <- as.numeric(fct_relevel(rest_df$Priority,'Resolved','Watch','Concern','Priority','Emergency','Outage'))
# 
rest_df$Stage<-toupper(rest_df$Stage)
rest_df <- rest_df %>% mutate(Stage = case_when(
   Stage == 'V' ~ 'Voluntary',
   Stage == '1' ~ 'Mild',
   Stage == '2' ~ 'Moderate',
   Stage == '3' ~ 'Severe',
   TRUE ~ 'Voluntary'
 ))
rest_df$Mandatory <- (rest_df$Stage != "Voluntary") + 0


colnames(rest_df)<-toupper(colnames(rest_df))
rest_df$NOTIFIED_YMD <- ymd(rest_df$NOTIFIED)
rest_df <- rest_df |> filter(STAGE %in% c('Mild','Moderate','Severe')) |>
  mutate(STAGE = case_when(
    STAGE=='Mild' ~ 'M1',
    STAGE=='Moderate' ~ 'M2',
    STAGE=='Severe' ~ 'M3'
  )) |>
  mutate(`PWS ID` = paste0('TX',`PWS ID`))

# --- ingest live-scraped CSV snapshots from input/texas_dww/ ---------------
# scrape_water_restrictions.R writes dated CSVs here; we treat each as another
# source of records and dedupe later on (PWS ID, NOTIFIED_YMD, STAGE).
csv_dir <- 'input/texas_dww'
csv_files <- if (dir.exists(csv_dir)) {
  list.files(csv_dir, pattern = '^system_water_restrictions.*\\.csv$', full.names = TRUE)
} else character(0)
if (length(csv_files) > 0) {
  message(sprintf("Ingesting %d live-CSV snapshots from %s", length(csv_files), csv_dir))
  csv_list <- pblapply(csv_files, function(p) {
    tryCatch({
      d <- fread(p, colClasses = 'character')
      # Some legacy CSVs have duplicate column names; disambiguate before merging
      if (any(duplicated(names(d)))) {
        setnames(d, make.unique(names(d)))
      }
      d$file <- p
      d
    }, error = function(e) { message(sprintf("Failed %s: %s", p, conditionMessage(e))); NULL })
  })
  csv_list <- csv_list[!sapply(csv_list, is.null)]
  csv_df <- rbindlist(csv_list, fill = TRUE, use.names = TRUE)
  # Positional setnames (no `old=`) so duplicate column names don't error
  setnames(csv_df, toupper(names(csv_df)))
  # Defensive: collapse any duplicates that still remain after toupper()
  if (any(duplicated(names(csv_df)))) {
    setnames(csv_df, make.unique(names(csv_df)))
  }
  # The live page now labels columns: PWS ID | PWS NAME | COUNTY | DATE NOTIFIED |
  # TCEQ STAGE | PRIORITY | POPULATION. Normalize to match rest_df.
  if ('DATE NOTIFIED' %in% names(csv_df)) setnames(csv_df, 'DATE NOTIFIED', 'NOTIFIED')
  if ('TCEQ STAGE'    %in% names(csv_df)) setnames(csv_df, 'TCEQ STAGE',    'STAGE')
  csv_df[, NOTIFIED_YMD := mdy(NOTIFIED)]
  csv_df <- csv_df[!is.na(NOTIFIED_YMD)]
  # Map TCEQ stage codes (V/M1/M2/M3) to the same scheme used by rest_df (M1/M2/M3 only)
  csv_df <- csv_df[STAGE %in% c('M1','M2','M3')]
  # Ensure PWS ID has the 'TX' prefix
  csv_df[, `PWS ID` := ifelse(grepl('^TX', `PWS ID`), `PWS ID`,
                              paste0('TX', formatC(as.numeric(`PWS ID`), width = 7, flag = '0', format = 'd')))]
  message(sprintf("  -> %d rows from live CSVs after filter", nrow(csv_df)))
  # Append to rest_df; column shape compatible enough for rbindlist fill=TRUE
  rest_df <- rbindlist(list(rest_df, csv_df), fill = TRUE, use.names = TRUE)
  rest_df <- rest_df[!duplicated(rest_df[, .(`PWS ID`, NOTIFIED_YMD, STAGE)])]
}

scraped_not_in_tceq_file <- rest_df[!paste(rest_df$`PWS ID`,rest_df$NOTIFIED_YMD) %in% paste(ntcM$`PWS ID`,ntcM$NOTIFIED_YMD),]
scraped_not_in_tceq_file$scraped = T
ntcM$scraped = F

ntcBoth <- merge(ntcM,scraped_not_in_tceq_file,all = T)

# Final dedup on the canonical key
ntcBoth <- as.data.table(ntcBoth)
ntcBoth <- ntcBoth[!duplicated(ntcBoth[, .(`PWS ID`, NOTIFIED_YMD, STAGE)])]

message(sprintf("Final: %d combined restriction records, max NOTIFIED_YMD = %s",
                nrow(ntcBoth), format(max(ntcBoth$NOTIFIED_YMD, na.rm = TRUE))))

saveRDS(ntcBoth,file = 'drought_and_debt/input/combined_restriction_records.RDS')



# saveRDS(rest_df,'drought_and_debt/input/combined_restriction_records.RDS')
# 
# table(year(test$Notified))
