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


# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(tidyverse)   # stringr/dplyr/lubridate/forcats used throughout
  library(rvest)
  library(pbapply)
})
tceq_file <- raw_input("TCEQ_FOIA", "PIR 98118_Copy_Drought_Database_Reported_MASTER.xlsx")
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

ntc$`PWS ID` <- format_pws_id(ntc$`PWS ID`)
ntc$Notified <- str_remove(ntc$Notified,'\\s.*')
ntc$Notified_ymd <- ymd(ntc$Notified)
colnames(ntc) <- toupper(colnames(ntc))

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
  temp <- temp |> dplyr::select(-NOTIFIED)
  temp$`PWS ID` <- format_pws_id(temp$`PWS ID`)
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

# ### older location: #dr <- 'web.archive.org/web/'
dr <- util("scraping", "dol-wayback")
fls <- list.files(dr,pattern = 'html',recursive = T,full.names = T)
#### skip empty files
flsize <- file.size(fls)
fls <- fls[flsize>0]
# 
# 
old_restrictions = pblapply(fls,function(p) {
   print(p)
  temp = read_html(p) %>% html_nodes('table') %>% html_table(trim=T,fill=T)
   if(length(temp)==0){return(NULL)}
   if(length(temp)==1){tdf = temp[[1]]}
   if(length(temp)>1){tdf = temp[[4]]}
   # Some snapshots (e.g. droughtw.html from late-2024) parse with extra blank
   # columns whose names are empty; dplyr::rename() below errors on empty names,
   # so give any blank/duplicate names safe placeholders first.
   nm <- names(tdf)
   blank <- is.na(nm) | nm == ''
   nm[blank] <- paste0('V', seq_along(nm))[blank]
   names(tdf) <- make.unique(nm)
   if(!any(grepl('PWS ID',names(tdf)))){
   # No header cell names the PWS column. Two sub-cases:
   #  (a) the real header is sitting in row 1 (older layout) -> promote it.
   #  (b) the page shipped no <th> header at all, so html_table() auto-named the
   #      columns X1..Xn and row 1 is a genuine data record (the late-2025/2026
   #      layout, same failure mode as the headerless live CSVs). Promoting row 1
   #      there would name columns after a PWS record ("TX2050011", "1/2/2025",
   #      ...), so Date Notified / TCEQ Stage vanish and every row is dropped as
   #      NA below. Detect that case and recover the columns by *content* instead.
   row1 <- as.character(unlist(tdf[1, ]))
   if (any(grepl('pws', row1, ignore.case = TRUE))) {
     colnames(tdf) <- tdf[1,]
     tdf = tdf[-1,]
   } else {
     tdf <- label_restriction_columns(as.data.frame(tdf, stringsAsFactors = FALSE))
     # label_restriction_columns() handles PWS ID / Date Notified / TCEQ Stage;
     # recover the Priority column by content too so Priority_Numeric survives.
     is_prio <- vapply(tdf, function(x) {
       v <- toupper(trimws(as.character(x)))
       mean(grepl('DAY SUPPLY', v) | v %in% c('W','C','P','E','R','O'), na.rm = TRUE) > 0.5
     }, logical(1))
     if (any(is_prio)) names(tdf)[which(is_prio)[1]] <- 'Priority'
   }}
   # Promoting row 1 to a header (above) can create blank/duplicate column names
   # (e.g. repeated 'Greater than 180-day supply'); make them unique so the
   # later dplyr::select(-file)/duplicated() dedup does not error on dup names.
   nm2 <- names(tdf)
   nm2[is.na(nm2) | nm2 == ''] <- 'V'
   names(tdf) <- make.unique(nm2)
   if(any(colnames(tdf)=='TCEQ Stage')){tdf = tdf %>% rename(Stage = `TCEQ Stage`)}
   if(any(colnames(tdf)=='Date Notified')){tdf = tdf %>% rename(Notified = `Date Notified`)}
   if(any(colnames(tdf)=='Last Updated')){tdf = tdf %>% rename(Notified = `Last Updated`)}
   tdf$file <- p
   tdf},cl = 8)
#
# Drop any files that yielded no parseable table so rbindlist doesn't choke
old_restrictions <- Filter(is.data.frame, old_restrictions)
rest_df <- rbindlist(old_restrictions,fill = T,use.names = T)
rest_df <- rest_df[order(-file),]
rest_df <- rest_df[!{rest_df %>% dplyr::select(-file) %>% duplicated(.)},]
# library(lubridate)
rest_df$Notified <- mdy(rest_df$Notified)
rest_df <- rest_df[!is.na(Notified),]
rest_df$`PWS ID` <- format_pws_id(rest_df$`PWS ID`)
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
  ))
# `PWS ID` is already in canonical TXnnnnnnn form (format_pws_id, above).

# --- ingest live-scraped CSV snapshots from input/texas_dww/ ---------------
# scrape_water_restrictions.R writes dated CSVs here; we treat each as another
# source of records and dedupe later on (PWS ID, NOTIFIED_YMD, STAGE).
csv_dir <- raw_input("texas_dww")
csv_files <- if (dir.exists(csv_dir)) {
  list.files(csv_dir, pattern = '^system_water_restrictions.*\\.csv$', full.names = TRUE)
} else character(0)
if (length(csv_files) > 0) {
  message(sprintf("Ingesting %d live-CSV snapshots from %s", length(csv_files), csv_dir))
  headerless_ct <- 0L
  csv_list <- pblapply(csv_files, function(p) {
    tryCatch({
      d <- fread(p, colClasses = 'character')
      # Some snapshots were scraped without a header row (the scraper's
      # html_table() promoted the first data record to column names). Left as-is
      # these lose their DATE NOTIFIED / TCEQ STAGE columns and get silently
      # dropped below (NA NOTIFIED_YMD), which is why whole years (2025-2026)
      # vanished. Symptom: no column name mentions "PWS" (fread either promoted a
      # real record to the header, e.g. "TX2050011", or auto-named them V1..Vn).
      # Re-read so the mis-used first record is kept as data, then recover the
      # columns by content below.
      if (!any(grepl('pws', names(d), ignore.case = TRUE))) {
        d <- fread(p, colClasses = 'character', header = FALSE)
        headerless_ct <<- headerless_ct + 1L
      }
      # Canonicalize PWS ID / Date Notified / TCEQ Stage by content (idempotent
      # on well-formed files) so the renames further down line up regardless of
      # whether this file had a header.
      d <- label_restriction_columns(d)
      # Some legacy CSVs have duplicate column names; disambiguate before merging
      if (any(duplicated(names(d)))) {
        setnames(d, make.unique(names(d)))
      }
      d$file <- p
      d
    }, error = function(e) { message(sprintf("Failed %s: %s", p, conditionMessage(e))); NULL })
  })
  if (headerless_ct > 0) {
    message(sprintf("  -> repaired %d headerless snapshot(s) (recovered columns by content)", headerless_ct))
  }
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
  # Ensure PWS ID has the canonical TXnnnnnnn form
  csv_df[, `PWS ID` := format_pws_id(`PWS ID`)]
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

saveRDS(ntcBoth,file = committed('combined_restriction_records.RDS'))



# saveRDS(rest_df,'input/combined_restriction_records.RDS')
# 
# table(year(test$Notified))
