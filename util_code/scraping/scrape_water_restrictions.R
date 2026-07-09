library(parallel)
library(rvest)
library(tidyverse)
library(stringr)
library(stringi)

drought_site <- 'https://www.tceq.texas.gov/drinkingwater/trot/droughtw.html'

# Pull raw HTML once so we can both (a) parse to CSV and (b) archive it
# into dol-wayback/ alongside Wayback snapshots for the assemble pipeline.
raw_html <- tryCatch(read_html(drought_site),
                     error = function(e) { stop("Failed to fetch ", drought_site, ": ", conditionMessage(e)) })

# --- archive the raw HTML in the wayback dir layout --------------------------
ts <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
wayback_path <- file.path('util_code/scraping/dol-wayback', ts,
                         'www.tceq.texas.gov/drinkingwater/trot/droughtw.html')
dir.create(dirname(wayback_path), recursive = TRUE, showWarnings = FALSE)
writeLines(as.character(raw_html), wayback_path)

# --- parse to CSV (legacy output, kept for backward compatibility) ----------
all_tables <- raw_html %>% html_table(fill = TRUE, header = TRUE)

# Pick the table containing PWS records. Use two heuristics so we don't fail on
# whitespace/icons in the header cell:
#   1. any column name matching /pws/i (covers "PWS ID", "PWS  ID", " PWS_ID", etc.)
#   2. any cell value matching the TX#######  PWS ID pattern
match_idx <- which(vapply(all_tables, function(t) {
  hdr_match  <- any(grepl('pws', names(t), ignore.case = TRUE))
  cell_match <- if (nrow(t) > 0 && ncol(t) > 0) {
    any(grepl('^TX[0-9]{7}$', as.character(unlist(t[, 1])), ignore.case = TRUE))
  } else FALSE
  hdr_match || cell_match
}, logical(1)))

if (length(match_idx) == 0) {
  # Diagnostic: dump headers of every table found so we can fix the selector next time
  message("DIAGNOSTIC - tables on page (", length(all_tables), " total):")
  for (i in seq_along(all_tables)) {
    message(sprintf("  table[%d] (%d x %d): %s", i,
                    nrow(all_tables[[i]]), ncol(all_tables[[i]]),
                    paste(names(all_tables[[i]]), collapse = " | ")))
  }
  stop("No table containing PWS records found on ", drought_site,
       " - page layout may have changed.")
}
dtable <- all_tables[[match_idx[1]]]

# --- guarantee a real header row ---------------------------------------------
# html_table(header = TRUE) uses the table's first row as column names. When the
# page ships its records without a <th> header row, row 1 is actually a PWS
# record, so the "names" come out as data (e.g. "TX2050011") and write_csv()
# then emits a HEADERLESS csv. The assemble step (02_assemble_drought_
# restrictions.R) silently drops those files, which is why the 2025-2026 scrapes
# never made it into the panel. Detect that case, recover the mis-used record as
# a data row, and relabel columns by content so the CSV always has a header.
label_restriction_columns <- function(d) {          # mirrors drought_and_debt/code/ingest_helpers.R
  majority <- function(ok) mean(ok, na.rm = TRUE) > 0.5
  cols     <- as.list(d)
  nm       <- names(d)
  stage_vocab <- c('V', 'M1', 'M2', 'M3', 'RESCINDING', 'IMPLEMENTING', 'CHANGING')
  is_pws   <- vapply(cols, function(x) majority(grepl('^TX[0-9]{7}$', trimws(x), ignore.case = TRUE)),   logical(1))
  is_date  <- vapply(cols, function(x) majority(grepl('^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$', trimws(x))), logical(1))
  is_stage <- vapply(cols, function(x) majority(toupper(trimws(x)) %in% stage_vocab),                     logical(1))
  if (any(is_pws))   nm[which(is_pws)[1]]   <- 'PWS ID'
  if (any(is_date))  nm[which(is_date)[1]]  <- 'Date Notified'
  if (any(is_stage)) nm[which(is_stage)[1]] <- 'TCEQ Stage'
  names(d) <- nm
  d
}
if (any(grepl('^TX[0-9]{7}$', names(dtable)))) {
  message("No <th> header on page: recovering first record as data and relabeling columns by content.")
  lost <- as.data.frame(as.list(names(dtable)), stringsAsFactors = FALSE, check.names = FALSE)
  names(lost)   <- paste0('V', seq_len(ncol(dtable)))
  names(dtable) <- paste0('V', seq_len(ncol(dtable)))
  dtable <- rbind(lost, as.data.frame(dtable, stringsAsFactors = FALSE, check.names = FALSE))
  dtable <- label_restriction_columns(dtable)
}

dir.create('input/texas_dww', recursive = TRUE, showWarnings = FALSE)
out_csv <- paste('input/texas_dww/system_water_restrictions',
                 paste0(Sys.Date(), '.csv'), sep = '_')
write_csv(dtable, out_csv)
message(sprintf("Wrote %d rows to %s and archived raw HTML to %s",
                nrow(dtable), out_csv, wayback_path))
