# =============================================================================
# ingest_helpers.R  —  small shared helpers for the assemble/combine stages
# -----------------------------------------------------------------------------
# Sourced by config.R (after the path helpers are defined), so any script that
# sources config.R gets these for free. They collapse idioms that were
# copy-pasted across the 00_assemble/* and 01_combine/* scripts:
#   * format_pws_id()            - the "TX" + 7-digit zero-pad PWS-ID formatting
#   * load_latest_district_list()- read the newest twdd_records/district_list_*
# =============================================================================

# --- format_pws_id() ----------------------------------------------------------
# Normalize a PWS identifier to the canonical "TXnnnnnnn" form (TX + 7 digits).
# Accepts either already-prefixed ids ("TX1234567") or bare numbers (1234567,
# "1234567"); returns NA for values that are neither. Vectorized.
format_pws_id <- function(x) {
  x       <- trimws(as.character(x))
  has_tx  <- grepl("^TX", x, ignore.case = TRUE)
  num     <- suppressWarnings(as.numeric(x))
  padded  <- paste0("TX", formatC(num, width = 7, flag = "0", format = "d"))
  ifelse(has_tx, toupper(x),
         ifelse(is.na(num), NA_character_, padded))
}

# --- label_restriction_columns() ---------------------------------------------
# The TCEQ drought-restrictions page is sometimes scraped without a real <th>
# header row, so html_table()/fread() promote the first *data* record to be the
# column names (the PWS ID column ends up named e.g. "TX2050011"). Given such a
# frame -- where the names are unreliable -- recover the three columns the
# pipeline needs by their *content* and rename them to the canonical
# "PWS ID" / "Date Notified" / "TCEQ Stage". Columns it can't classify are left
# as-is. Idempotent: on a well-formed frame it just renames the already-correct
# columns to the same names, so it is safe to run on every file.
label_restriction_columns <- function(d) {
  majority <- function(ok) mean(ok, na.rm = TRUE) > 0.5     # majority of non-NA rows
  cols     <- as.list(d)
  nm       <- names(d)
  stage_vocab <- c('V', 'M1', 'M2', 'M3', 'RESCINDING', 'IMPLEMENTING', 'CHANGING')
  is_pws   <- vapply(cols, function(x) majority(grepl('^TX[0-9]{7}$', trimws(x), ignore.case = TRUE)),      logical(1))
  is_date  <- vapply(cols, function(x) majority(grepl('^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$', trimws(x))),    logical(1))
  is_stage <- vapply(cols, function(x) majority(toupper(trimws(x)) %in% stage_vocab),                        logical(1))
  if (any(is_pws))   nm[which(is_pws)[1]]   <- 'PWS ID'
  if (any(is_date))  nm[which(is_date)[1]]  <- 'Date Notified'
  if (any(is_stage)) nm[which(is_stage)[1]] <- 'TCEQ Stage'
  if (data.table::is.data.table(d)) data.table::setnames(d, nm) else names(d) <- nm
  d
}

# --- load_latest_district_list() ---------------------------------------------
# Read the most recently modified twdd_records/district_list_*.csv (the TWDD
# district roster). Returns a data.table. Replaces the three-line
# list.files()/which.max(mtime)/fread() idiom the assemble/combine scripts each
# re-implemented.
load_latest_district_list <- function() {
  fs <- list.files(raw_input("twdd_records"), pattern = "district_list",
                   full.names = TRUE)
  if (!length(fs)) stop("No district_list_*.csv under ", raw_input("twdd_records"))
  data.table::fread(fs[which.max(file.info(fs)$mtime)])
}
