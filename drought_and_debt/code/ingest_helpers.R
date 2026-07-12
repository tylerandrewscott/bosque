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

# --- normalize_district_name() -------------------------------------------------
# The district-name canonicalization rules shared by 03_assemble_audits.R and
# 04_assemble_debt.R (both ultimately match names against the TWDD roster):
# strip zero-padded numbers ("MUD 07" -> "MUD 7"), expand " UD" to
# " UTILITY DISTRICT", and abbreviate "SPECIAL UTILITY DISTRICT" to "SUD"
# (in that order — " SPECIAL UD" must end up as "SUD"). Source-specific rules
# (TBRB abbreviation expansions, one-off aliases) stay in the assemble scripts.
normalize_district_name <- function(x) {
  # One gsub pass strips a single leading zero (the match consumes the space),
  # so iterate to a fixed point: "MUD 001" -> "MUD 01" -> "MUD 1".
  repeat {
    y <- gsub('(\\s)0(?=[0-9])', '\\1', x, perl = TRUE)
    if (identical(y, x)) break
    x <- y
  }
  x <- gsub(' UD\\b', ' UTILITY DISTRICT', x, perl = TRUE)
  gsub('SPECIAL UTILITY DISTRICT', 'SUD', x, perl = TRUE)
}

# --- strip_county_suffix() ------------------------------------------------------
# Drop a trailing " OF <X> COUNTY" qualifier ("... MUD 1 OF HARRIS COUNTY" ->
# "... MUD 1"). Both assemble scripts apply this as the LAST normalization step,
# after any name-specific aliases.
strip_county_suffix <- function(x) {
  gsub('\\sOF\\s[A-Z]+\\sCOUNTY$', '', x, perl = TRUE)
}

# --- mandatory_restriction_events() --------------------------------------------
# The mandatory-restriction event definition shared by the model panel
# (02_model/build_recurrent_panel.R) and the descriptive figure
# (02_model/02_make_figure1.R): distinct mandatory (STAGE M1/M2/M3) notices in
# [from, to], one row per (PWS_ID, event_date). Defined ONCE so the figure and
# the panel cannot drift apart.
mandatory_restriction_events <- function(from = start_date, to = end_date) {
  restr <- data.table::as.data.table(readRDS(committed("combined_restriction_records.RDS")))
  data.table::setnames(restr, "PWS ID", "PWS_ID")
  restr$NOTIFIED_YMD <- as.Date(restr$NOTIFIED_YMD)
  keep <- !is.na(restr$PWS_ID) & !is.na(restr$NOTIFIED_YMD) &
    restr$STAGE %in% c("M1", "M2", "M3") &
    restr$NOTIFIED_YMD >= from & restr$NOTIFIED_YMD <= to
  ev <- data.table::data.table(PWS_ID     = restr$PWS_ID[keep],
                               event_date = restr$NOTIFIED_YMD[keep])
  unique(ev, by = c("PWS_ID", "event_date"))
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
