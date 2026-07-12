# =============================================================================
# prep/assemble_kbdi.R
# -----------------------------------------------------------------------------
# Canonical builder for the Keetch-Byram Drought Index (KBDI) county series.
#
# This was previously buried mid-way through model/create_model_matrix.R
# (~lines 1010-1045) and written with fwrite() to a ".RDS" filename — i.e. CSV
# content under an .RDS extension, which fails if anything reads it with
# readRDS(). Extracted here as a standalone Stage-C step and written as a
# proper .RDS via saveRDS().
#
# Source: Texas A&M / Texas Water Conservation KBDI summaries
#   https://twc.tamu.edu/weather_images/summ/summYYYYMMDD.{txt,csv}
#   (.txt through Sep 2016, .csv from Oct 2016 onward)
#
# Output: scratch/kbdi_county.RDS  — county-level (CFIPS x Date) KBDI with
# 3/6/12-month trailing averages. The PWS-level area-weighted aggregation stays
# in the panel builder, which should readRDS() this file rather than re-scraping.
#
# Run from the bosque repo root (the script self-locates config.R):
#   source("drought_and_debt/code/explore/assemble_kbdi.R")
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(tidyverse)
  library(rvest)
  library(zoo)
  library(pbapply)
  library(tigris)
})

# Load config (sets wd = project root) if a caller hasn't already. Works from
# the bosque repo root, the drought_and_debt root, or a code/ subdirectory.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}

# --- Dates to query ----------------------------------------------------------
# Monthly steps from Jan 2009 up to the start of the weekly window, then the
# weekly panel dates. (Originally derived from the model panel's Date column;
# here we build the same monthly+weekly spine directly from the analysis window.)
weekly_dates  <- seq.Date(from = start_date, to = end_date, by = "week")
monthly_prefix <- seq.Date(from = as.Date("2009-01-01"), to = min(weekly_dates), by = "month")
dates <- sort(unique(c(monthly_prefix, weekly_dates)))
dates <- dates[dates < Sys.Date()]

qpref <- "https://twc.tamu.edu/weather_images/summ/summ"
qsuf  <- ifelse(year(dates) < 2016 | (year(dates) == 2016 & month(dates) < 10), ".txt", ".csv")

# --- Per-era readers ---------------------------------------------------------
# The two source formats do NOT share a column layout, so we parse each by
# meaning (not blind position) and emit a common County/KBDI_Avg/Max/Min schema.
#
#   .txt (<= Sep 2016): fixed-width, header  COUNTY  KBDI_MEAN  KBDI_MAX  KBDI_MIN
#     Whitespace-delimited reads break here because multi-word counties
#     (DE WITT, DEAF SMITH, PALO PINTO, ...) spill an extra token. Instead we
#     pull the trailing three integers off each line with a regex; the county
#     is everything before them. The header and "----" divider don't match the
#     "<text> <int> <int> <int>" pattern, so the regex self-filters (no skip=).
#
#   .csv (>= Oct 2016): comma-delimited, header  County,Min,Max,Average,Change
#     Note the order is Min,Max,Average -- NOT Avg,Max,Min. The old code mapped
#     these positionally and silently stored Min as "KBDI_Avg". We read by the
#     real header and select Average/Max/Min explicitly.
read_kbdi_txt <- function(url) {
  ln <- tryCatch(readr::read_lines(url), error = function(e) NULL)
  if (is.null(ln)) return(NULL)
  m <- stringr::str_match(ln, "^\\s*(.+?)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s*$")
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0) return(NULL)
  tibble::tibble(
    County   = stringr::str_squish(m[, 2]),
    KBDI_Avg = as.numeric(m[, 3]),   # KBDI_MEAN
    KBDI_Max = as.numeric(m[, 4]),
    KBDI_Min = as.numeric(m[, 5])
  )
}
read_kbdi_csv <- function(url) {
  raw <- tryCatch(
    readr::read_csv(url, show_col_types = FALSE, name_repair = "unique_quiet"),
    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  names(raw) <- tolower(names(raw))
  need <- c("county", "average", "max", "min")
  if (!all(need %in% names(raw))) return(NULL)
  tibble::tibble(
    County   = raw$county,
    KBDI_Avg = as.numeric(raw$average),
    KBDI_Max = as.numeric(raw$max),
    KBDI_Min = as.numeric(raw$min)
  )
}

# --- Scrape (walks backward a day at a time until a file is found) -----------
kbdi_list <- pblapply(seq_along(dates), function(x) {
  day <- dates[x]
  tab <- NULL
  guard <- 0
  is_txt <- grepl("txt", qsuf[x])
  while (is.null(tab) && guard < 14) {      # guard: don't loop forever on a gap
    url <- paste0(qpref, gsub("-", "", day), qsuf[x])
    tab <- if (is_txt) read_kbdi_txt(url) else read_kbdi_csv(url)
    if (is.null(tab)) { day <- day - days(1); guard <- guard + 1 }
  }
  if (is.null(tab)) return(NULL)
  tab %>% mutate(Query_Date = dates[x], KBDI_Date = day)
}, cl = 1)

kbdi_df <- as.data.table(do.call(rbind, kbdi_list))
kbdi_df$County <- toupper(kbdi_df$County)
kbdi_df <- kbdi_df[County != "\032", ]            # strip stray EOF marker rows
kbdi_df$Date <- as.Date(kbdi_df$KBDI_Date)

# --- Attach county FIPS ------------------------------------------------------
tx_county <- tigris::counties(state = 48, class = "sf", progress_bar = FALSE)
kbdi_df$CFIPS <- as.character(
  tx_county$GEOID[match(gsub(" ", "", toupper(kbdi_df$County)),
                        gsub(" ", "", toupper(tx_county$NAME)))]
)

# --- Trailing averages -------------------------------------------------------
kbdi_df[order(CFIPS, Date), KBDI_3Month_Average  := rollapplyr(KBDI_Avg, 3,  mean, fill = NA), by = CFIPS]
kbdi_df[order(CFIPS, Date), KBDI_6Month_Average  := rollapplyr(KBDI_Avg, 6,  mean, fill = NA), by = CFIPS]
kbdi_df[order(CFIPS, Date), KBDI_12Month_Average := rollapplyr(KBDI_Avg, 12, mean, fill = NA), by = CFIPS]

# --- Write proper .RDS -------------------------------------------------------
saveRDS(kbdi_df, scratch("kbdi_county.RDS"))
message("Wrote ", scratch("kbdi_county.RDS"), "  (", nrow(kbdi_df), " county-weeks, ",
        uniqueN(kbdi_df$CFIPS), " counties, max date ", max(kbdi_df$Date, na.rm = TRUE), ")")
