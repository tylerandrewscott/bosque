# fetch_wayback_drought.R
# Pull all Wayback Machine snapshots of the TCEQ drought-restrictions page
# since the most recent local snapshot, into util_code/scraping/dol-wayback/
# in the same {YYYYMMDDHHMMSS}/www.tceq.texas.gov/drinkingwater/trot/droughtw.html
# folder layout the existing pipeline expects.
#
# Run from repo root.

library(jsonlite)
library(httr)
library(stringr)

target_url      <- "www.tceq.texas.gov/drinkingwater/trot/droughtw.html"
wayback_dir     <- "util_code/scraping/dol-wayback"
relative_path   <- "www.tceq.texas.gov/drinkingwater/trot/droughtw.html"
polite_delay_s  <- 1.5
# collapse_digits: 8 = 1 per day (YYYYMMDD); 10 = 1 per hour; 14 = every snapshot.
# We default to per-day to balance coverage vs. politeness.
collapse_digits <- 8

# --- determine start date from latest local snapshot -------------------------
existing_ts <- list.dirs(wayback_dir, recursive = FALSE, full.names = FALSE)
existing_ts <- existing_ts[str_detect(existing_ts, "^[0-9]{14}$")]
if (length(existing_ts) == 0) {
  start_date <- "20100101"
} else {
  # +1 day past the latest local snapshot to avoid re-fetching it
  latest <- max(as.numeric(substr(existing_ts, 1, 8)))
  start_date <- format(as.Date(as.character(latest), format = "%Y%m%d") + 1, "%Y%m%d")
}
end_date <- format(Sys.Date(), "%Y%m%d")

message(sprintf("Fetching Wayback snapshots %s -> %s", start_date, end_date))

# --- query CDX API for snapshot list ----------------------------------------
cdx_url <- sprintf(
  "https://web.archive.org/cdx/search/cdx?url=%s&output=json&from=%s&to=%s&collapse=timestamp:%d&filter=statuscode:200&filter=mimetype:text/html",
  utils::URLencode(target_url, reserved = TRUE), start_date, end_date, collapse_digits
)

cdx_resp <- tryCatch(fromJSON(cdx_url, simplifyDataFrame = TRUE),
                     error = function(e) { message("CDX query failed: ", conditionMessage(e)); NULL })
if (is.null(cdx_resp) || length(cdx_resp) <= 1) {
  message("No new snapshots returned by CDX.")
} else {
  # First row is column headers
  cdx_df <- as.data.frame(cdx_resp[-1, , drop = FALSE], stringsAsFactors = FALSE)
  colnames(cdx_df) <- cdx_resp[1, ]
  message(sprintf("CDX returned %d snapshots.", nrow(cdx_df)))

  # --- download each (skip if already on disk) -------------------------------
  n_downloaded <- 0L
  n_skipped    <- 0L
  n_failed     <- 0L
  for (i in seq_len(nrow(cdx_df))) {
    ts        <- cdx_df$timestamp[i]
    out_dir   <- file.path(wayback_dir, ts, dirname(relative_path))
    out_file  <- file.path(wayback_dir, ts, relative_path)

    if (file.exists(out_file) && file.size(out_file) > 0) {
      n_skipped <- n_skipped + 1L
      next
    }

    # The 'id_' suffix returns the raw archived page without the Wayback toolbar.
    snap_url <- sprintf("https://web.archive.org/web/%sid_/https://%s", ts, target_url)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    ok <- tryCatch({
      resp <- GET(snap_url, timeout(60))
      if (status_code(resp) == 200) {
        writeBin(content(resp, "raw"), out_file)
        TRUE
      } else {
        message(sprintf("  HTTP %d for %s", status_code(resp), ts)); FALSE
      }
    }, error = function(e) { message(sprintf("  ERROR %s: %s", ts, conditionMessage(e))); FALSE })

    if (isTRUE(ok)) n_downloaded <- n_downloaded + 1L else n_failed <- n_failed + 1L
    Sys.sleep(polite_delay_s)
    if (n_downloaded %% 25 == 0 && n_downloaded > 0) {
      message(sprintf("  ... %d downloaded so far", n_downloaded))
    }
  }
  message(sprintf("Done. downloaded=%d  skipped=%d  failed=%d",
                  n_downloaded, n_skipped, n_failed))
}
