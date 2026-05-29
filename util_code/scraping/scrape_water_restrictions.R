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

dir.create('input/texas_dww', recursive = TRUE, showWarnings = FALSE)
out_csv <- paste('input/texas_dww/system_water_restrictions',
                 paste0(Sys.Date(), '.csv'), sep = '_')
write_csv(dtable, out_csv)
message(sprintf("Wrote %d rows to %s and archived raw HTML to %s",
                nrow(dtable), out_csv, wayback_path))
