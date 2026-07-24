###
# 08_assemble_sdwis_connections.R
# EPA SDWIS "Water System Summary" -> per-(PWS_ID, Year) service-connection and
# population-served table, the per-connection DENOMINATOR for the fiscal model.
#
# Source: yearly Q1 "Water System Summary" exports (Envirofacts / SDWIS federal
# reporting), one snapshot per submission year, filtered to Primacy Agency = TX.
# Files live on Box at bosquebox/input/epa_sdwis/water_system_summary_yearly/ as
#   "Water System Summary_<pulldate>_<YYYY>q1.xlsx"
# Each sheet carries a 3-row banner + blank row, so the real header is row 5
# (skip = 4). The federal `PWS ID` (TXnnnnnnn) is the SAME key as id_crosswalk's
# PWS_ID, so districts sum their member systems' connections directly (in the
# panel builder). Replaces a hand-pulled, time-invariant nationwide snapshot;
# the by-year series lets build_recurrent_panel.R match each audit to its year.

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
# (Dependency install is handled once by install_packages.R, not inline here.)
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)     # read_excel -- the SDWIS export is .xlsx
})

.out <- committed('pws_sdwis_connections.RDS')
if (reuse_prior(.out) && !isTRUE(REPROCESS)) {
  message("RESCRAPE=FALSE: reusing existing ", basename(.out),
          " (skipping SDWIS assembly; set REPROCESS=TRUE to re-run it).")
} else {

sdwis_dir <- raw_input('epa_sdwis', 'water_system_summary_yearly')
files <- list.files(sdwis_dir,
                    pattern = "Water System Summary_.*_20[0-9]{2}q1\\.xlsx$",
                    full.names = TRUE)
if (length(files) == 0)
  stop("No SDWIS 'Water System Summary_*_YYYYq1.xlsx' files found in ", sdwis_dir)

# Columns consumed below (post make.names() normalization of the row-5 header).
expected_cols <- c('PWS.ID', 'PWS.Type.Code', 'Service.Connections.Count',
                   'Population.Served.Count', 'Submission.Year')

read_year <- function(f) {
  # Submission year is taken from the filename (authoritative; also present in the
  # Submission.Year column, cross-checked below).
  yr <- as.integer(sub(".*_(20[0-9]{2})q1\\.xlsx$", "\\1", basename(f)))
  d  <- as.data.table(read_excel(f, sheet = 1, skip = 4))
  setnames(d, make.names(names(d)))
  miss <- setdiff(expected_cols, names(d))
  if (length(miss) > 0)
    stop(sprintf("%s missing expected column(s): %s", basename(f),
                 paste(miss, collapse = ", ")))
  # Cross-check the filename year against the file's own Submission.Year stamp.
  sy <- suppressWarnings(as.integer(d$Submission.Year))
  if (any(!is.na(sy) & sy != yr))
    warning(sprintf("%s: Submission.Year column disagrees with filename year %d.",
                    basename(f), yr))
  d[, .(PWS_ID            = PWS.ID,
        Year              = yr,
        pws_type_code     = PWS.Type.Code,
        Connections_SDWIS = suppressWarnings(as.numeric(Service.Connections.Count)),
        PopServed_SDWIS   = suppressWarnings(as.numeric(Population.Served.Count)))]
}

sdwis <- rbindlist(lapply(files, read_year))
sdwis <- sdwis[!is.na(PWS_ID) & PWS_ID != ""]
# One Q1 snapshot per system per year: guard against an accidental duplicate pull.
dups <- sdwis[, .N, by = .(PWS_ID, Year)][N > 1]
if (nrow(dups) > 0)
  stop(sprintf("%d duplicate (PWS_ID, Year) rows -- check for a repeated file in %s",
               nrow(dups), sdwis_dir))
setkey(sdwis, PWS_ID, Year)

saveRDS(sdwis, .out)
message(sprintf("Wrote %s: %s rows, %d systems x %d years (%d--%d).",
                basename(.out), format(nrow(sdwis), big.mark = ","),
                uniqueN(sdwis$PWS_ID), uniqueN(sdwis$Year),
                min(sdwis$Year), max(sdwis$Year)))

}  # end reuse_prior guard
