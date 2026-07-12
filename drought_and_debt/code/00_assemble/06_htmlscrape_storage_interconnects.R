# =============================================================================
# 06_htmlscrape_storage_interconnects.R
# -----------------------------------------------------------------------------
# Total storage capacity (TSTC) and the number of interconnections with other
# public water systems, per active COMMUNITY water system.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app was
# decommissioned and replaced by the "Drinking Water Viewer" JSON/OData service
# (dwv.tceq.texas.gov). The name "htmlscrape" is now historical — there is no
# HTML to scrape. This script calls the DWV API via
# util_code/scraping/dwv_api_helpers.R.
#
# Old datasheet value                     -> new DWV endpoint
#   "TSTC"  (Total Storage Capacity)      -> DashWaterSystemMeasures
#                                            (MEASURE_NAME == "TSTC - ...")
#   # interconnections (CC connections)   -> nrow(DashPurchases)  [systems this
#                                            PWS buys water from]
#
# Output (incremental/resumable append):
#   input/storage_connections_data.txt
#     columns: PWS_ID, Var (== "TSTC"), Value, Unit, Num_Interconnections, Source
#
# Value is TSTC in MG (DWV serves it already in megagallons, so no unit parsing
# is needed — Unit is carried for provenance). A `Source` column marks rows as
# "DWV". The committed baseline file was scraped from the DECOMMISSIONED dww2
# source; to avoid silently mixing sources, this script IGNORES any existing
# file that is not tagged Source == "DWV" and re-fetches from scratch. Resume
# still works across interrupted DWV runs.
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)
source(util("scraping", "dwv_api_helpers.R"))   # dwv_session/search/widget clients

output_file <- committed("storage_connections_data.txt")
# Rescrape policy from config.R: RESCRAPE = TRUE re-fetches every system;
# FALSE fetches only systems missing from the existing (DWV-sourced) file.
CLOBBER <- isTRUE(RESCRAPE)
# Periodic checkpoints go to a gitignored scratch file, NEVER to the committed
# output: an interrupted run must not replace the committed file with a partial
# one. The committed file is (re)written only after the fetch loop completes.
progress_file <- scratch("storage_connections_progress.txt")

ses <- dwv_session()

# --- Active community water systems (replaces SearchDispatch WaterSystemType=C)
systems <- dwv_search(ses, type = "C", active_only = TRUE,
                      select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME"))
message("Active community systems: ", nrow(systems))

# --- Resume support: skip systems already fetched ------------------------------
# Two sources of already-fetched rows: the committed output (previous completed
# runs; ignored under CLOBBER) and the scratch progress file (an interrupted
# run's partial rows — removed on successful completion, so if it exists it is
# always a resume). Only reuse files this (DWV) script wrote. A pre-DWV baseline
# (no Source column, or Source != "DWV") is discarded so old- and new-source
# rows never mix.
read_dwv_rows <- function(f) {
  if (!file.exists(f)) return(data.table())
  d <- fread(f, colClasses = list(character = "PWS_ID"))
  if (!("Source" %in% names(d)) || !all(d$Source == "DWV")) {
    message("Existing ", basename(f),
            " is not a DWV-sourced file — ignoring it and re-fetching from scratch.")
    return(data.table())
  }
  d
}
committed_rows <- if (CLOBBER) data.table() else read_dwv_rows(output_file)
existing_data  <- unique(
  rbindlist(list(committed_rows, read_dwv_rows(progress_file)),
            use.names = TRUE, fill = TRUE),
  by = "PWS_ID")

todo <- if (!nrow(existing_data)) systems else
  systems[!(trimws(NUMBER0) %in% existing_data$PWS_ID)]
message("Systems to fetch: ", nrow(todo))

# --- Fetch TSTC + interconnection count per system ----------------------------
for (i in seq_len(nrow(todo))) {
  pws_id  <- trimws(todo$NUMBER0[i])
  tinwsys <- todo$TINWSYS_IS_NUMBER[i]
  number0 <- todo$NUMBER0[i]

  # Total Storage Capacity (TSTC) from the system-measures widget.
  meas <- tryCatch(dwv_widget(ses, "DashWaterSystemMeasures", tinwsys, number0),
                   error = function(e) data.table())
  tstc <- if (nrow(meas) && "MEASURE_NAME" %in% names(meas))
            meas[grepl("^TSTC", MEASURE_NAME)] else data.table()
  temp_dt <- data.table(
    PWS_ID = pws_id, Var = "TSTC",
    Value  = if (nrow(tstc)) as.numeric(tstc$MEASURE_QUANTITY[1]) else NA_real_,
    Unit   = if (nrow(tstc)) trimws(tstc$MEASURE_UOM_CODE[1]) else NA_character_,
    Source = "DWV")

  # Interconnections = number of systems this PWS purchases water from.
  purch <- tryCatch(dwv_widget(ses, "DashPurchases", tinwsys, number0),
                    error = function(e) data.table())
  temp_dt[, Num_Interconnections := nrow(purch)]

  existing_data <- rbindlist(list(existing_data, temp_dt), use.names = TRUE, fill = TRUE)

  if (i %% 100 == 0) {
    message(sprintf("  %d/%d systems", i, nrow(todo)))
    fwrite(existing_data, progress_file)        # periodic checkpoint (scratch)
  }
  Sys.sleep(0.05)
}

# The fetch loop completed: (re)write the committed output only now, and clear
# the scratch checkpoint so the next run starts clean.
if (nrow(existing_data) > nrow(committed_rows)) {
  fwrite(existing_data, output_file)
  message("Wrote ", output_file, " (", nrow(existing_data), " rows).")
} else {
  message("No new data.")
}
if (file.exists(progress_file)) invisible(file.remove(progress_file))
