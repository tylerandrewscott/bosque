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
# Value is TSTC in whatever unit DWV serves (mostly MG, occasionally GAL); the
# Unit column is carried so build_recurrent_panel.R can normalize to MG — do
# NOT assume MG here. A `Source` column marks rows as "DWV". The committed
# baseline file was scraped from the DECOMMISSIONED dww2 source; to avoid
# silently mixing sources, this script IGNORES any existing file that is not
# tagged Source == "DWV" and re-fetches from scratch.
#
# FAILURE SEMANTICS: each widget call gets in-run retries (dwv_widget_retry);
# a query that still fails is recorded with Fetch_OK = FALSE and NA values —
# never as a zero — and the next run automatically re-queues those systems.
# Scrape steps are ADDITIVE (append/top-up/retry); they never clobber good rows.
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)
source(util("scraping", "dwv_api_helpers.R"))   # dwv_session/search/widget clients

output_file <- committed("storage_connections_data.txt")
# Rescrape policy from config.R: RESCRAPE = TRUE re-fetches every system;
# FALSE fetches only systems missing from the existing (DWV-sourced) file.
# Under BOTH policies the committed file is merged UNDER the fresh rows at
# write time: re-fetched systems get their fresh values, and systems that
# dropped off the DWV active list keep their prior rows (a full rescrape
# refreshes values, it never discards previously collected data).
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
# runs; counts as "done" only under !CLOBBER — under CLOBBER every system is
# re-fetched, though the committed rows are still merged back in at write time)
# and the scratch progress file (an interrupted run's partial rows — removed on
# successful completion, so if it exists it is always a resume, under either
# policy). Only reuse files this (DWV) script wrote. A pre-DWV baseline (no
# Source column, or Source != "DWV") is discarded so old- and new-source rows
# never mix.
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
committed_rows <- read_dwv_rows(output_file)
existing_data  <- unique(
  rbindlist(list(if (CLOBBER) data.table() else committed_rows,
                 read_dwv_rows(progress_file)),
            use.names = TRUE, fill = TRUE),
  by = "PWS_ID")
# Rows whose queries FAILED last run (Fetch_OK == FALSE; NA on legacy rows means
# OK) are re-queued instead of sitting as false zeros forever.
if ("Fetch_OK" %in% names(existing_data)) {
  .n_failed <- existing_data[Fetch_OK %in% FALSE, .N]
  if (.n_failed) message("Re-queuing ", .n_failed, " system(s) whose queries failed on a prior run.")
  existing_data <- existing_data[!Fetch_OK %in% FALSE]
}

todo <- if (!nrow(existing_data)) systems else
  systems[!(trimws(NUMBER0) %in% existing_data$PWS_ID)]
message("Systems to fetch: ", nrow(todo))

# --- Fetch TSTC + interconnection count per system ----------------------------
for (i in seq_len(nrow(todo))) {
  pws_id  <- trimws(todo$NUMBER0[i])
  tinwsys <- todo$TINWSYS_IS_NUMBER[i]
  number0 <- todo$NUMBER0[i]

  # Total Storage Capacity (TSTC) from the system-measures widget. NULL means
  # the query FAILED after retries (recorded Fetch_OK = FALSE below, re-queued
  # next run); an empty table is a genuine "system reports no TSTC".
  meas  <- dwv_widget_retry(ses, "DashWaterSystemMeasures", tinwsys, number0)
  # Interconnections = number of systems this PWS purchases water from.
  purch <- dwv_widget_retry(ses, "DashPurchases", tinwsys, number0)
  tstc <- if (!is.null(meas) && nrow(meas) && "MEASURE_NAME" %in% names(meas))
            meas[grepl("^TSTC", MEASURE_NAME)] else data.table()
  temp_dt <- data.table(
    PWS_ID = pws_id, Var = "TSTC",
    Value  = if (nrow(tstc)) as.numeric(tstc$MEASURE_QUANTITY[1]) else NA_real_,
    Unit   = if (nrow(tstc)) trimws(tstc$MEASURE_UOM_CODE[1]) else NA_character_,
    Source = "DWV",
    Fetch_OK = !is.null(meas) && !is.null(purch))
  temp_dt[, Num_Interconnections := if (is.null(purch)) NA_integer_ else nrow(purch)]

  existing_data <- rbindlist(list(existing_data, temp_dt), use.names = TRUE, fill = TRUE)

  if (i %% 100 == 0) {
    message(sprintf("  %d/%d systems", i, nrow(todo)))
    fwrite(existing_data, progress_file)        # periodic checkpoint (scratch)
  }
  Sys.sleep(0.05)
}

# The fetch loop completed: (re)write the committed output only now, and clear
# the scratch checkpoint so the next run starts clean. Fresh/resumed rows come
# FIRST so unique(by = "PWS_ID") keeps the just-fetched value for re-fetched
# systems, while committed rows survive for systems not fetched this run (e.g.
# systems that dropped off the DWV active list) — the file can gain rows or
# refresh them, never lose them.
final_data <- unique(
  rbindlist(list(existing_data, committed_rows), use.names = TRUE, fill = TRUE),
  by = "PWS_ID")
if (nrow(todo) > 0 || nrow(final_data) > nrow(committed_rows)) {
  fwrite(final_data, output_file)
  message("Wrote ", output_file, " (", nrow(final_data), " rows).")
} else {
  message("No new data.")
}
if (file.exists(progress_file)) invisible(file.remove(progress_file))
