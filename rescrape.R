# =============================================================================
# rescrape.R  —  quick ad-hoc lookup against the TCEQ Drinking Water Viewer.
# -----------------------------------------------------------------------------
# SOURCE CHANGE (2026): the old "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" JSON/OData service (dwv.tceq.texas.gov). This scratch
# script now uses the shared client in util_code/scraping/dwv_api_helpers.R.
#
# It pulls Total Storage Capacity (TSTC) for the PWS ids in the id crosswalk —
# a small example of the new API; the production pull lives in
# drought_and_debt/code/00_assemble/06_htmlscrape_storage_interconnects.R.
# =============================================================================

library(data.table)
source("util_code/scraping/dwv_api_helpers.R")

id_id <- readRDS("drought_and_debt/input/id_crosswalk.rds")
pws   <- unique(trimws(id_id$PWS_ID[!is.na(id_id$PWS_ID)]))

ses <- dwv_session()

tstc <- rbindlist(lapply(pws, function(pid) {
  s <- tryCatch(dwv_system(ses, pid), error = function(e) data.table())
  if (!nrow(s)) return(data.table(PWS_ID = pid, TSTC_MG = NA_real_))
  m <- tryCatch(dwv_widget(ses, "DashWaterSystemMeasures",
                           s$TINWSYS_IS_NUMBER, s$NUMBER0), error = function(e) data.table())
  v <- if (nrow(m)) m[grepl("^TSTC", MEASURE_NAME), MEASURE_QUANTITY] else numeric(0)
  data.table(PWS_ID = pid, TSTC_MG = if (length(v)) v[1] else NA_real_)
}), fill = TRUE)

print(head(tstc))
