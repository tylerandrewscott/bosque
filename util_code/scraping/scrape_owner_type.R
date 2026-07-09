# =============================================================================
# scrape_owner_type.R
# -----------------------------------------------------------------------------
# Owner / ownership type of each public water system.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" JSON/OData service (dwv.tceq.texas.gov).
#
# The old approach downloaded raw datasheet HTML for every system and later
# parsed the owner type out of it with an LLM (parse_owner_type_jsons.R). The
# new API exposes ownership directly as system indicators, so we fetch it
# cleanly — no HTML, no LLM step:
#   POWN - Previous Ownership Type Code (WUD ownership code, e.g. MUN, PRV, FED)
#   PRFT - For / Non-profit entity status
# plus the "Owner" contact record (DashContacts, Roles containing "OW").
#
# Output: input/texas_dww/owner_types_<date>.csv
#   columns: PWS_ID, NAME, OWNERSHIP_CODE, FOR_PROFIT, OWNER_NAME
# =============================================================================

library(data.table)

.dwv_helper <- Sys.glob(c("dwv_api_helpers.R",
                          "util_code/scraping/dwv_api_helpers.R",
                          "../util_code/scraping/dwv_api_helpers.R"))
if (!length(.dwv_helper)) stop("dwv_api_helpers.R not found next to this script.")
source(.dwv_helper[1])

ses <- dwv_session()

# All active systems (community, non-community, non-transient non-community) —
# matches the old script's three C / NC / NTNC SearchDispatch queries.
systems <- rbindlist(lapply(c("C", "NC", "NTNC"), function(ty)
  dwv_search(ses, type = ty, active_only = TRUE,
             select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME"))),
  use.names = TRUE, fill = TRUE)
message("Active systems: ", nrow(systems))

owner_of <- function(dt_ind, sysrow) {
  # dt_ind: DashWaterSystemIndicators for one system.
  get_ind <- function(code) {
    v <- dt_ind[grepl(paste0("^", code), INDICATOR_NAME), INDICATOR_VALUE_CD]
    if (length(v)) trimws(v[1]) else NA_character_
  }
  data.table(PWS_ID = trimws(sysrow$NUMBER0),
             NAME   = sysrow$NAME,
             OWNERSHIP_CODE = if (nrow(dt_ind)) get_ind("POWN") else NA_character_,
             FOR_PROFIT     = if (nrow(dt_ind)) get_ind("PRFT") else NA_character_)
}
owner_dt <- dwv_widget_over(ses, "DashWaterSystemIndicators", systems, transform = owner_of)

# Attach the owner contact name (Roles list contains "OW - Owner").
owner_name_of <- function(dt_con, sysrow) {
  if (!nrow(dt_con)) return(data.table(PWS_ID = trimws(sysrow$NUMBER0), OWNER_NAME = NA_character_))
  is_owner <- vapply(dt_con$Roles, function(r) any(grepl("OW", unlist(r))), logical(1))
  nm <- if (any(is_owner)) dt_con$NAME[which(is_owner)[1]] else NA_character_
  data.table(PWS_ID = trimws(sysrow$NUMBER0), OWNER_NAME = nm)
}
owner_names <- dwv_widget_over(ses, "DashContacts", systems, transform = owner_name_of)

owner_dt <- merge(owner_dt, owner_names, by = "PWS_ID", all.x = TRUE)

dir.create("input/texas_dww", showWarnings = FALSE, recursive = TRUE)
out <- file.path("input/texas_dww", paste0("owner_types_", Sys.Date(), ".csv"))
fwrite(owner_dt, out)
message("Wrote ", out, " (", nrow(owner_dt), " systems).")
