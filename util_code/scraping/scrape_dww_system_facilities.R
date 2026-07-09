# =============================================================================
# scrape_dww_system_facilities.R
# -----------------------------------------------------------------------------
# The list of facilities (wells, treatment plants, storage, distribution, etc.)
# operated by each public water system.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" JSON/OData service (dwv.tceq.texas.gov). This script
# no longer scrapes HTML tables; it calls the DWV API.
#
# Old datasheet table       -> new DWV endpoint
#   "Water System Facilities" -> FacilityList  (all facility types)
#
# NOTE: the old script also wrote a separate "system_facility_connections"
# file. Inter-system water transfers (facility interconnections) now come from
# the buyers/sellers endpoints — see scrape_water_connections.R.
#
# Output: input/texas_dww/system_facilities_<date>.csv
#   columns include: PWS_ID, ST_ASGN_IDENT_CD (facility id), NAME, TYPE_CODE,
#   ACTIVITY_STATUS_CD, AVAILABILITY_CODE, WATER_TYPE_CODE, LAT/LONG
# =============================================================================

library(data.table)

.dwv_helper <- Sys.glob(c("dwv_api_helpers.R",
                          "util_code/scraping/dwv_api_helpers.R",
                          "../util_code/scraping/dwv_api_helpers.R"))
if (!length(.dwv_helper)) stop("dwv_api_helpers.R not found next to this script.")
source(.dwv_helper[1])

ses <- dwv_session()

# Community water systems (matches the old WaterSystemType=C query).
systems <- dwv_search(ses, type = "C", active_only = TRUE,
                      select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME"))
message("Active community systems: ", nrow(systems))

# FacilityList scoped to the system (no TYPE_CODE filter -> all facilities).
facility_dt <- dwv_widget_over(ses, "FacilityList", systems,
                               orderby = "TYPE_CODE,ST_ASGN_IDENT_CD")

dir.create("input/texas_dww", showWarnings = FALSE, recursive = TRUE)
out <- file.path("input/texas_dww", paste0("system_facilities_", Sys.Date(), ".csv"))
fwrite(facility_dt, out)
message("Wrote ", out, " (", nrow(facility_dt), " facilities).")
