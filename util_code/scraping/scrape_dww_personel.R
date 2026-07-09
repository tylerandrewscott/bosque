# =============================================================================
# scrape_dww_personel.R
# -----------------------------------------------------------------------------
# Personnel / points of contact for each public water system.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" JSON/OData service (dwv.tceq.texas.gov). This script
# no longer scrapes HTML tables; it calls the DWV API.
#
# Old datasheet section  -> new DWV endpoint
#   points of contact    -> DashContacts   (name, address, phones, Role(s))
#
# NOTE: the old script also produced a separate "licensed_operators" file from
# a datasheet operator table. In SDWIS/DWV, operator licensing lives in a
# different system (TCEQ Licensing) and is no longer part of the water-system
# viewer, so that table is not reproduced here. Contact roles (operator, owner,
# administrative, etc.) are captured in the Roles column below.
#
# Output: input/texas_dww/personnel_records_<date>.csv
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

contact_of <- function(dt, sysrow) {
  if (!nrow(dt)) return(data.table())
  dt[, Roles := vapply(Roles, function(r) paste(trimws(unlist(r)), collapse = "; "), character(1))]
  dt[, System := trimws(sysrow$NUMBER0)]
  dt[, .(System, NAME, Roles,
         ADDRESS = trimws(paste(ADDR_LINE_ONE_TXT, ADDR_LINE_TWO_TXT)),
         CITY = ADDRESS_CITY_NAME, STATE = ADDRESS_STATE_CODE, ZIP = ADDRESS_ZIP_CODE,
         BUS_PHONE, MOB_PHONE, EMERG_PHONE)]
}
poc <- dwv_widget_over(ses, "DashContacts", systems, transform = contact_of)

dir.create("input/texas_dww", showWarnings = FALSE, recursive = TRUE)
out <- file.path("input/texas_dww", paste0("personnel_records_", Sys.Date(), ".csv"))
fwrite(poc, out)
message("Wrote ", out, " (", nrow(poc), " contact records).")
