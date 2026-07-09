# =============================================================================
# try_new_dww_scrape.R
# -----------------------------------------------------------------------------
# Build the master list of Texas public water systems
# (input/texas_dww/district_master_list.csv), consumed by 06_htmlscrape_* and
# the explore/ scripts.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app was
# decommissioned and replaced by the "Drinking Water Viewer" JSON/OData service
# (dwv.tceq.texas.gov). This script previously scraped the SearchDispatch
# result tables and also cached each system's datasheet HTML. Both are replaced
# by a single DashMain search per water-system type.
#
# Output: input/texas_dww/district_master_list.csv
#   columns (unchanged, so downstream scripts keep working):
#     "Water System No.", "Water System Name", "Type", "Status",
#     "Pri. Cnty Served", "Pri. Src. Water Type"
# =============================================================================

library(data.table)

.dwv_helper <- Sys.glob(c("dwv_api_helpers.R",
                          "util_code/scraping/dwv_api_helpers.R",
                          "../util_code/scraping/dwv_api_helpers.R"))
if (!length(.dwv_helper)) stop("dwv_api_helpers.R not found next to this script.")
source(.dwv_helper[1])

ses <- dwv_session()

# All system types, all activity statuses (the old SearchDispatch used
# ActivityStatusCD=All), so the master list mirrors the old coverage.
master <- rbindlist(lapply(c("C", "NC", "NTNC"), function(ty)
  dwv_search(ses, type = ty, active_only = FALSE,
             select = c("NUMBER0", "NAME", "D_PWS_FED_TYPE_CD", "ACTIVITY_STATUS_CD",
                        "D_PRIN_CNTY_SVD_NM", "D_FED_PRIM_SRC_CD"))),
  use.names = TRUE, fill = TRUE)

main_record <- master[, .(
  `Water System No.`      = trimws(NUMBER0),
  `Water System Name`     = NAME,
  `Type`                  = trimws(D_PWS_FED_TYPE_CD),
  `Status`                = trimws(ACTIVITY_STATUS_CD),
  `Pri. Cnty Served`      = D_PRIN_CNTY_SVD_NM,
  `Pri. Src. Water Type`  = trimws(D_FED_PRIM_SRC_CD))]

dir.create("input/texas_dww", showWarnings = FALSE, recursive = TRUE)
fwrite(main_record, "input/texas_dww/district_master_list.csv")
message("Wrote input/texas_dww/district_master_list.csv (", nrow(main_record), " systems).")
