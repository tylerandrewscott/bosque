# =============================================================================
# scrape_water_connections.R
# -----------------------------------------------------------------------------
# Wholesale water connections between public water systems: who buys from whom
# and who sells to whom.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" JSON/OData service (dwv.tceq.texas.gov). The old
# script scraped a free-text "buys from / sells to" datasheet block and
# regex-parsed the PWS ids out of it. The new API exposes these as structured
# tables, so the brittle regex parsing is gone:
#   "buys from"  -> DashPurchases  (SELLERWSNUMBER = system we buy from)
#   "sells to"   -> DashBuyers     (BUYERWSNUMBER  = system we sell to)
#
# Outputs (unchanged roles):
#   input/texas_dww/purchasing_connections_<date>.csv  (Buyer, Seller)
#   input/texas_dww/sales_connections_<date>.csv       (Seller, Buyer)
# =============================================================================

library(data.table)

.dwv_helper <- Sys.glob(c("dwv_api_helpers.R",
                          "util_code/scraping/dwv_api_helpers.R",
                          "../util_code/scraping/dwv_api_helpers.R"))
if (!length(.dwv_helper)) stop("dwv_api_helpers.R not found next to this script.")
source(.dwv_helper[1])

ses <- dwv_session()

# Active community water systems (matches the old WaterSystemType=C query).
systems <- dwv_search(ses, type = "C", active_only = TRUE,
                      select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME"))
message("Active community systems: ", nrow(systems))

# --- Purchases: for each system, the systems it BUYS water from ---------------
purch_of <- function(dt, sysrow) {
  if (!nrow(dt) || !"SELLERWSNUMBER" %in% names(dt)) return(data.table())
  data.table(Buyer  = trimws(sysrow$NUMBER0),
             Seller = trimws(dt$SELLERWSNUMBER),
             Seller_Name = if ("SELLERWS" %in% names(dt)) dt$SELLERWS else NA_character_)
}
purchase_df <- dwv_widget_over(ses, "DashPurchases", systems, transform = purch_of)
purchase_df <- unique(purchase_df[!is.na(Seller) & nzchar(Seller)])

# --- Sales: for each system, the systems it SELLS water to --------------------
sale_of <- function(dt, sysrow) {
  if (!nrow(dt) || !"BUYERWSNUMBER" %in% names(dt)) return(data.table())
  data.table(Seller = trimws(sysrow$NUMBER0),
             Buyer  = trimws(dt$BUYERWSNUMBER),
             Buyer_Name = if ("BUYERWS" %in% names(dt)) dt$BUYERWS else NA_character_)
}
sale_df <- dwv_widget_over(ses, "DashBuyers", systems, transform = sale_of)
sale_df <- unique(sale_df[!is.na(Buyer) & nzchar(Buyer)])

dir.create("input/texas_dww", showWarnings = FALSE, recursive = TRUE)
p_out <- file.path("input/texas_dww", paste0("purchasing_connections_", Sys.Date(), ".csv"))
s_out <- file.path("input/texas_dww", paste0("sales_connections_", Sys.Date(), ".csv"))
fwrite(purchase_df, p_out)
fwrite(sale_df, s_out)
message("Wrote ", p_out, " (", nrow(purchase_df), " rows) and ",
        s_out, " (", nrow(sale_df), " rows).")
