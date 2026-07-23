###

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
# (Dependency install is handled once by install_packages.R, not inline here.)
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)  # str_remove
  library(xml2)     # read_xml / xml_find_* -- parse the pinned TBRB debt XML export
})

.out <- committed('district_debt_issuances.RDS')
if (reuse_prior(.out)) {
  message("RESCRAPE=FALSE: reusing existing ", basename(.out), " (skipping debt XML parse).")
} else {

# TBRB "Debt Outstanding by Local Government" (Socrata dataset dyv5-3bjd), read
# from a PINNED local export in the raw tbrb folder rather than the live API, so
# the debt snapshot is fixed and reproducible. This full-history series covers WD
# FY2007-2025 -- the whole 2010-2025 analysis window -- with the GO (tax-backed) /
# REV (revenue-backed) pledge split, which is why (a) the old 6d42-4z7a pull
# (2016+ only) is gone and (b) the audit `BONDS OUTSTANDING` fallback could be
# dropped in build_recurrent_panel.R. The Socrata query.xml export nests one <row>
# per (government, fiscal year, pledge type); fields vary per row (REV rows omit
# the tax columns), so each needed field is pulled with xml_find_first (NA-safe and
# row-aligned) rather than assuming a fixed child order. Same column names the rest
# of this script expects, so it stays a drop-in.
# Source: https://data.texas.gov/d/dyv5-3bjd  (exported 2026-07-22)
.debt_rows <- xml_find_all(
  read_xml(raw_input('tbrb', 'Debt_Outstanding_By_Local_Government_20260722.xml')),
  "//row")
.getcol <- function(field) xml_text(xml_find_first(.debt_rows, paste0("./", field)))
debt <- data.table(
  GovernmentType              = .getcol('governmenttype'),
  GovernmentName              = .getcol('governmentname'),
  FiscalYear                  = as.integer(.getcol('fiscalyear')),
  PledgeType                  = .getcol('pledgetype'),
  TotalPrincipalOutstanding   = as.numeric(.getcol('totalprincipaloutstanding')),
  TotalDebtServiceOutstanding = as.numeric(.getcol('totaldebtserviceoutstanding'))
)

debt = debt[debt$GovernmentType == 'WD',]
debt$GovernmentName <- str_remove(toupper(debt$GovernmentName),"(\\s|-)DEFINED AREA.*")
debt = debt[,list(sum(TotalDebtServiceOutstanding),sum(TotalPrincipalOutstanding)),by=.(GovernmentName,FiscalYear,PledgeType)]
setnames(debt,c('V1','V2'),c('TotalDebtServiceOutstanding','TotalPrincipalOutstanding'))


debt <- dcast(data = debt,GovernmentName + FiscalYear ~ PledgeType,value.var = c('TotalDebtServiceOutstanding','TotalPrincipalOutstanding'))
    

# NOTE: the TBRB Local-Issuance (new-money) series is deliberately NOT used. It
# only reaches back to ~2015 and its NewMoney_* columns never entered the model
# (fiscal_vars uses debt OUTSTANDING per connection, not issuance), so the debt
# measure now rests solely on the outstanding balances parsed above.
fin <- debt

# ---- District_ID linkage: consume the committed crosswalk --------------------
# TBRB carries no district ID, only a government NAME. The NAME->District_ID
# mapping is a human-reviewed committed artifact (input/tbrb_district_crosswalk.csv),
# built and re-proposed by code/00_assemble/build_tbrb_crosswalk.R against the
# AUDIT district universe with a PWS-linked tiebreak (so a reformed district's debt
# lands on the operational ID the panel carries, never a no-PWS shell ID). This
# script does NO name matching of its own -- it is a plain join on GovernmentName,
# which above was uppercased + DEFINED-AREA-stripped byte-for-byte the same way the
# builder keys the crosswalk. Names absent from the crosswalk (new TBRB governments
# in a later export, or wholesale/regional authorities with no district audit) are
# reported with their dollar totals and dropped -- rerun the builder + re-review to
# fold in any genuinely recoverable ones.
xwalk <- data.table::fread(committed("tbrb_district_crosswalk.csv"),
                           colClasses = list(character = "District_ID"))
fin$District_ID <- xwalk$District_ID[match(fin$GovernmentName, xwalk$GovernmentName)]

# Outstanding principal is a STOCK, so summarize each unmatched government by its
# median annual principal (GO+REV), then total across governments -- summing the
# balance across all 19 years would overstate the dropped debt ~19x.
.prin_cols <- grep("^TotalPrincipalOutstanding", names(fin), value = TRUE)
fin$.prin  <- rowSums(as.matrix(fin[, .prin_cols, with = FALSE]), na.rm = TRUE)
miss <- is.na(fin$District_ID)
if (any(miss)) {
  miss_tab <- fin[miss, .(prin_med = median(.prin, na.rm = TRUE)), by = GovernmentName][order(-prin_med)]
  data.table::fwrite(miss_tab, scratch("tbrb_unmatched_governments.csv"))
  message(sprintf(
    "debt link: %d of %d district-years linked; %d unmatched (%d governments, $%.1fM median-year principal) dropped -> scratch/tbrb_unmatched_governments.csv",
    sum(!miss), nrow(fin), sum(miss), nrow(miss_tab), sum(miss_tab$prin_med) / 1e6))
} else {
  message(sprintf("debt link: all %d district-years linked to the committed crosswalk", nrow(fin)))
}
fin$.prin <- NULL
fin <- fin[!is.na(fin$District_ID),]

# Several TBRB governments can map to one operational District_ID -- a district's
# defined-area subunits (e.g. TRAVIS COUNTY WCID 17 (A)/(B)/(C)/(D)) each file
# their own bonds, but the district's single audit ID covers all of them. Sum the
# outstanding balances to the (District_ID, FiscalYear) key the rest of the
# pipeline joins on, so no subunit's debt is lost to the de-duplication in
# 04_combine_district_fiscal_data.R. A no-op for the 1:1 districts (one row each).
.val_cols <- grep("^Total(Debt|Principal)", names(fin), value = TRUE)
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
fin <- fin[, lapply(.SD, sum_or_na), by = .(District_ID, FiscalYear), .SDcols = .val_cols]

# Filename kept as-is for downstream compatibility (01_combine reads it), though it
# now holds debt OUTSTANDING only -- no issuance / new-money columns.
saveRDS(fin, committed('district_debt_issuances.RDS'))

}   # end RESCRAPE guard


