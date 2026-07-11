# =============================================================================
# 04_combine_district_fiscal_data.R
# -----------------------------------------------------------------------------
# Merge the district audit table with the debt/issuance table (both keyed on
# District_ID + FISCAL_YEAR) and add one-period lags of every fiscal variable.
# District-name normalization already happens upstream in the assemble scripts,
# so this stage only joins on District_ID and lags. (A large block of legacy
# name-normalization code lived here commented-out; removed — see git history.)
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
})

audits = readRDS(committed('district_audits.RDS'))
debt   = readRDS(committed('district_debt_issuances.RDS'))
audits$District_ID <- as.character(audits$District_ID)
debt$District_ID   <- as.character(debt$District_ID)
setnames(debt,'FiscalYear','FISCAL_YEAR')

# Both tables carry a handful of duplicate District_ID + FISCAL_YEAR rows
# (amended/duplicate filings). An outer merge on non-unique keys would
# cartesian-expand those district-years and then contaminate the lags below,
# so keep one row per key (first after ordering) and say how many were dropped.
key_cols <- c('District_ID','FISCAL_YEAR')
dedup_by_key <- function(dt, label) {
  dt <- dt[order(District_ID, FISCAL_YEAR)]
  ndup <- sum(duplicated(dt, by = key_cols))
  if (ndup > 0) message(label, ': dropping ', ndup, ' duplicate District_ID+FISCAL_YEAR rows')
  unique(dt, by = key_cols)
}
audits <- dedup_by_key(audits, 'audits')
debt   <- dedup_by_key(debt,   'debt')
fin_dt <- merge(audits, debt, by = key_cols, all = TRUE)

# One-period lag of every fiscal variable, within district, ordered by year.
# Keys are unique after the dedup above, so shift() moves to the previous
# available fiscal year for that district.
fvars = names(fin_dt)[grep('TAX|FUND|REV|EXP|ISSUE|DEBT|PRINC|INTER|BONDS',toupper(names(fin_dt)))]
newnames = paste0(fvars,'_P1')
fin_dt = fin_dt[order(District_ID,FISCAL_YEAR),]
fin_dt[,(newnames):=lapply(.SD,shift),by = .(District_ID),.SDcols = fvars]

saveRDS(fin_dt, committed('combined_and_lagged_finances.RDS'))
