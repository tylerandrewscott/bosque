# =============================================================================
# 03_combine_district_fiscal_data.R
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
  library(dplyr)     # lag()
})

audits = readRDS(committed('district_audits.RDS'))
debt   = readRDS(committed('district_debt_issuances.RDS'))
audits$District_ID <- as.character(audits$District_ID)
debt$District_ID   <- as.character(debt$District_ID)
setnames(debt,'FiscalYear','FISCAL_YEAR')
fin_dt <- merge(audits,debt,all = T)

# One-period lag of every fiscal variable, within district, ordered by year.
fvars = names(fin_dt)[grep('TAX|FUND|REV|EXP|ISSUE|DEBT|PRINC|INTER|BONDS',toupper(names(fin_dt)))]
newnames = paste0(fvars,'_P1')
fin_dt = fin_dt[order(District_ID,FISCAL_YEAR),]
fin_dt[,(newnames):=lapply(.SD,lag),by = .(District_ID),.SDcols = fvars]

saveRDS(fin_dt, committed('combined_and_lagged_finances.RDS'))
