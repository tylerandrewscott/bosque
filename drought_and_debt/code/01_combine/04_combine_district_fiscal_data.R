# =============================================================================
# 04_combine_district_fiscal_data.R
# -----------------------------------------------------------------------------
# Merge the district audit table with the debt/issuance table (both keyed on
# District_ID + FISCAL_YEAR). District-name normalization already happens
# upstream in the assemble scripts, so this stage only joins on District_ID.
# NOTE: the one-period "_P1" lag columns this stage used to add were REMOVED
# (2026-07-23): nothing consumed them — the panel builder time-aligns finances
# itself with a rolling join on FISCAL YEAR ENDED (max 730-day staleness), which
# is the correct previous-period logic. The output filename keeps its historical
# "_and_lagged_" name to avoid churning three consumers.
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
# (mostly amended/duplicate filings; a few may be distinct entities collapsed
# onto one District_ID by upstream name aliasing, which cannot be told apart
# here — so amounts are NOT summed). An outer merge on non-unique keys would
# cartesian-expand those district-years and then contaminate the lags below,
# so keep ONE row per key and say how many were dropped. The survivor must not
# depend on source row order: sort within key by completeness (fewest NAs
# first) and then by every remaining column as a tiebreak, so reruns and
# upstream reorderings always keep the same row.
key_cols <- c('District_ID','FISCAL_YEAR')
dedup_by_key <- function(dt, label) {
  dt <- copy(dt)
  dt[, .n_na := rowSums(is.na(dt))]
  setorderv(dt, c(key_cols, '.n_na', setdiff(names(dt), c(key_cols, '.n_na'))),
            na.last = TRUE)
  ndup <- sum(duplicated(dt, by = key_cols))
  if (ndup > 0) message(label, ': dropping ', ndup, ' duplicate District_ID+FISCAL_YEAR rows')
  unique(dt, by = key_cols)[, .n_na := NULL][]
}
audits <- dedup_by_key(audits, 'audits')
debt   <- dedup_by_key(debt,   'debt')
fin_dt <- merge(audits, debt, by = key_cols, all = TRUE)
fin_dt <- fin_dt[order(District_ID, FISCAL_YEAR)]

saveRDS(fin_dt, committed('combined_and_lagged_finances.RDS'))
