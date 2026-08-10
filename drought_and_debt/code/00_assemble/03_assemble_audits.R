###

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
# (Dependency install is handled once by install_packages.R, not inline here.)
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)  # year / mdy / decimal_date
  library(readr)      # parse_number
  library(tidyr)      # replace_na
})

.out <- committed('district_audits.RDS')
if (reuse_prior(.out) && !isTRUE(REPROCESS)) {
  message("RESCRAPE=FALSE: reusing existing ", basename(.out),
          " (skipping audit re-processing; set REPROCESS=TRUE to re-run it).")
} else {

audits = fread(raw_input('tceq_audits', 'district_audits.csv'))

# Schema-drift check: warn if expected columns (used by derived fields below) are missing
expected_cols <- c(
  'DISTRICT_ID','FISCAL YEAR ENDED','TOTAL TAX RATE',
  'GENERAL FUND - FUND BALANCE','GENERAL FUND - TOTAL REVENUES','GENERAL FUND - TOTAL EXPENDITURES',
  'ENTERPRISE FUND - OPERATING REVENUES','ENTERPRISE FUND - OPERATING EXPENSES',
  'WATER CUSTOMERS - EQ SINGLE FAMILY UNITS',
  'WASTEWATER CUST - EQ SINGLE FAMILY UNITS','WASTEWATER CUST - EQ SINGLE FAMILY UNIT'
)
missing_cols <- setdiff(expected_cols, names(audits))
if (length(missing_cols) > 0) {
  warning(sprintf("assemble_audits.R: %d expected column(s) missing from district_audits.csv — derived fields will be NA:\n  %s",
                  length(missing_cols), paste(missing_cols, collapse = "\n  ")))
}

dinfo_dt <- load_latest_district_list()

audits$DISTRICT_NAME <- dinfo_dt$District_Name[match(audits$DISTRICT_ID,dinfo_dt$District_ID)]

money = as.vector(which(apply(audits,2,function(x) any(grepl('\\$',x)))))
audits = cbind(audits[,-money,with=F],audits[,lapply(.SD,parse_number),.SDcols = money])

audits$`FISCAL YEAR ENDED` <- mdy(audits$`FISCAL YEAR ENDED`)
# Repair mangled year typos (e.g. "08/31/0005" -> 2005), then drop the rows
# with no FY-end date at all: every downstream use (the panel's rolling join,
# the SDWIS denominator year-match) keys on this date.
audits$`FISCAL YEAR ENDED`[!is.na(audits$`FISCAL YEAR ENDED`) &
                           year(audits$`FISCAL YEAR ENDED`) < 1000] <-
  audits$`FISCAL YEAR ENDED`[!is.na(audits$`FISCAL YEAR ENDED`) &
                             year(audits$`FISCAL YEAR ENDED`) < 1000] + years(2000)
if (anyNA(audits$`FISCAL YEAR ENDED`)) {
  message(sprintf("Dropping %d audit row(s) with no parseable FISCAL YEAR ENDED date.",
                  sum(is.na(audits$`FISCAL YEAR ENDED`))))
  audits <- audits[!is.na(`FISCAL YEAR ENDED`),]
}
audits$FISCAL_YEAR = year(audits$`FISCAL YEAR ENDED`)
setnames(audits,"DISTRICT_ID", "District_ID")
setnames(audits,"TOTAL TAX RATE", "Total_Tax_Rate")

# One audit per district-YEAR. Refilings for the same fiscal year are typically
# CORRECTIONS or completions of an earlier submission (e.g. a $300M revenue typo
# later refiled as $3.0M), so instead of picking a single winning row we FILL
# EACH FIELD FORWARD from the most-recent submission that reported a usable value.
# DOC_ID is a sequential submission counter (higher = later; Spearman 0.99 vs
# DATE_SUBMITTED, and fully populated where DATE_SUBMITTED often is not), so per
# district-year we walk filings newest->oldest and take, for EACH COLUMN
# INDEPENDENTLY, the first value that is non-missing AND non-zero. A zero is how a
# blank/omitted money field surfaces in this source, so treating it as "not
# reported" lets an all-zero refiling fall back to the real values in the prior
# filing. This coalesces per variable; it never sums a variable across filings
# (the only addition downstream is the existing GF+ENT definition of Total_Revenue
# below). If every filing is zero/NA for a field, the newest value is kept as-is.
setDT(audits)
audits[, common := paste(District_ID, FISCAL_YEAR, sep = '_')]
setorder(audits, common, -DOC_ID)          # newest submission first within each district-year
n_refiled <- audits[, .N, by = common][N > 1, .N]
pick_recent <- function(x) {               # x ordered newest->oldest within the group
  usable <- if (is.numeric(x)) which(!is.na(x) & x != 0) else which(!is.na(x) & nzchar(as.character(x)))
  if (length(usable)) x[usable[1]] else x[1]
}
audits <- audits[, lapply(.SD, pick_recent), by = common]
if (n_refiled > 0) message(sprintf(
  "assemble_audits.R: collapsed %d district-year(s) with refilings via most-recent-non-zero fill-forward.",
  n_refiled))
audits$Date = decimal_date(audits$`FISCAL YEAR ENDED`)
audits$Retail_Wastewater = (audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`>0)+0
audits$y = 1
audits$District_ID = as.character(audits$District_ID)
#audits$YEAR <- floor(audits$Date)
#audits = audits[audits$YEAR %in% c(2007:2022),]
#audits$PERIOD= ifelse(floor(audits$Date) %in% c(2009:2011),'2009-2011',ifelse(floor(audits$Date) %in% c(2012:2014),'2012-2014','2015-2017'))
audits$Fund_Balance <- audits$`GENERAL FUND - FUND BALANCE`
audits$Total_Revenue <- audits$`ENTERPRISE FUND - OPERATING REVENUES` + audits$`GENERAL FUND - TOTAL REVENUES`
audits$Total_Expenditure<- audits$`GENERAL FUND - TOTAL EXPENDITURES`+ audits$`ENTERPRISE FUND - OPERATING EXPENSES`
audits$Water_SFU <- audits$`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`
audits$Wastewater_SFU <- replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNIT`,0) + 
  replace_na(audits$`WASTEWATER CUST - EQ SINGLE FAMILY UNITS`,0)

# Roster-specific abbreviations first, then the shared canonicalization
# (normalize_district_name + strip_county_suffix from ingest_helpers.R).
audits$DISTRICT_NAME = gsub('DIST$|DISTR$','DISTRICT',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT ',' MUD ',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('WATER CONTROL DISTRICT','WCID',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = normalize_district_name(audits$DISTRICT_NAME)
audits$DISTRICT_NAME = strip_county_suffix(audits$DISTRICT_NAME)

saveRDS(audits, committed('district_audits.RDS'))

}   # end RESCRAPE guard
