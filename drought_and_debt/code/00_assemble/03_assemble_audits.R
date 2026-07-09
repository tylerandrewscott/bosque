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

audits$FISCAL_YEAR = year(mdy(audits$`FISCAL YEAR ENDED`))
audits$`FISCAL YEAR ENDED` <- mdy(audits$`FISCAL YEAR ENDED`)
setnames(audits,"DISTRICT_ID", "District_ID")
setnames(audits,"TOTAL TAX RATE", "Total_Tax_Rate")

audits$common = paste(audits$District_ID,audits$`FISCAL YEAR ENDED`,sep='_')
audits$zeros = rowSums(audits == 0,na.rm = T)
audits = audits[order(common, -zeros),]
audits = audits[!duplicated(audits, incomparables=FALSE, fromLast=FALSE, by='common'),]
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

audits$DISTRICT_NAME = gsub('(\\s)0(?=[0-9])','\\1\\2',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('DIST$|DISTR$','DISTRICT',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL UTILITY DISTRICT ',' MUD ',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' UD',' UTILITY DISTRICT',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(' MUNICIPAL DISTRICT$',' MUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('SPECIAL UTILITY DISTRICT','SUD',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub('WATER CONTROL DISTRICT','WCID',audits$DISTRICT_NAME,perl = T)
#audits$DISTRICT_NAME = gsub('NAVIGATION DISTRICT','ND',audits$DISTRICT_NAME,perl = T)
#audits$DISTRICT_NAME = gsub('IRRIGATION DISTRICT','ID',audits$DISTRICT_NAME,perl = T)
#audits$DISTRICT_NAME = gsub('DRAINAGE DISTRICT','DD',audits$DISTRICT_NAME,perl = T)
#audits$DISTRICT_NAME = gsub('RIVER AUTHORITY','RA',audits$DISTRICT_NAME,perl = T)
audits$DISTRICT_NAME = gsub(" OF [A-Z]{1,} COUNTY$","",audits$DISTRICT_NAME,perl=T)

saveRDS(audits, committed('district_audits.RDS'))
