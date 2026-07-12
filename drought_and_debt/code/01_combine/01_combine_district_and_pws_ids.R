# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)    # str_split
  library(lubridate)  # mdy
})

dinfo_dt <- load_latest_district_list()

dinfo_dt$PWS_ID[dinfo_dt$PWS_ID == "NA"] <- NA
dinfo_dt$District_ID = as.character(dinfo_dt$District_ID)
#dinfo_dt = dinfo_dt[!is.na(PWS_ID)]
setkey(dinfo_dt,District_ID)
dinfo_dt$PWS_ID = str_split(dinfo_dt$PWS_ID,'\\|')
dinfo_dt$District_Type = dinfo_dt$Type
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('WATER CONTROL AND IMPROVEMENT DISTR')] <- 'WCID'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('MUNICIPAL UTILITY DISTRICT')] <- 'MUD'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('FRESH WATER SUPPLY DISTRICT')] <- 'FWSD'
dinfo_dt$District_Type[dinfo_dt$District_Type %in% c('SPECIAL UTILITY DISTRICT')] <- 'SUD'
keep = c('SUD','FWSD','MUD','WCID')

dinfo_dt$District_Type[!dinfo_dt$District_Type %in% keep] <- 'Other'

dinfo_dt$Created = mdy(dinfo_dt$Created)
#dinfo_dt <- dinfo_dt[dinfo_dt$District_Type %in% keep,]
#dinfo_dt <- dinfo_dt[mdy(dinfo_dt$Created)<mdy('01/01/2006'),]
dinfo_dt$Ended[dinfo_dt$Ended=='']<-NA
dinfo_dt <- dinfo_dt[is.na(dinfo_dt$Ended)|mdy(dinfo_dt$Ended)>mdy('01/01/2000'),]
dinfo_dt <- dinfo_dt[!grepl('MWA',dinfo_dt$District_Name),]
dinfo_dt <- dinfo_dt[District_Type!='Other',]

id_crosswalk<-rbindlist(mapply(function(x,y) data.table(District_ID = x,PWS_ID = y),x = dinfo_dt$District_ID,y = dinfo_dt$PWS_ID,SIMPLIFY = F))
id_crosswalk$PWS_ID <- format_pws_id(id_crosswalk$PWS_ID)

# Manual PWS -> District_ID corrections for systems the roster maps incorrectly.
# Kept in a data file (crosswalks/pws_district_id_overrides.csv) rather than as a
# wall of assignments, so the mapping is diffable and editable without code.
# Both sides go through format_pws_id() so the match never hinges on how the
# roster happened to format its ids.
overrides <- fread(file.path(CODE_DIR, "crosswalks", "pws_district_id_overrides.csv"),
                   colClasses = "character")
overrides$PWS_ID <- format_pws_id(overrides$PWS_ID)
# Fail fast on a malformed override row. This file is hand-edited, and a bad
# row would corrupt silently: format_pws_id() returns NA for garbage, the
# crosswalk holds ~1,600 legitimately-NA PWS_IDs, and match() pairs NA with
# NA — so one NA override would overwrite District_ID on NA-keyed roster
# rows. TX-prefixed garbage ("TXFOO") passes format_pws_id() untouched and
# would be appended as a bogus new link below.
bad <- is.na(overrides$PWS_ID) | !grepl("^TX[0-9]{7}$", overrides$PWS_ID) |
  is.na(overrides$District_ID) | !nzchar(trimws(overrides$District_ID))
if (any(bad)) {
  stop("Malformed row(s) in pws_district_id_overrides.csv (need PWS_ID = TX + ",
       "7 digits and a non-empty District_ID): ",
       paste0("PWS_ID=", overrides$PWS_ID[bad], "/District_ID=",
              overrides$District_ID[bad], collapse = "; "))
}
ix <- match(id_crosswalk$PWS_ID, overrides$PWS_ID, incomparables = NA)
id_crosswalk$District_ID[!is.na(ix)] <- overrides$District_ID[ix[!is.na(ix)]]
# Overrides whose PWS_ID the roster no longer links to any district (roster lists
# PWS_ID = NA for these) are appended as new rows: each was verified against the
# TCEQ service-area boundaries (pwsName == District_Name), so the manual link is
# trusted over the roster's missing one.
add <- overrides[!overrides$PWS_ID %in% id_crosswalk$PWS_ID]
if (nrow(add)) {
  message("Appending ", nrow(add), " override link(s) absent from the roster: ",
          paste(add$PWS_ID, collapse = ", "))
  id_crosswalk <- rbind(id_crosswalk, add[, .(District_ID, PWS_ID)])
}

saveRDS(id_crosswalk, committed('id_crosswalk.RDS'))
