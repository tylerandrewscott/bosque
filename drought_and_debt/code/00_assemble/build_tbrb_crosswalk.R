#!/usr/bin/env Rscript
# =============================================================================
# build_tbrb_crosswalk.R  —  (re)generate the TBRB GovernmentName -> District_ID
# crosswalk seed for human review.
# -----------------------------------------------------------------------------
# NOT part of the main run_all pipeline. Run this by hand only when the pinned
# TBRB "Debt Outstanding by Local Government" export is refreshed (new fiscal
# year / new districts). It proposes a District_ID for every TBRB WD government
# and writes a REVIEWABLE seed to scratch/. After you check the REVIEW rows and
# recoverable UNMATCHED rows, promote the seed to the committed crosswalk:
#
#     cp scratch/tbrb_crosswalk_seed.csv input/tbrb_district_crosswalk.csv
#
# 04_assemble_debt.R is a thin CONSUMER of that committed CSV (a plain join on
# GovernmentName) -- it does no name matching of its own. This script is the one
# and only place the fuzzy name->ID logic (canon_key) lives.
#
# Target = the AUDIT district universe (the IDs debt must join to), so shell/PWS
# duplicate IDs can't strand the debt. Each proposed row carries a confidence
# tier; a key resolving to >1 audit ID (a reformed district) is flagged REVIEW.
# =============================================================================
suppressMessages({library(data.table); library(xml2); library(stringr)})
source("code/config.R"); source("code/ingest_helpers.R")

# ---- canon_key: the single home of the TBRB<->audit name normalization -------
# Applied to BOTH sides so abbreviation / county-suffix / punctuation / zero-pad
# differences collapse to a shared key. Entity types contract longest-phrase
# first so "MUNICIPAL UTILITY DISTRICT" -> MUD resolves before "UTILITY DISTRICT".
.TYPE_CONTRACTIONS <- c(
  "WATER CONTROL AND IMPROVEMENT DISTRICT" = "WCID",
  "FRESH WATER SUPPLY DISTRICT"            = "FWSD",
  "MUNICIPAL MANAGEMENT DISTRICT"          = "MMD",
  "MUNICIPAL UTILITY DISTRICT"             = "MUD",
  "MUNICIPAL WATER DISTRICT"               = "MWD",
  "MUNICIPAL WATER AUTHORITY"              = "MWA",
  "MUNICIPAL UTILITY AUTHORITY"            = "MUA",
  "REGIONAL WATER AUTHORITY"               = "RWA",
  "SPECIAL UTILITY DISTRICT"               = "SUD",
  "PUBLIC UTILITY DISTRICT"                = "PUD",
  "LEVEE IMPROVEMENT DISTRICT"             = "LID",
  "WATER SUPPLY DISTRICT"                  = "WSD",
  "WATER SUPPLY CORPORATION"               = "WSC",
  "NAVIGATION DISTRICT"                    = "ND",
  "DRAINAGE DISTRICT"                      = "DD",
  "IMPROVEMENT DISTRICT"                   = "ID",
  "IRRIGATION DISTRICT"                    = "ID",
  "UTILITY DISTRICT"                       = "UD",
  "WATER AUTHORITY"                        = "WA",
  "RIVER AUTHORITY"                        = "RA",
  "WATER DISTRICT"                         = "WD")
canon_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- str_remove(x, "(\\s|-)DEFINED AREA.*")
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[’'`.,]", "", x, perl = TRUE)                # drop apostrophes/periods/commas
  x <- gsub("-", " ", x, fixed = TRUE)                          # hyphen -> space
  x <- gsub("\\bN(O|UMBER)\\.?\\s+([0-9])", " \\2", x, perl = TRUE)  # "NO 5"/"NUMBER 5" -> " 5"
  x <- gsub("\\s+OF\\s+[A-Z ]+?\\s+COUNT(Y|IES)\\b.*$", "", x, perl = TRUE)  # drop "OF X COUNTY"
  repeat { y <- gsub("(\\s)0+([0-9])", "\\1\\2", x, perl = TRUE); if (identical(y, x)) break; x <- y }
  for (i in seq_along(.TYPE_CONTRACTIONS))
    x <- gsub(paste0("\\b", names(.TYPE_CONTRACTIONS)[i], "\\b"),
              .TYPE_CONTRACTIONS[i], x, perl = TRUE)
  gsub("\\s+", " ", trimws(x))
}
num <- function(x) as.numeric(gsub("[^0-9eE.+-]", "", as.character(x)))

# ---- audit universe: (District_ID, name), one operational ID per canon key ----
au <- as.data.table(readRDS(committed("district_audits.RDS")))
au[, District_ID := as.character(District_ID)]
au[, FY := as.integer(FISCAL_YEAR)]
if (all(is.na(au$FY))) au[, FY := as.integer(format(as.Date(`FISCAL YEAR ENDED`), "%Y"))]
au_uni <- unique(au[!is.na(District_ID), .(District_ID, DISTRICT_NAME)])
au_uni[, key := canon_key(DISTRICT_NAME)]
au_uni <- au_uni[!is.na(key) & key != ""]
# A canon key that resolves to >1 audit District_ID is a district that REFORMED
# under a new ID (same district). The correct target is the PWS-LINKED operational
# ID: it is the ID the panel's systems carry (so debt can reach them) and the ID
# the frailty groups on. Verified: every reformed district has exactly ONE
# PWS-linked sibling (the others are non-PWS shells with 0-1 in-window audits), so
# this is unambiguous. Fallbacks (for the rare all-shell key): most recent audit
# fiscal year, then larger numeric ID.
di <- as.data.table(load_latest_district_list()); di[, District_ID := as.character(District_ID)]
pws_ids <- unique(di[!is.na(PWS_ID) & PWS_ID != "", District_ID])
id_lastfy <- au[!is.na(FY), .(lastfy = max(FY)), by = District_ID]
ik <- unique(au_uni[, .(key, District_ID)])
ik <- merge(ik, id_lastfy, by = "District_ID", all.x = TRUE)
ik[, haspws := District_ID %in% pws_ids]
ik[, idnum := suppressWarnings(as.integer(District_ID))]
setorder(ik, key, -haspws, -lastfy, -idnum)   # PWS-linked first, then recency, then larger ID
amap <- ik[, .(District_ID = District_ID[1], n_audit_ids = .N,
               all_ids = paste(District_ID, collapse = "/")), by = key]
amap[, audit_name := au_uni$DISTRICT_NAME[
       match(paste(key, District_ID), paste(au_uni$key, au_uni$District_ID))]]

# ---- TBRB debt: raw GovernmentName -> principal by (name, FY) -----------------
# The GovernmentName here is uppercased + DEFINED-AREA-stripped EXACTLY as
# 04_assemble_debt.R does before aggregating, so the committed crosswalk keys on a
# string the consumer reproduces byte-for-byte.
doc <- read_xml(raw_input("tbrb", "Debt_Outstanding_By_Local_Government_20260722.xml"))
rows <- xml_find_all(doc, "//row")
g <- function(f) xml_text(xml_find_first(rows, paste0("./", f)))
d <- data.table(GovernmentType = g("governmenttype"), GovernmentName = toupper(g("governmentname")),
                FY = as.integer(g("fiscalyear")), Pledge = g("pledgetype"),
                Principal = as.numeric(g("totalprincipaloutstanding")))
d <- d[GovernmentType == "WD" & !is.na(GovernmentName)]
d[, GovernmentName := str_remove(GovernmentName, "(\\s|-)DEFINED AREA.*")]
tot <- d[, .(tbrb_principal = sum(Principal, na.rm = TRUE)), by = .(GovernmentName, FY)]
gov <- tot[, .(tbrb_yrs = .N, tbrb_prin_med = median(tbrb_principal),
               fy_lo = min(FY), fy_hi = max(FY)), by = GovernmentName]
gov[, key := canon_key(GovernmentName)]

# ---- propose a District_ID per GovernmentName --------------------------------
gov[, District_ID  := amap$District_ID[match(key, amap$key)]]
gov[, audit_name   := amap$audit_name[match(key, amap$key)]]
gov[, n_audit_ids  := amap$n_audit_ids[match(key, amap$key)]]
gov[, all_ids      := amap$all_ids[match(key, amap$key)]]
gov[, status := fifelse(!is.na(District_ID), "matched", "unmatched")]

# ---- confidence ---------------------------------------------------------------
# A canonical match means the GovernmentName and the audit district share an
# IDENTICAL canon key, so abbreviation/county/punctuation edits are cosmetic. The
# ONLY hazard is a key resolving to >1 audit district -- a reformed district (same
# district; PWS-linked rule picks the operational one) or, rarely, two genuinely
# different districts an over-aggressive strip merged. Both need a human glance.
gov[, raw_equal := !is.na(audit_name) & toupper(GovernmentName) == toupper(audit_name)]
gov[, confidence := fifelse(status != "matched", "UNMATCHED",
                     fifelse(n_audit_ids > 1, "REVIEW", "HIGH"))]

setorder(gov, -tbrb_prin_med)
seed <- gov[status == "matched", .(GovernmentName, District_ID, audit_name, raw_equal,
             reformed = n_audit_ids > 1, all_audit_ids = all_ids,
             tbrb_yrs, years = sprintf("%d-%d", fy_lo, fy_hi),
             tbrb_prin_med_M = round(tbrb_prin_med / 1e6, 2), confidence)]
unmatched <- gov[status != "matched", .(GovernmentName, key,
             tbrb_yrs, tbrb_prin_med_M = round(tbrb_prin_med / 1e6, 2))]
fwrite(seed, scratch("tbrb_crosswalk_seed.csv"))
fwrite(unmatched, scratch("tbrb_crosswalk_unmatched.csv"))

cat(sprintf("TBRB WD governments: %d\n", nrow(gov)))
cat("\n=== proposed matches by confidence ===\n")
print(gov[, .(govs = .N, debt_med_M = round(sum(tbrb_prin_med) / 1e6, 0)), by = confidence][order(-govs)])
cat("-> scratch/tbrb_crosswalk_seed.csv  &  scratch/tbrb_crosswalk_unmatched.csv\n")
cat("   promote after review:  cp scratch/tbrb_crosswalk_seed.csv input/tbrb_district_crosswalk.csv\n")

cat("\n=== REVIEW: reformed/collision cases (key -> >1 audit ID; PWS-linked chosen) ===\n")
print(gov[confidence == "REVIEW"][order(-tbrb_prin_med),
   .(GovernmentName, chosen_id = District_ID, all_ids, audit_name,
     debt_M = round(tbrb_prin_med / 1e6, 1))])
cat(sprintf("\n=== UNMATCHED (TBRB WD govt, no audit district): %d — top 15 by debt ===\n", nrow(unmatched)))
print(head(unmatched[order(-tbrb_prin_med_M)], 15))
