# =============================================================================
# 03_model/build_recurrent_panel.R
# -----------------------------------------------------------------------------
# Shared builder for the recurring-events counting-process panel. Sourced by
# both the frequentist (`scratch_models/05_fit_recurrent_cox.R`) and Bayesian/INLA
# (`01_fit_recurrent_cox_inla.R`) model scripts so the two approaches fit the
# exact same data.
#
# Sourcing this file (from the drought_and_debt project root) creates in the
# caller's environment:
#   analysis_start, analysis_end   analysis window (Dates)
#   events                         distinct mandatory notices (PWS_ID, event_time)
#   panel_m1                       full-sample weekly counting-process panel
#                                  (Surv(tstart, tstop, event), 0/1 event by week)
#   panel_m2                       district-linked subsample with time-varying
#                                  finances joined (Surv + fiscal covariates)
#   ctrl_vars, shared_vars, fiscal_vars   covariate name vectors
#   num()                          character -> numeric cleaner
#
# Model-specific pieces (Model 1 offset for the frequentist fit; frailty indices
# and priors for INLA) are added by the fitting scripts, not here.
# =============================================================================

# Load config (paths, window, spatial helpers) if it isn't already. config.R
# setwd()s to the project root, so this works from the bosque repo root, the
# drought_and_debt root, or a code/ subdirectory.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# --- Analysis window ----------------------------------------------------------
# Weekly, 2010 through 2025; committed data currently run through end-2024, so
# the effective window is capped at the last available week. Widen as newer
# scrapes land.
analysis_start <- as.Date("2010-01-01")
analysis_end   <- as.Date("2024-12-31")

num <- function(x) as.numeric(gsub("[^0-9eE.+-]", "", as.character(x)))

# =============================================================================
# 1. EVENTS -- distinct mandatory-restriction notices
# -----------------------------------------------------------------------------
# STAGE %in% {M1, M2, M3} are the mandatory stages. An event is a mandatory
# notice on a date; (PWS_ID, date) is collapsed so a notice seen several times
# in the scrape counts once. A same-stage repeat on a DIFFERENT date is a
# genuine new event (recurrent) and is kept.
# =============================================================================
restr <- as.data.table(readRDS(committed("combined_restriction_records.RDS")))
setnames(restr, "PWS ID", "PWS_ID")

events <- restr[
  !is.na(PWS_ID) & !is.na(NOTIFIED_YMD) & STAGE %in% c("M1", "M2", "M3") &
    NOTIFIED_YMD >= analysis_start & NOTIFIED_YMD <= analysis_end,
  .(PWS_ID, event_date = as.Date(NOTIFIED_YMD))
]
events <- unique(events, by = c("PWS_ID", "event_date"))
events[, event_time := as.numeric(event_date - analysis_start) / 7]

message(sprintf("Events: %d distinct mandatory notices across %d systems (%d with repeats).",
                nrow(events), uniqueN(events$PWS_ID),
                events[, .N, by = PWS_ID][N > 1, .N]))

# =============================================================================
# 2. WEEKLY DROUGHT PANEL -- counting-process skeleton (0/1 event by week)
# -----------------------------------------------------------------------------
# pws_drought_weekly.RDS is a common weekly grid. Each weekly observation is an
# interval (tstart, tstop] over which that week's DSCI applies; the last runs to
# the window end. An interval gets event = 1 iff a mandatory notice falls in it
# (a week with any notice is 1; multiple notices in a week collapse to one 1).
# =============================================================================
drought <- as.data.table(readRDS(committed("pws_drought_weekly.RDS")))
drought[, DroughtDate := ymd(DroughtDate)]
drought <- drought[DroughtDate >= analysis_start & DroughtDate <= analysis_end &
                     !is.na(PWS_ID)]
setorder(drought, PWS_ID, DroughtDate)

drought[, tstart := as.numeric(DroughtDate - analysis_start) / 7]
drought[, tstop  := shift(tstart, type = "lead"), by = PWS_ID]
end_time <- as.numeric(analysis_end - analysis_start) / 7
drought[is.na(tstop), tstop := end_time]
drought <- drought[tstop > tstart]
drought[, DSCI := as.numeric(DSCI)]
drought[, DSCI_100 := DSCI / 100]

panel <- drought[, .(PWS_ID, tstart, tstop, DSCI, DSCI_100)]
panel[, event := 0L]
panel[events, on = .(PWS_ID, tstart < event_time, tstop >= event_time), event := 1L]

message(sprintf("Counting-process panel: %s rows, %d systems, %d event-weeks.",
                format(nrow(panel), big.mark = ","), uniqueN(panel$PWS_ID),
                sum(panel$event)))

# =============================================================================
# 3. TIME-INVARIANT SYSTEM CONTROLS
# =============================================================================
# Population served + service connections: one row per system, from the DWV
# master totals written by 05_scrape_storage_and_pops.R (Connections =
# SVC_CONNECT_CNT, Population_Served = D_POPULATION_COUNT).
pop <- as.data.table(readRDS(committed("pws_population.RDS")))
pop <- pop[, .(Connections = num(Connections),
               PopServed   = num(Population_Served)), by = PWS_ID]

# Total storage capacity (TSTC, in MG) and the interconnection count come from
# the DWV measures/purchases widgets, written to storage_connections_data.txt by
# 06_htmlscrape_storage_interconnects.R. NOTE: DWV's flow-rate table
# (pws_storage.RDS) is demand/usage, NOT storage capacity, so it is not used
# here — total storage now lives only in the interconnects file.
# TODO(units): Value is assumed to be MG (DWV serves TSTC in MG); revisit when
# flow-rate/measure units are normalized (see 05_scrape_storage_and_pops.R).
sc <- fread(committed("storage_connections_data.txt"), colClasses = list(character = "PWS_ID"))
stor <- sc[Var == "TSTC", .(Storage_MG    = num(Value)[1],
                            Interconnects = num(Num_Interconnections)[1]), by = PWS_ID]

demos <- as.data.table(readRDS(committed("pws_demos_MR.RDS")))

controls <- Reduce(function(a, b) merge(a, b, by = "PWS_ID", all.x = TRUE),
                   list(pop, stor, demos))
controls[, `:=`(
  ln_connections     = log1p(Connections),
  storage_per_conn_g = asinh((Storage_MG * 1e6) / pmax(Connections, 1)),
  has_interconnect   = as.integer(Interconnects > 0),
  ln_income          = log(pmax(Med_Household_Income, 1)),
  perc_rural         = Perc_Rural,
  perc_hispanic      = Perc_Hispanic,
  perc_black         = Perc_Black,
  perc_dem_vote      = Perc_Dem_Vote_Share
)]

ctrl_vars   <- c("ln_connections", "storage_per_conn_g", "has_interconnect",
                 "ln_income", "perc_rural", "perc_hispanic", "perc_black",
                 "perc_dem_vote")
shared_vars <- c("DSCI_100", ctrl_vars)

panel <- merge(panel, controls[, c("PWS_ID", "Connections", ctrl_vars), with = FALSE],
               by = "PWS_ID", all.x = TRUE)
panel_m1 <- panel[complete.cases(panel[, ctrl_vars, with = FALSE])]
message(sprintf("Model 1 panel (complete controls): %s rows, %d systems.",
                format(nrow(panel_m1), big.mark = ","), uniqueN(panel_m1$PWS_ID)))

# =============================================================================
# 4. TIME-VARYING FINANCES + district-linked subsample (panel_m2)
# =============================================================================
xw <- as.data.table(readRDS(committed("id_crosswalk.RDS")))
xw <- unique(xw[!is.na(PWS_ID) & !is.na(District_ID)], by = "PWS_ID")

fin <- as.data.table(readRDS(committed("combined_and_lagged_finances.RDS")))
fin[, YEAR := suppressWarnings(as.integer(YEAR))]
fin <- fin[!is.na(YEAR) & YEAR >= year(analysis_start) & YEAR <= year(analysis_end)]
fin[, debt_outstanding := rowSums(cbind(num(TotalPrincipalOutstanding_GO),
                                        num(TotalPrincipalOutstanding_REV)), na.rm = TRUE)]
fin[debt_outstanding == 0, debt_outstanding := num(`BONDS OUTSTANDING`)]
fin[, `:=`(
  fund_balance  = num(Fund_Balance),
  total_revenue = num(Total_Revenue),
  total_expense = num(Total_Expenditure),
  water_conn    = num(`WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`),
  debt_svc_tax  = as.integer(num(`DEBT SERVICE TAX RATE`) > 0)
)]
# No real district reports zero expenditure -- it's an audit reporting error. Treat
# it as missing so operating_ratio is NA (and the district-year is dropped below)
# rather than flooring the denominator and inventing a huge ratio.
fin[total_expense <= 0, total_expense := NA_real_]
fin <- unique(fin[!is.na(District_ID), .(
  District_ID, YEAR,
  debt_per_conn     = asinh(debt_outstanding / pmax(water_conn, 1)),
  fund_bal_per_conn = asinh(fund_balance     / pmax(water_conn, 1)),
  revenue_per_conn  = asinh(total_revenue    / pmax(water_conn, 1)),
  operating_ratio   = asinh(total_revenue    / total_expense),
  debt_svc_tax
)], by = c("District_ID", "YEAR"))

fiscal_vars <- c("debt_per_conn", "fund_bal_per_conn", "revenue_per_conn",
                 "operating_ratio", "debt_svc_tax")

# The fiscal indicators are likely conflated, so each is fit in its OWN model
# rather than jointly. We therefore keep every district-year with a matched
# audit (each fiscal column may be NA independently) and let each fit script
# drop only the rows missing THAT covariate -- so e.g. a zero-expenditure audit
# is missing only from the operating_ratio model, not the debt/revenue models.
fin[, has_audit := 1L]
panel_m2 <- merge(panel_m1, xw, by = "PWS_ID")          # inner join -> district-linked
panel_m2[, cal_year := year(analysis_start + tstart * 7)]
panel_m2 <- merge(panel_m2, fin, by.x = c("District_ID", "cal_year"),
                  by.y = c("District_ID", "YEAR"), all.x = TRUE)
panel_m2 <- panel_m2[has_audit == 1L]                   # district-years with a matched audit

message(sprintf("Model 2 fiscal panel (any matched audit): %s rows, %d systems, %d districts, %d event-weeks.",
                format(nrow(panel_m2), big.mark = ","), uniqueN(panel_m2$PWS_ID),
                uniqueN(panel_m2$District_ID), sum(panel_m2$event)))
# Per-covariate coverage (each fiscal model uses this covariate's non-NA rows):
for (v in fiscal_vars) {
  ok <- !is.na(panel_m2[[v]])
  message(sprintf("  %-18s %s rows, %d events", v,
                  format(sum(ok), big.mark = ","), sum(panel_m2$event[ok])))
}
