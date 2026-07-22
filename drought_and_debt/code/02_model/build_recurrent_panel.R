# =============================================================================
# 02_model/build_recurrent_panel.R
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
# Taken from config.R (start_date / end_date) so the window is defined in ONE
# place. Weekly; the effective end is capped at the last committed data week,
# so widening the config window automatically widens the panel as scrapes land.
analysis_start <- start_date
analysis_end   <- end_date

num <- function(x) as.numeric(gsub("[^0-9eE.+-]", "", as.character(x)))

# =============================================================================
# COVARIATE-NAME VECTORS  (always defined -- above the panel-build guard)
# -----------------------------------------------------------------------------
# These are pure name lists with no dependency on the panel data, so they are
# (re)defined on EVERY source(), even when the expensive build below is skipped.
# That is what keeps them from going stale in a reused environment: a consumer
# can never see an old panel without the matching, up-to-date name vectors.
# =============================================================================
ctrl_vars   <- c("ln_connections", "storage_per_conn_g",
                 "ln_home_value", "median_structure_age",
                 "perc_dem_vote",
                 # DWV source / network flags (time-invariant; §3), from
                 # 07_scrape_source_and_purchases.R: (1) source_surface (surface-
                 # vs-ground) and (2) purchases_water (wholesale is the PRIMARY
                 # source), both from D_FED_PRIM_SRC_CD; (3) emergency_source (an
                 # emergency-designated supply facility / interconnect, from
                 # DashSourceWater); (4) wholesaler (the system SELLS water to
                 # others, i.e. appears as a Seller in the buys-from edges). In
                 # ctrl_vars so the complete-case sample gates on them too.
                 "source_surface", "purchases_water", "emergency_source",
                 "wholesaler")
# Time-varying NETWORK covariate (built in §2b below): 1 in weeks where any system
# the PWS buys water from is under a mandatory restriction. Not a time-invariant
# control, so it lives outside ctrl_vars (and the complete-case gate) but enters
# the model formula alongside the shared covariates.
network_vars <- c("seller_restricted")
# Prime-time Model 1 = drought + all time-invariant controls + the network term.
# There is no held-out appendix control set: the demographic composition controls
# (% rural / % Hispanic / % Black) and median household income were dropped from
# the specification entirely, so shared_vars is the whole story.
shared_vars <- c("DSCI_100", ctrl_vars, network_vars)
# Three core fiscal capacity measures (§4.4): debt / revenue / fund-balance per
# connection. (operating_ratio and debt_svc_tax were removed from the workflow.)
fiscal_vars <- c("debt_per_conn", "fund_bal_per_conn", "revenue_per_conn")

# =============================================================================
# PANEL BUILD GUARD  (single source of truth for "is the panel current?")
# -----------------------------------------------------------------------------
# Everything below rebuilds the expensive counting-process panel. Skip it when a
# CURRENT panel is already in the environment: present AND carrying the columns
# that get added/changed over time -- the frailty keys (District_ID, CFIPS) and the
# model covariates (shared_vars). Deriving the covariate part from shared_vars (which
# is refreshed on every source()) means adding a covariate auto-invalidates a stale
# cached panel, instead of it surviving reuse and failing later with `object not
# found`. This one check lives with the builder, so consumers just source() this
# file unconditionally instead of re-deriving -- and drifting on -- the staleness rule.
# =============================================================================
if (!exists("panel_m1") || !exists("panel_m2") ||
    !all(c("District_ID", "CFIPS", shared_vars) %in% names(panel_m1))) {

# =============================================================================
# 1. EVENTS -- distinct mandatory-restriction notices
# -----------------------------------------------------------------------------
# STAGE %in% {M1, M2, M3} are the mandatory stages. An event is a mandatory
# notice on a date; (PWS_ID, date) is collapsed so a notice seen several times
# in the scrape counts once. A same-stage repeat on a DIFFERENT date is a
# genuine new event (recurrent) and is kept. The definition lives in
# ingest_helpers.R (mandatory_restriction_events) and is shared with the
# Figure 2 descriptive plot so the two cannot drift.
# =============================================================================
events <- mandatory_restriction_events(analysis_start, analysis_end)
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
# 2b. TIME-VARYING NETWORK COVARIATE -- is an upstream SELLER under restriction?
# -----------------------------------------------------------------------------
# The DWV "buys-from" edges (pws_purchase_edges.RDS, from 07_scrape_source_and_
# purchases.R) are a TIME-INVARIANT snapshot of wholesale relationships. The
# covariate still varies in week-time because a seller's restriction STATUS comes
# from the same mandatory notices that define the event flag (§1): at each
# buyer-week we ask whether any system the buyer buys water from is currently
# under a mandatory restriction.
#
# A seller notice adopted at week E is treated as "in effect" for NEIGHBOR_PERSIST
# weeks -- a restriction persists, the weekly USDM grid is too fine for a literal
# single-week match to carry signal, and recurrent re-declarations refresh the
# window. A buyer interval (tstart, tstop] gets seller_restricted = 1 iff some
# seller-exposure window [E, E + NEIGHBOR_PERSIST) overlaps it (tstart < win_end
# AND tstop > win_start). Non-purchasers (no out-edges) stay 0. NEIGHBOR_PERSIST
# is the one modeling knob (how long an upstream restriction is treated as
# active); 4 weeks ~ a month. Override before source()ing to vary it.
# =============================================================================
if (!exists("NEIGHBOR_PERSIST")) NEIGHBOR_PERSIST <- 4   # weeks a seller restriction stays "on"

panel[, seller_restricted := 0L]
edges_net <- as.data.table(readRDS(committed("pws_purchase_edges.RDS")))
edges_net <- unique(edges_net[!is.na(Buyer) & !is.na(Seller) & nzchar(Seller),
                              .(Buyer, Seller)])
# Map each seller's notice times onto the systems that buy from it.
exposure <- merge(edges_net, events[, .(Seller = PWS_ID, event_time)],
                  by = "Seller", allow.cartesian = TRUE)
n_buyers_linked <- uniqueN(exposure$Buyer)
if (nrow(exposure)) {
  win <- exposure[, .(PWS_ID = Buyer, win_start = event_time,
                      win_end = event_time + NEIGHBOR_PERSIST)]
  panel[win, on = .(PWS_ID, tstart < win_end, tstop > win_start),
        seller_restricted := 1L]
}
message(sprintf("Network covariate: %d buyers linked to a restricting seller; %s buyer-weeks seller_restricted=1 (persist=%dw).",
                n_buyers_linked, format(sum(panel$seller_restricted), big.mark = ","),
                NEIGHBOR_PERSIST))

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
stor <- sc[Var == "TSTC", .(Storage_MG = num(Value)[1]), by = PWS_ID]

demos <- as.data.table(readRDS(committed("pws_demos_MR.RDS")))

# DWV source flags (one row per system; from 07_scrape_source_and_purchases.R):
# source_surface, purchases_water, emergency_source are already 0/1, so they pass
# straight through the transform below into ctrl_vars.
source_flags <- as.data.table(readRDS(committed("pws_source.RDS")))[
  , .(PWS_ID, source_surface, purchases_water, emergency_source)]

controls <- Reduce(function(a, b) merge(a, b, by = "PWS_ID", all.x = TRUE),
                   list(pop, stor, demos, source_flags))
# wholesaler: 1 if the system SELLS water to any other system -- i.e. it appears
# as a Seller in the buys-from edges loaded in §2b (edges_net). Non-sellers are 0,
# never NA, so this gates the complete-case sample without dropping anyone.
controls[, wholesaler := as.integer(PWS_ID %in% unique(edges_net$Seller))]
controls[, `:=`(
  ln_connections     = log1p(Connections),
  storage_per_conn_g = asinh((Storage_MG * 1e6) / pmax(Connections, 1)),
  # (has_interconnect dropped: the DWV source flags now capture interconnects more
  # specifically -- purchases_water is a wholesale interconnect and emergency_source
  # is an emergency-designated interconnect/supply -- so a bare "any interconnect"
  # indicator is redundant with them.)
  ln_home_value      = log(pmax(Median_Home_Value, 1)),
  # Time-invariant per-PWS control (median structure built is a single value per
  # system); age is taken at the analysis start and floored at 0 for the rare
  # tract median built after the window opens.
  median_structure_age = pmax(year(analysis_start) - Median_Year_Structure_Built, 0),
  perc_dem_vote      = Perc_Dem_Vote_Share
)]

panel <- merge(panel, controls[, c("PWS_ID", "Connections", ctrl_vars), with = FALSE],
               by = "PWS_ID", all.x = TRUE)
panel_m1 <- panel[complete.cases(panel[, ctrl_vars, with = FALSE])]

# Attach District_ID to every system (left join -> NA for systems with no water
# district: city-owned, investor-owned, or otherwise private utilities). Model 1's
# shared frailty then clusters by district where one exists and by the individual
# system otherwise (see the frailty setup in 01_fit_recurrent_cox_inla.R).
xw <- as.data.table(readRDS(committed("id_crosswalk.RDS")))
xw <- unique(xw[!is.na(PWS_ID) & !is.na(District_ID), .(PWS_ID, District_ID)], by = "PWS_ID")
panel_m1 <- merge(panel_m1, xw, by = "PWS_ID", all.x = TRUE)

# Attach each system's PRIMARY county (largest service-area overlap) so the fit
# scripts can build a county-level frailty (an iid random effect added ON TOP of
# the district/system frailty; there is no spatial ICAR term -- drought absorbs
# the core spatial variance). CFIPS may be NA for the rare system with no county
# overlap on record; those get their own catch-all county level in the fit.
co <- as.data.table(readRDS(committed("pws_county_overlaps.RDS")))
co <- co[order(-Prop_Over_County)][!duplicated(PWS_ID), .(PWS_ID, CFIPS)]
panel_m1 <- merge(panel_m1, co, by = "PWS_ID", all.x = TRUE)

message(sprintf("Model 1 panel (complete controls): %s rows, %d systems (%d district-linked, %d unaffiliated).",
                format(nrow(panel_m1), big.mark = ","), uniqueN(panel_m1$PWS_ID),
                uniqueN(panel_m1[!is.na(District_ID), PWS_ID]),
                uniqueN(panel_m1[is.na(District_ID), PWS_ID])))

# =============================================================================
# 4. TIME-VARYING FINANCES + district-linked subsample (panel_m2)
# =============================================================================

# Per-connection DENOMINATOR: district service connections from EPA SDWIS
# (Envirofacts WATER_SYSTEM snapshot -> input/pws_sdwis_connections.RDS, keyed by
# the same TXnnnnnnn PWS_ID as the panel). This REPLACES the audit-reported count
# `WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`, which is 0/blank for ~half of filings;
# the old `pmax(water_conn, 1)` silently floored those to a per-$1 ratio, collapsing
# "per connection" onto asinh(raw dollars) and mass points (e.g. debt median 0).
# A district's connections = the sum over its member PWS (id_crosswalk `xw`, loaded
# in §3). SDWIS covers ~99.8% of the fiscal panel's district-weeks. NOTE: this is a
# single current snapshot applied to all years (time-invariant); a yearly SDWIS/TWDB
# series can drop in later by adding a Year dimension to dist_conn and the join.
sdwis_conn <- as.data.table(readRDS(committed("pws_sdwis_connections.RDS")))
dist_conn  <- merge(xw, sdwis_conn[, .(PWS_ID, Connections_SDWIS)],
                    by = "PWS_ID", all.x = TRUE)[
                    , .(dist_conn = sum(Connections_SDWIS, na.rm = TRUE)), by = District_ID]
dist_conn  <- dist_conn[dist_conn > 0]   # 0 => treated as MISSING (ratio NA), never floored

# Each audit is stamped with its FISCAL YEAR ENDED date; we match on that date
# (not just the year) so the join uses the actual reporting period. The three core
# fiscal indicators (debt / fund-balance / revenue per connection, all asinh) are
# computed per audit. (operating_ratio and debt_svc_tax were removed from the
# workflow.)
fin <- as.data.table(readRDS(committed("combined_and_lagged_finances.RDS")))
fin[, District_ID := as.character(District_ID)]
fin[, fy_end := as.Date(`FISCAL YEAR ENDED`)]
fin <- fin[!is.na(fy_end) & !is.na(District_ID)]
fin[, debt_outstanding := rowSums(cbind(num(TotalPrincipalOutstanding_GO),
                                        num(TotalPrincipalOutstanding_REV)), na.rm = TRUE)]
fin[debt_outstanding == 0, debt_outstanding := num(`BONDS OUTSTANDING`)]
fin[, `:=`(
  fund_balance  = num(Fund_Balance),
  total_revenue = num(Total_Revenue)
)]
# Audits are occasionally re-filed for the same fiscal-year-end; keep one row per
# (District_ID, fy_end), preferring the most complete (fewest NAs) so the roll
# below picks a real observation and results are order-independent.
fin[, .n_na := rowSums(is.na(.SD)),
    .SDcols = c("debt_outstanding", "fund_balance", "total_revenue")]
setorder(fin, District_ID, fy_end, .n_na)
fin <- unique(fin, by = c("District_ID", "fy_end"))
# Attach the district connection denominator; districts with no SDWIS match (or 0
# connections) get dist_conn = NA, so their ratios are NA and drop per-covariate.
fin <- merge(fin, dist_conn, by = "District_ID", all.x = TRUE)
fin <- fin[, .(
  District_ID, fy_end,
  debt_per_conn     = asinh(debt_outstanding / dist_conn),
  fund_bal_per_conn = asinh(fund_balance     / dist_conn),
  revenue_per_conn  = asinh(total_revenue    / dist_conn),
  has_audit         = 1L
)]

# Nearest-audit join: each system-week carries its district's most recent audit
# whose FISCAL YEAR ENDED falls on/before the week, within a 730-day (two-year)
# window -- matching only PRIOR audits so we never capture post-restriction
# financials. Implemented as a data.table rolling join: match fin$fy_end to the
# week's date, rolling the last prior audit forward up to 730 days (roll = 730).
# Weeks with no audit in the window get NA fiscal columns (has_audit NA) and drop.
# The fiscal indicators are likely conflated, so each is fit in its OWN model; a
# matched audit may still have an individual NA covariate, and each fit script
# drops only the rows missing THAT covariate.
panel_m2 <- panel_m1[!is.na(District_ID)]               # district-linked subsample
panel_m2[, week_date := analysis_start + tstart * 7]
panel_m2 <- fin[panel_m2, on = .(District_ID, fy_end = week_date), roll = 730]
# The roll join surfaces the WEEK date under fin's key name (fy_end); rename it
# back so the column means what it says (the audit's own fy_end is consumed as the
# join key and not retained -- we only need the matched fiscal values + has_audit).
setnames(panel_m2, "fy_end", "week_date")
panel_m2 <- panel_m2[has_audit == 1L]                   # weeks with a prior audit ≤730d

message(sprintf("Model 2 fiscal panel (any matched audit): %s rows, %d systems, %d districts, %d event-weeks.",
                format(nrow(panel_m2), big.mark = ","), uniqueN(panel_m2$PWS_ID),
                uniqueN(panel_m2$District_ID), sum(panel_m2$event)))
# Per-covariate coverage (each fiscal model uses this covariate's non-NA rows):
for (v in fiscal_vars) {
  ok <- !is.na(panel_m2[[v]])
  message(sprintf("  %-18s %s rows, %d events", v,
                  format(sum(ok), big.mark = ","), sum(panel_m2$event[ok])))
}

} else {
  message("build_recurrent_panel.R: a current panel_m1/panel_m2 is already in ",
          "this environment; skipping the rebuild (covariate-name vectors refreshed).")
}   # end panel-build guard
