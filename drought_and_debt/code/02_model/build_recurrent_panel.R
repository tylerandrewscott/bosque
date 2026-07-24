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
# TIME-VARYING: ln_connections (yearly SDWIS service connections), the two
# tract ACS covariates ln_home_value / median_structure_age (two decennial-
# anchored vintages, 2006-2010 and 2016-2020, forward-filled by week-year in
# §3; structure age also advances with the week's calendar year), and
# perc_dem_vote (biennial TLC VTD general-election vintages 2012-2024,
# forward-filled the same way; 2010-2011 weeks back-fill from 2012). The rest
# are time-invariant per system.
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
# The TIME-VARYING subset of ctrl_vars (joined by week-year in §3). Consumers
# (e.g. the descriptive-stats table) use this to summarise these per system-week
# and the rest once per system.
tv_ctrl_vars <- c("ln_connections", "ln_home_value", "median_structure_age",
                  "perc_dem_vote")
# Time-varying NETWORK covariate (built in §2b below): 1 in weeks where any system
# the PWS buys water from is under a mandatory restriction. Not a time-invariant
# control, so it lives outside ctrl_vars (and the complete-case gate) but enters
# the model formula alongside the shared covariates.
network_vars <- c("seller_restricted")
# Prime-time Model 1 = drought + all system controls + the network term.
# There is no held-out appendix control set: the demographic composition controls
# (% rural / % Hispanic / % Black) and median household income were dropped from
# the specification entirely, so shared_vars is the whole story.
shared_vars <- c("DSCI_100", ctrl_vars, network_vars)
# Core fiscal capacity measures (§4.4): debt outstanding split by pledge type --
# GO/ad-valorem (tax-backed) and revenue-backed -- entered as two SEPARATE
# predictors, plus fund-balance and revenue, all per connection.
# (operating_ratio and debt_svc_tax were removed from the workflow.)
fiscal_vars <- c("debt_go_per_conn", "debt_rev_per_conn",
                 "fund_bal_per_conn", "revenue_per_conn")

# =============================================================================
# PANEL BUILD GUARD  (single source of truth for "is the panel current?")
# -----------------------------------------------------------------------------
# Everything below rebuilds the expensive counting-process panel. Skip it when a
# CURRENT panel is already in the environment: present AND carrying the columns
# that get added/changed over time -- the frailty key (District_ID) and the
# model covariates (shared_vars). Deriving the covariate part from shared_vars (which
# is refreshed on every source()) means adding a covariate auto-invalidates a stale
# cached panel, instead of it surviving reuse and failing later with `object not
# found`. This one check lives with the builder, so consumers just source() this
# file unconditionally instead of re-deriving -- and drifting on -- the staleness rule.
# =============================================================================
if (!exists("panel_m1") || !exists("panel_m2") ||
    !all(c("District_ID", shared_vars) %in% names(panel_m1)) ||
    !all(fiscal_vars %in% names(panel_m2))) {

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

# --- Sample restriction: COMMUNITY water systems only -------------------------
# The modeling universe is CWS: Model 1 = all mapped community systems, Model 2 =
# the district-linked subset. SDWIS types each system per year
# (pws_sdwis_connections.RDS); a system counts as CWS if any yearly snapshot
# types it CWS (type changes are rare and this keeps systems that deactivated
# mid-window). Non-CWS systems (transient/non-transient non-community) are out.
# (sdwis_conn is loaded once here and reused for the time-varying ln_connections
# control in §3 and the fiscal per-connection denominator in §4.)
sdwis_conn <- as.data.table(readRDS(committed("pws_sdwis_connections.RDS")))
.cws_ids <- unique(sdwis_conn[pws_type_code == "CWS", PWS_ID])
.n_mapped <- uniqueN(drought$PWS_ID)
drought <- drought[PWS_ID %in% .cws_ids]
message(sprintf("CWS filter: %d of %d mapped systems are SDWIS-typed CWS; non-CWS systems excluded from the risk set.",
                uniqueN(drought$PWS_ID), .n_mapped))

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

# --- Reconciliation: notices from systems OUTSIDE the risk set ----------------
# Some mandatory notices come from systems the risk set excludes by construction:
# non-CWS systems, or CWS with no polygon in the TCEQ retail service-area
# shapefile (mostly wholesale-only or no-longer-public systems — see
# PLANNING_REVIEW_2026-07-23.md, M2). Those events are DROPPED with the system;
# this accounting keeps the drop visible instead of silent.
.ev_excl   <- setdiff(unique(events$PWS_ID), unique(panel$PWS_ID))
.n_ev_excl <- events[PWS_ID %in% .ev_excl, .N]
message(sprintf("Excluded events: %d of %d notices, from %d systems outside the risk set (%d non-CWS; %d CWS without a mapped service area).",
                .n_ev_excl, nrow(events), length(.ev_excl),
                sum(!.ev_excl %in% .cws_ids), sum(.ev_excl %in% .cws_ids)))
if (.n_ev_excl > 0.10 * nrow(events)) warning(sprintf(
  "%.1f%% of mandatory notices fall outside the CWS+mapped risk set — check the service-area shapefile / SDWIS typing before trusting the event counts.",
  100 * .n_ev_excl / nrow(events)))

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
# 3. SYSTEM CONTROLS (time-invariant, except tv_ctrl_vars which vary by year)
# =============================================================================
# Population served + service connections: one row per system, from the DWV
# master totals written by 05_scrape_storage_and_pops.R (Connections =
# SVC_CONNECT_CNT, Population_Served = D_POPULATION_COUNT). This DWV snapshot
# count feeds the storage-per-connection ratio (both sides of that ratio are the
# same DWV snapshot); the ln_connections CONTROL is time-varying and comes from
# the yearly SDWIS series below.
pop <- as.data.table(readRDS(committed("pws_population.RDS")))
pop <- pop[, .(Connections = num(Connections),
               PopServed   = num(Population_Served)), by = PWS_ID]

# Total storage capacity (TSTC) and the interconnection count come from
# the DWV measures/purchases widgets, written to storage_connections_data.txt by
# 06_htmlscrape_storage_interconnects.R. NOTE: DWV's flow-rate table
# (pws_storage.RDS) is demand/usage, NOT storage capacity, so it is not used
# here — total storage now lives only in the interconnects file.
# UNITS: DWV serves TSTC mostly in MG, but some systems report GAL, so the Unit
# column is normalized to MG here rather than assumed. An unrecognized unit
# becomes NA (drops with complete cases) instead of a silently wrong magnitude.
sc <- fread(committed("storage_connections_data.txt"), colClasses = list(character = "PWS_ID"))
sc[, Value := num(Value)]
sc[Var == "TSTC" & !is.na(Unit) & Unit == "GAL", `:=`(Value = Value / 1e6, Unit = "MG")]
.unk <- sc[Var == "TSTC" & !is.na(Value) & !is.na(Unit) & !Unit %in% c("MG", ""), unique(Unit)]
if (length(.unk)) {
  message("Storage: unrecognized TSTC unit(s) ", paste(.unk, collapse = ", "),
          " set to NA — add a conversion here if they are real.")
  sc[Var == "TSTC" & Unit %in% .unk, Value := NA_real_]
}
stor <- sc[Var == "TSTC", .(Storage_MG = Value[1]), by = PWS_ID]

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
# (has_interconnect dropped: the DWV source flags now capture interconnects more
# specifically -- purchases_water is a wholesale interconnect and emergency_source
# is an emergency-designated interconnect/supply -- so a bare "any interconnect"
# indicator is redundant with them.)
controls[, storage_per_conn_g := asinh((Storage_MG * 1e6) / pmax(Connections, 1))]
# ln_home_value / median_structure_age / perc_dem_vote are NOT built here: they
# come from the vintage series and join by week-year in §3 below (tv_ctrl_vars).

panel <- merge(panel,
               controls[, c("PWS_ID", "Connections",
                            setdiff(ctrl_vars, tv_ctrl_vars)), with = FALSE],
               by = "PWS_ID", all.x = TRUE)

# --- TIME-VARYING ln_connections: yearly SDWIS service connections ------------
# The yearly Q1 SDWIS "Water System Summary" exports (2013-2026, sdwis_conn from
# §2 -- the same series the fiscal denominator uses in §4) give each system a
# service-connection count BY YEAR. Each weekly interval takes its calendar
# year's count via a rolling "nearest" join within PWS_ID, so weeks before the
# SDWIS span BACK-FILL from the earliest snapshot (2010-2012 -> 2013) and gap
# years take the nearest observed one. Systems with no SDWIS count in any year
# get NA and drop with the complete-case gate below.
conn_yr <- sdwis_conn[!is.na(Connections_SDWIS),
                      .(PWS_ID, Year, ln_connections = log1p(Connections_SDWIS))]
setkey(conn_yr, PWS_ID, Year)
panel[, conn_year := year(analysis_start + tstart * 7)]
panel <- conn_yr[panel, on = .(PWS_ID, Year = conn_year), roll = "nearest"]
setnames(panel, "Year", "conn_year")   # the join key carries the WEEK's year

# --- TIME-VARYING tract demographics: two ACS 5-year vintages -----------------
# pws_demos_MR.RDS carries the two model tract variables at both decennial-
# anchored vintages (2006-2010 ACS on 2010 tract lines; 2016-2020 ACS on 2020
# lines -- see 01_combine/05). Forward fill by the week's calendar year with a
# LOCF rolling join: weeks in 2010-2019 take the 2010 vintage, 2020 on the 2020
# vintage. Structure age = week-year minus the prevailing vintage's median year
# built (floored at 0), so it advances with the panel instead of freezing at
# the window start.
demos_tv <- rbind(
  demos[, .(PWS_ID, vint_year = 2010L,
            Median_Home_Value           = Median_Home_Value_2010,
            Median_Year_Structure_Built = Median_Year_Structure_Built_2010)],
  # A missing 2020 median (all overlapping tracts NA) keeps the 2010 value --
  # last OBSERVED vintage carried forward, not a mid-panel sample exit.
  demos[, .(PWS_ID, vint_year = 2020L,
            Median_Home_Value           = fcoalesce(Median_Home_Value_2020,
                                                    Median_Home_Value_2010),
            Median_Year_Structure_Built = fcoalesce(Median_Year_Structure_Built_2020,
                                                    Median_Year_Structure_Built_2010))])
# A median year built still NA after the fallback (no computable tract median
# over the system's area in either vintage) takes the GLOBAL median of its
# vintage, instead of dropping the system from the complete-case sample.
demos_tv[, Median_Year_Structure_Built := fcoalesce(
  Median_Year_Structure_Built,
  median(Median_Year_Structure_Built, na.rm = TRUE)), by = vint_year]
setkey(demos_tv, PWS_ID, vint_year)
panel <- demos_tv[panel, on = .(PWS_ID, vint_year = conn_year), roll = Inf]
setnames(panel, "vint_year", "conn_year")   # the join key carries the WEEK's year
panel[, `:=`(ln_home_value        = log(pmax(Median_Home_Value, 1)),
             median_structure_age = pmax(conn_year - Median_Year_Structure_Built, 0))]
panel[, c("Median_Home_Value", "Median_Year_Structure_Built") := NULL]

# --- TIME-VARYING dem vote share: biennial general-election vintages ----------
# pws_demos_MR.RDS carries Perc_Dem_2012..Perc_Dem_2024 (TLC VTD returns, all
# cycles on the 2024 VTD plan -- see 01_combine/05). Same LOCF forward fill by
# the week's calendar year; rollends=TRUE additionally BACK-fills 2010-2011
# weeks from the first (2012) vintage, matching the SDWIS connection convention.
vote_tv <- melt(demos[, c("PWS_ID", grep("^Perc_Dem_\\d{4}$", names(demos),
                                         value = TRUE)), with = FALSE],
                id.vars = "PWS_ID", variable.name = "vint_year",
                value.name = "perc_dem_vote")
vote_tv[, vint_year := as.integer(sub("^Perc_Dem_", "", as.character(vint_year)))]
# A vintage a system misses (all-NA VTD overlap that cycle) carries the nearest
# observed cycle instead of forcing a mid-panel complete-case exit.
vote_tv[is.nan(perc_dem_vote), perc_dem_vote := NA]
setorder(vote_tv, PWS_ID, vint_year)
vote_tv[, perc_dem_vote := nafill(nafill(perc_dem_vote, "locf"), "nocb"), by = PWS_ID]
setkey(vote_tv, PWS_ID, vint_year)
panel <- vote_tv[panel, on = .(PWS_ID, vint_year = conn_year),
                 roll = Inf, rollends = c(TRUE, TRUE)]
setnames(panel, "vint_year", "conn_year")   # the join key carries the WEEK's year

panel_m1 <- panel[complete.cases(panel[, ctrl_vars, with = FALSE])]

# Attach District_ID to every system (left join -> NA for systems with no water
# district: city-owned, investor-owned, or otherwise private utilities). Model 1's
# shared frailty then clusters by district where one exists and by the individual
# system otherwise (see the frailty setup in 01_fit_recurrent_cox_inla.R).
xw <- as.data.table(readRDS(committed("id_crosswalk.RDS")))
xw <- unique(xw[!is.na(PWS_ID) & !is.na(District_ID), .(PWS_ID, District_ID)], by = "PWS_ID")
panel_m1 <- merge(panel_m1, xw, by = "PWS_ID", all.x = TRUE)

# (The county-level iid frailty was removed from the specification -- drought
# conditions (DSCI) already carry the spatial variation, so no CFIPS attach here.)

message(sprintf("Model 1 panel (complete controls): %s rows, %d systems (%d district-linked, %d unaffiliated).",
                format(nrow(panel_m1), big.mark = ","), uniqueN(panel_m1$PWS_ID),
                uniqueN(panel_m1[!is.na(District_ID), PWS_ID]),
                uniqueN(panel_m1[is.na(District_ID), PWS_ID])))

# =============================================================================
# 4. TIME-VARYING FINANCES + district-linked subsample (panel_m2)
# =============================================================================

# Per-connection DENOMINATOR: district service connections from EPA SDWIS, BY YEAR
# (yearly Q1 "Water System Summary" exports -> input/pws_sdwis_connections.RDS via
# 00_assemble/08, keyed by the same TXnnnnnnn PWS_ID as the panel). This REPLACES
# the audit-reported count `WATER CUSTOMERS - EQ SINGLE FAMILY UNITS`, which is
# 0/blank for ~half of filings; the old `pmax(water_conn, 1)` silently floored those
# to a per-$1 ratio, collapsing "per connection" onto asinh(raw dollars) and mass
# points (e.g. debt median 0). A district's connections in a given year = the sum
# over its member PWS (id_crosswalk `xw`, loaded in §3) of that year's SDWIS count.
# The by-year table (2013-2026, sdwis_conn loaded in §2) is matched to each
# audit's fiscal year below; years outside the SDWIS span back-fill to the
# nearest available year (2010-2012 -> 2013).
dist_conn_yr <- merge(xw, sdwis_conn[, .(PWS_ID, Year, Connections_SDWIS)],
                      by = "PWS_ID", all.x = TRUE, allow.cartesian = TRUE)[
                      , .(dist_conn = sum(Connections_SDWIS, na.rm = TRUE)),
                      by = .(District_ID, Year)]
dist_conn_yr <- dist_conn_yr[dist_conn > 0]   # 0 => treated as MISSING (ratio NA), never floored
setkey(dist_conn_yr, District_ID, Year)

# Each audit is stamped with its FISCAL YEAR ENDED date; we match on that date
# (not just the year) so the join uses the actual reporting period. The three core
# fiscal indicators (debt / fund-balance / revenue per connection, all asinh) are
# computed per audit. (operating_ratio and debt_svc_tax were removed from the
# workflow.)
fin <- as.data.table(readRDS(committed("combined_and_lagged_finances.RDS")))
fin[, District_ID := as.character(District_ID)]
fin[, fy_end := as.Date(`FISCAL YEAR ENDED`)]
fin <- fin[!is.na(fy_end) & !is.na(District_ID)]
# Debt outstanding split by pledge type, straight from the TBRB principal-
# outstanding series: GO = general-obligation / ad-valorem (tax-backed), REV =
# revenue-backed. These enter the model as two SEPARATE covariates.
#
# Coding rules for the two debt terms:
#  (1) District-year IN TBRB: use the GO/REV principal directly. A missing pledge
#      side is a genuine $0 of that debt class (a district carrying that debt
#      would have a row for it), so the absent side is set to 0, not NA.
#  (2) District-year ABSENT from TBRB but with a filed audit reporting
#      `BONDS OUTSTANDING` == 0: the audit confirms the district is active AND
#      debt-free, so the TBRB absence is a true zero, not missingness -- set both
#      debt classes to 0 and keep the observation. (The TBRB debt-outstanding
#      series only enumerates governments that CARRY debt, so debt-free districts
#      never appear; without this rule they would drop wholesale.)
#  (3) District-year ABSENT from TBRB but audit `BONDS OUTSTANDING` > 0 (or NA):
#      a name-match / coverage conflict -- real debt exists but the audit gives no
#      pledge split, so it cannot feed the two-predictor model. Left NA on BOTH
#      terms -> drops from the debt models rather than being mis-coded $0.
fin[, `:=`(debt_go  = num(TotalPrincipalOutstanding_GO),
           debt_rev = num(TotalPrincipalOutstanding_REV))]
fin[, in_tbrb := !is.na(debt_go) | !is.na(debt_rev)]
fin[in_tbrb & is.na(debt_go),  debt_go  := 0]
fin[in_tbrb & is.na(debt_rev), debt_rev := 0]
# Audit-confirmed zeros for district-years absent from TBRB (rule 2 above).
fin[, bonds_audit := num(`BONDS OUTSTANDING`)]
fin[!in_tbrb & !is.na(bonds_audit) & bonds_audit == 0,
    `:=`(debt_go = 0, debt_rev = 0)]
fin[, `:=`(
  fund_balance  = num(Fund_Balance),
  total_revenue = num(Total_Revenue)
)]
# Audits are occasionally re-filed for the same fiscal-year-end; keep one row per
# (District_ID, fy_end), preferring the most complete (fewest NAs) so the roll
# below picks a real observation and results are order-independent.
fin[, .n_na := rowSums(is.na(.SD)),
    .SDcols = c("debt_go", "debt_rev", "fund_balance", "total_revenue")]
setorder(fin, District_ID, fy_end, .n_na)
fin <- unique(fin, by = c("District_ID", "fy_end"))
# Attach each audit's YEAR-matched district connection denominator: a rolling
# "nearest" join on year within District_ID, so each audit uses its own year's
# SDWIS snapshot and audits outside the SDWIS span (2013-2026) back-fill to the
# nearest available year (2010-2012 -> 2013). Districts with no SDWIS year at all
# (or 0 connections) get dist_conn = NA, so their ratios are NA and drop per-covariate.
fin[, fy_year := year(fy_end)]
fin <- dist_conn_yr[fin, on = .(District_ID, Year = fy_year), roll = "nearest"]
fin <- fin[, .(
  District_ID, fy_end,
  debt_go_per_conn  = asinh(debt_go       / dist_conn),
  debt_rev_per_conn = asinh(debt_rev      / dist_conn),
  fund_bal_per_conn = asinh(fund_balance  / dist_conn),
  revenue_per_conn  = asinh(total_revenue / dist_conn),
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
# Per-covariate coverage (each fiscal model uses this covariate's non-NA rows).
# Also report the NON-ZERO share: asinh(0)=0 marks a genuine $0 of that measure
# (e.g. a district with no revenue-backed debt), so a covariate that is mostly
# zeros carries little spread and its coefficient leans on relatively few nonzero
# districts -- watch this for debt_rev_per_conn in particular (GO >> REV in TBRB).
for (v in fiscal_vars) {
  x  <- panel_m2[[v]]
  ok <- !is.na(x)
  nz <- ok & x != 0
  message(sprintf("  %-18s %s rows, %d events | nonzero: %s rows (%.0f%%), %d districts",
                  v, format(sum(ok), big.mark = ","), sum(panel_m2$event[ok]),
                  format(sum(nz), big.mark = ","),
                  100 * sum(nz) / max(sum(ok), 1L),
                  uniqueN(panel_m2$District_ID[nz])))
}

} else {
  message("build_recurrent_panel.R: a current panel_m1/panel_m2 is already in ",
          "this environment; skipping the rebuild (covariate-name vectors refreshed).")
}   # end panel-build guard
