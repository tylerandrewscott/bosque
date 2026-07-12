# =============================================================================
# 05_scrape_storage_and_pops.R
# -----------------------------------------------------------------------------
# Population (by served type) and demand/flow-rate ("storage") data per public
# water system.
#
# SOURCE CHANGE (2026): the old TCEQ "Drinking Water Watch" JSP app
# (dww2.tceq.texas.gov/DWW/JSP) was decommissioned and replaced by the
# "Drinking Water Viewer" (dwv.tceq.texas.gov), a JSON/OData service. This
# script no longer scrapes HTML tables (rvest + html_table); it calls the DWV
# API via util_code/scraping/dwv_api_helpers.R.
#
# Old datasheet table            -> new DWV endpoint
#   "PopulationType"             -> DashAnnualOperatingPeriod.PopulationServed
#                                   (+ master D_POPULATION_COUNT / SVC_CONNECT_CNT)
#   "Max Daily Demand" / storage -> DashWaterSystemFlowRates
#
# Outputs (paths unchanged; SCHEMA changed for the DWV source — downstream code
# in 02_model/build_recurrent_panel.R and explore/simple_cox.R was rewired to
# match). Both are now ONE ROW PER SYSTEM:
#   input/pws_population.RDS  — PWS_ID, TINWSYS_IS_NUMBER, Population_Served
#                               (master total), Connections (master total),
#                               Pop_Residential, Pop_Wholesale (from AOP by type)
#   input/pws_storage.RDS     — PWS_ID, TINWSYS_IS_NUMBER, and, for each flow
#                               rate, a value + companion unit column:
#                               Avg_Daily_Usage(+_Unit),
#                               Max_Daily_Demand(+_Unit),
#                               Provided_Production_Capacity(+_Unit),
#                               Provided_Service_Pump_Capacity(+_Unit)
# NOTE: total STORAGE capacity (TSTC) is NOT here — DWV serves it from the
# measures widget, written to storage_connections_data.txt by script 06.
#
# UNITS: DWV reports each flow rate with its own FLOW_RATE_UOM_CODE (mostly MGD,
# but GPM for the pump-capacity PSPC rows, and it is NOT guaranteed uniform
# across systems). We therefore do NOT bake a unit into the column name / assume
# MGD; instead we carry the raw per-system unit in a companion <name>_Unit
# column so downstream code can normalize (or flag) explicitly. See the ADU
# normalization in explore/simple_cox.R.
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
library(data.table)
source(util("scraping", "dwv_api_helpers.R"))   # dwv_session/search/widget clients

ses <- dwv_session()

# --- Master list of systems ---------------------------------------------------
# Replaces the three old SearchDispatch URLs (WaterSystemType = C / NC / NTNC).
# One DashMain search per type, active systems only.
systems <- rbindlist(lapply(c("C", "NC", "NTNC"), function(ty) {
  message("Fetching ", ty, " systems ...")
  dwv_search(ses, type = ty, active_only = TRUE,
             select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME",
                        "D_PWS_FED_TYPE_CD", "D_POPULATION_COUNT", "SVC_CONNECT_CNT"))
}), use.names = TRUE, fill = TRUE)
message("Total active systems: ", nrow(systems))

# Optional: restrict to systems that have a service-area boundary polygon, as
# the old script did (it intersected against the PWS shapefile). Guarded so the
# script still runs if the Box shapefile isn't present.
.shp <- spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp")
if (file.exists(.shp) && requireNamespace("sf", quietly = TRUE)) {
  bnd_ids <- unique(trimws(sf::st_drop_geometry(sf::st_read(.shp, quiet = TRUE))$PWSId))
  systems <- systems[PWS_ID %in% bnd_ids]
  message("Restricted to ", nrow(systems), " systems with boundary polygons.")
}

# --- Incremental top-up when RESCRAPE = FALSE ---------------------------------
# Reuse the committed prior scrape and fetch ONLY systems missing from it. A
# system is treated as done when it appears in the prior POPULATION output:
# every fetched system gets a population row (the master totals cover all
# systems), whereas a system with no DWV flow-rate rows legitimately never
# appears in pws_storage.RDS — requiring both files would re-fetch those
# systems forever. The new rows are merged back in before saving (see the
# rbind before saveRDS). RESCRAPE = TRUE ignores the prior files and
# re-fetches every system.
.pop_out  <- committed("pws_population.RDS")
.stor_out <- committed("pws_storage.RDS")
prev_pop  <- if (!RESCRAPE && file.exists(.pop_out))  as.data.table(readRDS(.pop_out))  else NULL
prev_stor <- if (!RESCRAPE && file.exists(.stor_out)) as.data.table(readRDS(.stor_out)) else NULL
if (!RESCRAPE && !is.null(prev_pop)) {
  done_ids <- as.character(prev_pop$PWS_ID)
  n0 <- nrow(systems)
  systems <- systems[!(trimws(NUMBER0) %in% done_ids)]
  message("RESCRAPE=FALSE: ", length(done_ids), " systems already scraped; fetching ",
          nrow(systems), " new (of ", n0, ").")
}

# --- Population served, by type (replaces the old "PopulationType" table) ------
# DashAnnualOperatingPeriod returns one row per annual operating period, each
# carrying a nested PopulationServed list of {TYPE_CODE, AVG_DAILY_CNT}. We take
# the most recent period per system and unnest it into long (PWS_ID, TYPE_CODE,
# AVG_DAILY_CNT) rows, then pivot to one row per system below.
pop_pull <- function(dt, sysrow) {
  # dt: DashAnnualOperatingPeriod rows for one system (already scoped)
  ps <- dt$PopulationServed[[1]]                      # most-recent period is first
  if (is.null(ps) || length(ps) == 0) return(data.table())
  out <- rbindlist(lapply(ps, function(x) as.data.table(x)), use.names = TRUE, fill = TRUE)
  out[, `:=`(PWS_ID = trimws(sysrow$NUMBER0),
             TINWSYS_IS_NUMBER = sysrow$TINWSYS_IS_NUMBER)]
  out[]
}
population_long <- if (nrow(systems)) {
  dwv_widget_over(ses, "DashAnnualOperatingPeriod", systems,
                  transform = pop_pull, orderby = "EFF_BEGIN_DT desc", top = 1)
} else data.table()

# Pivot population-by-type to one row per system. TYPE_CODE R = residential
# (retail), W = wholesale; keep any other code as Pop_<code>.
pop_by_type <- if (nrow(population_long)) {
  population_long[, TYPE_CODE := trimws(TYPE_CODE)]   # DWV pads codes ("R ", "W ")
  w <- dcast(population_long, PWS_ID ~ TYPE_CODE,
             value.var = "AVG_DAILY_CNT", fun.aggregate = function(x) sum(x, na.rm = TRUE))
  tc_map <- c(R = "Pop_Residential", W = "Pop_Wholesale")
  for (cd in setdiff(names(w), "PWS_ID"))
    setnames(w, cd, if (!is.na(tc_map[cd])) tc_map[cd] else paste0("Pop_", cd))
  w
} else data.table(PWS_ID = character(0))

# Master totals (already fetched above): authoritative population served and
# service-connection counts per system.
master_tot <- systems[, .(PWS_ID,
                          TINWSYS_IS_NUMBER,
                          Population_Served = as.numeric(D_POPULATION_COUNT),
                          Connections       = as.numeric(SVC_CONNECT_CNT))]
population <- merge(master_tot, pop_by_type, by = "PWS_ID", all.x = TRUE)

# --- Flow rates / demand (replaces the old "Max Daily Demand" storage table) ---
# DashWaterSystemFlowRates is long: one row per FLOW_RATE_NAME ("ADU - Average
# Daily Usage", "MDD - Maximum Daily Demand", "PPRC - ...", "PSPC - ..."), each
# carrying its own FLOW_RATE_UOM_CODE. We can't assume a uniform unit, so we
# pivot BOTH the quantity and the unit to one row per system, keyed on the
# leading code token: each measure gets a value column and a <name>_Unit column.
storage_long <- if (nrow(systems)) {
  dwv_widget_over(ses, "DashWaterSystemFlowRates", systems)
} else data.table()
storage <- if (nrow(storage_long)) {
  storage_long[, code := trimws(sub("-.*$", "", FLOW_RATE_NAME))]
  if (!("FLOW_RATE_UOM_CODE" %in% names(storage_long)))
    storage_long[, FLOW_RATE_UOM_CODE := NA_character_]   # defensive: unit absent
  fr_map <- c(ADU  = "Avg_Daily_Usage",
              MDD  = "Max_Daily_Demand",
              PPRC = "Provided_Production_Capacity",
              PSPC = "Provided_Service_Pump_Capacity")
  friendly <- function(cd) ifelse(!is.na(fr_map[cd]), fr_map[cd], paste0("FlowRate_", cd))
  keys <- c("PWS_ID", "TINWSYS_IS_NUMBER")

  val <- dcast(storage_long, PWS_ID + TINWSYS_IS_NUMBER ~ code,
               value.var = "FLOW_RATE_QUANTITY", fun.aggregate = function(x) x[1])
  uom <- dcast(storage_long, PWS_ID + TINWSYS_IS_NUMBER ~ code,
               value.var = "FLOW_RATE_UOM_CODE",
               fun.aggregate = function(x) trimws(as.character(x))[1])
  for (cd in setdiff(names(val), keys)) setnames(val, cd, friendly(cd))
  for (cd in setdiff(names(uom), keys)) setnames(uom, cd, paste0(friendly(cd), "_Unit"))
  merge(val, uom, by = keys)
} else data.table(PWS_ID = character(0), TINWSYS_IS_NUMBER = integer(0))

# Merge newly-fetched rows back onto the reused prior scrape (RESCRAPE = FALSE).
# Fresh rows come FIRST so unique(by = "PWS_ID") keeps the just-fetched row for
# any system that got re-fetched, not the stale prior one.
if (!RESCRAPE && !is.null(prev_pop))
  population <- unique(rbindlist(list(population, prev_pop), use.names = TRUE, fill = TRUE), by = "PWS_ID")
if (!RESCRAPE && !is.null(prev_stor))
  storage <- unique(rbindlist(list(storage, prev_stor), use.names = TRUE, fill = TRUE), by = "PWS_ID")

saveRDS(storage,    committed("pws_storage.RDS"))
saveRDS(population, committed("pws_population.RDS"))
message("Wrote pws_storage.RDS (", nrow(storage), " rows) and ",
        "pws_population.RDS (", nrow(population), " rows).")
