# =============================================================================
# 02_model/05_paper_facts.R
# -----------------------------------------------------------------------------
# Emit the SCALAR facts the manuscript cites in prose (sample sizes, adoption
# rates, the audit-window comparison, the largest service population, ...). These
# numbers live nowhere in the model-estimate / descriptive CSVs, so without this
# file the manuscript's inline "XXX" counts would have to be hand-filled. Instead
# every one is computed here from the SAME shared panel the models are fit on and
# written to a single tidy lookup the paper reads at render time:
#
#   output/paper_facts.csv   columns: key, value, fmt, label
#     key    stable token the manuscript references (e.g. "n_districts_sample")
#     value  the raw numeric value (unformatted, full precision)
#     fmt    a formatting hint the paper's fact() helper applies
#            ("int" | "comma" | "pct0" | "pct1" | "num1" | "num2")
#     label  human description of what the number is (documentation only)
#
# Re-running the model pipeline and then this script refreshes every inline number
# in the paper automatically. Run in the SHARED panel environment (run_all.R does
# this) so it reuses the already-built panel instead of rebuilding it:
#     source("code/02_model/05_paper_facts.R")
# =============================================================================

# Load config (sets wd = project root) if a caller hasn't already.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}
# Build (or reuse) the shared panel -> panel_m1, panel_m2, events, *_vars.
source("code/02_model/build_recurrent_panel.R")

suppressPackageStartupMessages(library(data.table))

# Accumulate facts as (key, value, fmt, label) rows.
.facts <- list()
add_fact <- function(key, value, fmt, label) {
  # NB: data.table(key=) is a reserved argument, so build the row without it and
  # attach the key column by name.
  row <- data.table(value = as.numeric(value), fmt = fmt, label = label)
  row[, key := key]
  setcolorder(row, c("key", "value", "fmt", "label"))
  .facts[[length(.facts) + 1L]] <<- row
}

# =============================================================================
# 1. District subsample (panel_m2) -- the analysis sample for the fiscal models
# =============================================================================
n_districts_sample <- uniqueN(panel_m2$District_ID)
n_systems_sample   <- uniqueN(panel_m2$PWS_ID)
add_fact("n_districts_sample", n_districts_sample, "comma",
         "Distinct special water districts in the fiscal-model subsample")
add_fact("n_systems_sample", n_systems_sample, "comma",
         "Distinct CWSs in the fiscal-model subsample")

# Adoption: events per district over the sample. % adopting >=1 and >=2, and the
# max restrictions any one district adopts.
ev_dist <- panel_m2[, .(n_events = sum(event)), by = District_ID]
add_fact("pct_adopt_ge1", mean(ev_dist$n_events >= 1), "pct0",
         "% of sample districts adopting >=1 mandatory restriction")
add_fact("pct_adopt_ge2", mean(ev_dist$n_events >= 2), "pct0",
         "% of sample districts adopting >=2 mandatory restrictions")
add_fact("max_restrictions", max(ev_dist$n_events), "int",
         "Max mandatory restrictions adopted by a single sample district")
add_fact("n_event_weeks_sample", sum(panel_m2$event), "comma",
         "Restriction-adoption event-weeks in the fiscal-model subsample")

# =============================================================================
# 2. Global full sample (panel_m1) -- the prior-setting Model 1 sample
# =============================================================================
add_fact("n_cws_global", uniqueN(panel_m1$PWS_ID), "comma",
         "Distinct CWSs in the global (Model 1) full sample")
add_fact("n_districts_linked_global", uniqueN(panel_m1[!is.na(District_ID), District_ID]),
         "comma", "District-linked systems in the global full sample")
add_fact("n_event_weeks_global", sum(panel_m1$event), "comma",
         "Restriction-adoption event-weeks in the global full sample")

# =============================================================================
# 3. District <-> CWS mapping (footnote 7) -- from the id crosswalk
# =============================================================================
xw <- as.data.table(readRDS(committed("id_crosswalk.RDS")))
xw <- unique(xw[!is.na(PWS_ID) & !is.na(District_ID), .(PWS_ID, District_ID)])
# Restrict to districts that appear in the analysis sample so the counts describe
# the studied population, not every district on record.
xw <- xw[District_ID %in% unique(panel_m2$District_ID)]
cws_per_dist <- xw[, .(n_cws = uniqueN(PWS_ID)), by = District_ID]
add_fact("n_districts_total", nrow(cws_per_dist), "comma",
         "Total sample districts (footnote 7)")
add_fact("n_districts_one_cws", cws_per_dist[n_cws == 1L, .N], "comma",
         "Sample districts administering exactly one CWS")
add_fact("n_districts_multi_cws", cws_per_dist[n_cws > 1L, .N], "comma",
         "Sample districts administering more than one CWS")
add_fact("n_cws_in_multi", cws_per_dist[n_cws > 1L, sum(n_cws)], "comma",
         "CWSs administered by the multi-CWS districts")

# =============================================================================
# 4. Audit window: 730-day vs 365-day district coverage (footnote 19)
# -----------------------------------------------------------------------------
# Re-derive the finance roll join at both windows on the district-linked
# system-weeks (the pre-join base for panel_m2) and count the distinct districts
# that carry a matched prior audit under each rule. Mirrors §4 of the panel
# builder; kept self-contained so this reads only committed inputs.
# =============================================================================
fin <- as.data.table(readRDS(committed("combined_and_lagged_finances.RDS")))
fin[, District_ID := as.character(District_ID)]
fin[, fy_end := as.Date(`FISCAL YEAR ENDED`)]
fin <- fin[!is.na(fy_end) & !is.na(District_ID)]
fin <- unique(fin[, .(District_ID, fy_end, has_audit = 1L)], by = c("District_ID", "fy_end"))

base <- panel_m1[!is.na(District_ID), .(District_ID, PWS_ID, tstart, event)]
base[, week_date := analysis_start + tstart * 7]
count_districts <- function(roll_days) {
  j <- fin[base, on = .(District_ID, fy_end = week_date), roll = roll_days]
  uniqueN(j[has_audit == 1L, District_ID])
}
add_fact("n_districts_window730", count_districts(730), "comma",
         "Districts usable with a 730-day prior-audit window")
add_fact("n_districts_window365", count_districts(365), "comma",
         "Districts usable with a 365-day prior-audit window")

# =============================================================================
# 5. Service population extremes (discussion) -- from the population master
# =============================================================================
pop <- as.data.table(readRDS(committed("pws_population.RDS")))
pop <- pop[, .(PopServed = num(Population_Served)), by = PWS_ID]
pop_sample <- pop[PWS_ID %in% unique(panel_m2$PWS_ID) & is.finite(PopServed)]
if (nrow(pop_sample)) {
  add_fact("max_pop_served_sample", max(pop_sample$PopServed), "comma",
           "Largest service population among sample districts")
}

# =============================================================================
# 5b. Multicollinearity: the joint fiscal model's worst VIF (appendix prose)
# -----------------------------------------------------------------------------
# The full VIF table is built in 04_descriptive_stats_table.R; here we just pull
# out the single number the Appendix C prose cites -- the largest VIF in the
# joint fiscal model (all four fiscal predictors + shared controls), the design
# with the most competing predictors. Recomputed inline (the same inverse-
# correlation-matrix VIF) rather than parsing 04's display-ready CSV. `se_factor`
# = sqrt(VIF): how much that predictor's SE is inflated vs. an orthogonal design.
.vjoint <- c(shared_vars_m2, fiscal_vars)
.Xj <- as.matrix(panel_m2[, .SD, .SDcols = .vjoint])
.Xj <- .Xj[complete.cases(.Xj), , drop = FALSE]
.Xj <- .Xj[, apply(.Xj, 2L, function(col) stats::sd(col) > 0), drop = FALSE]
.vif_max_joint <- max(diag(solve(cor(.Xj))))
add_fact("vif_max_joint", .vif_max_joint, "num2",
         "Largest VIF in the joint fiscal model (all four fiscal predictors)")
add_fact("vif_max_joint_se", sqrt(.vif_max_joint), "num1",
         "SE inflation factor (sqrt VIF) for that worst-case joint-model predictor")

# =============================================================================
# 6. Observation window (from config.R) + baseline-hazard resolution
# =============================================================================
add_fact("start_year", as.integer(format(analysis_start, "%Y")), "int",
         "First year of the observation window")
add_fact("end_year", as.integer(format(analysis_end, "%Y")), "int",
         "Last year of the observation window")
add_fact("window_years",
         as.integer(format(analysis_end, "%Y")) - as.integer(format(analysis_start, "%Y")),
         "int", "Observation-window span in years")

# =============================================================================
# Write the lookup.
# =============================================================================
facts <- rbindlist(.facts, use.names = TRUE)
fwrite(facts, output("paper_facts.csv"))
message("Wrote ", nrow(facts), " manuscript facts -> ", output("paper_facts.csv"))
print(facts[, .(key, value = round(value, 3), fmt)])
