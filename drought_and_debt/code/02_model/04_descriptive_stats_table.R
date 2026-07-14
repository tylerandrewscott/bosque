# =============================================================================
# 02_model/04_descriptive_stats_table.R
# -----------------------------------------------------------------------------
# Descriptive-statistics table for every variable that enters the recurring-
# events Cox models fit in 01_fit_recurrent_cox_inla.R. It sources the SAME
# shared panel builder those models use (build_recurrent_panel.R), so the
# summary describes the exact analysis sample -- not a re-derived copy.
#
# The panel is a weekly counting-process panel (one row per system-week), so a
# variable's summary is only meaningful at its natural unit of analysis:
#   * Outcome (event) + drought (DSCI) + fiscal covariates are TIME-VARYING,
#     summarised over the system-week OBSERVATIONS the model actually sees.
#   * The system controls are TIME-INVARIANT, so they are summarised once per
#     SYSTEM (panel_m1 de-duplicated by PWS_ID) -- summarising them over weekly
#     rows would just weight each system by how long it is observed.
# Each row reports N (non-missing), mean, sd, min, p25, median, p75, max; the
# "unit" column names what one observation is.
#
# Writes:
#   output/descriptive_stats.html   grouped, human-readable table
#   output/descriptive_stats.csv    the tidy table behind it
#
# Run from the drought_and_debt project root, standalone (it fits nothing):
#     source("code/02_model/04_descriptive_stats_table.R")
# =============================================================================

# Load config (sets wd = project root) if a caller hasn't already. Works from
# the bosque repo root, the drought_and_debt root, or a code/ subdirectory.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}
# Build the shared panel. The builder is idempotent (skips the expensive rebuild
# when a current panel is already in this env, always refreshes the covariate-
# name vectors), so source it unconditionally rather than re-checking its outputs
# here — run_all.R runs the fit script and this one in one env so the
# ~minutes-long build happens once.
source("code/02_model/build_recurrent_panel.R") # -> panel_m1, panel_m2, *_vars

suppressPackageStartupMessages({
  library(data.table)
  library(knitr)
})

# --- Pretty labels (kept in sync with 03_model_results_table.R) --------------
term_labels <- c(
  event              = "Mandatory restriction (event)",
  DSCI               = "Drought severity (DSCI, 0-100)",
  DSCI_100           = "Drought severity (DSCI/100)",
  ln_connections     = "Log connections",
  storage_per_conn_g = "Storage per connection (asinh gal)",
  has_interconnect   = "Has interconnect (0/1)",
  ln_income          = "Log median income",
  ln_home_value      = "Log median home value",
  median_structure_age = "Median structure age (yrs)",
  perc_rural         = "% rural",
  perc_hispanic      = "% Hispanic",
  perc_black         = "% Black",
  perc_dem_vote      = "% Dem. vote share",
  debt_per_conn      = "Debt per connection (asinh)",
  fund_bal_per_conn  = "Fund balance per connection (asinh)",
  revenue_per_conn   = "Revenue per connection (asinh)",
  operating_ratio    = "Operating ratio (asinh)",
  debt_svc_tax       = "Debt-service tax (0/1)"
)

# --- describe(): per-variable summary stats over a data.table ----------------
# Summarises each `vars` column of `dt` on its own non-missing rows, tagging the
# result with a group + unit-of-observation label. Returns one row per variable.
describe <- function(dt, vars, group_label, unit_label) {
  rows <- lapply(vars, function(v) {
    x <- dt[[v]]
    x <- x[is.finite(x)]                 # drop NA / non-finite before summarising
    if (!length(x)) return(NULL)
    data.table(
      term   = v,
      group  = group_label,
      unit   = unit_label,
      N      = length(x),
      mean   = mean(x),
      sd     = stats::sd(x),
      min    = min(x),
      p25    = stats::quantile(x, 0.25, names = FALSE),
      median = stats::median(x),
      p75    = stats::quantile(x, 0.75, names = FALSE),
      max    = max(x)
    )
  })
  rbindlist(rows, use.names = TRUE)
}

# --- Assemble the summary at the right unit for each block -------------------
# Time-invariant controls: one row per system so exposure length doesn't weight
# the distribution. panel_m1 holds the full-sample controls (complete cases).
systems_m1 <- unique(panel_m1, by = "PWS_ID")

pieces <- list(
  # Outcome + drought are per system-week (what the Cox likelihood integrates).
  describe(panel_m1, c("event", "DSCI", "DSCI_100"),
           "Outcome & drought (system-week)", "system-week"),
  # System controls: one observation per water system.
  describe(systems_m1, ctrl_vars,
           "System controls (time-invariant)", "system"),
  # Fiscal covariates: per district-year-linked system-week in the subsample.
  # Each is summarised on its own non-NA rows (matching how each model is fit).
  describe(panel_m2, fiscal_vars,
           "Fiscal (district subsample, system-week)", "system-week")
)
desc <- rbindlist(pieces, use.names = TRUE)

# Order rows drought/controls/fiscal to match term_labels, and pretty-label.
desc[, label := term_labels[term]]
desc[is.na(label), label := term]                 # fall back to the raw name
desc <- desc[order(match(term, names(term_labels)))]

# =============================================================================
# 1. CSV (tidy, full precision)
# =============================================================================
fwrite(desc, output("descriptive_stats.csv"))
message("Wrote tidy descriptive stats -> ", output("descriptive_stats.csv"))

# =============================================================================
# 2. Grouped HTML table
# =============================================================================
fmt <- function(x) formatC(x, format = "f", digits = 3, big.mark = ",")
tab <- desc[, .(
  Variable = as.character(label),
  Unit     = unit,
  N        = formatC(N, format = "d", big.mark = ","),
  Mean     = fmt(mean),
  SD       = fmt(sd),
  Min      = fmt(min),
  P25      = fmt(p25),
  Median   = fmt(median),
  P75      = fmt(p75),
  Max      = fmt(max)
)]

html_path <- output("descriptive_stats.html")
caption   <- "Descriptive statistics for recurring-events Cox model variables"

if (requireNamespace("kableExtra", quietly = TRUE)) {
  html_tbl <- knitr::kable(tab, format = "html", align = "llrrrrrrrr", caption = caption)
  html_tbl <- kableExtra::kable_styling(
    html_tbl, bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, position = "left")
  html_tbl <- kableExtra::pack_rows(html_tbl, index = table(factor(desc$group,
                                    levels = unique(desc$group))))
  kableExtra::save_kable(html_tbl, file = html_path)
} else {
  # kableExtra not installed -> plain but valid standalone HTML from knitr::kable.
  body <- knitr::kable(tab, format = "html", align = "llrrrrrrrr", caption = caption)
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>body{font-family:sans-serif;margin:2em}",
    "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
    "th{background:#f2f2f2;text-align:left}td{text-align:right}",
    "td:first-child,td:nth-child(2){text-align:left}",
    "</style></head><body>", as.character(body), "</body></html>"
  ), html_path)
}
message("Wrote HTML table -> ", html_path)

# Console echo so a bare `source()` shows the table.
print(knitr::kable(tab, format = "simple"))

message("Done. Descriptive-stats outputs in ", OUTPUT_DIR, "/")
