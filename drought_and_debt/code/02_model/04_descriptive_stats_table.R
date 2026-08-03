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
#   * Outcome (event) + drought (DSCI) + ln_connections (yearly SDWIS) + fiscal
#     covariates are TIME-VARYING, summarised over the system-week OBSERVATIONS
#     the model actually sees.
#   * The remaining system controls are TIME-INVARIANT, so they are summarised
#     once per SYSTEM (de-duplicated by PWS_ID) -- summarising them over weekly
#     rows would just weight each system by how long it is observed.
# Outcome/drought and every system control are reported TWICE -- once for the
# global (Model 1) sample and once for the district-linked (Model 2) subsample
# -- as two sub-rows per variable, tagged in the "sample" column. The paper
# splits on that column: the main body shows the district sample, the appendix
# the global sample. Drought is reported as the raw DSCI (its model scale). The
# continuous controls enter the models as z-scores; here they are back-transformed
# to their NATURAL scale (via z_scale from the panel builder) so the table reads in
# real units. Fiscal covariates are likewise un-standardized and then reported
# UNTRANSFORMED ($ per connection) even though the models use z-scored asinh terms.
# Each row reports N (non-missing), mean, sd, min, p25, median, p75, max; the
# "unit" column names what one observation is.
#
# Writes:
#   output/descriptive_stats.html   grouped, human-readable table
#   output/descriptive_stats.csv    the tidy table behind it
#   output/correlation_global.{csv,html,png}   predictor correlations, Model 1
#   output/correlation_fiscal.{csv,html,png}   predictor correlations, Model 2
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

# --- Back-transform z-scored covariates to natural scale ---------------------
# The model panels carry the continuous covariates as z-scores (build_recurrent_
# panel.R standardizes them, storing each var's mean/sd in z_scale). The
# descriptives report NATURAL scale, so undo the z-scoring row-exactly here:
# x = z*sd + mean. Vars absent from z_scale (DSCI, the 0/1 flags) pass through
# untouched. Correlations are scale-invariant, so those still read the
# standardized panels directly below.
destd <- function(dt) {
  dt <- copy(dt)
  for (v in names(z_scale)) if (v %in% names(dt)) {
    ms <- z_scale[[v]]
    dt[, (v) := get(v) * ms[["sd"]] + ms[["mean"]]]
  }
  dt[]
}
panel_m1_nat <- destd(panel_m1)
panel_m2_nat <- destd(panel_m2)

# --- Pretty labels (kept in sync with 03_model_results_table.R) --------------
term_labels <- c(
  event              = "Mandatory restriction (event)",
  DSCI               = "Drought severity (DSCI, 0-100)",
  DSCI_100           = "Drought severity (DSCI/100)",
  seller_restricted  = "Seller under restriction",
  ln_connections     = "Log connections",
  storage_per_conn_g = "Storage per connection (asinh gal)",
  source_surface     = "Surface water (vs ground)",
  purchases_water    = "Purchases water (primary)",
  emergency_source   = "Emergency source/interconnect",
  wholesaler         = "Wholesaler (sells water)",
  ln_home_value      = "Log median home value",
  median_structure_age = "Median structure age (yrs)",
  perc_dem_vote      = "% Dem. vote share",
  debt_go_per_conn   = "GO (tax) debt per connection (asinh)",
  debt_rev_per_conn  = "Revenue debt per connection (asinh)",
  fund_bal_per_conn  = "Fund balance per connection (asinh)",
  revenue_per_conn   = "Revenue per connection (asinh)",
  # Untransformed ($ per connection) versions, reported in the descriptives
  # table; the models use the asinh-transformed variables above.
  debt_go_per_conn_raw  = "GO (tax) debt per connection ($)",
  debt_rev_per_conn_raw = "Revenue debt per connection ($)",
  fund_bal_per_conn_raw = "Fund balance per connection ($)",
  revenue_per_conn_raw  = "Revenue per connection ($)"
)

# --- describe(): per-variable summary stats over a data.table ----------------
# Summarises each `vars` column of `dt` on its own non-missing rows, tagging the
# result with a group + unit-of-observation + sample label. Returns one row per
# variable.
describe <- function(dt, vars, group_label, unit_label, sample_label) {
  rows <- lapply(vars, function(v) {
    x <- dt[[v]]
    x <- x[is.finite(x)]                 # drop NA / non-finite before summarising
    if (!length(x)) return(NULL)
    data.table(
      term   = v,
      group  = group_label,
      unit   = unit_label,
      sample = sample_label,
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
# the distribution. The tv_ctrl_vars subset (ln_connections from yearly SDWIS;
# ln_home_value / median_structure_age from the two forward-filled ACS vintages)
# is TIME-VARYING, so it is summarised per system-week like drought. Every
# system control gets TWO sub-rows: the global (Model 1) sample and the
# district-linked (Model 2) subsample the fiscal models are fit on.
systems_m1 <- unique(panel_m1_nat, by = "PWS_ID")
systems_m2 <- unique(panel_m2_nat, by = "PWS_ID")
static_ctrl <- setdiff(ctrl_vars, tv_ctrl_vars)
GLOBAL  <- "Global (Model 1)"
DIST    <- "District-only (Model 2)"

# Fiscal covariates enter the models z-scored asinh; the table reports the
# untransformed dollars-per-connection, recovered exactly by un-standardizing
# (panel_m2_nat) then sinh().
fiscal_raw <- panel_m2_nat[, lapply(.SD, sinh), .SDcols = fiscal_vars]
setnames(fiscal_raw, paste0(fiscal_vars, "_raw"))

pieces <- list(
  # Outcome + drought are per system-week (what the Cox likelihood integrates).
  # DSCI is reported on its raw natural scale (the scale it enters the models on).
  describe(panel_m1_nat, c("event", "DSCI"),
           "Outcome & drought (system-week)", "system-week", GLOBAL),
  describe(panel_m2_nat, c("event", "DSCI"),
           "Outcome & drought (system-week)", "system-week", DIST),
  # System controls, each in both samples. tv_ctrl_vars vary by year, so they
  # are summarised over system-weeks; the rest once per system. Reported on the
  # natural (un-standardized) scale via panel_*_nat.
  describe(panel_m1_nat, tv_ctrl_vars, "System controls", "system-week", GLOBAL),
  describe(panel_m2_nat, tv_ctrl_vars, "System controls", "system-week", DIST),
  describe(systems_m1, static_ctrl, "System controls", "system", GLOBAL),
  describe(systems_m2, static_ctrl, "System controls", "system", DIST),
  # Fiscal covariates: per district-year-linked system-week in the subsample.
  # Each is summarised on its own non-NA rows (matching how each model is fit).
  describe(fiscal_raw, names(fiscal_raw),
           "Fiscal (district subsample, system-week)", "system-week", DIST)
)
desc <- rbindlist(pieces, use.names = TRUE)

# Order rows drought/controls/fiscal to match term_labels, with the two sample
# sub-rows (global, then district-only) adjacent per variable; pretty-label.
desc[, label := term_labels[term]]
desc[is.na(label), label := term]                 # fall back to the raw name
desc <- desc[order(match(term, names(term_labels)),
                   match(sample, c(GLOBAL, DIST)))]

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
  Sample   = sample,
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
  html_tbl <- knitr::kable(tab, format = "html", align = "lllrrrrrrrr", caption = caption)
  html_tbl <- kableExtra::kable_styling(
    html_tbl, bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, position = "left")
  html_tbl <- kableExtra::pack_rows(html_tbl, index = table(factor(desc$group,
                                    levels = unique(desc$group))))
  # Merge the repeated variable label across its two sample sub-rows.
  html_tbl <- kableExtra::collapse_rows(html_tbl, columns = 1, valign = "top")
  kableExtra::save_kable(html_tbl, file = html_path)
} else {
  # kableExtra not installed -> plain but valid standalone HTML from knitr::kable.
  body <- knitr::kable(tab, format = "html", align = "lllrrrrrrrr", caption = caption)
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>body{font-family:sans-serif;margin:2em}",
    "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
    "th{background:#f2f2f2;text-align:left}td{text-align:right}",
    "td:first-child,td:nth-child(2),td:nth-child(3){text-align:left}",
    "</style></head><body>", as.character(body), "</body></html>"
  ), html_path)
}
message("Wrote HTML table -> ", html_path)

# Console echo so a bare `source()` shows the table.
print(knitr::kable(tab, format = "simple"))

# =============================================================================
# 3. Predictor correlation matrices (appendix)
# =============================================================================
# Pairwise-complete Pearson correlations among the model predictors, computed
# over the SAME system-week rows each model is fit on: the GLOBAL matrix uses
# the Model 1 panel and its predictors (shared_vars); the FISCAL matrix uses the
# district subsample and adds the fiscal covariates. Each writes a display-ready
# lower-triangle CSV (rendered as-is by the manuscript appendix), an HTML copy,
# and a heatmap PNG (diverging palette -- two hue poles, neutral gray at r = 0 --
# with every r printed, so the plot doubles as a readable table).
suppressPackageStartupMessages(library(ggplot2))

write_cor <- function(dt, vars, stem, title) {
  M    <- cor(as.matrix(dt[, ..vars]), use = "pairwise.complete.obs")
  labs <- ifelse(is.na(term_labels[vars]), vars, term_labels[vars])
  k    <- length(vars)
  num  <- paste0("(", seq_len(k), ")")

  # Tidy lower-triangle display table -> CSV + HTML.
  disp <- formatC(M, format = "f", digits = 2)
  disp[upper.tri(M)] <- ""
  ctab <- cbind(data.table(Variable = paste(num, labs)), as.data.table(disp))
  setnames(ctab, c("Variable", num))
  fwrite(ctab, output(paste0(stem, ".csv")))

  cbody <- knitr::kable(ctab, format = "html", align = c("l", rep("r", k)),
                        caption = title)
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>body{font-family:sans-serif;margin:2em}",
    "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
    "th{background:#f2f2f2;text-align:left}td{text-align:right}",
    "td:first-child{text-align:left}",
    "</style></head><body>", as.character(cbody), "</body></html>"
  ), output(paste0(stem, ".html")))

  # Heatmap PNG: lower triangle, rows labeled, columns numbered.
  long <- CJ(i = seq_len(k), j = seq_len(k))[i >= j]
  long[, r := M[cbind(i, j)]]
  long[, `:=`(x = factor(num[j], levels = num),
              y = factor(paste(num[i], labs[i]),
                         levels = rev(paste(num, labs))))]
  p <- ggplot(long, aes(x = x, y = y, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = formatC(r, format = "f", digits = 2)),
              size = 2.6, family = "Times",
              colour = fifelse(abs(long$r) > 0.6, "white", "grey15")) +
    scale_fill_gradient2(low = "#2166AC", mid = "grey92", high = "#B2182B",
                         limits = c(-1, 1), name = "r") +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(text = element_text(family = "Times"),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 9))
  ggsave(output(paste0(stem, ".png")), p,
         width = 2.6 + 0.42 * k, height = 1.6 + 0.3 * k, units = "in", dpi = 400)
  message("Wrote correlation table + plot -> output/", stem, ".{csv,html,png}")
}

write_cor(panel_m1, shared_vars, "correlation_global",
          "Global model (Model 1) predictor correlations")
write_cor(panel_m2, c(shared_vars, fiscal_vars), "correlation_fiscal",
          "Fiscal model (Model 2) predictor correlations")

message("Done. Descriptive-stats outputs in ", OUTPUT_DIR, "/")
