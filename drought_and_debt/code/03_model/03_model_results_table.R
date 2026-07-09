# =============================================================================
# 03_model/03_model_results_table.R
# -----------------------------------------------------------------------------
# Reporting for the Bayesian recurring-events Cox models fit in
# 01_fit_recurrent_cox_inla.R. Reads the saved INLA objects and produces:
#
#   output/model_estimates.html   posterior mean + 95% credible interval for
#                                 every fixed effect, on both the coefficient
#                                 (log hazard-ratio) and hazard-ratio scales.
#   output/model_credible_intervals.png
#                                 companion ggplot forest plot: each estimate is
#                                 a line SEGMENT spanning its 95% CrI with the
#                                 posterior mean marked as a point.
#   output/model_estimates.csv    the tidy estimate table behind both.
#
# INLA stores these directly in each fit's $summary.fixed (columns: mean, sd,
# `0.025quant`, `0.5quant`, `0.975quant`, ...), so no refitting is needed here.
#
# Run from the drought_and_debt project root, after 01_fit_recurrent_cox_inla.R:
#     source("code/03_model/03_model_results_table.R")
# =============================================================================

# Load config (sets wd = project root) if a caller hasn't already. Works from
# the bosque repo root, the drought_and_debt root, or a code/ subdirectory.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggthemes)
  library(knitr)
})

# --- Load the saved Bayesian fits --------------------------------------------
m1_path     <- scratch("recurrent_coxinla_model1_full.RDS")
m2_path     <- scratch("recurrent_coxinla_model2_by_fiscal.RDS")
m2_all_path <- scratch("recurrent_coxinla_model2_all_fiscal.RDS")

model1_inla           <- if (file.exists(m1_path))     readRDS(m1_path)     else NULL
model2_inla_by_fiscal <- if (file.exists(m2_path))     readRDS(m2_path)     else NULL
model2_inla_all_fiscal <- if (file.exists(m2_all_path)) readRDS(m2_all_path) else NULL

if (is.null(model1_inla) && is.null(model2_inla_by_fiscal) &&
    is.null(model2_inla_all_fiscal)) {
  stop("No fitted models found. Run code/03_model/01_fit_recurrent_cox_inla.R first.\n",
       "  looked for:\n    ", m1_path, "\n    ", m2_path, "\n    ", m2_all_path)
}

# --- Pretty labels for the model terms ---------------------------------------
term_labels <- c(
  DSCI_100           = "Drought severity (DSCI/100)",
  ln_connections     = "Log connections",
  storage_per_conn_g = "Storage per connection (asinh gal)",
  has_interconnect   = "Has interconnect",
  ln_income          = "Log median income",
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
# Top-to-bottom ordering in the plot / table (drought & controls, then fiscal).
term_order  <- names(term_labels)
fiscal_terms <- c("debt_per_conn", "fund_bal_per_conn", "revenue_per_conn",
                  "operating_ratio", "debt_svc_tax")

# --- Pull the posterior summaries into one tidy table ------------------------
# INLA's summary.fixed has one row per fixed effect; we keep the mean and the
# 2.5% / 97.5% quantiles (the bounds of the 95% credible interval).
tidy_fixed <- function(fit, keep_terms, model_label, group_label) {
  sf <- as.data.frame(fit$summary.fixed)
  sf$term <- rownames(sf)
  sf <- sf[sf$term %in% keep_terms, , drop = FALSE]
  if (!nrow(sf)) return(NULL)
  data.table(
    term  = sf$term,
    model = model_label,
    group = group_label,
    mean  = sf$mean,
    lower = sf[["0.025quant"]],
    upper = sf[["0.975quant"]]
  )
}

pieces <- list()

# Model 1: the drought effect + all system controls (full-sample estimates).
if (!is.null(model1_inla)) {
  shared <- setdiff(term_order, fiscal_terms)
  pieces[["m1"]] <- tidy_fixed(model1_inla, shared,
                               "Model 1 (full sample)", "Drought & controls")
}

# Model 2: one fit per fiscal covariate -> keep only that covariate's own row
# (the shared effects there are pinned to Model 1's posteriors as priors).
if (!is.null(model2_inla_by_fiscal)) {
  for (v in names(model2_inla_by_fiscal)) {
    fit <- model2_inla_by_fiscal[[v]]
    if (is.null(fit)) next
    pieces[[paste0("m2_", v)]] <- tidy_fixed(fit, v, "Model 2 (fiscal)", "Fiscal")
  }
}

# Joint fiscal model: one fit holding ALL fiscal covariates -> keep every fiscal
# row (each effect conditioned on the others, on the all-observed subsample).
if (!is.null(model2_inla_all_fiscal)) {
  pieces[["m2_all"]] <- tidy_fixed(model2_inla_all_fiscal, fiscal_terms,
                                   "Model 2 (all fiscal)", "Fiscal (joint)")
}

est <- rbindlist(pieces, use.names = TRUE)
if (!nrow(est)) stop("No matching model terms found in the fitted objects.")

# Ordered factor drives both table row order and plot y-axis order. Group order
# keeps each block (controls, isolated fiscal, joint fiscal) contiguous so the
# pack_rows() grouping below stays correct; within a block, order by term.
group_levels <- c("Drought & controls", "Fiscal", "Fiscal (joint)")
est[, group := factor(group, levels = group_levels)]
est[, label := factor(term_labels[term], levels = rev(term_labels[term_order]))]
est <- est[order(group, match(term, term_order))]

# Hazard-ratio scale (these are Cox coefficients -> exp() = hazard ratios).
est[, `:=`(hr = exp(mean), hr_lower = exp(lower), hr_upper = exp(upper))]

# =============================================================================
# 1. HTML table
# =============================================================================
fmt <- function(x) formatC(x, format = "f", digits = 3)
tab <- est[, .(
  Term        = as.character(label),
  Model       = model,
  `Post. mean (coef.)` = fmt(mean),
  `95% CrI (coef.)`    = paste0("[", fmt(lower), ", ", fmt(upper), "]"),
  `Hazard ratio`       = fmt(hr),
  `95% CrI (HR)`       = paste0("[", fmt(hr_lower), ", ", fmt(hr_upper), "]")
)]

html_path <- output("model_estimates.html")
caption   <- "Bayesian recurring-events Cox model: posterior means and 95% credible intervals"

if (requireNamespace("kableExtra", quietly = TRUE)) {
  html_tbl <- knitr::kable(tab, format = "html", align = "lrrrrr", caption = caption)
  html_tbl <- kableExtra::kable_styling(
    html_tbl, bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, position = "left")
  html_tbl <- kableExtra::pack_rows(html_tbl, index = table(factor(est$group,
                                    levels = unique(est$group))))
  kableExtra::save_kable(html_tbl, file = html_path)
} else {
  # kableExtra not installed -> plain but valid standalone HTML from knitr::kable.
  body <- knitr::kable(tab, format = "html", align = "lrrrrr", caption = caption)
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>body{font-family:sans-serif;margin:2em}",
    "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
    "th{background:#f2f2f2;text-align:left}td{text-align:right}td:first-child{text-align:left}",
    "</style></head><body>", as.character(body), "</body></html>"
  ), html_path)
}
message("Wrote HTML table -> ", html_path)

fwrite(est, output("model_estimates.csv"))
message("Wrote tidy estimates -> ", output("model_estimates.csv"))

# =============================================================================
# 2. Companion ggplot -- 95% credible intervals as line segments
# =============================================================================
# One row per term: a segment from the lower to the upper CrI bound with the
# posterior mean as a point. Plotted on the hazard-ratio scale (log x-axis) with
# a reference line at HR = 1 (no effect). A fiscal term appears in both the
# isolated and the joint model, so dodge by group to show them side by side at
# the same y instead of overplotting.
dodge <- position_dodge(width = 0.6)
ci_plot <- ggplot(est, aes(y = label, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_segment(aes(x = hr_lower, xend = hr_upper, yend = label),
               linewidth = 0.7, position = dodge) +
  geom_point(aes(x = hr), size = 2, position = dodge) +
  scale_x_continuous(trans = "log10", name = "Hazard ratio (95% credible interval)") +
  scale_colour_tableau(name = NULL) +
  labs(y = NULL,
       title = "Bayesian recurring-events Cox model",
       subtitle = "Posterior mean (point) and 95% credible interval (segment)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

plot_path <- output("model_credible_intervals.png")
ggsave(plot_path, ci_plot, width = 8,
       height = 1 + 0.35 * nrow(est), units = "in", dpi = 400)
message("Wrote credible-interval plot -> ", plot_path)

message("Done. Model reporting outputs in ", OUTPUT_DIR, "/")
