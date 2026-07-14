# =============================================================================
# 02_model/03_model_results_table.R
# -----------------------------------------------------------------------------
# Reporting for the Bayesian recurring-events Cox models fit in
# 01_fit_recurrent_cox_inla.R. Reads the saved INLA objects and produces:
#
#   output/model_estimates.html   WIDE table: one column per individual model
#                                 fit (Model 1, each isolated fiscal model, and
#                                 the joint fiscal model). Each cell is the
#                                 posterior mean of the coefficient (log hazard
#                                 ratio) with its 95% credible interval below.
#                                 Shared controls appear in every column; a
#                                 fiscal row is blank in any model that omits it.
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
#     source("code/02_model/03_model_results_table.R")
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
# Prefer the SLIM copies the fit script writes to output/: they keep
# $summary.fixed, which is all this script reads, and load in seconds where the
# full scratch/ fits run to multiple GB. The full fit is read only when it is
# strictly newer than the slim copy (i.e. the fit script saved it but died
# before refreshing the slim one) or the slim copy is missing.
load_fit <- function(stem) {
  full <- scratch(paste0(stem, ".RDS"))
  slim <- output(paste0(stem, "_slim.RDS"))
  if (file.exists(slim) &&
      (!file.exists(full) || file.mtime(slim) >= file.mtime(full)))
    return(readRDS(slim))
  if (file.exists(full)) {
    message("Slim copy missing or older than scratch/", basename(full),
            "; loading the full fit.")
    return(readRDS(full))
  }
  NULL
}
model1_inla            <- load_fit("recurrent_coxinla_model1_full")
model1_appendix_inla   <- load_fit("recurrent_coxinla_model1_appendix")
model2_inla_by_fiscal  <- load_fit("recurrent_coxinla_model2_by_fiscal")
model2_inla_all_fiscal <- load_fit("recurrent_coxinla_model2_all_fiscal")

if (is.null(model1_inla) && is.null(model2_inla_by_fiscal) &&
    is.null(model2_inla_all_fiscal)) {
  stop("No fitted models found (scratch/ or output/*_slim.RDS). ",
       "Run code/02_model/01_fit_recurrent_cox_inla.R first.")
}

# --- Pretty labels for the model terms ---------------------------------------
term_labels <- c(
  DSCI_100           = "Drought severity (DSCI/100)",
  ln_connections     = "Log connections",
  storage_per_conn_g = "Storage per connection (asinh gal)",
  has_interconnect   = "Has interconnect",
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
# Top-to-bottom ordering in the plot / table (drought & controls, then fiscal).
term_order  <- names(term_labels)
fiscal_terms <- c("debt_per_conn", "fund_bal_per_conn", "revenue_per_conn",
                  "operating_ratio", "debt_svc_tax")

# --- Pull the posterior summaries into one tidy table ------------------------
# INLA's summary.fixed has one row per fixed effect; we keep the mean and the
# 2.5% / 97.5% quantiles (the bounds of the 95% credible interval). Every column
# of the report is one model fit, so we now extract EVERY term each fit carries
# (the shared controls too, not just its fiscal covariate) so common effects can
# be compared across all models.
tidy_all <- function(fit, col_label) {
  if (is.null(fit)) return(NULL)
  sf <- as.data.frame(fit$summary.fixed)
  sf$term <- rownames(sf)
  sf <- sf[sf$term %in% term_order, , drop = FALSE]
  if (!nrow(sf)) return(NULL)
  data.table(
    term  = sf$term,
    col   = col_label,
    mean  = sf$mean,
    lower = sf[["0.025quant"]],
    upper = sf[["0.975quant"]]
  )
}

# Short, single-line column headers for each isolated fiscal model.
fiscal_short <- c(
  debt_per_conn      = "Debt / conn.",
  fund_bal_per_conn  = "Fund bal. / conn.",
  revenue_per_conn   = "Revenue / conn.",
  operating_ratio    = "Operating ratio",
  debt_svc_tax       = "Debt-service tax"
)
COL_M1    <- "Model 1 (full sample)"      # referenced again when building the plot
COL_JOINT <- "All fiscal (joint)"

# Assemble the columns in a fixed left-to-right order: Model 1, then one column
# per isolated fiscal model (in fiscal_terms order), then the joint fiscal model.
pieces <- list(); col_levels <- character(0)
add_col <- function(fit, label) {
  p <- tidy_all(fit, label)
  if (is.null(p)) return(invisible())
  pieces[[length(pieces) + 1L]] <<- p
  col_levels <<- c(col_levels, label)
}

add_col(model1_inla, COL_M1)                       # full sample, shared controls only
if (!is.null(model2_inla_by_fiscal))               # one isolated fiscal model per covariate
  for (v in fiscal_terms) add_col(model2_inla_by_fiscal[[v]], unname(fiscal_short[v]))
add_col(model2_inla_all_fiscal, COL_JOINT)         # joint model: shared controls + all fiscal

all_est <- rbindlist(pieces, use.names = TRUE)
if (!nrow(all_est)) stop("No matching model terms found in the fitted objects.")
all_est[, term := factor(term, levels = term_order)]
all_est[, col  := factor(col,  levels = col_levels)]

fmt <- function(x) formatC(x, format = "f", digits = 3)

# =============================================================================
# 1. HTML table -- wide: rows = terms, one column per model fit
# =============================================================================
# Each cell is the posterior mean with the 95% CrI beneath it (a <br> line
# break, so the table renders with escape = FALSE). Terms a model did not
# include stay blank.
all_est[, cell := paste0(fmt(mean), "<br>[", fmt(lower), ", ", fmt(upper), "]")]
wide <- dcast(all_est, term ~ col, value.var = "cell", drop = c(TRUE, FALSE))
wide <- wide[order(term)]

present_terms <- as.character(wide$term)
disp <- as.data.frame(wide)
disp$term <- unname(term_labels[present_terms])    # pretty row labels
names(disp)[1] <- "Term"
disp[is.na(disp)] <- ""                            # unmodelled terms -> blank cell

# Contiguous control/fiscal blocks (term_order lists controls first) drive
# pack_rows(); rle() gives the run lengths in display order.
blocks     <- ifelse(present_terms %in% fiscal_terms, "Fiscal", "Drought & controls")
block_runs <- rle(blocks)
group_index <- setNames(block_runs$lengths, block_runs$values)

html_path <- output("model_estimates.html")
caption   <- paste0("Bayesian recurring-events Cox model: posterior mean of the ",
                    "coefficient (log hazard ratio) with 95% credible interval, ",
                    "one column per model fit")
align <- c("l", rep("c", ncol(disp) - 1L))

if (requireNamespace("kableExtra", quietly = TRUE)) {
  html_tbl <- knitr::kable(disp, format = "html", align = align, escape = FALSE,
                           caption = caption)
  html_tbl <- kableExtra::kable_styling(
    html_tbl, bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, position = "left")
  html_tbl <- kableExtra::pack_rows(html_tbl, index = group_index)
  html_tbl <- kableExtra::footnote(html_tbl, general_title = "",
    general = "Each cell: posterior mean (top) and 95% credible interval (bottom), on the coefficient / log-hazard-ratio scale. Blank = term not included in that model.")
  kableExtra::save_kable(html_tbl, file = html_path)
} else {
  # kableExtra not installed -> plain but valid standalone HTML from knitr::kable.
  body <- knitr::kable(disp, format = "html", align = align, escape = FALSE,
                       caption = caption)
  writeLines(c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'>",
    "<style>body{font-family:sans-serif;margin:2em}",
    "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
    "th{background:#f2f2f2;text-align:center}td{text-align:center}",
    "th:first-child,td:first-child{text-align:left}",
    "</style></head><body>", as.character(body),
    "<p style='color:#555;font-size:90%'>Each cell: posterior mean (top) and 95% credible interval (bottom), on the coefficient / log-hazard-ratio scale. Blank = term not included in that model.</p>",
    "</body></html>"
  ), html_path)
}
message("Wrote HTML table -> ", html_path)

# Tidy long form behind the wide table (every term x every model fit).
all_est[, label := unname(term_labels[as.character(term)])]
fwrite(all_est[order(col, term), .(term, label, model = col, mean, lower, upper)],
       output("model_estimates.csv"))
message("Wrote tidy estimates -> ", output("model_estimates.csv"))

# =============================================================================
# 2. Companion ggplot -- 95% credible intervals as line segments
# =============================================================================
# The wide table shows the shared controls in every column; the plot would be
# unreadable with each control repeated across all models, so it keeps the
# parsimonious view: each shared control once (from Model 1) and each fiscal
# term from its isolated model and the joint model. Segment = 95% CrI, point =
# posterior mean, on the hazard-ratio scale (log x-axis) with a reference line
# at HR = 1. A fiscal term appears twice (isolated vs joint), so dodge by group.
shared   <- setdiff(term_order, fiscal_terms)
est      <- all_est[(as.character(term) %in% shared & col == COL_M1) |
                    (as.character(term) %in% fiscal_terms)]
est[, group := fifelse(as.character(term) %in% shared, "Drought & controls",
              fifelse(col == COL_JOINT, "Fiscal (joint)", "Fiscal"))]
est[, group := factor(group, levels = c("Drought & controls", "Fiscal", "Fiscal (joint)"))]
est[, label := factor(term_labels[as.character(term)], levels = rev(term_labels[term_order]))]
est[, `:=`(hr = exp(mean), hr_lower = exp(lower), hr_upper = exp(upper))]

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

# =============================================================================
# 3. Appendix -- Model 1 with the demographic composition controls
# -----------------------------------------------------------------------------
# % rural / % Hispanic / % Black are held out of the prime-time Model 1 (above)
# and reported ONLY here: the same full-sample fit with those three extra fixed
# effects. Emits a standalone estimate table, tidy CSV, and forest plot mirroring
# the main ones, with the demographic terms highlighted.
# =============================================================================
demo_terms <- c("perc_rural", "perc_hispanic", "perc_black")
if (!is.null(model1_appendix_inla)) {
  app <- tidy_all(model1_appendix_inla, "Model 1 (appendix)")
  app[, term := factor(term, levels = term_order)]
  app <- app[order(term)]
  app[, label := unname(term_labels[as.character(term)])]

  app_caption <- paste0("Appendix — Model 1 with demographic composition controls ",
    "(% rural, % Hispanic, % Black), held out of the prime-time model. Posterior ",
    "mean of the coefficient (log hazard ratio) with 95% credible interval.")
  app_disp <- data.frame(
    Term     = app$label,
    Estimate = paste0(fmt(app$mean), "<br>[", fmt(app$lower), ", ", fmt(app$upper), "]"),
    check.names = FALSE)
  app_html <- output("model_estimates_appendix.html")
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    t <- knitr::kable(app_disp, format = "html", align = c("l", "c"),
                      escape = FALSE, caption = app_caption)
    t <- kableExtra::kable_styling(t,
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE, position = "left")
    kableExtra::save_kable(t, file = app_html)
  } else {
    body <- knitr::kable(app_disp, format = "html", align = c("l", "c"),
                         escape = FALSE, caption = app_caption)
    writeLines(c(
      "<!DOCTYPE html><html><head><meta charset='utf-8'>",
      "<style>body{font-family:sans-serif;margin:2em}",
      "table{border-collapse:collapse}th,td{padding:4px 10px;border:1px solid #ccc}",
      "th{background:#f2f2f2;text-align:center}td{text-align:center}",
      "th:first-child,td:first-child{text-align:left}",
      "</style></head><body>", as.character(body), "</body></html>"), app_html)
  }
  message("Wrote appendix HTML table -> ", app_html)

  fwrite(app[, .(term, label, model = col, mean, lower, upper)],
         output("model_estimates_appendix.csv"))
  message("Wrote appendix tidy estimates -> ", output("model_estimates_appendix.csv"))

  # Forest plot: demographic (appendix) terms distinguished from the shared controls.
  app[, is_demo := as.character(term) %in% demo_terms]
  app[, label := factor(term_labels[as.character(term)],
                        levels = rev(term_labels[term_order]))]
  app[, `:=`(hr = exp(mean), hr_lower = exp(lower), hr_upper = exp(upper))]
  app_plot <- ggplot(app, aes(y = label, colour = is_demo)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_segment(aes(x = hr_lower, xend = hr_upper, yend = label), linewidth = 0.7) +
    geom_point(aes(x = hr), size = 2) +
    scale_x_continuous(trans = "log10", name = "Hazard ratio (95% credible interval)") +
    scale_colour_manual(values = c(`FALSE` = "grey30", `TRUE` = "#d62728"),
      labels = c(`FALSE` = "Prime-time controls", `TRUE` = "Demographic (appendix)"),
      name = NULL) +
    labs(y = NULL, title = "Appendix — Model 1 with demographic controls",
         subtitle = "Posterior mean (point) and 95% credible interval (segment)") +
    theme_bw() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  app_plot_path <- output("model_credible_intervals_appendix.png")
  ggsave(app_plot_path, app_plot, width = 8,
         height = 1 + 0.35 * nrow(app), units = "in", dpi = 400)
  message("Wrote appendix credible-interval plot -> ", app_plot_path)
} else {
  message("Appendix Model 1 fit not found; skipping appendix outputs.")
}

message("Done. Model reporting outputs in ", OUTPUT_DIR, "/")
