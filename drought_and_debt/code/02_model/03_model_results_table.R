# =============================================================================
# 02_model/03_model_results_table.R
# -----------------------------------------------------------------------------
# Reporting for the Bayesian recurring-events Cox models fit in
# 01_fit_recurrent_cox_inla.R. Reads the saved INLA objects and produces:
#
#   output/model_estimates.html   WIDE table: one column per fiscal model fit
#                                 (revenue; fund balance; debt with GO and REV
#                                 as separate covariates in one model; and the
#                                 joint model with all four). Each cell is the
#                                 posterior mean of the coefficient (log hazard
#                                 ratio) with its credible interval below;
#                                 cells whose CrI excludes zero are bold.
#                                 Shared controls appear in every column; a
#                                 fiscal row is blank in any model that omits it.
#   output/model_credible_intervals.png
#                                 companion ggplot forest plot: each estimate is
#                                 a line SEGMENT spanning its CrI with the
#                                 posterior mean marked as a point.
#   output/model_estimates.csv    the tidy estimate table behind both.
#
# The interval level is CI_LEVEL from config.R (default 0.95). The bounds come
# from each fit's summary quantile columns when they match, else are recomputed
# from the saved posterior marginals -- so no refitting is needed here.
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
model2_inla_by_fiscal  <- load_fit("recurrent_coxinla_model2_by_fiscal")
model2_inla_all_fiscal <- load_fit("recurrent_coxinla_model2_all_fiscal")
# Sensitivity fits (fiscal models refit dropping districts with <100 SDWIS
# service connections); optional -- the appendix report below is skipped if the
# fit script has not produced them yet.
model2_min100_by_fiscal  <- load_fit("recurrent_coxinla_model2_by_fiscal_min100")
model2_min100_all_fiscal <- load_fit("recurrent_coxinla_model2_all_fiscal_min100")

# Generation check: Model 2 consumes Model 1's posteriors as priors, so a
# Model 1 fit file NEWER than the Model 2 fits means the table would mix fit
# generations. Warn loudly and say what to run.
.newest_mtime <- function(stem) {
  f <- c(scratch(paste0(stem, ".RDS")), output(paste0(stem, "_slim.RDS")))
  f <- f[file.exists(f)]
  if (!length(f)) NA else max(file.mtime(f))
}
.m1_t <- .newest_mtime("recurrent_coxinla_model1_full")
.m2_t <- min(.newest_mtime("recurrent_coxinla_model2_by_fiscal"),
             .newest_mtime("recurrent_coxinla_model2_all_fiscal"))
if (!is.na(.m1_t) && !is.na(.m2_t) && .m1_t > .m2_t + 60)
  warning("Model 1 fit is NEWER than the Model 2 fits: the Model 2 columns were fit ",
          "against an older Model 1 posterior. Rerun 01_fit_recurrent_cox_inla.R ",
          "(all models in one pass) before publishing this table.")

# Data-staleness check: any committed input/ file newer than the OLDEST fit
# means the panel the fits were built from may no longer reflect the current
# data. (mtimes are heuristic — a fresh checkout re-stamps every input — but a
# false positive just says "refit to be sure", which is the safe direction.)
.fit_t <- suppressWarnings(min(.m1_t, .m2_t, na.rm = TRUE))
if (is.finite(.fit_t)) {
  .inputs <- list.files(COMMITTED_DIR, full.names = TRUE)
  .newer  <- basename(.inputs[file.mtime(.inputs) > .fit_t])
  if (length(.newer))
    warning("Committed input(s) are NEWER than the fitted models — the table may ",
            "be stale w.r.t. the data: ", paste(.newer, collapse = ", "),
            ". Rerun 01_fit_recurrent_cox_inla.R before publishing this table.")
}

if (is.null(model1_inla) && is.null(model2_inla_by_fiscal) &&
    is.null(model2_inla_all_fiscal)) {
  stop("No fitted models found (scratch/ or output/*_slim.RDS). ",
       "Run code/02_model/01_fit_recurrent_cox_inla.R first.")
}

# --- Pretty labels for the model terms ---------------------------------------
term_labels <- c(
  DSCI               = "Drought severity (DSCI)",
  seller_restricted  = "Seller under restriction",
  ln_connections     = "Log connections",
  storage_per_conn_g = "Storage per connection (asinh gal)",
  source_surface     = "Surface water (vs ground)",
  purchases_water    = "Purchases water (primary)",
  emergency_source   = "Emergency source/interconnect",
  ln_home_value      = "Log median home value",
  median_structure_age = "Median structure age (yrs)",
  perc_rural         = "% Rural (urban/rural gradient)",
  revenue_per_conn   = "Revenue per connection (asinh)",
  fund_bal_per_conn  = "Fund balance per connection (asinh)",
  debt_go_per_conn   = "GO (tax) debt per connection (asinh)",
  debt_rev_per_conn  = "Revenue debt per connection (asinh)",
  sd_frailty         = "Frailty SD (district/system)",
  sd_baseline        = "Baseline-hazard SD (RW1)"
)
# Top-to-bottom ordering in the plot / table (drought & controls, then fiscal
# in model-column order, then the random-effect hyperparameters).
term_order  <- names(term_labels)
fiscal_terms <- c("revenue_per_conn", "fund_bal_per_conn",
                  "debt_go_per_conn", "debt_rev_per_conn")
hyper_terms  <- c("sd_frailty", "sd_baseline")

# --- Credible-interval bounds at the configured CI_LEVEL (config.R) ----------
# The saved summaries only carry quantile columns at the levels requested at
# FIT time (e.g. `0.025quant`). When CI_LEVEL has changed since the fit, the
# bounds are recomputed from the saved posterior marginals (the slim fits keep
# $marginals.fixed / $marginals.hyperpar), so no refit is needed.
qcol <- paste0(CI_PROBS, "quant")            # e.g. "0.025quant" "0.975quant"
marg_q <- function(marglist, terms) {
  missing_m <- setdiff(terms, names(marglist))
  if (length(missing_m))
    stop("Fit carries no ", qcol[1], "/", qcol[2], " summary columns and no ",
         "stored marginals for: ", paste(missing_m, collapse = ", "),
         " -- refit (01 passes the configured quantiles to inla()) or set ",
         "CI_LEVEL back to the fit's level.")
  if (!requireNamespace("INLA", quietly = TRUE))
    stop("Recomputing the ", CI_LABEL, " CrI from the saved marginals needs ",
         "the INLA package (or refit with this CI_LEVEL).")
  vapply(marglist[terms], function(m) INLA::inla.qmarginal(CI_PROBS, m),
         numeric(2))
}

# --- Pull the posterior summaries into one tidy table ------------------------
# INLA's summary.fixed has one row per fixed effect; we keep the mean and the
# CI_PROBS quantiles (the bounds of the CI_LEVEL credible interval). Every
# column of the report is one model fit, so we now extract EVERY term each fit
# carries (the shared controls too, not just its fiscal covariate) so common
# effects can be compared across all models.
tidy_all <- function(fit, col_label) {
  if (is.null(fit)) return(NULL)
  sf <- as.data.frame(fit$summary.fixed)
  sf$term <- rownames(sf)
  sf <- sf[sf$term %in% term_order, , drop = FALSE]
  if (!nrow(sf)) return(NULL)
  if (all(qcol %in% colnames(sf))) {
    lo <- sf[[qcol[1]]]; hi <- sf[[qcol[2]]]
  } else {
    q  <- marg_q(fit$marginals.fixed, sf$term)
    lo <- q[1, ]; hi <- q[2, ]
  }
  rbind(
    data.table(
      term  = sf$term,
      col   = col_label,
      mean  = sf$mean,
      lower = lo,
      upper = hi
    ),
    tidy_hyper(fit, col_label)
  )
}

# --- Random-effect hyperparameters -------------------------------------------
# Each fit's $summary.hyperpar carries the posterior of its PRECISION
# hyperparameters: the shared frailty (cluster_idx in Model 1, district_idx in
# Model 2) and the RW1 baseline hazard. Report both as STANDARD DEVIATIONS,
# sd = prec^-1/2: a monotone transform, so the CrI bounds are the inverted
# opposite precision quantiles, and the point estimate is the posterior MEDIAN
# (the mean does not transform; the median does). These fill the `mean` column
# so the same cell/CSV machinery applies -- flagged in the table footnote.
hyper_map <- c("Precision for cluster_idx"     = "sd_frailty",
               "Precision for district_idx"    = "sd_frailty",
               "Precision for baseline.hazard" = "sd_baseline")
tidy_hyper <- function(fit, col_label) {
  sh <- as.data.frame(fit$summary.hyperpar)
  sh <- sh[rownames(sh) %in% names(hyper_map), , drop = FALSE]
  if (!nrow(sh)) return(NULL)
  if (all(qcol %in% colnames(sh))) {
    p_lo <- sh[[qcol[1]]]; p_hi <- sh[[qcol[2]]]
  } else {
    q    <- marg_q(fit$marginals.hyperpar, rownames(sh))
    p_lo <- q[1, ]; p_hi <- q[2, ]
  }
  data.table(
    term  = unname(hyper_map[rownames(sh)]),
    col   = col_label,
    mean  = 1 / sqrt(sh[["0.5quant"]]),
    lower = 1 / sqrt(p_hi),
    upper = 1 / sqrt(p_lo)
  )
}

# Short, single-line column headers, one per fiscal model fit (keyed by the
# fit-list names from 01_fit_recurrent_cox_inla.R's fiscal_specs). The debt
# model carries GO and REV debt as separate covariates (two rows, one column).
fiscal_cols <- c(
  revenue_per_conn  = "Revenue / conn.",
  fund_bal_per_conn = "Fund bal. / conn.",
  debt              = "Debt (GO & Rev.)"
)
COL_JOINT <- "All fiscal (joint)"

fmt <- function(x) formatC(x, format = "f", digits = 3)

# Assemble the fiscal-model columns left-to-right for one fit SET: revenue, fund
# balance, the debt model (GO and REV as separate covariates), then the joint
# fiscal model. Used for BOTH the main district-subsample fits and the
# >=100-connection sensitivity fits, so the two reports share one code path.
# Model 1 (the global fit) is NOT a column here -- it is reported separately in
# the appendix below.
assemble_est <- function(by_fiscal, all_fiscal) {
  pieces <- list(); col_levels <- character(0)
  add_col <- function(fit, label) {
    p <- tidy_all(fit, label)
    if (is.null(p)) return(invisible())
    pieces[[length(pieces) + 1L]] <<- p
    col_levels <<- c(col_levels, label)
  }
  if (!is.null(by_fiscal))
    for (nm in names(fiscal_cols)) add_col(by_fiscal[[nm]], unname(fiscal_cols[nm]))
  add_col(all_fiscal, COL_JOINT)                   # joint model: shared controls + all fiscal
  est <- rbindlist(pieces, use.names = TRUE)
  if (!nrow(est)) stop("No matching model terms found in the fitted objects.")
  est[, term := factor(term, levels = term_order)]
  est[, col  := factor(col,  levels = unique(col_levels))]
  est[]
}

# Write the wide HTML table + tidy CSV + companion forest plot for one fit set.
# Parameterised by output paths / caption / plot title so the main and
# sensitivity reports differ only in their file names and wording.
emit_fiscal_report <- function(all_est, html_path, csv_path, plot_path,
                               caption, plot_title) {
  all_est <- copy(all_est)

  # --- wide HTML table: rows = terms, one column per model fit ---------------
  # Each cell is the posterior mean with the CrI beneath it (a <br> line break,
  # so it renders with escape = FALSE). Terms a model did not include stay blank.
  # Cells whose CrI excludes zero are BOLD; the hyperparameter rows (SDs,
  # necessarily positive) are exempt.
  all_est[, sig := !(as.character(term) %in% hyper_terms) &
                   ((lower > 0 & upper > 0) | (lower < 0 & upper < 0))]
  all_est[, cell := paste0(fmt(mean), "<br>[", fmt(lower), ", ", fmt(upper), "]")]
  all_est[sig == TRUE, cell := paste0("<b>", cell, "</b>")]
  wide <- dcast(all_est, term ~ col, value.var = "cell", drop = c(TRUE, FALSE))
  wide <- wide[order(term)]

  present_terms <- as.character(wide$term)
  disp <- as.data.frame(wide)
  disp$term <- unname(term_labels[present_terms])  # pretty row labels
  names(disp)[1] <- "Term"
  disp[is.na(disp)] <- ""                          # unmodelled terms -> blank cell

  # Contiguous control/fiscal/hyperparameter blocks drive pack_rows(); rle()
  # gives the run lengths in display order.
  blocks     <- ifelse(present_terms %in% hyper_terms, "Random effects",
                ifelse(present_terms %in% fiscal_terms, "Fiscal", "Drought & controls"))
  block_runs <- rle(blocks)
  group_index <- setNames(block_runs$lengths, block_runs$values)

  align <- c("l", rep("c", ncol(disp) - 1L))
  footnote_txt <- paste0("Each cell: posterior mean (top) and ", CI_LABEL,
    " credible interval (bottom), on the coefficient / log-hazard-ratio scale. Bold = ",
    CI_LABEL, " credible interval excludes zero. Blank = term not included in that model. ",
    "Random-effect rows report the hyperparameter as a standard deviation (posterior median and ",
    CI_LABEL, " CrI).")

  if (requireNamespace("kableExtra", quietly = TRUE)) {
    html_tbl <- knitr::kable(disp, format = "html", align = align, escape = FALSE,
                             caption = caption)
    html_tbl <- kableExtra::kable_styling(
      html_tbl, bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE, position = "left")
    html_tbl <- kableExtra::pack_rows(html_tbl, index = group_index)
    html_tbl <- kableExtra::footnote(html_tbl, general_title = "",
      general = footnote_txt)
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
      paste0("<p style='color:#555;font-size:90%'>", footnote_txt, "</p>"),
      "</body></html>"
    ), html_path)
  }
  message("Wrote HTML table -> ", html_path)

  # --- tidy long form behind the wide table (every term x every model fit) ---
  all_est[, label := unname(term_labels[as.character(term)])]
  fwrite(all_est[order(col, term),
                 .(term, label, model = col, mean, lower, upper, ci = CI_LEVEL)],
         csv_path)
  message("Wrote tidy estimates -> ", csv_path)

  # --- companion forest plot: credible intervals as line segments ------------
  # Parsimonious view: each shared control once (from the joint fiscal model) and
  # each fiscal term from its own fiscal model and the joint model. Segment = CrI,
  # point = posterior mean, on the hazard-ratio scale (log x-axis) with a
  # reference line at HR = 1. A fiscal term appears twice (isolated vs joint), so
  # dodge by group. The hyperparameter rows are TABLE-ONLY (SDs, not log-hazard
  # coefficients, so exp() would be meaningless here).
  shared <- setdiff(term_order, c(fiscal_terms, hyper_terms))
  est    <- all_est[(as.character(term) %in% shared & col == COL_JOINT) |
                    (as.character(term) %in% fiscal_terms)]
  est[, group := fifelse(as.character(term) %in% shared, "Drought & controls",
                fifelse(col == COL_JOINT, "Fiscal (joint)", "Fiscal"))]
  est[, group := factor(group, levels = c("Drought & controls", "Fiscal", "Fiscal (joint)"))]
  est[, label := factor(term_labels[as.character(term)], levels = rev(term_labels[term_order]))]
  est[, `:=`(hr = exp(mean), hr_lower = exp(lower), hr_upper = exp(upper))]

  dodge <- position_dodge(width = 0.6)
  ci_plot <- ggplot(est, aes(y = label, colour = group)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_linerange(aes(xmin = hr_lower, xmax = hr_upper),
                   linewidth = 0.7, position = dodge) +
    geom_point(aes(x = hr), size = 2, position = dodge) +
    scale_x_continuous(trans = "log10",
                       name = paste0("Hazard ratio (", CI_LABEL, " credible interval)")) +
    scale_colour_tableau(name = NULL) +
    labs(y = NULL, title = plot_title,
         subtitle = paste0("Posterior mean (point) and ", CI_LABEL,
                           " credible interval (segment)")) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  ggsave(plot_path, ci_plot, width = 8,
         height = 1 + 0.35 * nrow(est), units = "in", dpi = 400)
  message("Wrote credible-interval plot -> ", plot_path)
}

# --- MAIN report: the district-subsample fiscal fits -------------------------
all_est <- assemble_est(model2_inla_by_fiscal, model2_inla_all_fiscal)
emit_fiscal_report(
  all_est,
  html_path = output("model_estimates.html"),
  csv_path  = output("model_estimates.csv"),
  plot_path = output("model_credible_intervals.png"),
  caption   = paste0("Bayesian recurring-events Cox model: posterior mean of the ",
                     "coefficient (log hazard ratio) with ", CI_LABEL,
                     " credible interval, one column per model fit"),
  plot_title = "Bayesian recurring-events Cox model")

# =============================================================================
# 3. Appendix -- Model 1 (the global full-sample fit)
# -----------------------------------------------------------------------------
# Model 1 is the global model fit on the full CWS sample (drought + controls +
# the seller-restriction network term, two frailties). It seeds Model 2's priors
# and is reported HERE, in the appendix, rather than as a main column. Emits a
# standalone estimate table, tidy CSV, and forest plot mirroring the main ones.
# =============================================================================
if (!is.null(model1_inla)) {
  app <- tidy_all(model1_inla, "Model 1 (global)")
  app[, term := factor(term, levels = term_order)]
  app <- app[order(term)]
  app[, label := unname(term_labels[as.character(term)])]

  app_caption <- paste0("Appendix — Model 1, the global full-sample fit (drought, ",
    "controls, and the seller-restriction network term) whose posteriors seed the ",
    "Model 2 priors. Posterior mean of the coefficient (log hazard ratio) with ",
    CI_LABEL, " credible interval; bold = ", CI_LABEL, " CrI excludes zero; ",
    "random-effect rows report the hyperparameter as a standard deviation ",
    "(posterior median and ", CI_LABEL, " CrI).")
  app[, sig := !(as.character(term) %in% hyper_terms) &
               ((lower > 0 & upper > 0) | (lower < 0 & upper < 0))]
  app_cell <- paste0(fmt(app$mean), "<br>[", fmt(app$lower), ", ", fmt(app$upper), "]")
  app_disp <- data.frame(
    Term     = app$label,
    Estimate = ifelse(app$sig, paste0("<b>", app_cell, "</b>"), app_cell),
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

  fwrite(app[, .(term, label, model = col, mean, lower, upper, ci = CI_LEVEL)],
         output("model_estimates_appendix.csv"))
  message("Wrote appendix tidy estimates -> ", output("model_estimates_appendix.csv"))

  # Forest plot: Model 1 covariates on the hazard-ratio scale (hyperparameter
  # rows are table-only -- they are SDs, not log-hazard coefficients).
  app <- app[!as.character(term) %in% hyper_terms]
  app[, label := factor(term_labels[as.character(term)],
                        levels = rev(term_labels[term_order]))]
  app[, `:=`(hr = exp(mean), hr_lower = exp(lower), hr_upper = exp(upper))]
  app_plot <- ggplot(app, aes(y = label)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_segment(aes(x = hr_lower, xend = hr_upper, yend = label), linewidth = 0.7,
                 colour = "grey30") +
    geom_point(aes(x = hr), size = 2, colour = "grey30") +
    scale_x_continuous(trans = "log10",
                       name = paste0("Hazard ratio (", CI_LABEL, " credible interval)")) +
    labs(y = NULL, title = "Appendix — Model 1 (global full-sample fit)",
         subtitle = paste0("Posterior mean (point) and ", CI_LABEL,
                           " credible interval (segment)")) +
    theme_bw() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  app_plot_path <- output("model_credible_intervals_appendix.png")
  ggsave(app_plot_path, app_plot, width = 8,
         height = 1 + 0.35 * nrow(app), units = "in", dpi = 400)
  message("Wrote appendix credible-interval plot -> ", app_plot_path)
} else {
  message("Model 1 fit not found; skipping appendix outputs.")
}

# =============================================================================
# 4. Appendix -- sensitivity: fiscal models dropping <100-connection districts
# -----------------------------------------------------------------------------
# The main fiscal per-connection ratios divide a dollar amount by the district's
# SDWIS service-connection count, so a handful of very small districts produce
# extreme per-connection values. 01_fit_recurrent_cox_inla.R refits every fiscal
# model on the subsample of district-weeks with >=100 connections (same priors,
# same z-scaled covariates). This block reports that fit set with the SAME wide
# table + forest plot as the main one, so the two are read side by side; if the
# fiscal effects agree, the tiny districts are not driving them. Skipped when the
# sensitivity fits are absent (fit script not yet rerun with the min100 block).
if (!is.null(model2_min100_by_fiscal) || !is.null(model2_min100_all_fiscal)) {
  sens_est <- assemble_est(model2_min100_by_fiscal, model2_min100_all_fiscal)
  emit_fiscal_report(
    sens_est,
    html_path = output("model_estimates_sensitivity_min100.html"),
    csv_path  = output("model_estimates_sensitivity_min100.csv"),
    plot_path = output("model_credible_intervals_sensitivity_min100.png"),
    caption   = paste0("Appendix (sensitivity) — fiscal models refit after ",
                       "dropping district-weeks with fewer than 100 SDWIS service ",
                       "connections, whose per-connection ratios are inflated by a ",
                       "small denominator. Same priors and z-scaled covariates as ",
                       "the main models; posterior mean (log hazard ratio) with ",
                       CI_LABEL, " credible interval, one column per fit."),
    plot_title = "Appendix — fiscal models, districts with >=100 connections")
} else {
  message("Sensitivity (min100) fits not found; skipping the sensitivity appendix outputs.")
}

message("Done. Model reporting outputs in ", OUTPUT_DIR, "/")
