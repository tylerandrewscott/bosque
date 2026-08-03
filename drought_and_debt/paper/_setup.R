# =============================================================================
# paper/_setup.R
# -----------------------------------------------------------------------------
# Render-time data layer for manuscript.qmd. Sourced once by the paper's setup
# chunk; it loads the model-output CSVs and defines a small accessor + table API
# the prose calls so EVERY number in the manuscript is pulled from output/ at
# render time (never hand-typed). Re-run the model pipeline (which rewrites those
# CSVs) then re-render, and the paper's numbers update themselves.
#
# Reads (all written by code/02_model/*):
#   output/paper_facts.csv             scalar prose facts   (key,value,fmt,label)
#   output/model_estimates.csv         main fiscal models   (term,label,model,mean,lower,upper)
#   output/model_estimates_appendix.csv global Model 1       (same columns)
#   output/descriptive_stats.csv       summary statistics   (term,group,unit,N,mean,...,label)
#
# Public API (see manuscript.qmd for usage):
#   fact("key")                 formatted scalar fact
#   b("term","model")           coefficient (log-HR) posterior mean
#   bci("term","model")         coefficient with credible interval
#   hr("term","model")          hazard ratio (exp of the coefficient)
#   hrci("term","model")        hazard ratio with credible interval
#   CI_LABEL / CI_PROB / CI_LO_Q / CI_HI_Q   CrI level strings for prose
#   results_table()             wide main results table (kable)
#   appendix_table()            global Model 1 table (kable)
#   descriptives_table()        summary stats, district (Model 2) sample (kable)
#   descriptives_table_global() summary stats, global (Model 1) sample (kable)
#   correlation_plot("global"|"fiscal")   predictor correlation heatmap (ggplot)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(knitr)
})

# --- Locate output/ ----------------------------------------------------------
# Walk up from this file / the wd until we find the drought_and_debt/output dir,
# so the paper renders whether knitr's wd is paper/, the project root, or the
# bosque repo root. Deliberately avoids sourcing config.R (which pulls the heavy
# spatial stack) -- the paper only needs the CSVs.
.find_output <- function() {
  cands <- c("output", "../output", "drought_and_debt/output",
             "../drought_and_debt/output", "../../drought_and_debt/output")
  hit <- Find(function(d) file.exists(file.path(d, "model_estimates.csv")), cands)
  if (is.null(hit))
    stop("paper/_setup.R: could not locate drought_and_debt/output/. ",
         "Run the model pipeline first (code/run_all.R).")
  normalizePath(hit)
}
OUT <- .find_output()
.opath <- function(f) file.path(OUT, f)

.read_csv <- function(f, required = TRUE) {
  p <- .opath(f)
  if (!file.exists(p)) {
    if (required)
      stop("paper/_setup.R: missing ", p, " -- run the model pipeline first.")
    return(NULL)
  }
  fread(p)
}

.facts     <- .read_csv("paper_facts.csv")
.est       <- .read_csv("model_estimates.csv")
.appendix  <- .read_csv("model_estimates_appendix.csv", required = FALSE)
.desc      <- .read_csv("descriptive_stats.csv", required = FALSE)

# --- Credible-interval level --------------------------------------------------
# The level the estimates were reported at (config.R's CI_LEVEL, written into
# model_estimates.csv by 03_model_results_table.R; CSVs predating the `ci`
# column are 95%). Drives every CrI label in the prose, captions, and tables.
.ci      <- if (!is.null(.est$ci)) .est$ci[1] else 0.95
CI_LABEL <- paste0(format(round(100 * .ci, 2)), "%")   # "95%", "89%", ...
CI_PROB  <- format(.ci)                                # "0.95"  (prose)
CI_LO_Q  <- format(round((1 - .ci) / 2, 6))            # "0.025" (prose)
CI_HI_Q  <- format(round(1 - (1 - .ci) / 2, 6))        # "0.975" (prose)

# --- Model-column constants (must match 03_model_results_table.R headers) -----
M_REVENUE <- "Revenue / conn."
M_FUNDBAL <- "Fund bal. / conn."
# ONE debt model carrying GO (tax) and revenue-backed debt as separate
# covariates; both debt terms are read from this column. Also the representative
# column for reading SHARED-control effects in prose (every fiscal model
# carries the same controls).
M_DEBT    <- "Debt (GO & Rev.)"
M_JOINT   <- "All fiscal (joint)"
M_GLOBAL  <- "Model 1 (global)"      # appendix table's single column

# =============================================================================
# Scalar facts
# =============================================================================
.fmt_fact <- function(value, fmt) {
  switch(fmt,
    int   = formatC(round(value), format = "d", big.mark = ""),
    comma = formatC(round(value), format = "d", big.mark = ","),
    pct0  = paste0(formatC(round(value * 100), format = "d"), "%"),
    pct1  = paste0(formatC(value * 100, format = "f", digits = 1), "%"),
    num1  = formatC(value, format = "f", digits = 1),
    num2  = formatC(value, format = "f", digits = 2),
    as.character(value))
}
fact <- function(key) {
  .k <- key                                        # avoid colliding with the `key` column
  r <- .facts[key == .k]
  if (!nrow(r)) return(paste0("??", key, "??"))   # visible in render if a key is missing
  .fmt_fact(r$value[1], r$fmt[1])
}

# =============================================================================
# Coefficient / hazard-ratio accessors
# =============================================================================
# Resolve one estimate row by term name (or its pretty label) and model column.
# Looks in the main estimates first, then the appendix (so global-Model-1 terms
# are reachable with model = M_GLOBAL).
.est_row <- function(term, model) {
  .t <- term; .m <- model                          # avoid colliding with the term/model columns
  pick <- function(dt) {
    if (is.null(dt)) return(NULL)
    r <- dt[(term == .t | label == .t) & model == .m]
    if (nrow(r)) r[1] else NULL
  }
  r <- pick(.est)
  if (is.null(r)) r <- pick(.appendix)
  if (is.null(r))
    stop("paper/_setup.R: no estimate for term='", term, "', model='", model, "'.")
  r
}
.num <- function(x, d) formatC(x, format = "f", digits = d)

b    <- function(term, model, d = 2) .num(.est_row(term, model)$mean, d)         # log-HR coefficient
bci  <- function(term, model, d = 2) {
  r <- .est_row(term, model)
  paste0(.num(r$mean, d), " (", CI_LABEL, " CrI: ",
         .num(r$lower, d), ", ", .num(r$upper, d), ")")
}
hr   <- function(term, model, d = 2) .num(exp(.est_row(term, model)$mean), d)     # hazard ratio
hrci <- function(term, model, d = 2) {
  r <- .est_row(term, model)
  paste0(.num(exp(r$mean), d), " (", CI_LABEL, " CrI: ",
         .num(exp(r$lower), d), ", ", .num(exp(r$upper), d), ")")
}
# TRUE when the CrI excludes 0 (coef) / 1 (HR) -- i.e. a "credibly nonzero" effect.
credible <- function(term, model) {
  r <- .est_row(term, model)
  (r$lower > 0 & r$upper > 0) | (r$lower < 0 & r$upper < 0)
}

# =============================================================================
# Tables (returned as knitr::kable; Quarto renders each per output format, so
# these stay portable to HTML *and* docx / Google Docs without gt/kableExtra).
# =============================================================================
# One estimate cell: mean over its 95% CrI. bold = TRUE (the CrI excludes zero)
# bolds each line with markdown ** so it carries through HTML and docx alike.
.cell <- function(mean, lower, upper, d = 2, bold = FALSE) {
  top <- .num(mean, d)
  bot <- paste0("[", .num(lower, d), ", ", .num(upper, d), "]")
  ifelse(bold, paste0("**", top, "**<br>**", bot, "**"),
         paste0(top, "<br>", bot))
}
# The random-effect precision terms, reported as SDs by the fit pipeline.
.HYPER_TERMS <- c("sd_frailty", "sd_baseline")

# TRUE where the 95% CrI excludes zero; never for the random-effect SD rows
# (an SD is necessarily positive, so "excludes zero" is not evidence there).
.sig <- function(term, lower, upper)
  !(term %in% .HYPER_TERMS) &
  ((lower > 0 & upper > 0) | (lower < 0 & upper < 0))

# Move the precision (random-effect SD) rows to the BOTTOM of a display table
# and set a bold spanning label row above them, so they read as a block
# distinct from the coefficients in HTML and docx alike.
.hyper_last <- function(tab, is_hyper) {
  if (!any(is_hyper)) return(tab)
  hdr <- setNames(as.list(rep("", ncol(tab))), names(tab))
  hdr[[1]] <- "**Random effects (posterior SD)**"
  rbindlist(list(tab[!is_hyper], hdr, tab[is_hyper]))
}

# Wide main table: rows = terms (in the CSV's row order), one column per fiscal
# model fit. A term absent from a model renders blank; cells whose 95% CrI
# excludes zero are bold.
results_table <- function(digits = 2) {
  dt <- copy(.est)
  dt[, cell := .cell(mean, lower, upper, digits, bold = .sig(term, lower, upper))]
  # Preserve first-seen term order (the fit script already orders controls->fiscal).
  term_lvls <- unique(dt$term)
  col_lvls  <- unique(dt$model)
  labels    <- dt[, .(label = label[1]), by = term][match(term_lvls, term), label]
  wide <- dcast(dt, term ~ model, value.var = "cell")
  # dcast orders columns alphabetically; restore the CSV's model order
  # (revenue, fund balance, debt, joint).
  setcolorder(wide, c("term", intersect(col_lvls, names(wide))))
  wide <- wide[match(term_lvls, term)]
  is_hyper <- wide$term %in% .HYPER_TERMS
  wide[, term := labels]
  setnames(wide, "term", "Term")
  for (j in names(wide)) set(wide, which(is.na(wide[[j]])), j, "")
  wide <- .hyper_last(wide, is_hyper)
  # No kable captions on any paper table: pandoc auto-numbers them on a counter
  # separate from the manuscript's manual italic "Table N:" labels, so the two
  # schemes disagree. Table notes live in the manual labels in manuscript.qmd.
  knitr::kable(wide, format = "pipe", escape = FALSE,
               align = c("l", rep("c", ncol(wide) - 1L)))
}

# Appendix table: the global Model 1 fit (single estimate column).
appendix_table <- function(digits = 2) {
  if (is.null(.appendix)) return(knitr::kable(data.frame(Note = "Model 1 appendix estimates not found.")))
  dt <- copy(.appendix)
  dt[, Estimate := .cell(mean, lower, upper, digits, bold = .sig(term, lower, upper))]
  out <- .hyper_last(dt[, .(Term = label, Estimate)], dt$term %in% .HYPER_TERMS)
  knitr::kable(out, format = "pipe", escape = FALSE, align = c("l", "c"))
}

# Grouped descriptive-statistics tables. The tidy CSV carries both samples in a
# "sample" column; the main body shows the district-linked (Model 2) sample and
# the appendix the global (Model 1) sample, so each table is single-sample and
# the column is dropped from display. A "Group" column carries the block label
# (docx-safe; no pack_rows dependency).
.desc_table <- function(which_sample) {
  if (is.null(.desc)) return(knitr::kable(data.frame(Note = "Descriptive stats not found.")))
  dt <- .desc[sample == which_sample]
  num <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
  out <- dt[, .(
    Group    = group,
    Variable = label,
    Unit     = unit,
    N        = formatC(N, format = "d", big.mark = ","),
    Mean     = num(mean), SD = num(sd),
    Min      = num(min), Median = num(median), Max = num(max))]
  knitr::kable(out, format = "pipe", align = c("l", "l", "l", rep("r", 6)))
}
descriptives_table        <- function() .desc_table("District-only (Model 2)")
descriptives_table_global <- function() .desc_table("Global (Model 1)")

# Predictor correlation heatmap (appendix): the lower-triangle CSVs written by
# 04_descriptive_stats_table.R, drawn ggcorrplot-style -- cells colored by a
# diverging red/white/blue scale, r printed in each cell, variable names on
# both axes.
correlation_plot <- function(which = c("global", "fiscal")) {
  which <- match.arg(which)
  dt <- .read_csv(paste0("correlation_", which, ".csv"), required = FALSE)
  if (is.null(dt)) {
    plot.new()
    text(0.5, 0.5, paste0("correlation_", which, ".csv not found -- run the model pipeline first."))
    return(invisible(NULL))
  }
  suppressPackageStartupMessages(library(ggplot2))
  vars <- sub("^\\(\\d+\\) ", "", dt$Variable)          # drop the "(n) " numbering
  m <- as.matrix(dt[, -1]); mode(m) <- "numeric"        # lower triangle; upper is NA
  long <- data.table(
    row = factor(rep(vars, ncol(m)), levels = rev(vars)),
    col = factor(rep(vars, each = nrow(m)), levels = vars),
    r   = as.vector(m))[!is.na(r)]
  long[, txt := ifelse(abs(r) > 0.6, "white", "grey20")]
  ggplot(long, aes(col, row, fill = r)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", r), color = txt), size = 2.6) +
    scale_color_identity() +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         limits = c(-1, 1), name = "r") +
    coord_fixed() +
    theme_minimal(base_size = 10) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          legend.position = "right")
}

message("paper/_setup.R: loaded ", nrow(.facts), " facts, ",
        nrow(.est), " main estimates",
        if (!is.null(.appendix)) paste0(", ", nrow(.appendix), " appendix estimates") else "",
        " from ", OUT)
