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
#   bci("term","model")         coefficient with 95% credible interval
#   hr("term","model")          hazard ratio (exp of the coefficient)
#   hrci("term","model")        hazard ratio with 95% credible interval
#   results_table()             wide main results table (kable)
#   appendix_table()            global Model 1 table (kable)
#   descriptives_table()        grouped summary-statistics table (kable)
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

# --- Model-column constants (must match 03_model_results_table.R headers) -----
M_DEBT    <- "Debt / conn."
M_FUNDBAL <- "Fund bal. / conn."
M_REVENUE <- "Revenue / conn."
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
  paste0(.num(r$mean, d), " (95% CrI: ", .num(r$lower, d), ", ", .num(r$upper, d), ")")
}
hr   <- function(term, model, d = 2) .num(exp(.est_row(term, model)$mean), d)     # hazard ratio
hrci <- function(term, model, d = 2) {
  r <- .est_row(term, model)
  paste0(.num(exp(r$mean), d), " (95% CrI: ",
         .num(exp(r$lower), d), ", ", .num(exp(r$upper), d), ")")
}
# TRUE when the 95% CrI excludes 0 (coef) / 1 (HR) -- i.e. a "credibly nonzero" effect.
credible <- function(term, model) {
  r <- .est_row(term, model)
  (r$lower > 0 & r$upper > 0) | (r$lower < 0 & r$upper < 0)
}

# =============================================================================
# Tables (returned as knitr::kable; Quarto renders each per output format, so
# these stay portable to HTML *and* docx / Google Docs without gt/kableExtra).
# =============================================================================
.cell <- function(mean, lower, upper, d = 2)
  paste0(.num(mean, d), "<br>[", .num(lower, d), ", ", .num(upper, d), "]")

# Wide main table: rows = terms (in the CSV's row order), one column per fiscal
# model fit. A term absent from a model renders blank.
results_table <- function(digits = 2) {
  dt <- copy(.est)
  dt[, cell := .cell(mean, lower, upper, digits)]
  # Preserve first-seen term order (the fit script already orders controls->fiscal).
  term_lvls <- unique(dt$term)
  col_lvls  <- unique(dt$model)
  labels    <- dt[, .(label = label[1]), by = term][match(term_lvls, term), label]
  wide <- dcast(dt, term ~ model, value.var = "cell")
  wide <- wide[match(term_lvls, term)]
  wide[, term := labels]
  setnames(wide, "term", "Term")
  for (j in names(wide)) set(wide, which(is.na(wide[[j]])), j, "")
  knitr::kable(wide, format = "pipe", escape = FALSE, align = c("l", rep("c", ncol(wide) - 1L)),
               caption = "Bayesian recurring-events Cox models: posterior mean and 95% credible interval (log-hazard scale). Blank = term not in that model.")
}

# Appendix table: the global Model 1 fit (single estimate column).
appendix_table <- function(digits = 2) {
  if (is.null(.appendix)) return(knitr::kable(data.frame(Note = "Model 1 appendix estimates not found.")))
  dt <- copy(.appendix)
  dt[, Estimate := .cell(mean, lower, upper, digits)]
  out <- dt[, .(Term = label, Estimate)]
  knitr::kable(out, format = "pipe", escape = FALSE, align = c("l", "c"),
               caption = "Appendix: global Model 1 (full-sample) posterior means and 95% credible intervals (log-hazard scale).")
}

# Grouped descriptive-statistics table. A "Group" column carries the block label
# (docx-safe; no pack_rows dependency).
descriptives_table <- function() {
  if (is.null(.desc)) return(knitr::kable(data.frame(Note = "Descriptive stats not found.")))
  dt <- copy(.desc)
  num <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
  out <- dt[, .(
    Group    = group,
    Variable = label,
    Unit     = unit,
    N        = formatC(N, format = "d", big.mark = ","),
    Mean     = num(mean), SD = num(sd),
    Min      = num(min), Median = num(median), Max = num(max))]
  knitr::kable(out, format = "pipe", align = c("l", "l", "l", rep("r", 6)),
               caption = "Descriptive statistics for the recurring-events Cox model variables.")
}

message("paper/_setup.R: loaded ", nrow(.facts), " facts, ",
        nrow(.est), " main estimates",
        if (!is.null(.appendix)) paste0(", ", nrow(.appendix), " appendix estimates") else "",
        " from ", OUT)
