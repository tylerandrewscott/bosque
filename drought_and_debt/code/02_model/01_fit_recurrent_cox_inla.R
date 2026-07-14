# =============================================================================
# 02_model/01_fit_recurrent_cox_inla.R
# -----------------------------------------------------------------------------
# Bayesian recurring-events Cox model (the primary Stage-D model), fit with INLA
# on the shared counting-process panel from build_recurrent_panel.R. A
# frequentist `survival` version of the same model is kept for reference under
# scratch_models/05_fit_recurrent_cox.R.
#
# How the pieces map from the frequentist model:
#   * Cox PH               -> family = "coxph" with a piecewise (RW1) baseline
#                             hazard (the INLA Cox representation).
#   * time-varying (start,stop] rows, recurrent events (Andersen-Gill)
#                          -> each weekly interval is a LEFT-TRUNCATED survival
#                             observation: inla.surv(time = tstop, event,
#                             truncation = tstart). The counting process runs
#                             across events with no reset.
#   * robust variance clustered by district
#                          -> a SHARED FRAILTY: an iid Gaussian random effect per
#                             cluster. Model 1 clusters by water district where the
#                             system has one and by the individual system otherwise
#                             (unaffiliated utilities); Model 2 clusters by district.
#                             This is the Bayesian analog of the sandwich cluster.
#   * "Model 1 as a prior for Model 2"
#                          -> LITERAL here: Model 2's shared-covariate priors are
#                             set to Model 1's posterior mean/precision via
#                             control.fixed. Model 2 then updates the fiscal
#                             effects on the district subsample.
#
# Requires INLA (non-CRAN):
#   install.packages("INLA",
#     repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
#     dep = TRUE)
#
# NOTE: INLA's Cox likelihood expands the (already ~3.6M-row) panel into a
# Poisson representation, so the FULL-sample Bayesian fit is memory-heavy. It is
# most practical on the district subsample (Model 2) or with a coarse baseline-
# hazard grid (n.intervals). Set FULL_SAMPLE_BAYES <- FALSE to skip Model 1.
#
# Run from the drought_and_debt project root:
#     source("code/02_model/01_fit_recurrent_cox_inla.R")
# =============================================================================

# Load config first (it setwd()s to the project root) so the relative source()
# below resolves from the bosque repo root, the drought_and_debt root, or a
# code/ subdirectory. build_recurrent_panel.R then skips reloading config.
if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}
# Build the shared panel. The builder is idempotent: it skips its expensive
# rebuild when a current panel is already in this environment (run_all.R runs
# this and 04_descriptive_stats_table.R in one env so the ~minutes-long build
# happens once) and always (re)defines the covariate-name vectors. So just
# source it unconditionally rather than re-deriving its staleness rule here.
source("code/02_model/build_recurrent_panel.R") # -> panel_m1, panel_m2, *_vars

if (!requireNamespace("INLA", quietly = TRUE)) {
  stop("INLA is not installed. See the install command in this script's header.")
}
suppressPackageStartupMessages(library(INLA))

# --- knobs -------------------------------------------------------------------
FULL_SAMPLE_BAYES <- TRUE    # FALSE -> only fit the (small) fiscal-subsample model
USE_SPATIAL       <- FALSE   # TRUE  -> add a BESAG county spatial effect (needs spdep/tigris)
N_HAZARD_INTERVALS <- 30     # RW1 baseline-hazard resolution (higher = finer, slower)

# PC prior on each frailty SD: P(sigma > 1) = 0.01 (weakly informative on the
# log-hazard scale). Shared by every iid random effect below.
frailty_hyper <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
hazard_ctrl   <- list(model = "rw1", n.intervals = N_HAZARD_INTERVALS,
                      constr = TRUE, scale.model = TRUE)
# Empirical-Bayes integration keeps these large fits tractable; switch to the
# default ("auto"/"ccd") for fuller posterior uncertainty on the hyperparameters.
inla_ctrl <- list(int.strategy = "eb")

# Integer frailty indices (INLA wants 1..K grouping indices, not id strings).
# Model 1's shared frailty clusters by water district where the system has one and
# by the individual system otherwise (unaffiliated city-/investor-owned/private
# utilities carry no District_ID). Build one combined cluster key, prefixed so the
# district and PWS id namespaces can't collide, then an integer index for INLA.
panel_m1[, cluster_key := fifelse(!is.na(District_ID),
                                  paste0("D", District_ID), paste0("P", PWS_ID))]
panel_m1[, cluster_idx := .GRP, by = cluster_key]
panel_m2[, district_idx := .GRP, by = District_ID]

resp <- "inla.surv(time = tstop, event = event, truncation = tstart)"
f_m1   <- 'f(cluster_idx, model = "iid", hyper = frailty_hyper)'
f_dist <- 'f(district_idx, model = "iid", hyper = frailty_hyper)'

# --- slim_inla(): a reduced fit for reporting/sharing ------------------------
# The full inla object is huge here because the Cox likelihood expands the panel
# to millions of Poisson rows: INLA then stores per-observation objects and, with
# config = TRUE, the full posterior configurations. The reporting side
# (03_model_results_table.R) only needs the posterior *summaries*. slim_inla()
# strips the heavy components while keeping the object's class + summaries so
# summary()/print() and the estimate tables still work. Dropped, in order of
# bloat (see the "Result object" section of ?inla):
#   * $.args$data etc. -- the full (expanded) input data stored with the call.
#     For Model 2, $.args$control.mode$result also embeds an ENTIRE copy of
#     Model 1 (it was passed in as the warm-start), so drop that too.
#   * $misc$configs           -- config=TRUE posterior configs (Q, means per theta)
#   * $marginals/summary .linear.predictor & .fitted.values -- one entry per row
#   * $model.matrix, $graph   -- the expanded design matrix / neighbour graph
# Kept: summary.fixed/.hyperpar/.random, marginals.fixed/.hyperpar, dic, waic,
# mlik, cpo, and the light $.args metadata that summary() prints.
slim_inla <- function(fit) {
  if (is.null(fit)) return(NULL)
  fit$summary.linear.predictor   <- NULL
  fit$summary.fitted.values      <- NULL
  fit$marginals.linear.predictor <- NULL
  fit$marginals.fitted.values    <- NULL
  fit$misc$configs               <- NULL
  fit$model.matrix               <- NULL
  fit$graph                      <- NULL
  # Per-observation vectors (one entry per expanded panel row — the real bulk):
  # residuals, pointwise po/cpo, the latent-field mode, per-row link/family
  # tags, and the LOCAL dic/waic contributions. The scalar DIC/WAIC/p.eff that
  # summary() prints are kept.
  fit$residuals               <- NULL
  fit$po                      <- NULL
  fit$cpo                     <- NULL
  fit$offset.linear.predictor <- NULL
  fit$mode$x                  <- NULL
  fit$misc$linkfunctions      <- NULL
  fit$misc$family             <- NULL
  # $all.hyper's prior-spec functions close over an environment that holds the
  # entire expanded Cox data (~6 GB serialized for Model 1). Nothing in the
  # reporting path reads it, so drop it wholesale.
  fit$all.hyper               <- NULL
  drop_local <- function(comp) {
    if (is.null(comp)) return(NULL)
    comp[grepl("^local\\.|^family$", names(comp))] <- NULL
    comp
  }
  fit$dic  <- drop_local(fit$dic)
  fit$waic <- drop_local(fit$waic)
  if (!is.null(fit$.args)) {
    # $.args$.parent.frame is the environment inla() was called from; it closes
    # over the ENTIRE expanded Cox panel (~8.7 GB serialized for Model 1 — the
    # single biggest contributor to the "slim" file, invisible to object.size()
    # since it doesn't traverse environments). Nothing in reporting reads it.
    fit$.args[c("data", "E", "Ntrials", "weights", "offset", "scale",
                "lincomb", "y", "response", ".parent.frame")] <- NULL
    if (!is.null(fit$.args$control.mode)) fit$.args$control.mode$result <- NULL
  }
  fit
}

# =============================================================================
# MODEL 1 (Bayesian) -- full sample, system-level shared frailty
# -----------------------------------------------------------------------------
# Two variants on the same panel/sample/frailty:
#   * prime-time (model1_inla)          -- shared_vars, the reported main model.
#   * appendix   (model1_appendix_inla) -- shared_vars_appendix, adds the
#     demographic composition controls (% rural / % Hispanic / % Black) held out
#     of the prime-time model. Reported only in the appendix.
# Model 2's prior transfer reads the PRIME-TIME fit, so its shared-covariate
# priors never include the demographic terms.
# =============================================================================
model1_inla          <- NULL
model1_appendix_inla <- NULL
if (FULL_SAMPLE_BAYES) {
  form1 <- as.formula(paste(resp, "~", paste(c(shared_vars, f_m1), collapse = " + ")))
  message("Fitting Bayesian Model 1 (full sample) -- this is the heavy one...")
  model1_inla <- inla(
    form1, family = "coxph", data = as.list(panel_m1),
    control.hazard = hazard_ctrl, control.inla = inla_ctrl,
    control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
    num.threads = parallel::detectCores(), verbose = FALSE
  )
  cat("\n============== MODEL 1 (INLA): full sample ==============\n")
  print(summary(model1_inla))
  saveRDS(model1_inla, scratch("recurrent_coxinla_model1_full.RDS"))
  # Slim copy goes to output/ (git-tracked) so it can be shared via GitHub.
  saveRDS(slim_inla(model1_inla), output("recurrent_coxinla_model1_full_slim.RDS"))

  # --- Appendix variant: prime-time spec PLUS demographic composition controls -
  form1_app <- as.formula(paste(resp, "~",
    paste(c(shared_vars_appendix, f_m1), collapse = " + ")))
  message("Fitting Bayesian Model 1 (appendix, + demographic controls)...")
  model1_appendix_inla <- inla(
    form1_app, family = "coxph", data = as.list(panel_m1),
    control.hazard = hazard_ctrl, control.inla = inla_ctrl,
    control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
    num.threads = parallel::detectCores(), verbose = FALSE
  )
  cat("\n============== MODEL 1 (INLA): appendix (+ demographics) ==============\n")
  print(summary(model1_appendix_inla))
  saveRDS(model1_appendix_inla, scratch("recurrent_coxinla_model1_appendix.RDS"))
  saveRDS(slim_inla(model1_appendix_inla),
          output("recurrent_coxinla_model1_appendix_slim.RDS"))
}

# =============================================================================
# Optional BESAG county spatial effect for Model 2
# =============================================================================
spatial_term <- character(0)
if (USE_SPATIAL) {
  if (!all(vapply(c("spdep", "tigris"), requireNamespace, logical(1), quietly = TRUE))) {
    warning("USE_SPATIAL = TRUE but spdep/tigris unavailable; skipping spatial term.")
  } else {
    co <- as.data.table(readRDS(committed("pws_county_overlaps.RDS")))
    co <- co[order(-Prop_Over_County)][!duplicated(PWS_ID), .(PWS_ID, CFIPS)]  # primary county
    tx_county_sp <- tx_county_adjacency(crs = albersNA, adj_file = "tx.adj")   # writes tx.adj
    co[, county_idx := match(CFIPS, tx_county_sp$GEOID)]
    panel_m2 <- merge(panel_m2, co[, .(PWS_ID, county_idx)], by = "PWS_ID", all.x = TRUE)
    spatial_term <- 'f(county_idx, model = "besag", graph = "tx.adj", hyper = frailty_hyper)'
  }
}

# =============================================================================
# MODEL 2 (Bayesian) -- fiscal effects, Model 1 as prior
# -----------------------------------------------------------------------------
# The fiscal indicators are likely conflated, so each is fit in its own model.
# Each uses that covariate's own non-missing rows (a zero-expenditure audit is
# missing only from the operating_ratio model). A companion JOINT model then
# enters all fiscal predictors together (on the rows where all are observed) for
# comparison. The shared covariates' priors are set to Model 1's posteriors
# (mean + precision); every fiscal term gets a weakly-informative prior (sd = 1 on
# the log-hazard scale). If Model 1 was not fit, fall back to vague priors throughout.
# =============================================================================
if (!is.null(model1_inla)) {
  sf1 <- model1_inla$summary.fixed
  sf1 <- sf1[rownames(sf1) %in% shared_vars, ]
  prior_mean <- as.list(setNames(sf1[["mean"]], rownames(sf1)))
  prior_prec <- as.list(setNames(1 / sf1[["sd"]]^2, rownames(sf1)))
  prior_mean$default <- 0; prior_prec$default <- 1       # weakly-informative on the fiscal term (sd = 1, log-hazard scale)
  control_fixed <- list(mean = prior_mean, prec = prior_prec)
  warm_start <- NULL   # no cross-structure mode restart; the control.fixed prior transfer carries Model 1
  message("Fitting Bayesian fiscal models with Model 1 posteriors as priors on shared effects.")
} else {
  control_fixed <- list(mean = 0, prec = 0.001)
  warm_start <- NULL
  message("Fitting Bayesian fiscal models with default vague priors (Model 1 not fit).")
}

# Fit one Model 2 given a vector of fiscal covariates. With a single element this
# is the isolated per-covariate model; with all of `fiscal_vars` it is the joint
# model. The subsample is the rows where EVERY requested covariate is observed
# (complete cases), so the joint model runs on the intersection of coverage.
fit_fiscal_inla <- function(vs) {
  d <- panel_m2[complete.cases(panel_m2[, ..vs])]
  d[, district_idx := .GRP, by = District_ID]           # reindex within subsample
  form <- as.formula(paste(resp, "~",
    paste(c(shared_vars, vs, f_dist, spatial_term), collapse = " + ")))
  inla(form, family = "coxph", data = as.list(d),
       control.hazard = hazard_ctrl, control.inla = inla_ctrl,
       control.fixed = control_fixed, control.mode = warm_start,
       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
       num.threads = parallel::detectCores(), verbose = FALSE)
}

model2_inla_by_fiscal <- setNames(vector("list", length(fiscal_vars)), fiscal_vars)
for (v in fiscal_vars) {
  message("  fitting fiscal model: ", v)
  model2_inla_by_fiscal[[v]] <- fit_fiscal_inla(v)
  cat(sprintf("\n====== MODEL 2 (INLA) [%s]: Model 1 as prior ======\n", v))
  print(summary(model2_inla_by_fiscal[[v]]))
}
saveRDS(model2_inla_by_fiscal, scratch("recurrent_coxinla_model2_by_fiscal.RDS"))
saveRDS(lapply(model2_inla_by_fiscal, slim_inla),
        output("recurrent_coxinla_model2_by_fiscal_slim.RDS"))

# --- Joint model: ALL fiscal predictors together -----------------------------
# The per-covariate models above deliberately isolate each fiscal indicator. This
# companion model enters every fiscal predictor at once (on the rows where ALL are
# observed) so each effect is read net of the others, showing how much of a
# single-covariate association survives conditioning on the rest.
message("  fitting joint fiscal model: all fiscal predictors")
model2_inla_all_fiscal <- fit_fiscal_inla(fiscal_vars)
cat("\n====== MODEL 2 (INLA) [all fiscal predictors]: Model 1 as prior ======\n")
print(summary(model2_inla_all_fiscal))
saveRDS(model2_inla_all_fiscal, scratch("recurrent_coxinla_model2_all_fiscal.RDS"))
saveRDS(slim_inla(model2_inla_all_fiscal),
        output("recurrent_coxinla_model2_all_fiscal_slim.RDS"))

message("Done. Full Bayesian models -> scratch/ (gitignored): ",
        if (FULL_SAMPLE_BAYES) "recurrent_coxinla_model1_full.RDS, recurrent_coxinla_model1_appendix.RDS, " else "",
        "recurrent_coxinla_model2_by_fiscal.RDS, ",
        "recurrent_coxinla_model2_all_fiscal.RDS. ",
        "Reduced *_slim.RDS copies -> output/ (git-tracked).")
