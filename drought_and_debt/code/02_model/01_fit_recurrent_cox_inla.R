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
N_HAZARD_INTERVALS <- 30     # RW1 baseline-hazard resolution (higher = finer, slower)
# When TRUE, reuse a cached full-sample Model 1 fit from scratch/ (if present)
# instead of refitting it. Model 1 depends only on panel_m1 (drought + controls),
# so when just the fiscal subsample / denominator changed, its posterior -- carried
# into Model 2 as priors -- is unchanged and the ~heavy full-sample fit is skippable.
# The cached fit carries a fingerprint of the panel it was fit on; if the current
# panel doesn't match (rebuilt inputs, changed window/spec), Model 1 is REFIT
# despite this flag rather than silently seeding Model 2 with stale priors.
if (!exists("REUSE_MODEL1_FIT")) REUSE_MODEL1_FIT <- FALSE

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

# (A county-level iid frailty used to sit on top of the district/system frailty;
# it was REMOVED from the specification -- the drought covariate (DSCI) already
# carries the spatial variation, so county membership shouldn't matter.)

resp <- "inla.surv(time = tstop, event = event, truncation = tstart)"
f_m1     <- 'f(cluster_idx, model = "iid", hyper = frailty_hyper)'
# Model 2's frailty prior (frailty_hyper_m2, the district frailty) is set below:
# Model 1's matching frailty posterior when Model 1 was fit, else the same
# default PC prior.
f_dist   <- 'f(district_idx, model = "iid", hyper = frailty_hyper_m2)'

# --- slim_inla(): a reduced fit for reporting/sharing ------------------------
# The full inla object is huge here because the Cox likelihood expands the panel
# to millions of Poisson rows: INLA then stores per-observation objects and, with
# config = TRUE, the full posterior configurations. The reporting side
# (03_model_results_table.R) only needs the posterior *summaries*. slim_inla()
# strips the heavy components while keeping the object's class + summaries so
# summary()/print() and the estimate tables still work. Dropped, in order of
# bloat (see the "Result object" section of ?inla):
#   * $.args$data etc. -- the full (expanded) input data stored with the call.
#     (Model 2 once warm-started from Model 1, embedding an ENTIRE copy of it in
#     $.args$control.mode$result; that warm-start has since been removed -- the
#     prior transfer alone now carries Model 1 -- so there is no such copy to strip.)
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
  }
  fit
}

# =============================================================================
# MODEL 1 (Bayesian) -- full sample; district/system frailty
# -----------------------------------------------------------------------------
# The global model: shared_vars (drought + controls + the seller-restriction
# network term) with ONE iid frailty -- district-or-system (f_m1).
# Fit on the full CWS sample; its posteriors seed Model 2's priors, and
# its coefficients are REPORTED IN THE APPENDIX (the main results are the Model 2
# fiscal models). There is no held-out demographic/income variant -- those controls
# were dropped from the specification entirely.
# =============================================================================
model1_inla <- NULL
if (FULL_SAMPLE_BAYES) {
  .m1_cache <- scratch("recurrent_coxinla_model1_full.RDS")
  # Fingerprint of the panel Model 1 is fit on, stored on the saved fit (attr
  # "panel_fp") so a cached fit can be verified against the CURRENT data, not
  # just the current covariate spec. Cheap summaries only — enough to catch a
  # rebuilt input, changed window, or changed specification.
  .m1_fp <- list(n        = nrow(panel_m1),
                 events   = sum(panel_m1$event),
                 clusters = uniqueN(panel_m1$cluster_idx),
                 vars     = sort(shared_vars),
                 window   = as.character(c(start_date, end_date)))
  if (REUSE_MODEL1_FIT && file.exists(.m1_cache)) {
    message("Reusing cached full-sample Model 1 fit (REUSE_MODEL1_FIT=TRUE): ", .m1_cache)
    model1_inla <- readRDS(.m1_cache)
    .fp_cache <- attr(model1_inla, "panel_fp", exact = TRUE)
    if (is.null(.fp_cache)) {
      warning("Cached Model 1 fit carries no panel fingerprint (saved before ",
              "fingerprinting was added), so it cannot be verified against the ",
              "current panel. Reusing it anyway; refit once with ",
              "REUSE_MODEL1_FIT=FALSE to stamp it.")
    } else if (!identical(.fp_cache[names(.m1_fp)], .m1_fp)) {
      .fp_diff <- names(.m1_fp)[!mapply(identical, .m1_fp, .fp_cache[names(.m1_fp)])]
      message("Cached Model 1 fit was fit on a DIFFERENT panel (fingerprint ",
              "mismatch on: ", paste(.fp_diff, collapse = ", "),
              ") — refitting Model 1 despite REUSE_MODEL1_FIT=TRUE.")
      model1_inla <- NULL
    }
    # Spec backstop for legacy (unfingerprinted) fits: the cached fit must carry
    # the CURRENT shared covariates, or the prior transfer below would silently
    # hand any missing one a default prior. (Fingerprinted fits already proved
    # this via vars above.)
    if (!is.null(model1_inla)) {
      .m1_missing <- setdiff(shared_vars, rownames(model1_inla$summary.fixed))
      if (length(.m1_missing)) {
        stop("Cached Model 1 fit lacks covariate(s): ", paste(.m1_missing, collapse = ", "),
             " — the specification changed since it was fit. ",
             "Rerun with REUSE_MODEL1_FIT=FALSE to refit Model 1 first.")
      }
    }
  }
  if (is.null(model1_inla)) {
    form1 <- as.formula(paste(resp, "~",
      paste(c(shared_vars, f_m1), collapse = " + ")))
    message("Fitting Bayesian Model 1 (full sample) -- this is the heavy one...")
    model1_inla <- inla(
      form1, family = "coxph", data = as.list(panel_m1),
      quantiles = c(CI_PROBS[1], 0.5, CI_PROBS[2]),
      control.hazard = hazard_ctrl, control.inla = inla_ctrl,
      control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
      num.threads = parallel::detectCores(), verbose = FALSE
    )
    cat("\n============== MODEL 1 (INLA): full sample ==============\n")
    print(summary(model1_inla))
    attr(model1_inla, "panel_fp") <- .m1_fp
    saveRDS(model1_inla, .m1_cache)
    # Slim copy goes to output/ (git-tracked) so it can be shared via GitHub.
    saveRDS(slim_inla(model1_inla), output("recurrent_coxinla_model1_full_slim.RDS"))
  }
}

# =============================================================================
# MODEL 2 (Bayesian) -- fiscal effects, Model 1 as prior
# -----------------------------------------------------------------------------
# The fiscal indicators are likely conflated, so they are fit in separate
# models: revenue per connection alone, fund balance alone, and a debt model
# carrying the two pledge types (GO/tax-backed and revenue-backed) as SEPARATE
# covariates in the same fit (not summed). Each fit uses its own covariates'
# complete cases. A companion JOINT model then enters all four fiscal predictors
# together (on the rows where all are observed) for
# comparison. Model 1's posteriors are carried into Model 2 as priors on BOTH the
# shared fixed effects (each covariate's mean + precision) AND the two
# hyperparameters -- the district/system-frailty precision and the RW1
# baseline-hazard precision. Every fiscal term gets a
# weakly-informative prior (sd = 1 on the log-hazard scale). If Model 1 was not
# fit, fall back to vague/default priors throughout.
# =============================================================================
if (!is.null(model1_inla)) {
  # (a) Fixed effects: Model 1's posterior mean/precision on each shared covariate;
  #     a weakly-informative default (mean 0, sd 1) for the as-yet-unseen fiscal term.
  sf1 <- model1_inla$summary.fixed
  sf1 <- sf1[rownames(sf1) %in% shared_vars, ]
  if (!all(shared_vars %in% rownames(sf1)))
    stop("Model 1 fit does not carry shared covariate(s): ",
         paste(setdiff(shared_vars, rownames(sf1)), collapse = ", "),
         " — they would silently get a default prior. Refit Model 1 (REUSE_MODEL1_FIT=FALSE).")
  prior_mean <- as.list(setNames(sf1[["mean"]], rownames(sf1)))
  prior_prec <- as.list(setNames(1 / sf1[["sd"]]^2, rownames(sf1)))
  prior_mean$default <- 0; prior_prec$default <- 1       # weakly-informative on the fiscal term (sd = 1, log-hazard scale)
  control_fixed <- list(mean = prior_mean, prec = prior_prec)

  # (b) Hyperparameters: carry Model 1's frailty precision and RW1 baseline-hazard
  #     precision forward as priors too. INLA parameterises both precisions on an
  #     internal LOG scale ($internal.summary.hyperpar), where the posterior marginal
  #     is ~Gaussian, so a "normal" prior on that scale with Model 1's internal mean
  #     and precision (1/sd^2) is the matching posterior-as-prior transfer (i.e. a
  #     log-normal prior on the precision itself). scale.model = TRUE keeps the RW1
  #     precision comparable across the full sample and the subsample.
  int_prior <- function(pattern) {
    ish <- as.data.frame(model1_inla$internal.summary.hyperpar)
    i <- grep(pattern, rownames(ish), fixed = TRUE)
    if (length(i) != 1L)
      stop("expected exactly one internal hyperparameter matching '", pattern,
           "', found ", length(i))
    list(prior = "normal", param = c(ish[i, "mean"], 1 / ish[i, "sd"]^2))
  }
  # Model 1's district/system frailty sits on cluster_idx; Model 2's is the same
  # iid structure on district_idx, so its precision inherits Model 1's cluster-
  # frailty posterior.
  frailty_hyper_m2 <- list(prec = int_prior("cluster_idx"))
  hazard_ctrl_m2   <- modifyList(hazard_ctrl,
                                 list(hyper = list(prec = int_prior("baseline.hazard"))))
  warm_start <- NULL   # no cross-structure mode restart; the control.fixed / hyper priors carry Model 1
  message("Fitting Bayesian fiscal models with Model 1 posteriors as priors on the ",
          "shared effects and the frailty / baseline-hazard hyperparameters.")
} else {
  control_fixed      <- list(mean = 0, prec = 0.001)
  frailty_hyper_m2   <- frailty_hyper    # default PC prior on the district frailty
  hazard_ctrl_m2     <- hazard_ctrl      # default RW1 baseline-hazard prior
  warm_start <- NULL
  message("Fitting Bayesian fiscal models with default vague priors (Model 1 not fit).")
}

# =============================================================================
# MODELS 0A / 0B -- NULL (restricted) reference fits for the DIC/WAIC table
# -----------------------------------------------------------------------------
# Covariate-free versions of the two samples' models, carrying ONLY the RW1
# baseline hazard and the shared frailty. Their DIC/WAIC anchor the manuscript's
# goodness-of-fit table (Table A1) so the unrestricted models can be read
# against a no-covariate baseline.
#   * Model 0A: full CWS sample (panel_m1), district-or-system frailty, the
#     same default PC priors as Model 1 -- the restricted Model 1.
#   * Model 0B: district subsample (panel_m2), district frailty, the same
#     Model-1-derived hyperpriors as the fiscal fits -- so its WAIC differs
#     from theirs only through the covariates. NOTE: each fiscal fit runs on
#     its own covariates' complete cases; 0B runs on the full district panel,
#     so the comparison is exact only for near-complete fiscal coverage.
# =============================================================================
model0A_inla <- NULL
if (FULL_SAMPLE_BAYES) {
  .m0_cache <- scratch("recurrent_coxinla_model0_nulls.RDS")
  # Same panel fingerprint as Model 1, minus the covariate spec (a null model
  # has none), so a cached 0A survives spec-only changes but not data changes.
  .m0_fp <- .m1_fp[setdiff(names(.m1_fp), "vars")]
  if (REUSE_MODEL1_FIT && file.exists(.m0_cache)) {
    .m0_old <- readRDS(.m0_cache)$global
    if (!is.null(.m0_old) &&
        identical(attr(.m0_old, "panel_fp", exact = TRUE), .m0_fp)) {
      message("Reusing cached null Model 0A fit (REUSE_MODEL1_FIT=TRUE): ", .m0_cache)
      model0A_inla <- .m0_old
    } else {
      message("Cached null Model 0A missing or fit on a different panel -- refitting.")
    }
  }
  if (is.null(model0A_inla)) {
    message("Fitting Bayesian Model 0A (null: baseline hazard + frailty, full sample)...")
    model0A_inla <- inla(
      as.formula(paste(resp, "~", f_m1)),
      family = "coxph", data = as.list(panel_m1),
      quantiles = c(CI_PROBS[1], 0.5, CI_PROBS[2]),
      control.hazard = hazard_ctrl, control.inla = inla_ctrl,
      control.compute = list(dic = TRUE, waic = TRUE),
      num.threads = parallel::detectCores(), verbose = FALSE
    )
    cat("\n============== MODEL 0A (INLA): null, full sample ==============\n")
    print(summary(model0A_inla))
    attr(model0A_inla, "panel_fp") <- .m0_fp
  }
}

message("Fitting Bayesian Model 0B (null: baseline hazard + frailty, district subsample)...")
model0B_inla <- inla(
  as.formula(paste(resp, "~", f_dist)),
  family = "coxph", data = as.list(panel_m2),
  quantiles = c(CI_PROBS[1], 0.5, CI_PROBS[2]),
  control.hazard = hazard_ctrl_m2, control.inla = inla_ctrl,
  control.compute = list(dic = TRUE, waic = TRUE),
  num.threads = parallel::detectCores(), verbose = FALSE
)
cat("\n============== MODEL 0B (INLA): null, district subsample ==============\n")
print(summary(model0B_inla))

model0_nulls <- list(global = model0A_inla, district = model0B_inla)
saveRDS(model0_nulls, scratch("recurrent_coxinla_model0_nulls.RDS"))
saveRDS(lapply(model0_nulls, slim_inla),
        output("recurrent_coxinla_model0_nulls_slim.RDS"))

# Fit one Model 2 given a vector of fiscal covariates: a single covariate for
# the isolated models, the GO + REV pair for the debt model, or all of
# `fiscal_vars` for the joint model. The subsample is the rows where EVERY
# requested covariate is observed (complete cases), so multi-covariate models
# run on the intersection of coverage.
fit_fiscal_inla <- function(vs) {
  d <- panel_m2[complete.cases(panel_m2[, ..vs])]
  d[, district_idx := .GRP, by = District_ID]           # reindex within subsample
  form <- as.formula(paste(resp, "~",
    paste(c(shared_vars, vs, f_dist), collapse = " + ")))
  inla(form, family = "coxph", data = as.list(d),
       quantiles = c(CI_PROBS[1], 0.5, CI_PROBS[2]),
       control.hazard = hazard_ctrl_m2, control.inla = inla_ctrl,
       control.fixed = control_fixed, control.mode = warm_start,
       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
       num.threads = parallel::detectCores(), verbose = FALSE)
}

# The presented model set (in order): revenue alone, fund balance alone, and a
# debt model with GO and REV debt as separate covariates in the same fit. (The
# GO-only and REV-only debt models were dropped from the specification.)
fiscal_specs <- list(
  revenue_per_conn  = "revenue_per_conn",
  fund_bal_per_conn = "fund_bal_per_conn",
  debt              = c("debt_go_per_conn", "debt_rev_per_conn")
)
model2_inla_by_fiscal <- setNames(vector("list", length(fiscal_specs)), names(fiscal_specs))
for (nm in names(fiscal_specs)) {
  vs <- fiscal_specs[[nm]]
  message("  fitting fiscal model: ", paste(vs, collapse = " + "))
  model2_inla_by_fiscal[[nm]] <- fit_fiscal_inla(vs)
  cat(sprintf("\n====== MODEL 2 (INLA) [%s]: Model 1 as prior ======\n",
              paste(vs, collapse = " + ")))
  print(summary(model2_inla_by_fiscal[[nm]]))
}
saveRDS(model2_inla_by_fiscal, scratch("recurrent_coxinla_model2_by_fiscal.RDS"))
saveRDS(lapply(model2_inla_by_fiscal, slim_inla),
        output("recurrent_coxinla_model2_by_fiscal_slim.RDS"))

# --- Joint model: ALL fiscal predictors together -----------------------------
# The models above deliberately isolate the fiscal indicators (debt's two pledge
# types aside). This fourth model enters every fiscal predictor at once (on the
# rows where ALL are observed) so each effect is read net of the others, showing
# how much of an isolated association survives conditioning on the rest.
message("  fitting joint fiscal model: all fiscal predictors")
model2_inla_all_fiscal <- fit_fiscal_inla(fiscal_vars)
cat("\n====== MODEL 2 (INLA) [all fiscal predictors]: Model 1 as prior ======\n")
print(summary(model2_inla_all_fiscal))
saveRDS(model2_inla_all_fiscal, scratch("recurrent_coxinla_model2_all_fiscal.RDS"))
saveRDS(slim_inla(model2_inla_all_fiscal),
        output("recurrent_coxinla_model2_all_fiscal_slim.RDS"))

message("Done. Full Bayesian models -> scratch/ (gitignored): ",
        if (FULL_SAMPLE_BAYES) "recurrent_coxinla_model1_full.RDS, " else "",
        "recurrent_coxinla_model0_nulls.RDS, ",
        "recurrent_coxinla_model2_by_fiscal.RDS, ",
        "recurrent_coxinla_model2_all_fiscal.RDS. ",
        "Reduced *_slim.RDS copies -> output/ (git-tracked).")
