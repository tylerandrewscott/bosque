# =============================================================================
# 03_model/scratch_models/05_fit_recurrent_cox.R
# -----------------------------------------------------------------------------
# Recurring-events Cox proportional hazards model of MANDATORY drought
# restriction adoption by Texas water systems (frequentist; `survival` package).
#
# We treat the outcome -- mandatory restriction adoption -- as a repeated event
# potentially occurring on a weekly basis from 2010 through 2025. We use a Cox
# proportional-hazards framework (Therneau & Grambsch 2013) with time-varying
# covariates including drought conditions (weekly DSCI) and financial indicators
# (annual district audits). Because some systems adopted restrictions multiple
# times within the window, this is a recurring-events (Andersen-Gill) model,
# with variance clustered by district.
#
# Two models (per analysis design):
#   MODEL 1 (full sample)      : all systems; drought + system controls; robust
#                                variance clustered by system (PWS_ID).
#   MODEL 2 (fiscal subsample) : special districts with audited finances; adds
#                                time-varying financial indicators; clustered by
#                                District_ID. Model 1's shared-covariate linear
#                                predictor is carried in as a fixed OFFSET -- the
#                                frequentist analog of "using Model 1 as a prior"
#                                (two-stage / empirical-Bayes-style estimator).
#
# The counting-process panel (0/1 event by system-week) is built by the shared
# `build_recurrent_panel.R`, so this and the Bayesian `01_*_inla.R` fit the same
# data. Run from the drought_and_debt project root:
#     source("code/03_model/scratch_models/05_fit_recurrent_cox.R")
# =============================================================================

source("code/03_model/build_recurrent_panel.R")   # -> panel_m1, panel_m2, *_vars
suppressPackageStartupMessages(library(survival))

# =============================================================================
# MODEL 1 -- full sample, recurring-events (Andersen-Gill) Cox
# -----------------------------------------------------------------------------
# Counting-process rows carry the recurrent-event structure (no strata by event
# number = Andersen-Gill); robust variance clustered by system.
# NOTE: the full weekly panel is ~3.6M rows; this fit can take several minutes.
# =============================================================================
form_m1 <- reformulate(c(shared_vars, "cluster(PWS_ID)"),
                       response = "Surv(tstart, tstop, event)")
model1 <- coxph(form_m1, data = panel_m1)
cat("\n===================== MODEL 1: full sample =====================\n")
print(summary(model1))
saveRDS(model1, scratch("recurrent_cox_model1_full.RDS"))

# =============================================================================
# MODEL 2 -- ONE model per fiscal covariate, informed by Model 1
# -----------------------------------------------------------------------------
# The fiscal indicators are likely conflated, so each is fit in its own model
# (drought + controls carried from Model 1). Each model uses that covariate's
# own non-missing rows -- so a zero-expenditure audit drops only from the
# operating_ratio model, not the others.
#
# Model 1 is carried forward as a fixed offset lp1 = X_shared %*% coef(model1),
# holding the drought + control effects at their full-sample values so each
# fiscal model estimates only that indicator's incremental effect. Covariate
# columns are plain numeric, so coef names align 1:1 with column names.
# =============================================================================
b1 <- coef(model1)[shared_vars]

fit_fiscal <- function(v) {
  d <- panel_m2[!is.na(get(v))]
  d[, lp1 := as.numeric(as.matrix(d[, shared_vars, with = FALSE]) %*% b1)]
  coxph(reformulate(c(v, "offset(lp1)", "cluster(District_ID)"),
                    response = "Surv(tstart, tstop, event)"), data = d)
}

# One model per fiscal covariate: Model 1 is carried in as the offset, so the
# initial (shared, no-fiscal) model is fit ONCE and each fiscal model just adds
# its own incremental term.
model2_by_fiscal <- setNames(lapply(fiscal_vars, fit_fiscal), fiscal_vars)
for (v in fiscal_vars) {
  cat(sprintf("\n===== MODEL 2 [%s]: fiscal (Model 1 as offset) =====\n", v))
  print(summary(model2_by_fiscal[[v]]))
}
saveRDS(model2_by_fiscal, scratch("recurrent_cox_model2_by_fiscal.RDS"))

# =============================================================================
# Proportional-hazards diagnostics (Therneau & Grambsch scaled Schoenfeld)
# =============================================================================
cat("\n===================== PH test: Model 1 =====================\n")
print(cox.zph(model1, transform = "identity"))
for (v in fiscal_vars) {
  cat(sprintf("\n===== PH test: Model 2 [%s] =====\n", v))
  print(cox.zph(model2_by_fiscal[[v]], transform = "identity"))
}

message("Done. Frequentist models -> scratch/: recurrent_cox_model1_full.RDS, ",
        "recurrent_cox_model2_by_fiscal.RDS")
