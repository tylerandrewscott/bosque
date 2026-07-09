# =============================================================================
# run_all.R  —  end-to-end driver for the drought_and_debt pipeline
# -----------------------------------------------------------------------------
# Run it from the bosque repo root (no setwd() needed — config.R locates the
# project root and normalizes the working directory for every downstream script):
#     cd .../bosque
#     Rscript drought_and_debt/code/run_all.R
#   or interactively (e.g. with bosque.Rproj open, wd = repo root):
#     source("drought_and_debt/code/run_all.R")
# It also still works if wd is already the drought_and_debt project root
# (Rscript code/run_all.R).
#
# Prerequisites (one-time):
#   1. bash setup_symlinks.sh                     # creates bosquebox -> Box
#   2. Install INLA + a modern spatial stack (terra, sf, spdep, tigris).
#   3. A Census API key for tidycensus (Stage C).
#
# Stages are grouped so you can run only what you need. Raw ingestion (Stage A)
# hits live APIs / web scrapes and is slow; its de-duplicated outputs are already
# committed under input/, so you can normally SKIP Stage A and start at B/C.
# Flip the flags below.
# =============================================================================

# Locate config.R whether launched from the bosque repo root, the
# drought_and_debt project root, or a code/ subdirectory. config.R then
# setwd()s to the project root, so everything below is relative to it.
.find_file <- function(f) {
  p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f))
  if (is.null(p)) f else p
}
source(.find_file("code/config.R"))   # paths, window, projection, spatial helpers; sets wd = PROJ_ROOT
CODE <- "code"                          # wd is now PROJ_ROOT, so "code" resolves for run_step()

run_assemble <- TRUE   # Stage A: re-scrape raw sources (slow; needs bosquebox + network)
run_combine  <- T   # Stage B: merges + spatial overlaps (needs bosquebox shapefiles)
run_prep     <- T    # Stage C: KBDI + analysis-panel construction
run_model    <- T    # Stage D: fit models

run_step <- function(path) {
  message("\n==== ", path, " ====")
  source(file.path(CODE, path), local = new.env())
}

# --- Stage A: assemble (raw ingestion) ---------------------------------------
if (run_assemble) {
  run_step("00_assemble/01_assemble_drought_data.R")            # -> input/dsci_measures.RDS
  run_step("00_assemble/02_assemble_drought_restrictions.R")    # -> input/combined_restriction_records.RDS
  run_step("00_assemble/03_assemble_audits.R")                  # -> input/district_audits.RDS
  run_step("00_assemble/04_assemble_debt.R")                    # -> input/district_debt_issuances.RDS
  run_step("00_assemble/05_scrape_storage_and_pops.R")          # -> input/pws_storage.RDS, pws_population.RDS
  run_step("00_assemble/06_htmlscrape_storage_interconnects.R") # -> input/storage_connections_data.txt
}

# --- Stage B: combine --------------------------------------------------------
if (run_combine) {
  run_step("01_combine/01_combine_district_and_pws_ids.R")      # -> input/id_crosswalk.RDS
  run_step("01_combine/02_combine_district_and_drought.R")      # -> input/pws_drought_weekly.RDS
  run_step("01_combine/03_combine_district_fiscal_data.R")      # -> input/combined_and_lagged_finances.RDS
  run_step("01_combine/04_combine_pws_with_tracts_counties.R")  # -> input/pws_{tract,county}_overlaps.RDS
  run_step("01_combine/05_combine_districts_tracts_counties.R") # -> input/district_{tract,county}_overlaps.RDS
  # explore/combine_storage_interconnects.R is an empty stub — implement or drop.
}

# --- Stage C: prep (panel construction) --------------------------------------
if (run_prep) {
  run_step("02_prep/01_assemble_kbdi.R")     # -> scratch/kbdi_county.RDS  (canonical KBDI builder)
  run_step("02_prep/02_census_block_PWS.R")  # -> input/pws_demos_MR.RDS   (needs Census API key)
  run_step("02_prep/03_make_eh_data.R")      # -> scratch/data_for_coxph_model.RDS, full_panel_data.RDS, base_inlacoxph_object.RDS
  # explore/make_coxph_data.R is the district-focused variant of make_eh_data.R;
  # pick one as canonical (see REBOOT_PLAN.md §5a). Not run by default.
}

# --- Stage D: model ----------------------------------------------------------
if (run_model) {
  run_step("03_model/01_fit_recurrent_cox_inla.R")  # Bayesian recurring-events Cox (INLA) -> scratch/recurrent_coxinla_*.RDS
  run_step("03_model/02_make_figure1.R")            # -> output/figure1.png
  run_step("03_model/03_model_results_table.R")     # -> output/model_estimates.{html,csv}, model_credible_intervals.png
  # Shared panel builder: 03_model/build_recurrent_panel.R (sourced by the fit script).
  # A frequentist `survival` version of the model is kept at
  # scratch_models/05_fit_recurrent_cox.R (reference, not on the default path),
  # alongside the prior INLA/joint specs (01_fit_baseline_models.R, 02_fit_glm_models.R,
  # 03_fit_joint_model.R, 04_fit_linear_combinations.R). Exploratory notebooks under explore/ likewise.
}

message("\nDone. Scratch intermediates in ", SCRATCH_DIR, "; figures in ", OUTPUT_DIR, ".")
