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
#   3. A Census API key for tidycensus (Stage B, 05_census_block_PWS.R).
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

# RESCRAPE (an object set in config.R; flip it there; default FALSE) governs
# Stage A: FALSE reuses the committed prior scrape and only tops up systems
# missing from the per-system DWV outputs; TRUE re-fetches every system but
# still merges over the committed files (never discards prior rows).
# REPROCESS (also config.R; default FALSE) re-runs just the local-raw
# processing scripts (02-04, 08) after a parsing-logic edit — no network.
run_assemble <- F  # Stage A: raw ingestion (slow; needs bosquebox + network).
                       # Outputs are committed under input/, so OFF by default.
run_combine  <- F   # Stage B: merges + spatial overlaps + demographics (needs bosquebox shapefiles + Census API key)
run_model    <- TRUE   # Stage C: fit models

run_step <- function(path, env = new.env()) {
  message("\n==== ", path, " ====")
  source(file.path(CODE, path), local = env)
}

# --- Stage A: assemble (raw ingestion) ---------------------------------------
if (run_assemble) {
  run_step("00_assemble/01_assemble_drought_data.R")            # -> input/dsci_measures.RDS
  run_step("00_assemble/02_assemble_drought_restrictions.R")    # -> input/combined_restriction_records.RDS
  run_step("00_assemble/03_assemble_audits.R")                  # -> input/district_audits.RDS
  run_step("00_assemble/04_assemble_debt.R")                    # -> input/district_debt_issuances.RDS
  run_step("00_assemble/05_scrape_storage_and_pops.R")          # -> input/pws_storage.RDS, pws_population.RDS
  run_step("00_assemble/06_htmlscrape_storage_interconnects.R") # -> input/storage_connections_data.txt
  run_step("00_assemble/07_scrape_source_and_purchases.R")      # -> input/pws_source.RDS, pws_purchase_edges.RDS
  run_step("00_assemble/08_assemble_sdwis_connections.R")       # -> input/pws_sdwis_connections.RDS
}

# --- Stage B: combine + demographics -----------------------------------------
# Order matters: 02 (pws<->county overlap) MUST precede 03 (drought->PWS), which
# reads pws_county_overlaps.RDS. 05 (census demographics) is a PWS spatial
# overlay too (moved here from the former 02_prep stage).
# Stage B needs gitignored local resources (the bosquebox Box symlink for the
# roster + PWS shapefile; a Census API key for 05). On a fresh clone those are
# absent — its outputs are already committed under input/, so SKIP the stage
# (with a message) rather than abort before Stage C ever runs.
if (run_combine && !file.exists(BOX_ROOT)) {
  message("Stage B SKIPPED: bosquebox symlink not found (run setup_symlinks.sh). ",
          "Using the committed input/ intermediates instead.")
  run_combine <- FALSE
}
if (run_combine) {
  run_step("01_combine/01_combine_district_and_pws_ids.R")   # -> input/id_crosswalk.RDS
  run_step("01_combine/02_combine_pws_with_counties.R")      # -> input/pws_county_overlaps.RDS
  run_step("01_combine/03_combine_district_and_drought.R")   # -> input/pws_drought_weekly.RDS (needs 02)
  run_step("01_combine/04_combine_district_fiscal_data.R")   # -> input/combined_and_lagged_finances.RDS
  # 05 additionally needs a Census API key (env var or the key file one level
  # above the repo); without one it stop()s, so check here and skip just it.
  if (nzchar(Sys.getenv("CENSUS_API_KEY")) ||
      file.exists(file.path(dirname(REPO_ROOT), "census_api_key"))) {
    run_step("01_combine/05_census_block_PWS.R")             # -> input/pws_demos_MR.RDS
  } else {
    message("01_combine/05_census_block_PWS.R SKIPPED: no Census API key ",
            "(set CENSUS_API_KEY or create ../census_api_key). ",
            "Using the committed input/pws_demos_MR.RDS.")
  }
  # Retired / off-path (in explore/): combine_districts_tracts_counties.R (district
  # overlaps, unused), assemble_kbdi.R (KBDI unused — model uses DSCI), make_eh_data.R.
}

# --- Stage C: model ----------------------------------------------------------
if (run_model) {
  # The fit script and the descriptive table both need the (expensive) shared
  # counting-process panel. They run in ONE environment so the guarded source
  # of build_recurrent_panel.R in each builds it once and the second reuses it.
  panel_env <- new.env()
  run_step("02_model/01_fit_recurrent_cox_inla.R", env = panel_env)  # Bayesian recurring-events Cox (INLA) -> scratch/recurrent_coxinla_*.RDS
  run_step("02_model/02_make_figure1.R")            # -> output/figure1.png, figure2.png
  run_step("02_model/03_model_results_table.R")     # -> output/model_estimates.{html,csv}, model_credible_intervals.png
  run_step("02_model/04_descriptive_stats_table.R", env = panel_env) # -> output/descriptive_stats.{csv,html}
  run_step("02_model/05_paper_facts.R", env = panel_env)             # -> output/paper_facts.csv (scalar facts the manuscript cites)
  run_step("02_model/06_make_appendix_figures.R", env = panel_env)   # -> output/baseline_hazard_appendix.png, frailty_map_appendix.png
  # Shared panel builder: 02_model/build_recurrent_panel.R (sourced by the fit script).
  # A frequentist `survival` version of the model is kept at
  # scratch_models/05_fit_recurrent_cox.R (reference, not on the default path),
  # alongside the prior INLA/joint specs (01_fit_baseline_models.R, 02_fit_glm_models.R,
  # 03_fit_joint_model.R, 04_fit_linear_combinations.R). Exploratory notebooks under explore/ likewise.
}

message("\nDone. Scratch intermediates in ", SCRATCH_DIR, "; figures in ", OUTPUT_DIR, ".")
