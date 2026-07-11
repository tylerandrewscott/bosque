# `code/` — drought_and_debt pipeline

Scripts are grouped into three numbered stage folders that run in order
(`00_assemble` → `01_combine` → `02_model`), and within each folder the numeric
filename prefix (`01_`, `02_`, …) gives the run order **within** that stage.
`code/run_all.R` sources them in exactly this sequence.

Run everything from the **`drought_and_debt` project root** (the folder holding
`setup_symlinks.sh` and `code/`):

```sh
cd .../bosque/drought_and_debt
bash setup_symlinks.sh          # one-time: create the bosquebox -> Box symlink
Rscript code/run_all.R          # flip run_assemble/run_combine/... flags inside
```

Path conventions (see `config.R`): `input/` = committed intermediate `.RDS`
(in git); `bosquebox/…` = raw Box data via the gitignored symlink; `scratch/` =
local gitignored intermediates; `output/` = figures/tables.

## Infrastructure (not workflow steps)

| File | What it does |
|---|---|
| `config.R` | Single source of paths (`committed()`/`raw_input()`/`spatial()`/`util()`/`scratch()`/`output()`), the analysis window (weekly, May 2010 through 2025 — `start_date`/`end_date`, consumed by `build_recurrent_panel.R`), the Albers projection, and guarded INLA/PARDISO options. Creates `scratch/` + `output/` on load, and sources `spatial_helpers.R` + `ingest_helpers.R`. Every assemble/combine script sources it (idempotently) at the top. |
| `spatial_helpers.R` | Modern spatial stack (terra + sf + spdep). Provides `reproject_sf()`, `tx_county_adjacency()`, `load_tx_tracts()`/`load_tx_counties()`, and `area_overlay()` (shared area-proportion overlay for the Stage-B tract/county joins); replaces the retired rgeos/rgdal/maptools/sp. |
| `ingest_helpers.R` | Small tabular helpers for the assemble/combine stages: `format_pws_id()` (canonical `TXnnnnnnn`), `normalize_district_name()` + `strip_county_suffix()` (the district-name canonicalization shared by the audit and debt assemblers), `label_restriction_columns()`, and `load_latest_district_list()` (newest `twdd_records/district_list_*`). |
| `crosswalks/` | Committed lookup tables kept out of code, e.g. `pws_district_id_overrides.csv` (manual PWS→District_ID corrections applied in `01_combine`). |
| `install_packages.R` | One-time dependency bootstrap: scans `code/` for referenced packages and installs what's missing (defaults a CRAN mirror for non-interactive `Rscript` runs; knows INLA's own repo). Run once before `run_all.R`; scripts no longer install packages inline. Note it scans `explore/` and `scratch_models/` too, so it also installs archive-only packages. |
| `run_all.R` | Stage-gated driver. Sources `config.R`, then runs stages A–C per the `run_assemble`/`run_combine`/`run_model` flags. |

## Stage A — `00_assemble/` (raw ingestion; slow, hits live APIs/scrapes)

Each script is independent; outputs are already committed under `input/`, so this
stage can normally be skipped.

| Script | Reads | Writes |
|---|---|---|
| `01_assemble_drought_data.R` | UNL DSCI API | `input/dsci_measures.RDS` |
| `02_assemble_drought_restrictions.R` | TCEQ FOIA xlsx, wayback HTML, `texas_dww/*.csv` | `input/combined_restriction_records.RDS` |
| `03_assemble_audits.R` | TCEQ audit CSVs, TWDB district lists | `input/district_audits.RDS` |
| `04_assemble_debt.R` | TX debt API, TBRB local-issuance CSVs, district list | `input/district_debt_issuances.RDS` |
| `05_scrape_storage_and_pops.R` | PWS shapefile, DWV OData API (live) | `input/pws_storage.RDS`, `input/pws_population.RDS` |
| `06_htmlscrape_storage_interconnects.R` | DWV OData API (live) + its own prior output (resume) | `input/storage_connections_data.txt` |

## Stage B — `01_combine/` (merges + spatial overlaps + demographics)

Run order matters: `02` (PWS↔county overlap) **must precede** `03`, which reads
`pws_county_overlaps.RDS` to map weekly county DSCI onto each PWS.

| Script | Reads | Writes |
|---|---|---|
| `01_combine_district_and_pws_ids.R` | TWDB district lists | `input/id_crosswalk.RDS` |
| `02_combine_pws_with_counties.R` | PWS shapefile, tigris counties | `input/pws_county_overlaps.RDS` |
| `03_combine_district_and_drought.R` | `dsci_measures.RDS`, `pws_county_overlaps.RDS` | `input/pws_drought_weekly.RDS` |
| `04_combine_district_fiscal_data.R` | `district_audits.RDS`, `district_debt_issuances.RDS` | `input/combined_and_lagged_finances.RDS` |
| `05_census_block_PWS.R` | PWS shapefile, tidycensus (needs a Census API key: `CENSUS_API_KEY` or a `census_api_key` file one level above the repo), precinct `.dta` + precinct geojson | `input/pws_demos_MR.RDS` (incl. `Median_Home_Value`, `Median_Year_Structure_Built` for the panel builder) |

## Stage C — `02_model/` (fit models)

The primary Stage-C model is a **Bayesian recurring-events (Andersen-Gill) Cox
model** of mandatory drought-restriction adoption, fit with INLA. It reads only
the committed `input/*.RDS` via one shared panel builder.

| Script | Reads | Produces |
|---|---|---|
| `build_recurrent_panel.R` | `combined_restriction_records.RDS`, `pws_drought_weekly.RDS`, `pws_population.RDS`, `pws_demos_MR.RDS`, `storage_connections_data.txt`, `combined_and_lagged_finances.RDS`, `id_crosswalk.RDS` (window from config `start_date`/`end_date`; `pws_storage.RDS` is deliberately unused — it is flow/demand, not capacity) | (sourced helper) weekly counting-process panels `panel_m1` (full) + `panel_m2` (fiscal subsample), 0/1 event by system-week |
| `01_fit_recurrent_cox_inla.R` | the builder | `scratch/recurrent_coxinla_model1_full.RDS` (initial: drought + controls, system frailty), `…_model2_by_fiscal.RDS` (one model per fiscal covariate, Model 1 posteriors as priors, district frailty), plus `output/*_slim.RDS` copies for sharing. **Needs INLA**, memory-heavy |
| `02_make_figure1.R` | `dsci_measures.RDS`, `statewide_drought_area.RDS` (cached from the UNL API; re-fetched only when `RESCRAPE=TRUE` or absent), `combined_restriction_records.RDS`, `id_crosswalk.RDS` | `output/figure1.png` (metro-county + statewide drought) and `output/figure2.png` (timing of first mandatory-restriction adoption) |
| `03_model_results_table.R` | `scratch/recurrent_coxinla_*.RDS`, falling back to `output/*_slim.RDS` | `output/model_estimates.{html,csv}`, `output/model_credible_intervals.png` (posterior mean + 95% CrI forest table for every fixed effect, coefficient and hazard-ratio scales) |
| `04_descriptive_stats_table.R` | the builder | `output/descriptive_stats.{csv,html}` (N/mean/sd/quantiles per model variable, at the correct unit of analysis) |

A frequentist `survival` version of the same model
(`scratch_models/05_fit_recurrent_cox.R`) and the earlier INLA/joint specs
(`01_fit_baseline_models.R`, `02_fit_glm_models.R`, `03_fit_joint_model.R`,
`04_fit_linear_combinations.R`) live under `02_model/scratch_models/` — kept for
reference, not on the default reproducible path.

## `explore/` — not part of the reproducible workflow

Kept for reference; run individually as needed. Not sourced by `run_all.R`.

| Script | Status |
|---|---|
| `assemble_kbdi.R` | County KBDI scrape (Texas A&M). Retired from the core pipeline — the model uses DSCI, not KBDI; kept here in case KBDI is wired in later. |
| `make_eh_data.R` | Superseded legacy panel builder (halts on undefined objects; outputs unused). Replaced by `02_model/build_recurrent_panel.R`. Figure 2 was moved to `02_model/02_make_figure1.R`. |
| `combine_districts_tracts_counties.R` | District↔tract/county overlays. Retired — nothing on-path consumed `district_{tract,county}_overlaps.RDS`. |
| `create_model_matrix.R` | Exploratory notebook; overlaps with `make_eh_data.R`. |
| `make_coxph_data.R` | District-focused variant of `make_eh_data.R`; pick one canonical builder (REBOOT_PLAN §5a). |
| `fit_coxph_models.R` | Exploratory stratified/joint Cox experiments. |
| `fit_interaction_linear_combs.R` | Interaction linear-combination experiments. |
| `fit_analysis_models.R` | Analysis-model scratch; not wired into `run_all.R`. |
| `simple_cox.R` | Mullin & Rubado replication check (self-contained). |
| `parse_json_parses.R` | Duplicate of `00_assemble/06_htmlscrape_storage_interconnects.R` — delete candidate. |
| `combine_storage_interconnects.R` | Empty stub — implement or drop. |
| `wrangling.R` | Abandoned scratch. |

See `../REBOOT_PLAN.md` for the full reboot map, blockers, and cleanup punch list.
