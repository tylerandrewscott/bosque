# drought_and_debt — Reboot Plan (Summer 2026)

_Prepared 2026-07-08. Branch: `reboot-summer-2026`. This is a working map + reboot
checklist, not a final workflow doc. Treat the "run order" and I/O tables as the
authoritative pipeline description until the README is rewritten._

---

## 0. Reboot scaffolding — what's already been done (2026-07-08)

The infrastructure from §5–§6 is now in place:

- **`setup_symlinks.sh`** (repo subdir root) — creates a gitignored `bosquebox`
  symlink into the Box `bosque` folder, auto-detecting Box-Box / Box / ~/Box,
  mirroring the tijuana/salinas pattern. Run once: `bash setup_symlinks.sh`.
  Box data is then reachable as `bosquebox/input/…` and `bosquebox/spatial_inputs/…`.
- **`.gitignore`** — ignores `bosquebox`, `scratch/`, `tx.adj`, R cruft. (`output/`
  is tracked: it holds the figures/tables and the slim shareable model objects.)
- **`code/restart/config.R`** — single source of paths (`BOX_ROOT`, `RAW_INPUT`,
  `SPATIAL_DIR`, `COMMITTED_DIR`, `SCRATCH_DIR`, `OUTPUT_DIR`), the analysis window
  (`start_date`/`end_date`), the `albersNA` projection, and guarded INLA/PARDISO
  options. Creates `scratch/` and `output/` on load. Works from either the bosque
  root or the drought_and_debt project root.
- **`code/restart/spatial_helpers.R`** — modern spatial stack (terra + sf + spdep),
  no retired packages. Provides `reproject_sf()` (terra-backed) and
  `tx_county_adjacency()`.
- **`code/restart/prep/assemble_kbdi.R`** — canonical KBDI builder, extracted from
  `create_model_matrix.R`, writing `scratch/kbdi_county.RDS` via `saveRDS` (the old
  CSV-under-`.RDS` bug is fixed).
- **`code/restart/run_all.R`** — stage-gated driver (A assemble / B combine /
  C prep / D model) with flags; sources `config.R` first.
- **terra migration** — `rgeos`/`rgdal`/`maptools`/`sp` removed from
  `assemble_debt.R`, `assemble_audits.R`, `fit_glm_models.R`, `fit_joint_model.R`,
  `fit_coxph_models.R`, `make_coxph_data.R`, `make_eh_data.R`,
  `create_model_matrix.R`. The `spTransform()`/`CRS()` reprojection idiom is
  replaced by `reproject_sf()`; `poly2nb()` now takes `sf` directly.

**Scratch relocation:** intermediates now live in the local gitignored
`drought_and_debt/scratch/` (was `scratch/proj5/` at the bosque root and an HPC
`net/tmp/…` path). `SCRATCH_DIR` in `config.R` points there.

**Path migration (done 2026-07-08):** every data-path literal in the scripts was
migrated to a single working-directory convention — **wd = the `drought_and_debt`
project root**. The mapping applied across all `.R` files:

- raw Box data `input/…`, `spatial_inputs/…` → `bosquebox/input/…`, `bosquebox/spatial_inputs/…`
- committed intermediates `drought_and_debt/input/…` → `input/…`
- scratch `scratch/proj5/…` and `…/net/tmp/tscott1/bosque_scratch/proj5/…` → `scratch/…`
- figures `output/proj5/…` and `drought_and_debt/output/…` → `output/…`
- repo-root helper `util_code/…` → `../util_code/…`
- `source('drought_and_debt/code/restart/…')` → `source('code/restart/…')`

**Run everything from the `drought_and_debt` project root** (not the bosque root).
`run_all.R` sources `config.R` and then `setwd(PROJ_ROOT)` to enforce this.
`config.R` still auto-detects `PROJ_ROOT`, so it also resolves if sourced from the
bosque root. (Note: FUSE `.fuse_hidden*` shadow files may appear if the scripts
were open in RStudio during the edit; they're gitignored and clear on restart.)

---

## 1. What this project is

An R study of **why and when Texas community water systems adopt mandatory drought
restrictions**, and whether a system's **fiscal capacity (debt, revenue, fund balance,
tax structure)** shapes that timing. The core method is **Bayesian survival / event-history
analysis (Cox PH and joint binomial-plus-survival models) fit with INLA**, with spatial
random effects on the Texas county adjacency graph. Study window: **weekly, 4 May 2010 –
7 July 2015** (some data assembled through 9/2024).

Unit of analysis: public water systems (PWS), with a district-focused subset for
MUD/FWSD/WCID/SUD special districts (matched to TCEQ audit records).

---

## 2. Current state at a glance

| Layer | State | Notes |
|---|---|---|
| **Assemble** (raw ingestion/scraping) | Mostly complete | 6 working scripts; 1 duplicate, live web scrapes are fragility risk |
| **Combine** (merge/spatial) | Mostly complete | 5 working scripts; 1 empty stub (`combine_storage_interconnects.R`) |
| **Data prep** (`make_eh_data`, `make_coxph_data`) | Complete but heavy | 800–1300 lines each, many hardcoded fixes; produce the analysis panel |
| **Model** | Exploratory / partially broken | Several 600–1900-line notebooks; depend on missing `scratch/` intermediates |
| **Committed data** (`input/*.RDS`) | Present | The de-duplicated intermediate RDS files are in the repo and load fine |
| **Box-linked data** (`input/`, `spatial_inputs/` symlinks) | Local only | Point to your Box CloudStorage; not portable, not in sandbox |
| **`scratch/` intermediates** | **Missing / gitignored** | The whole modeling layer reads/writes these — see §4 |
| **README** | Placeholder | Still `# TITLE` / `[SUMMARY]` |

**Bottom line:** the assemble→combine→data-prep chain is in good shape and reproducible
from committed inputs. The **modeling layer is the reboot's main work**: it is split
across overlapping exploratory scripts and depends on intermediate files that don't
exist in the repo.

---

## 3. Pipeline map (run order + I/O)

Paths below are relative to the repo root (`bosque/`). `input/` and `spatial_inputs/`
at the root are **Box symlinks**; `drought_and_debt/input/` holds committed RDS outputs.

### Stage A — Assemble (each is independent; run in any order)

| Script (`drought_and_debt/code/restart/assemble/`) | Reads | Writes |
|---|---|---|
| `assemble_drought_data.R` | UNL DSCI API | `input/dsci_measures.RDS` |
| `assemble_drought_restrictions.R` | TCEQ FOIA xlsx, wayback HTML (`util_code/`), `texas_dww/*.csv` | `input/combined_restriction_records.RDS` |
| `assemble_audits.R` | `tceq_audits/…csv`, `twdd_records/district_list*` | `input/district_audits.RDS` |
| `assemble_debt.R` | TX debt API, `tbrb/Local_Issuance_*.csv`, district list | `input/district_debt_issuances.RDS` |
| `scrape_storage_and_pops.R` | PWS shapefile, TCEQ data sheets (live) | `input/pws_storage.RDS`, `input/pws_population.RDS` |
| `htmlscrape_storage_interconnects.R` | `texas_dww/district_master_list.csv`, TCEQ (live) | `input/storage_connections_data.txt` |
| `parse_json_parses.R` | — | **duplicate of the line above; delete** |

### Stage B — Combine (after Stage A)

| Script (`…/combine/`) | Reads | Writes |
|---|---|---|
| `combine_district_and_pws_ids.R` | `twdd_records/district_list*` | `input/id_crosswalk.RDS` |
| `combine_district_and_drought.R` | `dsci_measures.RDS`, `pws_county_overlaps.RDS` | `input/pws_drought_weekly.RDS` |
| `combine_district_fiscal_data.R` | `district_audits.RDS`, `district_debt_issuances.RDS` | `input/combined_and_lagged_finances.RDS` |
| `combine_pws_with_tracts_counties.R` | PWS shapefile, tigris tracts/counties | `input/pws_{tract,county}_overlaps.RDS` |
| `combine_districts_tracts_counties.R` | `id_crosswalk.RDS`, PWS shapefile, TCEQ geojson, tigris | `input/district_{tract,county}_overlaps.RDS` |
| `combine_storage_interconnects.R` | — | **EMPTY STUB — intended to fold storage + interconnect scrapes into the panel** |

### Stage C — Data prep / panel construction

| Script (`…/restart/`) | Reads | Writes |
|---|---|---|
| `census_block_PWS.R` | PWS shapefile, census (tidycensus), precinct `.dta`, restriction xlsx | `input/pws_demos_MR.RDS` |
| `make_eh_data.R` | overlaps, finances, drought, demos, `MR_drought_scores.csv`, `scratch/fiscal_data.RDS` | `scratch/data_for_coxph_model.RDS`, `scratch/proj5/full_panel_data.RDS`, `…/base_inlacoxph_object.RDS`, `…/ready_to_coxph.RDS`, `output/figure2.png` |
| `make_coxph_data.R` | same family as above (district-focused) | `scratch/data_for_coxph_model.RDS` (district variant) |

### Stage D — Model (after Stage C)

| Script (`…/model/`) | Reads | Produces |
|---|---|---|
| `create_model_matrix.R` | panel + base objects | **also writes** `full_panel_data`, `base_inlacoxph_object`, `ready_to_coxph` — overlaps with `make_eh_data.R` |
| `fit_baseline_models.R` | `full_panel_data.RDS`, `base_inlacoxph_object.RDS` | `basemod_hyperparameters.RDS` (warm-start base) |
| `fit_glm_models.R` | panel + base priors | `gof_table.RDS` (15 specs, form_0–form_14) |
| `fit_coxph_models.R` | `data_for_coxph_model.RDS` | stratified + joint models, `figureA2–A4.png` |
| `fit_joint_model.R` | `data_for_coxph_model.RDS` | `joint_…_all_texas_model.RDS`, figures |
| `fit_linear_combinations.R` | panel + base | form_14 hazard predictions (lincombs) |
| `fit_interaction_linear_combs.R` | panel + base | `mod_object_list.RDS`, `interaction_mod_object_list.RDS` |
| `fit_analysis_models.R` | `data_used_in_model.RDS` | analysis models |
| `simple_cox.R` | self-contained | `mullin_rubado_almost_repliation.html` (Mullin & Rubado replication check) |
| `make_figure1.R` | `dsci_measures.RDS`, UNL API | `output/figure1.png` |

---

## 4. The reboot blockers (fix these first)

**1. Missing `scratch/` intermediates — the pipeline is severed between Stage C and Stage D.**
Every modeling script reads from `scratch/proj5/*.RDS` (`full_panel_data.RDS`,
`base_inlacoxph_object.RDS`, `ready_to_coxph.RDS`, `data_for_coxph_model.RDS`) and several
write to an HPC path `../../../../net/tmp/tscott1/bosque_scratch/proj5/`. `scratch/` is
**gitignored and absent**, and `net/tmp/...` is a cluster path. So the models cannot run
until Stage C is re-run to regenerate those files. `full_panel_data`/`base_inlacoxph_object`/
`ready_to_coxph` are produced by **both** `make_eh_data.R` and `create_model_matrix.R` —
you must decide which is canonical (see §5).

**2. KBDI generation is buried inside an "exploratory" script, with a file-format quirk.**
The models require `KBDI_Avg` (Keetch-Byram Drought Index). It is scraped and assembled
**inside `create_model_matrix.R`** (~lines 1015–1045), which means that 1356-line script
is actually on the critical path, not purely exploratory (see §5b). Note the quirk:
`create_model_matrix.R` writes it with `fwrite(kbdi_df, 'scratch/proj5/kbdi_dt.RDS')` —
**CSV content under a `.RDS` extension**. Anything that later `readRDS()`-es it will fail;
it must be read with `fread()`. Pull the KBDI block into its own Stage-C script and fix the
extension.

**3. Retired spatial packages.** `assemble_debt.R` (and others) call `rgeos`, `rgdal`,
`maptools`, which were **archived from CRAN in Oct 2023** and won't install on modern R.
These must be migrated to `sf`/`terra` equivalents. Also non-CRAN: `INLA` (own repo),
`esri2sf`, `brinla`, `lucr`; and `sjplot` is likely a typo for `sjPlot`.

**4. Box symlinks are machine-specific.** `input/` and `spatial_inputs/` resolve only on
your machine (they point into `~/Library/CloudStorage/Box-Box/bosque/`). Fine for you-run,
but the raw-ingestion scripts can't run anywhere else. Committed `drought_and_debt/input/*.RDS`
are the portable handoff — the modeling layer should depend only on those.

**5. R version.** README pins R 4.1.1 (2021). A 2026 reboot on a current R will surface
the package issues above; decide whether to pin the old toolchain (renv) or modernize.

---

## 5. Reorganization proposal

The goal: a linear, documented pipeline where each script has one job and reads/writes
declared paths — no reliance on undocumented `scratch/` state.

**a. Resolve the duplicate panel-builder.** `make_eh_data.R`, `make_coxph_data.R`, and
`create_model_matrix.R` all build the analysis panel and overlap heavily. Pick ONE as the
canonical panel builder (recommend `make_eh_data.R` — it's the most complete and produces
`figure2`), demote the others to documented variants or archive them.

**b. Split `create_model_matrix.R` (1356 lines) and `fit_coxph_models.R`/`fit_joint_model.R`
(866/609 lines).** These are notebooks mixing data construction, model fitting,
stratification experiments, and figures. `create_model_matrix.R` is a special case: it does
real, load-bearing work (KBDI assembly + `full_panel_data`/`base_inlacoxph_object`/
`ready_to_coxph`) tangled together with exploration. Extract its load-bearing blocks into
Stage-C scripts, then move the rest — and the exploratory portions of the two fit notebooks —
into an `explore/` or `archive/` folder so the reproducible path is obvious.

**c. Centralize paths.** Replace scattered `scratch/proj5/…` and `net/tmp/tscott1/…`
literals with a single config (e.g., an `.Rprofile` or `config.R` defining `SCRATCH_DIR`,
`INPUT_DIR`, `OUTPUT_DIR`). Create `scratch/` on load. This kills blocker #1's fragility.

**d. Delete/fill stubs.** Remove `parse_json_parses.R` (duplicate) and `wrangling.R`
(abandoned). Either implement `combine_storage_interconnects.R` or delete it and fold the
storage/interconnect merge into the panel builder.

**e. Add a top-level `run_all.R`** (or a numbered file convention `00_`, `01_`, …) that
sources the pipeline in dependency order, so "reboot" becomes one command.

**f. Rewrite README** — fill in title, abstract, the dependency list from §6, and the
Stage A→D workflow from §3.

Suggested target layout:

```
code/
  00_assemble/      # A: ingestion (rename from assemble/)
  01_combine/       # B: merges + spatial overlaps
  02_prep/          # C: ONE canonical panel builder + census
  03_model/         # D: baseline → glm → joint → lincombs (reproducible only)
  explore/          # archived exploratory notebooks
  config.R          # all paths + projections + date window
  run_all.R         # sources 00→03 in order
```

---

## 6. Dependencies & environment setup (for local run)

R packages actually used across `restart/` (by frequency):

- **Core:** data.table, tidyverse, lubridate, stringr, forcats, scales, zoo, foreign
- **Spatial:** sf, lwgeom, tigris, geojsonsf, spdep, tidycensus, esri2sf — **and legacy
  rgeos/rgdal/maptools/sp that must be migrated to sf/terra**
- **Modeling:** INLA (non-CRAN), survival, mstate, coxme, MASS, brinla
- **Scraping/IO:** rvest, jsonlite, readxl, R.utils, pbapply
- **Tables/figures:** ggthemes, cowplot, gridExtra, htmlTable, stargazer, texreg, sjPlot(?), corrplot, ggfortify, hhi, tidyquant, neatRanges, reReg, lucr

Setup checklist:

1. **Install INLA** from its own repo (`install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)`). Optional **PARDISO** license is referenced at `~/Documents/pardiso.lic` in `fit_joint_model.R` — restore it or remove the `inla.setOption()` line.
2. **Census API key** for tidycensus (`census_api_key()`), used in `census_block_PWS.R` / `make_eh_data.R`.
3. **Migrate rgeos/rgdal/maptools → sf/terra** (blocker #3) before those assemble scripts will install/run on current R.
4. **Decide R toolchain:** either install R 4.1.x to match, or modernize and fix the retired-package calls. Recommend capturing whatever you choose in **renv** so the reboot is reproducible.
5. Confirm Box is mounted so the `input/`/`spatial_inputs/` symlinks resolve.

---

## 7. Verification checklist (run locally, in order)

Sanity checks before trusting the pipeline:

1. **Inputs load:** `lapply(list.files("drought_and_debt/input", "\\.RDS$", full.names=TRUE), readRDS)` — confirm all committed RDS read without error.
2. **Symlinks resolve:** `file.exists("input")`, `file.exists("spatial_inputs")`, and list a shapefile under `spatial_inputs/Service_Area_Boundaries/`.
3. **Packages install:** attempt to load every package in §6; log which fail (expect rgeos/rgdal/maptools/INLA).
4. **Stage C runs:** run the canonical panel builder end-to-end; confirm it writes `scratch/proj5/full_panel_data.RDS`, `base_inlacoxph_object.RDS`, `ready_to_coxph.RDS`, `data_for_coxph_model.RDS`.
5. **KBDI regenerates** (blocker #2) — run the KBDI block in `create_model_matrix.R`; confirm `scratch/proj5/kbdi_dt.RDS` is produced and that downstream code reads it with `fread`, not `readRDS`.
6. **Baseline model fits:** run `fit_baseline_models.R`; confirm `basemod_hyperparameters.RDS` is produced and converges.
7. **Figures regenerate:** `make_figure1.R` and Stage-C `figure2.png` reproduce.
8. **Replication check:** `simple_cox.R` reproduces the Mullin & Rubado comparison HTML.

---

## 8. Resume the analysis — suggested next steps

Once the pipeline runs end-to-end:

1. **Re-establish the canonical model set.** From the many exploratory specs, fix the
   reported models: the baseline (form_0), the fiscal-capacity specs (debt / revenue /
   fund-balance per connection, operating ratio, tax indicators), and the joint
   binomial+survival model. Save these as named, versioned RDS objects.
2. **Regenerate `gof_table.RDS`** (WAIC/DIC across form_0–form_14) so model selection is
   documented and reproducible.
3. **Finalize the interaction story** via `fit_linear_combinations.R` — predicted hazard
   across revenue×usage, debt×home-value, fund-balance×size grids — which looks like the
   paper's key result.
4. **Lock the figure set:** figure1 (statewide drought), figure2 (adoption timing),
   figureA2–A4 (stratified hazards). Confirm each is produced by exactly one script.
5. **Decide the sample:** all-CWS vs. district-only (MUD/FWSD/WCID/SUD with audits). The
   code carries both; the paper should commit to one primary and one robustness sample.
6. **Update README** with title, abstract, and the finalized workflow.

---

## 9. Cleanup punch list

- [x] Migrate rgeos/rgdal/maptools/sp → terra/sf (done 2026-07-08)
- [x] Extract KBDI into `prep/assemble_kbdi.R`; fix the `.RDS`-extension-on-CSV quirk (done)
- [x] Set up portable symlinks (`bosquebox` + `setup_symlinks.sh`) like tijuana/salinas (done)
- [x] Move scratch intermediates to local gitignored `drought_and_debt/scratch/` (done)
- [x] Add `config.R` (central paths/window/projection) and `run_all.R` driver (done)
- [x] Guard the PARDISO license reference so it's optional (done, in `config.R`)
- [x] Migrate all literal data paths to the `drought_and_debt`-root convention (raw→`bosquebox/`, committed→`input/`, scratch→`scratch/`) (done 2026-07-08, §0)
- [ ] Delete `parse_json_parses.R` (exact duplicate of `htmlscrape_storage_interconnects.R`)
- [ ] Delete or repurpose `wrangling.R` (abandoned, 28 lines)
- [ ] Implement or delete `combine_storage_interconnects.R` (empty stub)
- [x] Resolve the 3-way panel-builder overlap (§5a) — `02_model/build_recurrent_panel.R` is canonical; the others live in `explore/`
- [x] Fill in README (title/abstract still placeholders; dependencies + workflow done 2026-07-10)
- [x] Adopt the numbered-stage folder layout (00_assemble/01_combine/02_model; done 2026-07-10)
