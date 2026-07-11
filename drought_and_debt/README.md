# Drought and Debt

*(Working title — paper title and abstract to be added.)*

An R study of why and when Texas community water systems adopt **mandatory
drought restrictions**, and whether a system's **fiscal capacity** (debt,
revenue, fund balance, tax structure) shapes that timing. The core method is a
**Bayesian recurring-events (Andersen–Gill) Cox model** fit with INLA on a
weekly system-week panel, May 2010 through 2025. Unit of analysis: public water
systems (PWS), with a district-linked subsample (MUD/FWSD/WCID/SUD special
districts matched to TCEQ audit records) for the fiscal models.

## Layout

| Path | What it is |
|---|---|
| `code/` | The pipeline: `00_assemble` → `01_combine` → `02_model`, driven by `code/run_all.R`. See **`code/README.md`** for the authoritative script-by-script I/O map. |
| `input/` | Committed intermediate `.RDS` — the portable handoff. The modeling stage reads only these, so the analysis reproduces from a clone without any raw-data access. |
| `bosquebox` | Gitignored symlink to the Box `bosque` folder (raw scrape inputs, shapefiles). Created by `setup_symlinks.sh`; only needed to re-run Stages A/B. |
| `scratch/` | Local, gitignored heavy intermediates (full INLA fits). |
| `output/` | Figures, tables, and slim (shareable) model objects. |

## Getting started

```sh
cd .../bosque/drought_and_debt
bash setup_symlinks.sh                  # one-time: bosquebox -> Box (Stages A/B only)
Rscript code/install_packages.R         # one-time: installs CRAN deps + INLA
Rscript code/run_all.R                  # Stage flags inside; Stage A off by default
```

Prerequisites beyond packages:

* **INLA** (installed by `install_packages.R` from its own repository).
* **Census API key** for Stage B's demographics script: export `CENSUS_API_KEY`,
  or place the key in a `census_api_key` file one directory above the git repo.
* **Box access** (the `bosquebox` symlink) for raw ingestion and the spatial
  overlays. Not needed to fit models — the committed `input/*.RDS` suffice.

The pipeline was rebooted in summer 2026 on R 4.5 with a terra/sf spatial stack
(the retired rgeos/rgdal/maptools/sp are gone). `REBOOT_PLAN.md` documents the
reboot; `code/README.md` documents the current workflow. Exploratory notebooks
are archived under `code/explore/` and `code/02_model/scratch_models/` and are
not part of the reproducible path.

## Authors

* Tyler A. Scott, tascott at ucdavis dot edu, @atylerscott
* Robert A. Greer, rgreer at tamu dot edu
* Emily Bell

## License

This project is licensed under the MIT License — see `LICENSE.md`.
