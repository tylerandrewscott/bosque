# =============================================================================
# config.R  —  central configuration for the drought_and_debt pipeline
# -----------------------------------------------------------------------------
# Just source it — no setwd() needed. PROJ_ROOT is auto-detected and config
# then setwd()s to it, so the whole pipeline runs from the bosque repo root:
#     source("drought_and_debt/code/config.R")   # wd = bosque repo root
# It still works if wd is already the drought_and_debt project root, or any
# code/ subdirectory:
#     source("code/config.R")
#
# It defines the paths, the analysis window, the map projection, and the
# spatial helpers every stage relies on, so those values live in ONE place
# instead of being copy-pasted across ~30 scripts.
#
# Path convention (config sets working directory = drought_and_debt project root):
#   input/...      committed intermediate .RDS (tracked in git)
#   bosquebox/...  raw Box data via the gitignored symlink (setup_symlinks.sh)
#   scratch/...    local, gitignored pipeline intermediates
#   output/...     figures / tables
# Because config setwd()s to PROJ_ROOT, every downstream script can keep using
# these bare relative paths regardless of where it was launched from.
# =============================================================================

# --- Locate the drought_and_debt project root --------------------------------
# Walks up from getwd() looking for the project-root marker (setup_symlinks.sh +
# code/), so it resolves whether wd is the bosque repo root, the drought_and_debt
# subdirectory, or any nested code/ subdirectory.
.find_proj_root <- function() {
  is_root <- function(p) {
    file.exists(file.path(p, "setup_symlinks.sh")) && dir.exists(file.path(p, "code"))
  }
  d <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    if (is_root(d)) return(d)                                   # wd = drought_and_debt (or a copy of it)
    child <- file.path(d, "drought_and_debt")
    if (is_root(child)) return(normalizePath(child))           # wd = bosque repo root
    parent <- dirname(d)
    if (identical(parent, d)) break                            # reached the filesystem root
    d <- parent                                                 # keep walking up (e.g. wd = code/<sub>)
  }
  # Last resort: assume wd is the bosque repo root
  normalizePath(file.path(getwd(), "drought_and_debt"), mustWork = FALSE)
}

PROJ_ROOT <- .find_proj_root()

# Normalize the working directory to the project root ONCE, here. Every script
# in the pipeline uses paths relative to it (input/, bosquebox/, scratch/,
# output/), so this is what lets the pipeline run from the bosque repo root
# without each script (or run_all.R) having to setwd() itself.
setwd(PROJ_ROOT)

# Absolute path to the code/ tree, for scripts that source other scripts.
CODE_DIR <- file.path(PROJ_ROOT, "code")

# --- Box data (via the gitignored `bosquebox` symlink) -----------------------
# Run setup_symlinks.sh once to create PROJ_ROOT/bosquebox -> the Box bosque
# folder. Raw ingestion inputs and spatial data live under it.
REPO_ROOT   <- dirname(PROJ_ROOT)                     # bosque git repo root
BOX_ROOT    <- file.path(PROJ_ROOT, "bosquebox")
RAW_INPUT   <- file.path(BOX_ROOT, "input")          # twdd_records, tceq_*, texas_dww, ...
SPATIAL_DIR <- file.path(BOX_ROOT, "spatial_inputs")  # PWS shapefile, geojson
UTIL_DIR    <- file.path(REPO_ROOT, "util_code")      # scraping helpers: dwv api, wayback html

if (!file.exists(BOX_ROOT)) {
  warning("bosquebox symlink not found at ", BOX_ROOT,
          "\n  -> run:  bash ", file.path(PROJ_ROOT, "setup_symlinks.sh"),
          "\n  Raw-ingestion (assemble/*) scripts will not find their inputs until this exists.")
}

# --- Repo-committed + local dirs ---------------------------------------------
COMMITTED_DIR <- file.path(PROJ_ROOT, "input")    # de-duplicated intermediate .RDS committed to git
SCRATCH_DIR   <- file.path(PROJ_ROOT, "scratch")  # local, gitignored, machine-specific intermediates
OUTPUT_DIR    <- file.path(PROJ_ROOT, "output")   # figures / tables

# Create local dirs on load so scripts never fail on a missing scratch/ or output/.
for (d in c(SCRATCH_DIR, OUTPUT_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Convenience helpers so scripts don't hardcode file.path() everywhere.
committed <- function(...) file.path(COMMITTED_DIR, ...)
scratch   <- function(...) file.path(SCRATCH_DIR, ...)
raw_input <- function(...) file.path(RAW_INPUT, ...)
spatial   <- function(...) file.path(SPATIAL_DIR, ...)
output    <- function(...) file.path(OUTPUT_DIR, ...)
util      <- function(...) file.path(UTIL_DIR, ...)

# --- Rescrape policy (Stage A raw ingestion, 00_assemble/*) --------------------
# One switch governs whether the raw-ingestion scripts re-hit their live sources:
#   RESCRAPE = FALSE  -> (default) reuse the committed prior scrape. The
#                        per-system DWV scrapers (05_scrape_storage_and_pops.R,
#                        06_htmlscrape_storage_interconnects.R,
#                        07_scrape_source_and_purchases.R) still top up ONLY
#                        systems missing from the existing output (incremental);
#                        the bulk scrapers (01-04) keep their existing output as-is.
#   RESCRAPE = TRUE   -> re-fetch every system from the source (full refresh).
#                        Fresh rows REPLACE prior rows per system, but systems no
#                        longer listed by the source keep their prior rows (the
#                        scrapers merge over the committed file; a full rescrape
#                        never discards previously collected data).
# Flip this one object to control Stage A re-fetching. (A value set before
# sourcing config still wins, so run_all.R could override it if desired.)
if (!exists("RESCRAPE")) {
  RESCRAPE <- FALSE
}

# REPROCESS: re-run the assemble scripts that only PROCESS LOCAL raw files —
# 02 (FOIA sheet + wayback snapshots), 03 (audits CSV), 04 (debt XML),
# 08 (SDWIS summaries) — even though their committed output exists. Flip this
# after editing their parsing logic; unlike RESCRAPE = TRUE it touches no live
# source (01, 05-07 are unaffected). RESCRAPE = TRUE already implies
# reprocessing (reuse_prior() returns FALSE then).
if (!exists("REPROCESS")) {
  REPROCESS <- FALSE
}

# reuse_prior(paths): TRUE when a bulk scraper should skip re-fetching, i.e.
# RESCRAPE is FALSE and every named committed output already exists. Used by the
# 01-04 assemble scripts to guard their body.
reuse_prior <- function(...) {
  paths <- c(...)
  !isTRUE(RESCRAPE) && length(paths) > 0 && all(file.exists(paths))
}

# --- Analysis window ----------------------------------------------------------
# Weekly panel, Aug 2010 through Dec 2025. The window OPENS at the month of the
# first observed mandatory restriction notice (2010-08-01): the restriction
# sources (FOIA sheets, wayback snapshots) have no coverage before then, so
# earlier weeks would be structurally event-free at-risk time. (Pre-reboot the
# window was 2010-01-01; changed 2026-07-23, see PLANNING_REVIEW M9.)
# build_recurrent_panel.R reads these, so this is the ONE place to change it.
start_date <- as.Date("2010-08-01")
end_date   <- as.Date("2025-12-31")
start_year <- as.integer(format(start_date, "%Y"))

# --- Map projection (Texas-centered Albers Equal Area, NAD83) -----------------
# Defined once here; previously copy-pasted verbatim into ~9 scripts.
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# --- Spatial helpers (terra/sf; replaces retired rgeos/rgdal/maptools/sp) -----
# Cache TIGER shapefile downloads (counties, tracts, block groups) locally so
# Stage B doesn't re-download them from the Census Bureau on every run.
options(tigris_use_cache = TRUE)
source(file.path(PROJ_ROOT, "code", "spatial_helpers.R"))

# --- Ingest helpers (format_pws_id, load_latest_district_list) ----------------
source(file.path(PROJ_ROOT, "code", "ingest_helpers.R"))

# --- INLA options (guarded: only if INLA is installed) ------------------------
if (requireNamespace("INLA", quietly = TRUE)) {
  # PARDISO speeds up sparse solves but needs a license file. Only enable it if
  # the license actually exists, so the pipeline runs without it.
  .pardiso <- path.expand("~/Documents/pardiso.lic")
  if (file.exists(.pardiso)) INLA::inla.setOption(pardiso.license = .pardiso)
}

message("config.R loaded. PROJ_ROOT = ", PROJ_ROOT)
