# =============================================================================
# install_packages.R  —  one-time dependency bootstrap for drought_and_debt
# -----------------------------------------------------------------------------
# Scans every .R file under code/ for the packages the pipeline actually uses
# (library()/require()/requireNamespace() calls and pkg:: references), then
# installs whatever is missing. Run it once before run_all.R:
#     cd .../bosque
#     Rscript drought_and_debt/code/install_packages.R
#   or interactively:
#     source("drought_and_debt/code/install_packages.R")
#
# Already-installed packages are left untouched, so it is safe to re-run. Most
# dependencies come from CRAN; the handful that don't (INLA and a few
# GitHub-only helpers) are mapped to their real sources in .special below.
# =============================================================================

# --- CRAN mirror: non-interactive Rscript sessions often have none set --------
# (install.packages() would die with "trying to use CRAN without setting a
# mirror"), so default to the cloud mirror when repos is unset.
if (!nzchar(getOption("repos")["CRAN"]) || getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# --- Locate the code/ tree (works from repo root or project root) ------------
.find_code_dir <- function() {
  cands <- c("drought_and_debt/code", "code", ".")
  hit <- Find(function(d) file.exists(file.path(d, "config.R")), cands)
  if (is.null(hit)) stop("Could not find the code/ directory (looked for config.R).")
  normalizePath(hit)
}
CODE_DIR <- .find_code_dir()

# --- Packages that ship with R: never need installing, never scan for --------
# (priority "base"/"recommended" come with every R install.)
.base_pkgs <- rownames(installed.packages(priority = c("base", "recommended")))
.always_skip <- unique(c(.base_pkgs, "base"))

# --- Non-CRAN packages: how to install each ----------------------------------
# Anything not listed here is assumed to be a plain CRAN package.
.special <- list(
  # Bayesian latent-Gaussian models — its own download repo, not CRAN.
  INLA    = function() install.packages(
    "INLA",
    repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"),
    dependencies = TRUE),
  # GitHub-only helpers referenced in the scripts' install comments.
  brinla  = function() remotes::install_github("julianfaraway/brinla"),
  esri2sf = function() remotes::install_github("yonghah/esri2sf"),
  lucr    = function() remotes::install_github("Ironholds/lucr")
)

# --- Name fixups: what the code writes -> the real installable package name ---
# (R package names are case-sensitive; the code has a couple of typo/casing slips.)
.rename <- c(sjplot = "sjPlot")

# --- Scan every .R file for referenced packages ------------------------------
r_files <- list.files(CODE_DIR, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

extract_pkgs <- function(file) {
  txt <- readLines(file, warn = FALSE)
  txt <- txt[!grepl("^\\s*#", txt)]           # drop whole-line comments
  # library(pkg) / require(pkg) / requireNamespace("pkg")
  m1 <- regmatches(txt, gregexpr(
    "(?:library|require|requireNamespace)\\(\\s*['\"]?([A-Za-z][A-Za-z0-9._]+)",
    txt, perl = TRUE))
  m1 <- sub(".*\\(\\s*['\"]?", "", unlist(m1))
  # pkg::function
  m2 <- regmatches(txt, gregexpr("([A-Za-z][A-Za-z0-9._]+)::", txt, perl = TRUE))
  m2 <- sub("::$", "", unlist(m2))
  unique(c(m1, m2))
}

pkgs <- sort(unique(unlist(lapply(r_files, extract_pkgs))))

# Apply casing/typo fixups and drop base R packages.
pkgs <- ifelse(pkgs %in% names(.rename), .rename[pkgs], pkgs)
pkgs <- sort(unique(pkgs))
pkgs <- setdiff(pkgs, .always_skip)

# --- Figure out what's missing -----------------------------------------------
is_installed <- function(p) requireNamespace(p, quietly = TRUE)
missing <- pkgs[!vapply(pkgs, is_installed, logical(1))]

message("Scanned ", length(r_files), " R files; found ", length(pkgs),
        " third-party packages, ", length(missing), " missing.")
if (length(missing)) message("  Missing: ", paste(missing, collapse = ", "))

# --- Install the missing ones ------------------------------------------------
# GitHub installs need `remotes`; make sure it's present first if we'll need it.
needs_github <- any(missing %in% c("brinla", "esri2sf", "lucr"))
if (needs_github && !is_installed("remotes")) install.packages("remotes")

installed_ok <- character(0)
failed       <- character(0)
for (p in missing) {
  message("\n---- installing ", p, " ----")
  ok <- tryCatch({
    if (!is.null(.special[[p]])) .special[[p]]() else install.packages(p)
    is_installed(p)                                  # confirm it actually loaded
  }, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(ok)) installed_ok <- c(installed_ok, p) else failed <- c(failed, p)
}

# --- Summary -----------------------------------------------------------------
message("\n==== install_packages.R summary ====")
message("Already present: ", length(pkgs) - length(missing))
if (length(installed_ok)) message("Newly installed: ", paste(installed_ok, collapse = ", "))
if (length(failed)) {
  message("FAILED (install by hand): ", paste(failed, collapse = ", "))
} else {
  message("All required packages are installed.")
}
