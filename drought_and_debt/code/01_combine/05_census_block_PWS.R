# =============================================================================
# 05_census_block_PWS.R  —  PWS service-area demographics (Mullin/Rubado method)
# -----------------------------------------------------------------------------
# To characterize the population each water system serves, census values are
# aggregated up to service-area boundaries, weighting each census unit by the
# share of the SYSTEM's area that falls in it (intersection area / system area,
# the Mullin & Rubado convention).
#   * block-group level, 2010 decennial SF1: % Hispanic, % Black, % rural
#   * tract level, 2006-2010 ACS: % bachelor's, median household income,
#     % housing built since 1980, % under poverty line, median home value,
#     median year structure built. (M/R used block groups throughout; these
#     variables are not in the public block-group data, so tracts are the
#     closest public equivalent.)
#   * tract level, 2016-2020 ACS (2020 tract lines): median home value and
#     median year structure built AGAIN -- the two tract variables in the model
#     spec are pulled at both decennial-anchored vintages so the panel can
#     forward-fill them (2010 vintage covers panel years 2010-2019, 2020
#     vintage 2020 on; the rolling join lives in build_recurrent_panel.R §3).
#   * VTD (voting district) level: Democratic vote share at BIENNIAL vintages
#     2012-2024 (TLC Capitol Data Portal returns, all cycles re-tabulated onto
#     the single 2024 VTD plan), forward-filled by week-year in the panel
#     builder alongside the ACS vintages.
#
# Needs a Census API key: export CENSUS_API_KEY, or put the key in a
# `census_api_key` file one directory above the bosque repo root.
# Writes input/pws_demos_MR.RDS, consumed by 02_model/build_recurrent_panel.R
# (which needs the vintage-suffixed Median_Home_Value_*,
# Median_Year_Structure_Built_*, and Perc_Dem_* columns in particular).
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(stringr)
  library(tigris)
})

# --- Census API key: fail fast, not twelve API calls in ------------------------
if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  key_file <- file.path(dirname(REPO_ROOT), "census_api_key")
  if (file.exists(key_file)) {
    tidycensus::census_api_key(trimws(readLines(key_file, warn = FALSE)[1]))
  } else {
    stop("No Census API key found: set CENSUS_API_KEY or put the key in ", key_file)
  }
}

# --- PWS service-area boundaries (dissolved to one polygon per system) --------
# Same TCEQ service-area shapefile the county overlay (01_combine/02) uses,
# via the shared loader (spatial_helpers.R: read + rename + reproject + valid).
# https://tceq.maps.arcgis.com/apps/webappviewer/index.html?id=04bbf8b322b34d8abaea7b06996d3775
twd_boundaries <- load_pws_boundaries()
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()

# --- Block-group demographics: 2010 decennial SF1 ------------------------------
# (These variables are no longer in the primary model specification; kept
# consistent for descriptives / future use.) One batched pull:
#   P004003/P004001  -> % Hispanic, of TOTAL population
#   P006003/P003001  -> % Black (alone or in combination), of TOTAL population.
#                       (P010 was used before — wrongly: P010 is race for the
#                       population 18 YEARS AND OVER, not total population.)
#   H002005/H002001  -> % rural, of HOUSING UNITS (not persons — label carefully)
dec <- tidycensus::get_decennial(geography = 'block group',
                                 state = 'TX', year = 2010,
                                 variables = c('P004001', 'P004003',
                                               'P003001', 'P006003',
                                               'H002001', 'H002005'))
dec <- dcast(data.table(dec), GEOID ~ variable, value.var = 'value')
dec[, Perc_Hispanic := 100 * (P004003 / P004001)]
dec[, Perc_Black    := 100 * (P006003 / P003001)]
dec[, Perc_Rural    := 100 * (H002005 / H002001)]
block_demos <- dec[, .(GEOID, Perc_Hispanic, Perc_Black, Perc_Rural)]

tx_blocks <- tigris::block_groups(state = 'TX', cb = TRUE, year = 2010)
tx_blocks$GEOID <- str_remove(tx_blocks$GEO_ID, '^1500000US')
tx_blocks <- st_make_valid(st_transform(tx_blocks, st_crs(albersNA)))
tx_blocks <- left_join(tx_blocks, block_demos, by = 'GEOID')

# --- Tract demographics: 2006-2010 ACS 5-year ----------------------------------
# Two batched pulls (identical geography/survey/year) instead of six sequential
# single-variable ones. Subject (S*) and detailed (B*) tables live on different
# API endpoints, so they can't share one get_acs() call. Named variable vectors
# make tidycensus emit the friendly name in the `variable` column directly.
tx_tracts <- tigris::tracts(state = 'TX', cb = TRUE, year = 2010)
tx_tracts$GEOID <- str_remove(tx_tracts$GEO_ID, '^1400000US')

acs_subject <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                                   state = 'TX', year = 2010,
                                   variables = c(Perc_Bachelors          = 'S1501_C01_015',
                                                 Med_Household_Income    = 'S1903_C02_001',
                                                 Perc_Under_Poverty_Line = 'S1702_C02_001'))
acs_subject <- dcast(data.table(acs_subject), GEOID ~ variable, value.var = 'estimate')

# B25034_002..005 = built 1980 or later; B25034_001 = total housing units.
# Median home value (B25077_001) and median year structure built (B25035_001)
# are carried through to build_recurrent_panel.R (ln_home_value,
# median_structure_age) -- vintage-suffixed _2010 here, with a _2020 companion
# pulled below; the rest of the tract variables were dropped from the model
# spec and stay 2010-only.
.since1980 <- c(House_2005_Later = 'B25034_002', House_2000_2004 = 'B25034_003',
                House_1990_1999 = 'B25034_004', House_1980_1989 = 'B25034_005')
acs_detail <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                                  state = 'TX', year = 2010,
                                  variables = c(House_Total = 'B25034_001', .since1980,
                                                Median_Home_Value           = 'B25077_001',
                                                Median_Year_Structure_Built = 'B25035_001'))
acs_detail <- dcast(data.table(acs_detail), GEOID ~ variable, value.var = 'estimate')
acs_detail[, Perc_Houses_Since1980 := 100 * rowSums(.SD, na.rm = TRUE) / House_Total,
           .SDcols = names(.since1980)]

tract_demos <- merge(acs_subject,
                     acs_detail[, .(GEOID, Perc_Houses_Since1980,
                                    Median_Home_Value_2010           = Median_Home_Value,
                                    Median_Year_Structure_Built_2010 = Median_Year_Structure_Built)],
                     by = 'GEOID', all = TRUE)
tx_tracts <- left_join(tx_tracts, tract_demos, by = 'GEOID')
tx_tracts <- st_make_valid(st_transform(tx_tracts, st_crs(albersNA)))

# --- Tract demographics, second vintage: 2016-2020 ACS 5-year ------------------
# Same two model variables on the 2020 tract lines (tract boundaries changed
# substantially in 2020, so this vintage needs its own geometry + overlay).
# 2020+ cb files carry GEOID directly -- no GEO_ID prefix strip needed.
tx_tracts20 <- tigris::tracts(state = 'TX', cb = TRUE, year = 2020)
acs_2020 <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                                state = 'TX', year = 2020,
                                variables = c(Median_Home_Value_2020           = 'B25077_001',
                                              Median_Year_Structure_Built_2020 = 'B25035_001'))
acs_2020 <- dcast(data.table(acs_2020), GEOID ~ variable, value.var = 'estimate')
# The 2016-2020 files ZERO-code a median year built they cannot compute (the
# 2006-2010 files delivered NA there). 0 is missingness, not a year: left in
# place it gets area-weighted into absurd system medians (built ~100 AD), so
# anything below the 1939 bottom code ("1939 or earlier") becomes NA. The panel
# builder then falls back to the system's 2010 vintage where 2020 is all-NA.
acs_2020[Median_Year_Structure_Built_2020 < 1939, Median_Year_Structure_Built_2020 := NA]
tx_tracts20 <- left_join(tx_tracts20, acs_2020, by = 'GEOID')
tx_tracts20 <- st_make_valid(st_transform(tx_tracts20, st_crs(albersNA)))

# --- Democratic vote share by VTD, biennial vintages 2012-2024 -----------------
# TLC Capitol Data Portal comprehensive election dataset: every general
# election 2012-2024 re-tabulated by the TLC onto the SINGLE 2024 VTD plan
# (9,712 VTDs), so one geometry + one overlay covers all seven cycles.
#   returns:   https://data.capitol.texas.gov/dataset/comprehensive-election-datasets-compressed-format
#   shapefile: https://data.capitol.texas.gov/dataset/vtds  (VTDs_24PG)
# Raw zip + extracted files live in bosquebox (input/vtd_elections_tlc/),
# downloaded once -- additive-scrape convention, nothing re-fetched here.
# (Replaces the static 2000s Harvard EDA / princeton-geojson average.)
vtd_dir <- raw_input('vtd_elections_tlc')
vtds <- st_read(file.path(vtd_dir, 'VTDs_24PG'), quiet = TRUE)
vtds <- st_make_valid(st_transform(vtds, st_crs(albersNA)))

# Top-of-ticket Dem share per VTD per cycle: President in presidential years;
# mean of Governor and U.S. Sen in midterms (the same statewide races the old
# GOV/USP averaging used). Share = D votes / all votes cast in the race.
vote_years <- seq(2012, 2024, by = 2)
dem_by_year <- rbindlist(lapply(vote_years, function(y) {
  ret <- fread(file.path(vtd_dir, sprintf('%d_General_Election_Returns.csv', y)))
  races <- if ('President' %in% ret$Office) 'President' else c('Governor', 'U.S. Sen')
  race_share <- ret[Office %in% races,
                    .(Perc_Dem = 100 * sum(Votes[Party == 'D']) / sum(Votes)),
                    by = .(vtdkeyvalue, Office)]
  race_share[, .(YEAR = y, Perc_Dem = mean(Perc_Dem, na.rm = TRUE)), by = vtdkeyvalue]
}))
dem_wide <- dcast(dem_by_year, vtdkeyvalue ~ YEAR, value.var = 'Perc_Dem')
setnames(dem_wide, as.character(vote_years), paste0('Perc_Dem_', vote_years))
vtds <- left_join(vtds, dem_wide, by = c('VTDKEY' = 'vtdkeyvalue'))

# --- Area-weighted aggregation to PWS service areas ----------------------------
# One shared overlay (spatial_helpers::area_overlay) instead of three hand-rolled
# st_intersection blocks. Weight = intersection area / PWS area (denom = "x"),
# the M/R convention; weighted.mean() renormalizes partial coverage.
pws_weighted <- function(units, unit_id, vars) {
  w    <- area_overlay(twd_boundaries, units, x_id = 'PWS_ID', y_id = unit_id,
                       prop_out = 'w', denom = 'x')
  vals <- data.table(st_drop_geometry(units))[, c(unit_id, vars), with = FALSE]
  merge(w, vals, by = unit_id)[, lapply(.SD, weighted.mean, w = w, na.rm = TRUE),
                               by = .(PWS_ID), .SDcols = vars]
}

pws_blocks_dt <- pws_weighted(tx_blocks, 'GEOID',
                              c('Perc_Hispanic', 'Perc_Black', 'Perc_Rural'))
pws_tracts_dt <- pws_weighted(tx_tracts, 'GEOID',
                              c('Perc_Bachelors', 'Med_Household_Income',
                                'Perc_Houses_Since1980', 'Perc_Under_Poverty_Line',
                                'Median_Home_Value_2010', 'Median_Year_Structure_Built_2010'))
pws_tracts20_dt <- pws_weighted(tx_tracts20, 'GEOID',
                                c('Median_Home_Value_2020', 'Median_Year_Structure_Built_2020'))
pws_vote_dt     <- pws_weighted(vtds, 'VTDKEY', paste0('Perc_Dem_', vote_years))

pws_demos_dt <- Reduce(function(a, b) merge(a, b, by = 'PWS_ID'),
                       list(pws_blocks_dt, pws_tracts_dt, pws_tracts20_dt, pws_vote_dt))
saveRDS(pws_demos_dt, committed('pws_demos_MR.RDS'))
