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
#   * voting-precinct level: Democratic vote share (Harvard EDA / UT geodata)
#
# Needs a Census API key: export CENSUS_API_KEY, or put the key in a
# `census_api_key` file one directory above the bosque repo root.
# Writes input/pws_demos_MR.RDS, consumed by 02_model/build_recurrent_panel.R
# (which needs Median_Home_Value and Median_Year_Structure_Built in particular).
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
  library(foreign)   # read.dta (precinct votes)
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
# Same TCEQ service-area shapefile the county overlay (01_combine/02) uses.
# https://tceq.maps.arcgis.com/apps/webappviewer/index.html?id=04bbf8b322b34d8abaea7b06996d3775
twd_boundaries <- st_read(spatial('Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp'), quiet = TRUE)
twd_boundaries <- twd_boundaries %>% rename(PWS_ID = PWSId, PWS_NAME = pwsName)
twd_boundaries <- st_make_valid(st_transform(st_make_valid(twd_boundaries), st_crs(albersNA)))
twd_boundaries <- twd_boundaries %>% group_by(PWS_ID) %>% summarise()

# --- Block-group demographics: 2010 decennial SF1 ------------------------------
# % hispanic
hispanic <- tidycensus::get_decennial(geography = 'block group',
                                      state = 'TX', year = 2010, variables = c('P004001', 'P004003'))
hispanic <- dcast(data.table(hispanic), GEOID ~ variable, value.var = 'value')
hispanic$Perc_Hispanic <- 100 * (hispanic$P004003 / hispanic$P004001)

# % black
black <- tidycensus::get_decennial(geography = 'block group',
                                   state = 'TX', year = 2010,
                                   variables = c('P010001', 'P010004', 'P010011', 'P010016',
                                                 'P010017', 'P010018', 'P010019'))
black <- data.table(black)
black$is_total <- ifelse((black$variable == 'P010001'), 'total', 'sub')
black <- dcast(black[, sum(value, na.rm = T), by = .(GEOID, is_total)], GEOID ~ is_total, value.var = 'V1')
black$Perc_Black <- 100 * (black$sub / black$total)

# % rural
rural <- tidycensus::get_decennial(geography = 'block group',
                                   state = 'TX', year = 2010,
                                   variables = c('H002001', 'H002005'))
rural <- dcast(data.table(rural), GEOID ~ variable, value.var = 'value')
rural$Perc_Rural <- 100 * (rural$H002005 / rural$H002001)

block_demos <- Reduce(function(a, b) merge(a, b, by = 'GEOID', all = TRUE),
                      list(hispanic[, .(GEOID, Perc_Hispanic)],
                           black[,    .(GEOID, Perc_Black)],
                           rural[,    .(GEOID, Perc_Rural)]))

tx_blocks <- tigris::block_groups(state = 'TX', cb = TRUE, year = 2010)
tx_blocks$GEOID <- str_remove(tx_blocks$GEO_ID, '^1500000US')
tx_blocks <- st_make_valid(st_transform(tx_blocks, st_crs(albersNA)))
tx_blocks <- left_join(tx_blocks, block_demos, by = 'GEOID')

# --- Tract demographics: 2006-2010 ACS 5-year ----------------------------------
tx_tracts <- tigris::tracts(state = 'TX', cb = TRUE, year = 2010)
tx_tracts$GEOID <- str_remove(tx_tracts$GEO_ID, '^1400000US')

bach <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                            state = 'TX', year = 2010,
                            variables = c('S1501_C01_015E'))
setnames(bach, c('estimate'), c('Perc_Bachelors'))
bach <- data.table(bach)
tx_tracts <- left_join(tx_tracts, bach[, .(GEOID, Perc_Bachelors)])

income <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                              state = 'TX', year = 2010,
                              variables = c('S1903_C02_001E'))
setnames(income, c('estimate'), c('Med_Household_Income'))
income <- data.table(income)
tx_tracts <- left_join(tx_tracts, income[, .(GEOID, Med_Household_Income)])

house <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                             state = 'TX', year = 2010,
                             variables = c("B25034_001",
                                           "B25034_002",
                                           "B25034_003",
                                           "B25034_004",
                                           "B25034_005"))
house_tots <- house %>% mutate(cat = ifelse(variable == 'B25034_001', 'total', 'since 1980')) %>%
  group_by(GEOID, cat) %>% summarise(value = sum(estimate, na.rm = T))
house_dt <- dcast(data.table(house_tots), GEOID ~ cat, value.var = 'value') %>%
  mutate(Perc_Houses_Since1980 = 100 * `since 1980` / total)
tx_tracts <- left_join(tx_tracts, house_dt[, .(GEOID, Perc_Houses_Since1980)])

poverty <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                               state = 'TX', year = 2010,
                               variables = c("S1702_C02_001E"))
setnames(poverty, 'estimate', 'Perc_Under_Poverty_Line')
poverty <- data.table(poverty)
tx_tracts <- left_join(tx_tracts, poverty[, .(GEOID, Perc_Under_Poverty_Line)])

# Median home value (B25077_001) and median year structure built (B25035_001).
# Carried through to build_recurrent_panel.R (ln_home_value, median_structure_age).
home_value <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                                  state = 'TX', year = 2010,
                                  variables = c("B25077_001"))
setnames(home_value, 'estimate', 'Median_Home_Value')
home_value <- data.table(home_value)
tx_tracts <- left_join(tx_tracts, home_value[, .(GEOID, Median_Home_Value)])

year_built <- tidycensus::get_acs(geography = 'tract', survey = 'acs5',
                                  state = 'TX', year = 2010,
                                  variables = c("B25035_001"))
setnames(year_built, 'estimate', 'Median_Year_Structure_Built')
year_built <- data.table(year_built)
tx_tracts <- left_join(tx_tracts, year_built[, .(GEOID, Median_Year_Structure_Built)])

tx_tracts <- st_make_valid(st_transform(tx_tracts, st_crs(albersNA)))

# --- Democratic vote share by precinct (not from census) -----------------------
# https://dataverse.harvard.edu/dataverse/eda
# https://geodata.lib.utexas.edu/catalog/princeton-ww72bg01w
precincts <- st_read(spatial('princeton-ww72bg01w-geojson.json'), quiet = TRUE)
precincts$COUNTY_VTD <- paste0('48', precincts$COUNTYFP10, '_', precincts$VTDST10)
elections <- list.files(committed('precinct_votes'), full.names = TRUE)
elects <- rbindlist(lapply(elections, read.dta), fill = T, use.names = T)
elects$CFIPS <- paste0('48', formatC(elects$fips, width = 3, flag = '0'))
elects$COUNTY_VTD <- paste(elects$CFIPS, elects$vtd, sep = '_')

elect_dt <- data.table(elects %>% dplyr::select(fips, COUNTY_VTD, contains('GOV'), contains('USP')))

elect_dt2 <- melt(elect_dt, id.vars = c('fips', 'COUNTY_VTD')) %>%
  filter(grepl('dv$|tv$', variable)) %>%
  mutate(YEAR = str_extract(variable, '[0-9]{4}')) %>%
  mutate(variable = str_extract(variable, '(GOV|USP)_(tv|dv)')) %>%
  filter(!is.na(value))

elect_dt2 <- data.table(elect_dt2)
dem_vote <- dcast(elect_dt2, COUNTY_VTD + fips + YEAR ~ variable, value.var = 'value') %>%
  mutate(tv = ifelse(is.na(USP_tv), GOV_tv, USP_tv),
         dv = ifelse(is.na(USP_dv), GOV_dv, USP_dv)) %>%
  mutate(Perc_Dem = 100 * dv / tv)
dem_vote_share <- dem_vote[, mean(Perc_Dem, na.rm = T), by = .(COUNTY_VTD)]
setnames(dem_vote_share, 'V1', 'Perc_Dem_Vote_Share')
precincts <- left_join(precincts, dem_vote_share)
precincts <- st_make_valid(st_transform(precincts, st_crs(albersNA)))

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
                                'Median_Home_Value', 'Median_Year_Structure_Built'))
pws_vote_dt   <- pws_weighted(precincts, 'COUNTY_VTD', 'Perc_Dem_Vote_Share')

pws_demos_dt <- Reduce(function(a, b) merge(a, b, by = 'PWS_ID'),
                       list(pws_blocks_dt, pws_tracts_dt, pws_vote_dt))
saveRDS(pws_demos_dt, committed('pws_demos_MR.RDS'))
