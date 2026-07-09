# =============================================================================
# 05_combine_districts_tracts_counties.R
# -----------------------------------------------------------------------------
# District-level analogue of 04_combine. Builds a district boundary layer by
# dissolving the TWDB PWS shapefile to District_ID (via id_crosswalk) and adding
# TCEQ water-district polygons for districts the TWDB layer doesn't cover, then
# overlays it against census tracts (2010 & 2020) and counties. Outputs:
#   input/district_tract_overlaps.RDS   list(tracts_2010, tracts_2020); each has
#                                       District_ID, GEOID10, Prop_Of_Tract
#   input/district_county_overlaps.RDS  District_ID, CFIPS, Prop_Over_County
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)     # %>% / rename / group_by / summarise
})

id_crosswalk <- readRDS(committed("id_crosswalk.RDS"))

# --- District boundaries: dissolve the TWDB PWS shapefile to District_ID -------
twd_boundaries = st_read(spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp"))
twd_boundaries = twd_boundaries %>% rename(PWS_ID = PWSId, PWS_NAME = pwsName)
twd_boundaries = st_make_valid(st_transform(twd_boundaries, st_crs(albersNA)))
twd_boundaries$District_ID <- id_crosswalk$District_ID[match(twd_boundaries$PWS_ID, id_crosswalk$PWS_ID)]
twd_boundaries <- twd_boundaries[!is.na(twd_boundaries$District_ID), ]
twd_boundaries_combo <- twd_boundaries %>%
  st_set_precision(10000) %>%
  group_by(District_ID) %>%
  summarise()

# TCEQ water-district polygons: add districts not covered by the TWDB shapefile.
tceq_geojson = 'https://opendata.arcgis.com/datasets/e7f6dd0a88c046fba1f54d440941a061_0.geojson'
wd = st_make_valid(st_read(tceq_geojson))
wd = st_transform(wd, st_crs(albersNA))
wd$District_ID <- as.character(wd$DISTRICT_ID)
wd_simple <- wd[!wd$District_ID %in% twd_boundaries_combo$District_ID, "District_ID"]
wd_simple <- wd_simple[!duplicated(wd_simple$District_ID), ]
district_sf = rbind(twd_boundaries_combo, wd_simple)
district_sf$total_area <- st_area(district_sf)
district_sf <- district_sf[st_is_valid(district_sf), ]

tx_tracts2010 = load_tx_tracts(2010)
tx_tracts2020 = load_tx_tracts(2020)
tx_county     = load_tx_counties()

# --- Tract overlaps (2010 tracts carry GEOID10; 2020 carry GEOID) -------------
tract_overs <- list(
  tracts_2010 = area_overlay(district_sf, tx_tracts2010, "District_ID", "GEOID10",
                             prop_out = "Prop_Of_Tract"),
  tracts_2020 = area_overlay(district_sf, tx_tracts2020, "District_ID", "GEOID",
                             y_out = "GEOID10", prop_out = "Prop_Of_Tract"))
saveRDS(tract_overs, committed("district_tract_overlaps.RDS"))

# --- County overlaps ----------------------------------------------------------
# Uses the (undissolved) twd_boundaries layer, matching the prior output: county
# shares are computed only for TWDB-shapefile districts, not the geojson-only add-ins.
county_overs = area_overlay(twd_boundaries, tx_county, "District_ID", "GEOID",
                            y_out = "CFIPS", prop_out = "Prop_Over_County",
                            denom = "x")
county_overs[, Prop_Over_County := round(Prop_Over_County, 2)]
county_overs = county_overs[Prop_Over_County > 0]
saveRDS(county_overs, committed("district_county_overlaps.RDS"))
