# =============================================================================
# 04_combine_pws_with_tracts_counties.R
# -----------------------------------------------------------------------------
# Spatial overlays of PWS service-area boundaries against census tracts (2010 &
# 2020 vintages) and counties. Outputs, per PWS:
#   input/pws_tract_overlaps.RDS   list(tracts_2010, tracts_2020); each has
#                                  PWS_ID, GEOID10, Prop_Of_Tract
#   input/pws_county_overlaps.RDS  PWS_ID, CFIPS, Prop_Over_County
# Boundary/tract/county loading and the area-proportion overlay are shared with
# 05_combine via spatial_helpers.R (load_tx_tracts/load_tx_counties/area_overlay).
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)     # %>% / rename
})

#https://www3.twdb.texas.gov/apps/waterserviceboundaries
pws_boundaries = st_read(spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp"))
pws_boundaries = pws_boundaries %>% rename(PWS_ID = PWSId, PWS_NAME = pwsName)
pws_boundaries = st_make_valid(st_transform(pws_boundaries, st_crs(albersNA)))

tx_tracts2010 = load_tx_tracts(2010)
tx_tracts2020 = load_tx_tracts(2020)
tx_county     = load_tx_counties()

# --- Tract overlaps (proportion of each tract covered by the PWS) -------------
# 2010 tracts carry GEOID10; 2020 tracts carry GEOID — both emitted as GEOID10
# to keep a single downstream schema.
tract_overs <- list(
  tracts_2010 = area_overlay(pws_boundaries, tx_tracts2010, "PWS_ID", "GEOID10",
                             prop_out = "Prop_Of_Tract"),
  tracts_2020 = area_overlay(pws_boundaries, tx_tracts2020, "PWS_ID", "GEOID",
                             y_out = "GEOID10", prop_out = "Prop_Of_Tract"))
saveRDS(tract_overs, committed("pws_tract_overlaps.RDS"))

# --- County overlaps (proportion of each PWS that falls in the county) ---------
county_overs = area_overlay(pws_boundaries, tx_county, "PWS_ID", "GEOID",
                            y_out = "CFIPS", prop_out = "Prop_Over_County",
                            denom = "x")
county_overs[, Prop_Over_County := round(Prop_Over_County, 2)]
county_overs = county_overs[Prop_Over_County > 0]
saveRDS(county_overs, committed("pws_county_overlaps.RDS"))
