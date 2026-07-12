# =============================================================================
# 02_combine_pws_with_counties.R
# -----------------------------------------------------------------------------
# Spatial overlay of PWS service-area boundaries against counties. Output, per PWS:
#   input/pws_county_overlaps.RDS  PWS_ID, CFIPS, Prop_Over_County
# This must run BEFORE 03_combine_district_and_drought.R, which reads
# pws_county_overlaps.RDS to map weekly county DSCI onto each PWS.
# Boundary/county loading and the area-proportion overlay come from
# spatial_helpers.R (load_tx_counties/area_overlay).
#
# NOTE: the PWS<->census-tract overlay (pws_tract_overlaps.RDS) that used to live
# here was dropped — nothing consumed it. PWS demographics are area-weighted
# directly in 05_census_block_PWS.R.
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
})

#https://www3.twdb.texas.gov/apps/waterserviceboundaries
pws_boundaries = load_pws_boundaries()   # spatial_helpers.R; shared with 01_combine/05

tx_county     = load_tx_counties()

# --- County overlaps (proportion of each PWS that falls in the county) ---------
county_overs = area_overlay(pws_boundaries, tx_county, "PWS_ID", "GEOID",
                            y_out = "CFIPS", prop_out = "Prop_Over_County",
                            denom = "x")
county_overs[, Prop_Over_County := round(Prop_Over_County, 2)]
county_overs = county_overs[Prop_Over_County > 0]
saveRDS(county_overs, committed("pws_county_overlaps.RDS"))
