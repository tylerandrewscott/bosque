# =============================================================================
# spatial_helpers.R  —  modern spatial stack for the drought_and_debt pipeline
# -----------------------------------------------------------------------------
# Replaces the retired rgeos / rgdal / maptools / sp packages (archived from
# CRAN in Oct 2023) with terra + sf, which install and run on current R.
#
# The pipeline's vector overlays are already written in sf (st_read,
# st_intersection, st_transform); those stay. The ONLY place the retired
# packages were actually used was reprojecting a polygon layer via
# spTransform()/CRS() before feeding it to spdep::poly2nb(). That idiom is
# replaced by reproject_sf() below, which uses terra as the reprojection
# engine. spdep::poly2nb() accepts sf polygons directly, so no Spatial* objects
# are needed anywhere.
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(spdep)   # poly2nb / nb2INLA — current, not retired
  library(tigris)
})

# Texas-centered Albers Equal Area (NAD83). Defined here so any script sourcing
# spatial_helpers.R has it, even when config.R is not loaded. config.R also sets
# it (identical value) as the documented central location.
if (!exists("albersNA")) {
  albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
}

# --- reproject_sf() -----------------------------------------------------------
# Reproject an sf object to a target CRS using terra as the engine.
#   x   : an sf / sfc object (or anything terra::vect() accepts)
#   crs : proj string, WKT, or EPSG code (e.g. albersNA, "EPSG:5070", 5070)
# Returns an sf object in the target CRS.
reproject_sf <- function(x, crs) {
  if (inherits(x, "SpatVector")) {
    return(sf::st_as_sf(terra::project(x, as.character(crs))))
  }
  v  <- terra::vect(x)                       # sf -> SpatVector
  vp <- terra::project(v, as.character(crs)) # terra reprojection
  sf::st_as_sf(vp)                           # back to sf for downstream sf/spdep code
}

# --- tx_county_adjacency() ----------------------------------------------------
# Build the Texas county layer, reproject it (terra), and write the INLA
# adjacency graph used by the BESAG spatial random effect. Collapses the
# 3–4 line idiom that was duplicated across the modeling scripts.
#   crs   : target CRS (default albersNA if defined in the calling env)
#   queen : contiguity rule for poly2nb (FALSE = rook, matches most scripts;
#           fit_joint_model.R used TRUE)
#   year  : vintage passed to tigris::counties() (NULL = tigris default)
#   adj_file : path for the INLA adjacency file (default "tx.adj" in wd)
# Returns the reprojected sf county layer (also writes adj_file as a side effect).
tx_county_adjacency <- function(crs = if (exists("albersNA")) albersNA else NULL,
                                 queen = FALSE, year = NULL, adj_file = "tx.adj") {
  stopifnot(!is.null(crs))
  tx_county_sp <- tigris::counties(state = 48, year = year, class = "sf",
                                   progress_bar = FALSE)
  tx_county_sp <- reproject_sf(tx_county_sp, crs)
  tx.nb <- spdep::poly2nb(tx_county_sp, row.names = tx_county_sp$GEOID, queen = queen)
  spdep::nb2INLA(adj_file, tx.nb)
  tx_county_sp
}

# --- Boundary / census layer loaders -----------------------------------------
# Load a TX census-tract layer for `year`, reprojected to `crs` and made valid.
# (2010 tracts carry a GEOID10 id column; 2020 tracts carry GEOID.)
load_tx_tracts <- function(year, crs = albersNA) {
  t <- tigris::tracts(state = "TX", class = "sf", year = year, progress_bar = FALSE)
  sf::st_make_valid(sf::st_transform(t, sf::st_crs(crs)))
}

# Load the TX county layer, reprojected to `crs` and made valid.
load_tx_counties <- function(crs = albersNA) {
  cty <- tigris::counties(state = "TX", class = "sf", progress_bar = FALSE)
  sf::st_make_valid(sf::st_transform(cty, sf::st_crs(crs)))
}

# Load the TCEQ PWS service-area boundary shapefile (via the spatial() path
# helper from config.R), rename to the pipeline's PWS_ID/PWS_NAME schema, and
# reproject + make valid ONCE. Shared by 01_combine/02 (county overlay) and
# 01_combine/05 (census demographics), which had duplicated this block.
load_pws_boundaries <- function(crs = albersNA) {
  b <- sf::st_read(spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp"),
                   quiet = TRUE)
  names(b)[names(b) == "PWSId"]   <- "PWS_ID"
  names(b)[names(b) == "pwsName"] <- "PWS_NAME"
  sf::st_make_valid(sf::st_transform(b, sf::st_crs(crs)))
}

# --- area_overlay() -----------------------------------------------------------
# Intersect polygon layer `x` with `y` and return a data.table of area
# proportions. Collapses the ~15-line st_intersection()/st_area()/match() block
# that 01_combine/04 and /05 each duplicated for tracts and counties.
#   denom = "y": prop = intersection_area / area(y)  -> share of each y feature
#                covered by x (used for tract overlays)
#   denom = "x": prop = intersection_area / area(x)  -> share of each x feature
#                that falls in y (used for county overlays; matches the prior
#                first-match-per-id behavior when x has repeated ids)
# Output columns are named via x_out / y_out / prop_out so callers keep their
# historical schema — e.g. a 2020 tract id (source column GEOID) can still be
# emitted as "GEOID10" to match the 2010 output.
area_overlay <- function(x, y, x_id, y_id, x_out = x_id, y_out = y_id,
                         prop_out = "Prop", denom = c("y", "x")) {
  denom <- match.arg(denom)
  inter <- sf::st_intersection(x, y)
  denom_area <- if (denom == "y")
      sf::st_area(y)[match(inter[[y_id]], y[[y_id]])]
    else
      sf::st_area(x)[match(inter[[x_id]], x[[x_id]])]
  dt <- data.table::data.table(
      .x = inter[[x_id]], .y = inter[[y_id]],
      .p = as.numeric(sf::st_area(inter) / denom_area))
  data.table::setnames(dt, c(".x", ".y", ".p"), c(x_out, y_out, prop_out))
  dt[]
}
