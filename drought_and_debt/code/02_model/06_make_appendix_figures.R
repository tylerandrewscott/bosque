# =============================================================================
# 02_model/06_make_appendix_figures.R  —  appendix figures from the Model 1 fit
# -----------------------------------------------------------------------------
# Two appendix figures read off the FITTED Bayesian Model 1 (no refitting here;
# the slim git-tracked fit in output/ is enough — slim_inla() keeps
# summary.random):
#
#   A (output/baseline_hazard_appendix.png)
#      The estimated RW1 baseline hazard: posterior mean + credible ribbon,
#      drawn as the piecewise-constant step function INLA actually fits
#      (N_HAZARD_INTERVALS knots on the week scale), overlaid on the panel's
#      drought exposure (mean DSCI across the Model 1 risk set, by week) on a
#      secondary axis. The hazard is exponentiated, so the y-axis is the
#      baseline hazard RELATIVE to the sample average (the RW1 is sum-to-zero
#      constrained; the intercept carries the level).
#
#   B (output/frailty_map_appendix.png)
#      Choropleth of the water districts that have boundary geometry, shaded by
#      the posterior mean of their Model 1 shared frailty (the iid cluster
#      random effect, log-hazard scale), over a Texas county basemap. District
#      boundaries = the TWDB PWS service-area shapefile dissolved to
#      District_ID via id_crosswalk, plus TCEQ water-district polygons for
#      districts the TWDB layer misses (same construction as
#      explore/combine_districts_tracts_counties.R). Districts in the model but
#      with no polygon in either source are simply not drawn.
#
# The frailty indices saved with the fit are bare integers (cluster_idx), so the
# panel is rebuilt (build_recurrent_panel.R is idempotent) and the fit-script's
# cluster_key -> cluster_idx construction is repeated VERBATIM to map them back
# to districts; the fit's stored panel fingerprint is checked against the
# rebuilt panel so a stale fit fails loudly instead of mislabeling frailties.
#
# RECONSTRUCTION OVERRIDE: when the fit predates the current committed inputs
# (updates are batched; refits are triggered explicitly), the rebuild-and-verify
# path above is impossible from the current tree. Supplying BOTH
#   appendix_cluster_map   data.table(cluster_idx, cluster_key)
#   appendix_dsci_weekly   data.table(tstart, DSCI)
# before source()ing skips the rebuild and uses them as the fit-time panel
# derivations — the caller is asserting they were reconstructed from (and
# verified against) the inputs the fit was actually fit on.
#
# Run from the drought_and_debt project root:
#     source("code/02_model/06_make_appendix_figures.R")
# =============================================================================

if (!exists("PROJ_ROOT")) {
  .find_file <- function(f) { p <- Find(file.exists, file.path(c(".", "drought_and_debt", "..", "../.."), f)); if (is.null(p)) f else p }
  source(.find_file("code/config.R"))
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
})

# --- Model 1 fit (slim is sufficient: summary.random survives slim_inla()) ----
.fit_path <- output("recurrent_coxinla_model1_full_slim.RDS")
if (!file.exists(.fit_path)) stop("No Model 1 fit at ", .fit_path,
                                  " — run 01_fit_recurrent_cox_inla.R first.")
m1 <- readRDS(.fit_path)

if (exists("appendix_cluster_map") && exists("appendix_dsci_weekly")) {
  message("Using pre-supplied appendix_cluster_map / appendix_dsci_weekly ",
          "(reconstruction override); skipping the panel rebuild + fingerprint check.")
  cl      <- as.data.table(appendix_cluster_map)
  dsci_wk <- as.data.table(appendix_dsci_weekly)[, .(date = start_date + tstart * 7, DSCI)]
} else {
  source("code/02_model/build_recurrent_panel.R") # -> panel_m1, shared_vars (idempotent)

  # The fit-script's frailty index, repeated verbatim (01_fit_recurrent_cox_inla.R):
  # districts prefixed "D", unaffiliated systems "P"; integer index by first
  # appearance. Identical panel => identical mapping.
  panel_m1[, cluster_key := fifelse(!is.na(District_ID),
                                    paste0("D", District_ID), paste0("P", PWS_ID))]
  panel_m1[, cluster_idx := .GRP, by = cluster_key]

  # Guard: the saved fit must have been fit on THIS panel, or the integer frailty
  # IDs would map to the wrong clusters (and the hazard to a different window).
  .fp_now <- list(n        = nrow(panel_m1),
                  events   = sum(panel_m1$event),
                  clusters = uniqueN(panel_m1$cluster_idx),
                  vars     = sort(shared_vars),
                  window   = as.character(c(start_date, end_date)))
  .fp_fit <- attr(m1, "panel_fp", exact = TRUE)
  if (is.null(.fp_fit)) {
    warning("Model 1 fit carries no panel fingerprint; frailty labels cannot be ",
            "verified against the current panel.")
  } else if (!identical(.fp_fit[names(.fp_now)], .fp_now)) {
    stop("Model 1 fit was fit on a DIFFERENT panel (fingerprint mismatch on: ",
         paste(names(.fp_now)[!mapply(identical, .fp_now, .fp_fit[names(.fp_now)])],
               collapse = ", "),
         ") — refit Model 1, or supply a verified reconstruction (see header).")
  }
  cl <- unique(panel_m1[, .(cluster_idx, cluster_key)])
  # Drought exposure actually faced by the risk set: mean DSCI across the Model 1
  # panel, by week. (DSCI is 0-500; the model covariate is DSCI/100.)
  dsci_wk <- panel_m1[, .(DSCI = mean(DSCI)), by = .(date = start_date + tstart * 7)]
}

# =============================================================================
# Figure A — RW1 baseline hazard vs drought exposure
# =============================================================================
bh <- as.data.table(m1$summary.random$baseline.hazard)
# ID = interval start in weeks since analysis_start; the fitted hazard is
# piecewise constant on (ID_k, ID_{k+1}]. Close the last interval at the window
# end so the step spans the full panel.
setorder(bh, ID)
bh[, `:=`(t0 = ID, t1 = shift(ID, type = "lead",
                              fill = as.numeric(end_date - start_date) / 7))]
# Ribbon bounds at the configured CI_LEVEL (config.R). summary.random only
# carries the quantiles requested at FIT time (the slim fits keep no
# marginals.random to recompute from), so a fit predating a CI_LEVEL change
# falls back to its stored 95% columns until the next refit.
.qcol <- paste0(CI_PROBS, "quant")
ribbon_lab <- CI_LABEL
if (!all(.qcol %in% names(bh))) {
  if (!all(c("0.025quant", "0.975quant") %in% names(bh)))
    stop("Model 1 fit carries neither the ", CI_LABEL,
         " nor the 95% baseline-hazard quantiles -- refit Model 1.")
  warning("Model 1 fit predates CI_LEVEL = ", CI_LEVEL, "; drawing the ",
          "baseline-hazard ribbon at its stored 95% level. Refit to update.")
  .qcol <- c("0.025quant", "0.975quant"); ribbon_lab <- "95%"
}
bh[, `:=`(date0 = start_date + t0 * 7, date1 = start_date + t1 * 7,
          haz   = exp(mean),
          lo    = exp(get(.qcol[1])), hi = exp(get(.qcol[2])))]
# Step-function coordinates: each interval contributes its start and end at the
# same level, INTERLEAVED (start_k, end_k, start_{k+1}, ...) so ribbon + line
# render the piecewise-constant estimate exactly. (A global sort on date would
# scramble the tied knot dates into diagonal ramps.)
bh_step <- bh[, .(date = c(date0, date1), haz, lo, hi), by = .(k = ID)][, k := NULL][]

# Secondary-axis scaling: DSCI (0-500) mapped onto the hazard axis.
.k <- max(bh_step$hi) / 500

figA <- ggplot() +
  geom_area(data = dsci_wk, aes(x = date, y = DSCI * .k),
            fill = "tan1", alpha = 0.45) +
  geom_ribbon(data = bh_step, aes(x = date, ymin = lo, ymax = hi),
              fill = "grey30", alpha = 0.25) +
  geom_line(data = bh_step, aes(x = date, y = haz), colour = "grey15") +
  scale_x_date(name = "Week", expand = c(0, 0)) +
  scale_y_continuous(
    name = paste0("Relative baseline hazard (posterior mean, ", ribbon_lab, " CI)"),
    expand = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(~ . / .k, name = "Mean DSCI across systems (0-500)")) +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.title = element_text(size = 12),
        axis.title.y.right = element_text(colour = "tan3"),
        axis.text.y.right  = element_text(colour = "tan3")) +
  ggtitle(sprintf("Estimated RW1 baseline hazard and drought conditions, %s to %s",
                  format(start_date, "%Y"), format(end_date, "%Y")))
ggsave(figA, filename = output("baseline_hazard_appendix.png"),
       width = 7, height = 4.5, units = "in", dpi = 400)
message("Wrote ", output("baseline_hazard_appendix.png"))

# =============================================================================
# Figure B — district frailty map
# =============================================================================
# Posterior mean frailty per DISTRICT cluster ("D"-prefixed keys; unaffiliated
# "P" system clusters are not mappable district polygons and are left out).
fr <- as.data.table(m1$summary.random$cluster_idx)[cl, on = .(ID = cluster_idx)]
fr <- fr[startsWith(cluster_key, "D"),
         .(District_ID = sub("^D", "", cluster_key), frailty = mean)]

# District boundaries — two constructions, by Box availability:
#   Box readable   -> TWDB PWS service areas dissolved to District_ID, plus TCEQ
#                     water-district polygons for districts the TWDB layer
#                     misses (same construction as explore/
#                     combine_districts_tracts_counties.R; fullest coverage).
#   Box unreadable -> the TCEQ water-district layer alone, fetched from the
#                     opendata.arcgis.com geojson (cached in scratch/). Box
#                     Drive sometimes serves metadata (and small files) but
#                     stalls forever streaming large content, so neither
#                     file.exists() nor a tiny read is a usable probe: the probe
#                     is a bounded-time FULL read of both shapefiles, which
#                     doubles as the download/cache warm-up when Box is healthy.
.box_files <- c(spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.shp"),
                spatial("Service_Area_Boundaries/PWS_shapefile_9-24/PWS_Export.dbf"),
                spatial("water_districts_shp/TCEQ_WaterDistricts.shp"),
                spatial("water_districts_shp/TCEQ_WaterDistricts.dbf"))
.box_ok <- tryCatch(
  suppressWarnings(system2("cat", shQuote(.box_files), stdout = FALSE,
                           stderr = FALSE, timeout = 120)) == 0L,
  error = function(e) FALSE)

.tceq_layer <- function() {
  .gj <- scratch("tceq_water_districts.geojson")
  if (!file.exists(.gj)) {
    message("Fetching TCEQ water-district polygons from opendata.arcgis.com -> ", .gj)
    download.file(paste0("https://opendata.arcgis.com/datasets/",
                         "e7f6dd0a88c046fba1f54d440941a061_0.geojson"),
                  .gj, mode = "wb", quiet = TRUE)
  }
  wd <- st_make_valid(st_transform(st_read(.gj, quiet = TRUE), st_crs(albersNA)))
  wd$District_ID <- as.character(wd$DISTRICT_ID)
  wd
}

if (.box_ok) {
  xw_sp <- as.data.table(readRDS(committed("id_crosswalk.RDS")))
  twd <- load_pws_boundaries()                     # spatial_helpers.R (Box)
  twd$District_ID <- xw_sp$District_ID[match(twd$PWS_ID, xw_sp$PWS_ID)]
  twd <- twd[!is.na(twd$District_ID), ]
  twd <- st_set_precision(twd, 10000)
  twd_dist <- aggregate(twd[, "geometry"], by = list(District_ID = twd$District_ID),
                        FUN = head, n = 1) |> st_as_sf()
  tceq <- st_read(spatial("water_districts_shp/TCEQ_WaterDistricts.shp"), quiet = TRUE)
  tceq <- st_make_valid(st_transform(tceq, st_crs(albersNA)))
  tceq$District_ID <- as.character(tceq$DISTRICT_ID)
  tceq <- tceq[!tceq$District_ID %in% twd_dist$District_ID, "District_ID"]
  tceq <- tceq[!duplicated(tceq$District_ID), ]
  district_sf <- rbind(twd_dist, tceq)
} else {
  message("Box (bosquebox) is not readable — using the TCEQ open-data ",
          "water-district layer alone for boundaries.")
  district_sf <- .tceq_layer()[, "District_ID"]
  district_sf <- district_sf[!duplicated(district_sf$District_ID), ]
}

district_sf <- merge(district_sf, fr, by = "District_ID")
message(sprintf("Frailty map: %d of %d district frailties have boundary geometry.",
                nrow(district_sf), nrow(fr)))

tx <- st_union(load_tx_counties())                 # state outline basemap

figB <- ggplot() +
  geom_sf(data = tx, fill = "grey95", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = district_sf, aes(fill = frailty), colour = NA) +
  scale_fill_gradient2(name = "Posterior mean\nfrailty (log-hazard)",
                       low = "#2166AC", mid = "grey85", high = "#B2182B",
                       midpoint = 0) +
  theme_void() +
  theme(text = element_text(family = "Times"),
        legend.title = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  ggtitle("District shared-frailty estimates (Model 1)")
ggsave(figB, filename = output("frailty_map_appendix.png"),
       width = 6.5, height = 6, units = "in", dpi = 400)
message("Wrote ", output("frailty_map_appendix.png"))
