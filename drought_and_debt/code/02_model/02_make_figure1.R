# =============================================================================
# 02_make_figure1.R  —  descriptive figures
# -----------------------------------------------------------------------------
# Figure 1 (output/figure1.png):
#   A (bottom): weekly frequency of mandatory water-use restriction events
#      (every M1/M2/M3 notice, the recurrent event set), binned by week to
#      align with the drought series above.
#   B (top): statewide % of land area in drought category D0-D4, from the UNL
#      state statistics API. The series is cached to input/statewide_drought_area.RDS
#      and re-fetched ONLY when no cache exists or the cache does not cover the
#      analysis window, so default runs rebuild offline.
# Figure 2 (output/figure2.png): timing of first mandatory water-use
#   restriction adoption, split by district linkage (the fiscal subsample).
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(ggthemes)
  library(stringr)
  library(gridExtra)
})

# --- Figure 1A: weekly frequency of mandatory restriction events --------------
# Bottom panel mirrors the top panel's weekly x-axis: a frequency plot of
# mandatory (STAGE M1/M2/M3) restriction notices binned by week. This is a
# recurrent-events model, so EVERY distinct (PWS_ID, notice-date) event counts,
# not just each system's first — the exact event set the model panel consumes
# (mandatory_restriction_events, shared with build_recurrent_panel.R), so the
# event timing lines up week-for-week against the drought conditions above.
# Restricted to the FOCAL SAMPLE: district-linked systems (present in
# id_crosswalk.RDS = the sampled districts). Figure 2 keeps the two-series split.
events <- mandatory_restriction_events(start_date, end_date)
xw <- data.table(readRDS(committed('id_crosswalk.RDS')))
events <- events[PWS_ID %in% xw$PWS_ID]

year_span <- paste(year(start_date), 'to', year(end_date))
figure1A <- ggplot(data = events, aes(x = event_date)) +
  geom_freqpoly(binwidth = 7) +
  scale_y_continuous(name = '# of restriction events', expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = 'Times'), axis.title = element_text(size = 12)) +
  ggtitle(paste0('Mandatory restriction events by week, water districts, ', year_span)) +
  scale_x_date(expand = c(0, 0), name = 'Week',
               limits = c(start_date, end_date))

# --- Figure 1B: statewide % area in drought (UNL state statistics) ------------
# Committed cache first; the historical series never changes, so hit the live
# API ONLY when the cache is absent or does not cover the analysis window
# (i.e. the window was extended). NOT keyed on RESCRAPE: a default model-only
# run must stay offline and must not rewrite the committed input.
.statewide_cache <- committed('statewide_drought_area.RDS')
.unl_url <- paste0(
  'https://usdmdataservices.unl.edu/api/StateStatistics/',
  'GetDroughtSeverityStatisticsByAreaPercent?aoi=48&dx=1',
  '&DxLevelThresholdFrom=10&DxLevelThresholdTo=70',
  '&startdate=1/1/', year(start_date),
  '&enddate=', format(end_date, '%m/%d/%Y'),
  '&statisticsType=1')
dcols <- c('D0', 'D1', 'D2', 'D3', 'D4')
.cache_covers <- FALSE
if (file.exists(.statewide_cache)) {
  txc <- readRDS(.statewide_cache)
  .rng <- suppressWarnings(range(ymd(txc$ValidStart), na.rm = TRUE))
  .cache_covers <- !anyNA(.rng) && .rng[1] <= start_date + 7 && .rng[2] >= end_date - 7
  if (!.cache_covers)
    message('Cached statewide drought series does not cover the analysis window; refetching.')
}
if (!.cache_covers) {
  # The UNL endpoint serves CSV by default (its JSON variant now lowercases
  # the field names), so fread() it. Validate the schema BEFORE caching: a
  # 200-status response in an unexpected format (e.g. the endpoint switching
  # back to JSON) must not be written to the cache, where it would poison
  # every later offline rebuild.
  .unl_cols <- c('StatisticFormatID', 'ValidStart', dcols)
  txc <- tryCatch(fread(.unl_url), error = function(e) {
    message('UNL state-statistics fetch failed: ', conditionMessage(e))
    NULL
  })
  if (!is.null(txc) && !all(.unl_cols %in% names(txc))) {
    message('UNL state-statistics response is missing expected column(s) ',
            paste(setdiff(.unl_cols, names(txc)), collapse = ', '),
            ' — treating as a failed fetch (not cached).')
    txc <- NULL
  }
  if (is.null(txc)) {
    if (!file.exists(.statewide_cache))
      stop('No statewide drought series: UNL API unreachable and no cache at ',
           .statewide_cache)
    message('Reusing cached ', basename(.statewide_cache), '.')
    txc <- readRDS(.statewide_cache)
  } else {
    saveRDS(txc, .statewide_cache)
  }
}

txc <- as.data.table(txc)
txc[, (dcols) := lapply(.SD, as.numeric), .SDcols = dcols]
txc <- txc[StatisticFormatID == 1]
txc[, ValidStart := ymd(ValidStart)]
txc <- txc[ValidStart >= start_date & ValidStart <= end_date]

rib_cols <- tableau_color_pal(type = 'ordered-sequential', palette = 'Classic Orange')(7)[c(1, 2, 3, 5, 6)]
figure1B <- ggplot(data = txc, aes(x = ValidStart)) +
  geom_ribbon(aes(ymin = 0, ymax = D0, fill = rib_cols[1])) +
  geom_ribbon(aes(ymin = 0, ymax = D1, fill = rib_cols[2])) +
  geom_ribbon(aes(ymin = 0, ymax = D2, fill = rib_cols[3])) +
  geom_ribbon(aes(ymin = 0, ymax = D3, fill = rib_cols[4])) +
  geom_ribbon(aes(ymin = 0, ymax = D4, fill = rib_cols[5])) +
  theme_bw() + scale_y_continuous(name = '% of land area in status', expand = c(0, 0)) +
  scale_x_date(name = 'Weekly drought status', expand = c(0, 0)) +
  theme(text = element_text(family = 'Times'), legend.position = c(0.55, 0.6),
        axis.title = element_text(size = 12),
        legend.background = element_rect(fill = alpha('white', 0.5))) +
  ggtitle(paste0('Statewide (TX) drought conditions, ', year_span)) +
  # breaks= is REQUIRED here: without it scale_fill_identity sorts the breaks
  # alphabetically by hex string, so the labels land on the wrong ribbons.
  # rev(rib_cols) puts the darkest band (D4) first, matching the label order.
  scale_fill_identity(breaks = rev(rib_cols),
                      labels = c('D4', 'D3-D4', 'D2-D4', 'D1-D4', 'D0-D4'),
                      guide = 'legend', name = 'Category')

grob <- grid.arrange(figure1B, figure1A, ncol = 1)
ggsave(grob, filename = output('figure1.png'), width = 7, height = 6, units = 'in', dpi = 400)

# -----------------------------------------------------------------------------
# Figure 2 — timing of first mandatory water-use restriction adoption
# -----------------------------------------------------------------------------
# Moved here from the retired make_eh_data.R (now in explore/) and rebuilt from committed
# data only. One observation per system = the date of its FIRST mandatory
# (STAGE M1/M2/M3) restriction notice; a monthly frequency polygon of those dates.
# The event definition is the SAME one the model panel uses
# (ingest_helpers.R::mandatory_restriction_events, shared with
# build_recurrent_panel.R), so the figure describes the analysis sample.
# The two series split systems by whether they are district-linked (present in
# id_crosswalk.RDS) — i.e. the fiscal-analysis subsample — versus not. (The
# original split on District_Type == 'MUD'; district-linkage is the current
# pipeline's sample definition and needs no external roster.)
mand <- mandatory_restriction_events(start_date, end_date)
first_adopt <- mand[, .(adopt_date = min(event_date)), by = PWS_ID]

xw <- data.table(readRDS(committed('id_crosswalk.RDS')))
first_adopt[, In_Sample := (PWS_ID %in% xw$PWS_ID) + 0L]

figure2 <- ggplot(first_adopt) +
  geom_freqpoly(aes(x = adopt_date, colour = as.factor(In_Sample)), binwidth = 30) +
  scale_x_date(name = 'Month of restriction adoption', expand = c(0,0)) +
  scale_y_continuous(name = '# of restrictions adopted', expand = c(0,0)) +
  ggtitle('First observed mandatory water use restriction adoption') +
  scale_color_tableau(name = 'Water systems',
                      labels = c('Not district-linked','District-linked')) +
  theme_bw() +
  theme(legend.position = c(0.8,0.5),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
ggsave(figure2, filename = output('figure2.png'), dpi = 500, width = 6, height = 4, units = 'in')
