# =============================================================================
# 07_scrape_source_and_purchases.R
# -----------------------------------------------------------------------------
# Water-source type and wholesale "buys-from" relationships per public water
# system, from the TCEQ Drinking Water Viewer (dwv.tceq.texas.gov) OData API via
# util_code/scraping/dwv_api_helpers.R. Both are treated as TIME-INVARIANT (a
# current snapshot): the DWV API exposes only a current state, and wholesale
# contracts / source infrastructure are sticky, so a single per-system value is
# broadcast across all analysis weeks downstream.
#
# We deliberately use the DWV records (not the TWDB Water Use Survey) because the
# TWDB buyer/seller volumes are messy and only a per-PWS Power BI report now, with
# no clean bulk pull. The DWV "Sources" and "Buys From" records are structured,
# 100% PWS-linkable, and refreshable with the existing client.
#
# Two DWV sources:
#   Source type   -> DashMain.D_FED_PRIM_SRC_CD   (system-level primary source
#                    code: GW / GWP / SW / SWP / GU / GUP -- surface-vs-ground AND
#                    purchased-vs-self in ONE field). The GRND/SURF_WTR*_RATIO
#                    columns are essentially unpopulated in DWV (only a handful of
#                    non-zero statewide), so the CODE SUFFIX -- not the ratios --
#                    is the authoritative purchased/self signal.
#   Emergency src -> DashSourceWater.AVAILABILITY_CODE == 'E' on a supply facility
#                    (well / intake / consecutive-connection interconnect / ...).
#   "Buys From"   -> DashPurchases.SELLERWSNUMBER (the systems each PWS buys from).
#
# Outputs (ONE ROW PER SYSTEM / per edge; written to input/, git-tracked):
#   input/pws_source.RDS          -- PWS_ID, prim_src_cd, source_surface (0/1),
#                                    purchases_water (0/1), emergency_source (0/1).
#                                    One row per system.
#   input/pws_purchase_edges.RDS  -- Buyer, Seller (both trimmed PWS ids),
#                                    Seller_Name. One row per buyer->seller edge.
#
# Consumed by 02_model/build_recurrent_panel.R:
#   * source_surface / purchases_water / emergency_source join into the
#     time-invariant controls (§3).
#   * the edge list builds the time-varying "seller under restriction" covariate
#     (new §2b): 1 in weeks where any of a buyer's fixed sellers has a mandatory
#     restriction notice.
# =============================================================================

# --- Shared config: paths, projection, window, helpers (idempotent) -----------
if (!exists("committed")) source(Find(file.exists, file.path(
  c(".", "..", "../..", "drought_and_debt"), "code", "config.R")))
suppressPackageStartupMessages(library(data.table))
source(util("scraping", "dwv_api_helpers.R"))   # dwv_session/search/widget clients

ses <- dwv_session()

# =============================================================================
# 1. SOURCE TYPE -- one DashMain query over all active community systems
# -----------------------------------------------------------------------------
# DashMain returns every system in one paged pull, so source type costs a single
# query (no per-system widget calls). D_FED_PRIM_SRC_CD carries the SDWIS federal
# primary-source code; we derive two 0/1 flags plus keep the raw code.
# =============================================================================
message("Pulling DashMain source codes for all active community systems...")
systems <- dwv_search(ses, type = "C", active_only = TRUE,
                      select = c("TINWSYS_IS_NUMBER", "NUMBER0", "NAME",
                                 "D_FED_PRIM_SRC_CD"))
systems[, PWS_ID := trimws(NUMBER0)]

src <- unique(systems[!is.na(PWS_ID) & nzchar(PWS_ID),
                      .(PWS_ID, prim_src_cd = trimws(D_FED_PRIM_SRC_CD))], by = "PWS_ID")
# Surface if the code starts with S (SW/SWP); groundwater otherwise (GW/GWP/GU/
# GUP -- GU = groundwater under the influence of surface water, grouped with
# ground). Purchased if the code ends in P (SWP/GWP/GUP).
src[, source_surface  := as.integer(grepl("^S", prim_src_cd))]
src[, purchases_water := as.integer(grepl("P$", prim_src_cd))]
src[prim_src_cd == "" | is.na(prim_src_cd),
    c("source_surface", "purchases_water") := NA_integer_]

message(sprintf("Source codes: %d systems. surface=%d, purchased=%d (missing code=%d).",
                nrow(src), sum(src$source_surface, na.rm = TRUE),
                sum(src$purchases_water, na.rm = TRUE),
                sum(is.na(src$prim_src_cd) | src$prim_src_cd == "")))
print(src[, .N, by = prim_src_cd][order(-N)])   # saved below, after emergency_source is merged in

# =============================================================================
# 2. PER-SYSTEM SCRAPE -- "buys-from" edges (DashPurchases) AND emergency sources
#    (DashSourceWater), in ONE pass (both widgets per system).
# -----------------------------------------------------------------------------
# * DashPurchases.SELLERWSNUMBER -> buyer->seller edges (the network-variable
#   base). SELLERWSNUMBER is a real PWS id, so edges link directly to the event /
#   restriction data. Scraped over ALL systems (not only the *P-coded ones)
#   because emergency / backup interconnects appear as purchases for primarily
#   self-supplied systems.
# * DashSourceWater -> the system's facilities, each with an AVAILABILITY_CODE
#   (SDWIS: E = emergency, P = permanent, I = interim, S = seasonal). A SUPPLY
#   facility (well / intake / spring / consecutive-connection interconnect / ...)
#   marked 'E' sets emergency_source = 1. This is variable (3) -- "emergency
#   interconnects / emergency sources" -- distinct from purchases_water (a
#   PRIMARY purchased supply). DashPurchases carries no availability field, so
#   the emergency designation must come from DashSourceWater's CC/well/intake
#   rows here.
# =============================================================================
SRC_SUPPLY_TYPES <- c("WL", "IN", "SP", "IG", "RC", "RS", "CC", "IE")  # water-source facility types
EMERGENCY_AVAIL  <- "E"                                                 # SDWIS availability: E = emergency

# Gentler pacing knobs. DWV rate-limits aggressively: a too-short pause trips 429s
# and each triggers a 3s exponential-backoff retry inside dwv_get(), so the naive
# 0.05s cadence is *slower* wall-clock than a steady sub-second pause. REQ_PAUSE is
# a base gap applied after EACH widget call (two per system); a small uniform jitter
# de-synchronizes the request train so bursts don't cluster. Override before source().
if (!exists("REQ_PAUSE"))  REQ_PAUSE  <- 0.35   # base seconds between requests
if (!exists("REQ_JITTER")) REQ_JITTER <- 0.25   # + runif(0, REQ_JITTER) per request
pace <- function() Sys.sleep(REQ_PAUSE + runif(1, 0, REQ_JITTER))

edge_list <- vector("list", nrow(systems))
emg       <- integer(nrow(systems))
message(sprintf("Scraping DashPurchases + DashSourceWater over %d systems (two widgets each -- the slow step; pace ~%.2f-%.2fs/request)...",
                nrow(systems), REQ_PAUSE, REQ_PAUSE + REQ_JITTER))
for (i in seq_len(nrow(systems))) {
  tin <- systems$TINWSYS_IS_NUMBER[i]
  n0  <- formatC(trimws(systems$NUMBER0[i]), width = -12)
  # (a) buys-from edges
  pu <- tryCatch(dwv_widget(ses, "DashPurchases", tin, n0), error = function(e) data.table())
  if (nrow(pu) && "SELLERWSNUMBER" %in% names(pu))
    edge_list[[i]] <- data.table(
      Buyer       = trimws(systems$NUMBER0[i]),
      Seller      = trimws(pu$SELLERWSNUMBER),
      Seller_Name = if ("SELLERWS" %in% names(pu)) pu$SELLERWS else NA_character_)
  pace()                                          # gap before the second widget
  # (b) emergency source / interconnect flag
  sw <- tryCatch(dwv_widget(ses, "DashSourceWater", tin, n0), error = function(e) data.table())
  if (nrow(sw) && all(c("TYPE_CODE", "AVAILABILITY_CODE") %in% names(sw)))
    emg[i] <- as.integer(any(trimws(sw$TYPE_CODE) %in% SRC_SUPPLY_TYPES &
                             trimws(sw$AVAILABILITY_CODE) == EMERGENCY_AVAIL))
  if (i %% 200 == 0) message(sprintf("  %d/%d systems", i, nrow(systems)))
  pace()                                          # gap before the next system
}
systems[, emergency_source := emg]

edges <- unique(rbindlist(edge_list, use.names = TRUE, fill = TRUE)[
  !is.na(Seller) & nzchar(Seller) & Seller != Buyer])
message(sprintf("Purchase edges: %s edges, %d distinct buyers, %d distinct sellers. Emergency-source systems: %d.",
                format(nrow(edges), big.mark = ","), uniqueN(edges$Buyer),
                uniqueN(edges$Seller), sum(emg)))

# Attach emergency_source to the source table; write both outputs.
src <- merge(src, systems[, .(PWS_ID, emergency_source)], by = "PWS_ID", all.x = TRUE)
src[is.na(emergency_source), emergency_source := 0L]
saveRDS(src,   committed("pws_source.RDS"))
saveRDS(edges, committed("pws_purchase_edges.RDS"))
message("Wrote ", committed("pws_source.RDS"), " and ", committed("pws_purchase_edges.RDS"))
