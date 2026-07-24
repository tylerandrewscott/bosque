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
# RESCRAPE POLICY (config.R; same semantics as scripts 05/06): the cheap bulk
# DashMain pull always runs (it refreshes source codes and discovers new
# systems), but the expensive per-system widget loop is incremental:
#   RESCRAPE = FALSE -> fetch ONLY systems missing from the committed
#                       pws_source.RDS or whose prior query failed
#                       (emergency_source NA = failed; re-queued, mirroring
#                       06's Fetch_OK).
#   RESCRAPE = TRUE  -> re-fetch every system.
# Either way the prior outputs are merged UNDER the fresh rows at write time:
# re-fetched systems get fresh values (including their purchase edges, which
# are REPLACED per buyer), systems not fetched this run — top-up skips, DWV
# dropouts, failed queries — keep their prior rows/edges. A rescrape never
# discards previously collected data. Periodic checkpoints go to gitignored
# scratch files, NEVER to the committed outputs; an interrupted run resumes
# from them under either policy.
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

# --- Prior outputs + resume/top-up bookkeeping (see RESCRAPE POLICY, header) ---
.src_out   <- committed("pws_source.RDS")
.edge_out  <- committed("pws_purchase_edges.RDS")
prev_src   <- if (file.exists(.src_out))  as.data.table(readRDS(.src_out))  else NULL
prev_edges <- if (file.exists(.edge_out)) as.data.table(readRDS(.edge_out)) else NULL

emg_progress_file  <- scratch("source_purchases_progress.csv")
edge_progress_file <- scratch("source_purchases_edges_progress.csv")
resume_emg <- if (file.exists(emg_progress_file))
  fread(emg_progress_file, colClasses = list(character = "PWS_ID")) else
  data.table(PWS_ID = character(0), emergency_source = integer(0), Fetch_OK = logical(0))
resume_emg <- resume_emg[Fetch_OK %in% TRUE]        # failed rows are re-queued
resume_edges <- if (file.exists(edge_progress_file))
  fread(edge_progress_file, colClasses = list(character = c("Buyer", "Seller", "Seller_Name"))) else
  data.table(Buyer = character(0), Seller = character(0), Seller_Name = character(0))
resume_edges <- resume_edges[Buyer %in% resume_emg$PWS_ID]

# "Done" = successfully fetched in an interrupted run (always resumed), plus —
# under RESCRAPE = FALSE only — systems whose committed emergency_source is
# non-NA (NA marks a failed prior query, so those are re-queued).
done_ids <- resume_emg$PWS_ID
if (!RESCRAPE && !is.null(prev_src))
  done_ids <- union(done_ids, prev_src[!is.na(emergency_source), PWS_ID])
todo <- systems[!(trimws(NUMBER0) %in% done_ids)]

# emergency_source: NA = query FAILED (kept NA downstream, never coerced to 0);
# 0/1 = queried fine. A system counts as fetched only if BOTH widget queries
# succeeded; anything less is re-queued on the next run (each call already gets
# in-run retries).
edge_list <- vector("list", nrow(todo))
emg       <- rep(NA_integer_, nrow(todo))
ok_vec    <- rep(FALSE, nrow(todo))
message(sprintf("RESCRAPE=%s: %d systems already done; scraping DashPurchases + DashSourceWater over %d of %d systems (two widgets each -- the slow step; pace ~%.2f-%.2fs/request)...",
                RESCRAPE, length(done_ids), nrow(todo), nrow(systems),
                REQ_PAUSE, REQ_PAUSE + REQ_JITTER))
checkpoint <- function() {
  ck <- data.table(PWS_ID = trimws(todo$NUMBER0),
                   emergency_source = emg, Fetch_OK = ok_vec)[Fetch_OK %in% TRUE]
  fwrite(rbindlist(list(resume_emg, ck), use.names = TRUE), emg_progress_file)
  fwrite(rbindlist(c(list(resume_edges), edge_list), use.names = TRUE, fill = TRUE),
         edge_progress_file)
}
for (i in seq_len(nrow(todo))) {
  tin <- todo$TINWSYS_IS_NUMBER[i]
  n0  <- formatC(trimws(todo$NUMBER0[i]), width = -12)
  # (a) buys-from edges (NULL = query failed after retries; edges unknowable)
  pu <- dwv_widget_retry(ses, "DashPurchases", tin, n0)
  pace()                                          # gap before the second widget
  # (b) emergency source / interconnect flag (NULL = failed)
  sw <- dwv_widget_retry(ses, "DashSourceWater", tin, n0)
  if (!is.null(pu) && !is.null(sw)) {
    ok_vec[i] <- TRUE
    emg[i] <- if (nrow(sw) && all(c("TYPE_CODE", "AVAILABILITY_CODE") %in% names(sw)))
      as.integer(any(trimws(sw$TYPE_CODE) %in% SRC_SUPPLY_TYPES &
                     trimws(sw$AVAILABILITY_CODE) == EMERGENCY_AVAIL)) else 0L
    if (nrow(pu) && "SELLERWSNUMBER" %in% names(pu))
      edge_list[[i]] <- data.table(
        Buyer       = trimws(todo$NUMBER0[i]),
        Seller      = trimws(pu$SELLERWSNUMBER),
        Seller_Name = if ("SELLERWS" %in% names(pu)) pu$SELLERWS else NA_character_)
  }
  if (i %% 200 == 0) {
    message(sprintf("  %d/%d systems", i, nrow(todo)))
    checkpoint()                                  # periodic checkpoint (scratch)
  }
  pace()                                          # gap before the next system
}
fail_ct <- sum(!ok_vec)
if (fail_ct) warning(sprintf(
  "%d system(s) had a failed DashPurchases/DashSourceWater query after retries: their prior values (if any) are kept and they are re-queued on the next run.",
  fail_ct))

# Everything successfully fetched: this run + the resumed checkpoint. These are
# the systems whose values (and purchase edges) get REPLACED by fresh data.
run_emg <- rbindlist(list(
  resume_emg[, .(PWS_ID, emergency_source)],
  data.table(PWS_ID = trimws(todo$NUMBER0), emergency_source = emg)[ok_vec]),
  use.names = TRUE)
ok_ids <- run_emg$PWS_ID

# Attach emergency_source to the source table: fresh value where fetched OK,
# prior committed value otherwise, NA if never successfully queried (the
# panel's complete-case gate drops those systems honestly; NA is NOT coerced
# to 0, which would fabricate a "no emergency source" answer).
emg_map <- unique(rbindlist(list(
  run_emg,
  if (!is.null(prev_src)) prev_src[, .(PWS_ID, emergency_source)] else NULL),
  use.names = TRUE), by = "PWS_ID")
src <- merge(src, emg_map, by = "PWS_ID", all.x = TRUE)
# Retain prior rows for systems no longer on the DWV active list (fresh first).
if (!is.null(prev_src))
  src <- unique(rbindlist(list(src, prev_src), use.names = TRUE, fill = TRUE), by = "PWS_ID")

# Edges: fresh edges for fetched-OK buyers replace their prior edges; prior
# edges survive for every other buyer.
fresh_edges <- rbindlist(c(list(resume_edges), edge_list), use.names = TRUE, fill = TRUE)
if (!is.null(prev_edges))
  fresh_edges <- rbindlist(list(fresh_edges, prev_edges[!(Buyer %in% ok_ids)]),
                           use.names = TRUE, fill = TRUE)
edges <- unique(fresh_edges[!is.na(Seller) & nzchar(Seller) & Seller != Buyer])

message(sprintf("Purchase edges: %s edges, %d distinct buyers, %d distinct sellers. Emergency-source systems: %d.",
                format(nrow(edges), big.mark = ","), uniqueN(edges$Buyer),
                uniqueN(edges$Seller), sum(src$emergency_source, na.rm = TRUE)))

saveRDS(src,   committed("pws_source.RDS"))
saveRDS(edges, committed("pws_purchase_edges.RDS"))
message("Wrote ", committed("pws_source.RDS"), " and ", committed("pws_purchase_edges.RDS"))
# Committed outputs written: clear the scratch checkpoints so the next run
# starts clean.
for (f in c(emg_progress_file, edge_progress_file))
  if (file.exists(f)) invisible(file.remove(f))
