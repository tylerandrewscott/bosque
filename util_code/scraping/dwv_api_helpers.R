# =============================================================================
# dwv_api_helpers.R  —  client for the TCEQ Drinking Water Viewer (DWV) API
# -----------------------------------------------------------------------------
# The old scrapers targeted the JSP "Drinking Water Watch" app at
#   https://dww2.tceq.texas.gov/DWW/JSP/...
# which was DECOMMISSIONED and replaced (May 2026) by the "Drinking Water
# Viewer" single-page app at
#   https://dwv.tceq.texas.gov/
#
# DWV is an Angular front-end over an ASP.NET Core / SDWIS OData backend. There
# is no more HTML to scrape: the data comes back as clean JSON from OData
# endpoints. This file replaces all the rvest/html_table() parsing the old
# scrapers did with a small typed client.
#
# ---- How the API works (reverse-engineered from the SPA bundle) -------------
# Base:      https://dwv.tceq.texas.gov/
# Auth:      GET / once to obtain cookies. The response sets an `XSRF-TOKEN`
#            cookie whose value must be echoed back in the `X-XSRF-TOKEN`
#            header on every subsequent API call (Angular anti-forgery). No
#            login/API key is needed for the public read endpoints.
# Data:      GET sdwis/<Endpoint>?n0=<PWSID>&$filter=...&$select=...&$orderby=...
#            &$top=...&$skip=...   -> OData JSON  { "value": [ ... ] }
#
# Two request shapes:
#   1. System search / master list  ->  sdwis/DashMain
#        filter on NUMBER0 / NAME / county / type / status, page with
#        $top + $skip (server caps a page at 1000 rows).
#        Key fields: TINWSYS_IS_NUMBER (internal key used to scope every other
#        endpoint), NUMBER0 (the PWS id, space-padded to width 12, e.g.
#        "TX0010005   "), NAME, ACTIVITY_STATUS_CD, D_PRIN_CNTY_SVD_NM (county),
#        D_PWS_FED_TYPE_CD ("C   "/"NC  "/"NTNC"), D_FED_PRIM_SRC_CD (GW/SW),
#        D_POPULATION_COUNT (total pop served), SVC_CONNECT_CNT (connections).
#
#   2. Per-system detail widgets  ->  sdwis/Dash* and sdwis/FacilityList
#        MUST be scoped with $filter=TINWSYS_IS_NUMBER eq <n> and
#        TINWSYS_ST_CODE eq 'TX'.  (`n0` alone does NOT filter these grid
#        widgets — it returns a global, page-capped list. This is the single
#        biggest gotcha of the new API.)
#
# ---- Old DWW datasheet table  ->  new DWV endpoint --------------------------
#   Total Storage Capacity (TSTC)   -> DashWaterSystemMeasures  (MEASURE_NAME
#                                       == "TSTC - Total Storage Capacity")
#   Max Daily Demand / flow rates   -> DashWaterSystemFlowRates
#   Population by type              -> DashAnnualOperatingPeriod.PopulationServed
#   Service connections by type     -> DashServiceConnections
#   Sources of water                -> DashSourceWater
#   "buys from" (purchases)         -> DashPurchases  (SELLERWSNUMBER)
#   "sells to"  (buyers)            -> DashBuyers      (BUYERWSNUMBER)
#   # interconnections w/ other PWS -> nrow(DashPurchases)
#   Owner type / for-profit         -> DashWaterSystemIndicators  (POWN, PRFT)
#   Personnel / contacts            -> DashContacts
#   System facilities               -> FacilityList
#
# Usage:
#   source("util_code/scraping/dwv_api_helpers.R")
#   ses <- dwv_session()
#   sys <- dwv_search(ses, type = "C", active_only = TRUE)   # master list
#   m   <- dwv_widget(ses, "DashWaterSystemMeasures",
#                     tinwsys = sys$TINWSYS_IS_NUMBER[1], number0 = sys$NUMBER0[1])
# =============================================================================

library(httr2)
library(data.table)

DWV_BASE <- "https://dwv.tceq.texas.gov/"

# --- Session ------------------------------------------------------------------
# Hits the site root to pick up the anti-forgery cookies, then returns a small
# environment holding the shared curl cookie-jar handle and the XSRF token that
# every API call needs. Reuse one session for a whole scrape run.
dwv_session <- function(base = DWV_BASE, user_agent = "bosque-research-scraper (tascott@ucdavis.edu)") {
  ses <- new.env(parent = emptyenv())
  ses$base <- base
  ses$ua   <- user_agent
  ses$cookies <- tempfile(fileext = ".txt")   # curl cookie jar, shared across calls

  resp <- request(base) |>
    req_user_agent(user_agent) |>
    req_options(cookiefile = ses$cookies, cookiejar = ses$cookies) |>
    req_perform()

  # The XSRF-TOKEN cookie value must be sent back as the X-XSRF-TOKEN header.
  jar <- resp$headers[names(resp$headers) == "set-cookie"]
  tok <- NULL
  for (h in unlist(jar)) {
    m <- regmatches(h, regexec("XSRF-TOKEN=([^;]+)", h))[[1]]
    if (length(m) == 2) tok <- m[2]
  }
  if (is.null(tok)) stop("Could not obtain XSRF-TOKEN from ", base,
                         " — the DWV site layout may have changed.")
  ses$xsrf <- tok
  ses
}

# --- Low-level OData GET -------------------------------------------------------
# One GET against an sdwis/ endpoint. `query` is a named list of raw query
# params (OData keys like `$filter` are passed literally). Returns the parsed
# `value` array as a data.table (empty data.table if none). Retries transient
# failures; a 204 (server's "page too large / no content") yields empty.
dwv_get <- function(ses, endpoint, query = list(), max_tries = 4) {
  req <- request(paste0(ses$base, endpoint)) |>
    req_user_agent(ses$ua) |>
    req_headers("X-XSRF-TOKEN" = ses$xsrf, "Content-Type" = "application/json") |>
    req_options(cookiefile = ses$cookies, cookiejar = ses$cookies) |>
    req_url_query(!!!query) |>
    req_retry(max_tries = max_tries,
              is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    req_error(is_error = function(r) FALSE)   # inspect status ourselves

  resp <- req_perform(req)
  st <- resp_status(resp)
  if (st == 204) return(data.table())                     # no content
  if (st >= 400) {
    stop(sprintf("DWV %s -> HTTP %d: %s", endpoint, st,
                 substr(resp_body_string(resp), 1, 200)))
  }
  body <- resp_body_json(resp, simplifyVector = FALSE)
  vals <- body$value
  if (is.null(vals) || length(vals) == 0) return(data.table())
  # Flatten each record's scalar fields; keep nested objects/lists as list-cols.
  rbindlist(lapply(vals, function(r) {
    flat <- lapply(r, function(x) if (is.null(x)) NA else if (length(x) == 1 && !is.list(x)) x else list(x))
    as.data.table(flat)
  }), use.names = TRUE, fill = TRUE)
}

# --- OData $filter helpers ----------------------------------------------------
.dwv_odata_quote <- function(x) paste0("'", gsub("'", "''", x), "'")

# Scope filter for the per-system detail widgets.
dwv_scope_filter <- function(tinwsys, st_code = "TX") {
  sprintf("TINWSYS_IS_NUMBER eq %s and TINWSYS_ST_CODE eq %s",
          as.character(tinwsys), .dwv_odata_quote(st_code))
}

# --- System search / master list (replaces SearchDispatch) --------------------
# Pages DashMain and returns every matching water system as a data.table.
#   type        : "C" (community), "NC", "NTNC", or NULL for all. Matches the
#                 space-padded D_PWS_FED_TYPE_CD ("C   ", "NC  ", "NTNC").
#   active_only : TRUE keeps ACTIVITY_STATUS_CD == 'A'.
#   county      : optional county name (D_PRIN_CNTY_SVD_NM), upper-case.
#   extra_filter: optional raw OData filter fragment, ANDed in.
#   select      : optional character vector of fields to return ($select).
dwv_search <- function(ses, type = NULL, active_only = TRUE, county = NULL,
                       extra_filter = NULL, select = NULL, page = 1000,
                       sleep = 0.1, verbose = TRUE) {
  clauses <- character(0)
  if (!is.null(type))   clauses <- c(clauses, sprintf("D_PWS_FED_TYPE_CD eq %s",
                                     .dwv_odata_quote(formatC(type, width = -4))))
  if (active_only)      clauses <- c(clauses, "ACTIVITY_STATUS_CD eq 'A'")
  if (!is.null(county)) clauses <- c(clauses, sprintf("D_PRIN_CNTY_SVD_NM eq %s",
                                     .dwv_odata_quote(toupper(county))))
  if (!is.null(extra_filter)) clauses <- c(clauses, sprintf("(%s)", extra_filter))
  filt <- paste(clauses, collapse = " and ")

  out <- list(); skip <- 0
  repeat {
    q <- list(`$orderby` = "NUMBER0", `$top` = page, `$skip` = skip, `$count` = "false")
    if (nzchar(filt)) q[["$filter"]] <- filt
    if (!is.null(select)) q[["$select"]] <- paste(select, collapse = ",")
    dt <- dwv_get(ses, "sdwis/DashMain", q)
    if (nrow(dt) == 0) break
    out[[length(out) + 1]] <- dt
    if (verbose) message(sprintf("  DashMain: fetched %d (skip=%d)", nrow(dt), skip))
    if (nrow(dt) < page) break
    skip <- skip + page
    if (sleep > 0) Sys.sleep(sleep)
  }
  res <- rbindlist(out, use.names = TRUE, fill = TRUE)
  if (nrow(res) && "NUMBER0" %in% names(res)) res[, PWS_ID := trimws(NUMBER0)]
  res[]
}

# Look up a single system by its (trimmed or padded) PWS id. Returns a 1-row
# data.table with TINWSYS_IS_NUMBER + NUMBER0 you can feed to dwv_widget().
dwv_system <- function(ses, pws_id) {
  padded <- formatC(trimws(pws_id), width = -12)   # left-justify, pad to 12
  q <- list(`$filter` = sprintf("NUMBER0 eq %s", .dwv_odata_quote(padded)),
            n0 = padded, `$count` = "false")
  dt <- dwv_get(ses, "sdwis/DashMain", q)
  if (nrow(dt) && "NUMBER0" %in% names(dt)) dt[, PWS_ID := trimws(NUMBER0)]
  dt[]
}

# --- Per-system detail widget fetch -------------------------------------------
# Fetch one Dash*/FacilityList widget scoped to a single system.
#   endpoint : e.g. "DashWaterSystemMeasures", "DashPurchases", "FacilityList"
#   tinwsys  : the system's TINWSYS_IS_NUMBER (from dwv_search/dwv_system)
#   number0  : the system's NUMBER0 (padded); used for the contextual n0 param
#   extra_filter/select/orderby/top : optional OData refinements
# Adds PWS_ID + TINWSYS_IS_NUMBER columns to the result for easy stacking.
dwv_widget <- function(ses, endpoint, tinwsys, number0 = NULL, st_code = "TX",
                       extra_filter = NULL, select = NULL, orderby = NULL,
                       top = NULL) {
  filt <- dwv_scope_filter(tinwsys, st_code)
  if (!is.null(extra_filter)) filt <- paste0("(", filt, ") and (", extra_filter, ")")
  q <- list(`$filter` = filt)
  if (!is.null(number0)) q$n0 <- number0
  if (!is.null(select))  q[["$select"]]  <- paste(select, collapse = ",")
  if (!is.null(orderby)) q[["$orderby"]] <- orderby
  if (!is.null(top))     q[["$top"]]     <- top
  dt <- dwv_get(ses, paste0("sdwis/", endpoint), q)
  if (nrow(dt)) {
    dt[, TINWSYS_IS_NUMBER := tinwsys]
    if (!is.null(number0)) dt[, PWS_ID := trimws(number0)]
  }
  dt[]
}

# --- Widget fetch with retries + explicit failure signal ----------------------
# dwv_widget() already retries transient HTTP failures inside dwv_get(); this
# wrapper adds whole-call retries (session hiccups, parse errors) and — the
# important part — returns NULL on final failure so callers can distinguish
# "query FAILED" (NULL) from "queried fine, zero rows" (empty data.table).
# Callers should record failures as NA / refetchable, never as zeros.
dwv_widget_retry <- function(ses, endpoint, tinwsys, number0 = NULL,
                             tries = 3, ...) {
  for (k in seq_len(tries)) {
    out <- tryCatch(dwv_widget(ses, endpoint, tinwsys, number0, ...),
                    error = identity)
    if (!inherits(out, "error")) return(out)
    if (k < tries) Sys.sleep(3 * k)
  }
  message(sprintf("  %s [%s]: FAILED after %d attempts: %s", endpoint,
                  trimws(number0 %||% as.character(tinwsys)), tries,
                  conditionMessage(out)))
  NULL
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Convenience: iterate a widget over many systems --------------------------
# Applies dwv_widget across a systems data.table (needs TINWSYS_IS_NUMBER +
# NUMBER0 columns, e.g. the output of dwv_search) and row-binds the results.
# `transform` optionally post-processes each system's data.table before binding
# (e.g. to pull a single measure). Politely sleeps between systems.
dwv_widget_over <- function(ses, endpoint, systems, transform = NULL,
                            sleep = 0.05, verbose = TRUE, ...) {
  stopifnot(all(c("TINWSYS_IS_NUMBER", "NUMBER0") %in% names(systems)))
  n <- nrow(systems); out <- vector("list", n)
  for (i in seq_len(n)) {
    dt <- tryCatch(
      dwv_widget(ses, endpoint,
                 tinwsys = systems$TINWSYS_IS_NUMBER[i],
                 number0 = systems$NUMBER0[i], ...),
      error = function(e) { warning(sprintf("%s [%s]: %s", endpoint,
                              trimws(systems$NUMBER0[i]), conditionMessage(e))); data.table() })
    if (!is.null(transform) && nrow(dt)) dt <- transform(dt, systems[i])
    out[[i]] <- dt
    if (verbose && i %% 200 == 0) message(sprintf("  %s: %d/%d systems", endpoint, i, n))
    if (sleep > 0) Sys.sleep(sleep)
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}
