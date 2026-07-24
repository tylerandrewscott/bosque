# Planning review — drought_and_debt pipeline (2026-07-23)

Four parallel audit agents (Stage A assemble, Stage B combine, Stage C model, cross-stage
contracts); every HIGH finding hand-verified against code and committed data files.

**How to use this doc:** add your notes / decisions on the `> Direction:` line under any
item (or anywhere — I'll re-read the whole file). Suggested vocabulary: `fix`, `skip`,
`investigate`, `defer`, or free-form instructions.

---

## HIGH — bias results or published outputs

### H1. Wayback scraper drops mandatory restrictions from 2024–2026 snapshots
`code/00_assemble/02_assemble_drought_restrictions.R:172-185`

The wayback-HTML path maps stages with a `case_when` that only handles `V/1/2/3` and
defaults `TRUE ~ 'Voluntary'`. Newer TCEQ page layouts use literal `M1/M2/M3` codes
(`ingest_helpers.R:37` even lists them in `stage_vocab`, and they appear in 2024–2026
wayback snapshots). Those records become "Voluntary" and are dropped by the
`filter(STAGE %in% c('Mild','Moderate','Severe'))` at line 185 — which then recodes the
survivors *back* to M1/M2/M3. This is the **outcome variable**; recent-year event counts
look thin (2024: 228, 2025: 67, 2026: 13 vs 2022: 505, 2023: 635). The live-CSV path
handles M1/M2/M3 correctly, so the loss is wayback-only notice dates.

**Fix sketch:** add `M1/M2/M3` branches to the case_when (M1→Mild, M2→Moderate, M3→Severe),
then rescrape or re-parse cached wayback HTML.

> Direction: Yes, edit and reparse. The key is just to capture all mandatory restriction adoptions, so we want to catch all the 1/2/3 and M1/M2/M3 cases.

### H2. Figure 1B legend mislabels 3 of 5 drought bands
`code/02_model/02_make_figure1.R:116`

`scale_fill_identity(labels = c('D4','D3-D4','D2-D4','D1-D4','D0-D4'))` has no `breaks=`
argument, and ggplot sorts identity-scale breaks **alphabetically by hex string**. Verified
via `ggplot_build`: the D0 ribbon is labeled "D2-D4", D2 is labeled "D1-D4", D1 is labeled
"D0-D4". Pre-existing (not from the uncommitted diff), but `output/figure1.png` was
regenerated today with the bug live.

**Fix sketch:** one line — add `breaks = rib_cols` (draw order) so labels pair positionally.

> Direction: Fix to be correct.

### H3. Perc_Black computed over the 18-and-over population
`code/01_combine/05_census_block_PWS.R:56-64`

Table **P010** is 2010 SF1 "Race for the population **18 years and over**", while
`Perc_Hispanic` uses P004 (total population). The comment claims "total vs. any-part-black".
Total-population equivalents: **P003** (single race) / **P006** (any-part). Related
semantics mislabels: `Perc_Rural` = % of *housing units* rural (H002); poverty variable =
% of *families* below poverty (S1702), not persons.

**Fix sketch:** swap `.black_sub` to the P006 any-part-black columns with P006001-style
total, rerun 05 + downstream. (Rural/poverty: either relabel in docs or swap tables.)

> Direction: Fix the code to make it consistent in case we want to use it later, but note that we don't use these variables anymore in the primary analysis.

### H4. Audit dedup keeps the EMPTIEST filing (inverted sort)
`code/00_assemble/03_assemble_audits.R:47-50`

`order(common, -zeros)` sorts zero-count **descending**, then keep-first retains the row
with the *most* zero-valued fields. Measured: of 298 duplicate (District_ID, FY-end-date)
groups in the raw CSV, **113 keep a strictly emptier row** than the alternative. These are
the fiscal covariates. Second defect on line 47: the dedup key is the exact fiscal-year-end
**date**, not the year, so 50 duplicate (District_ID, FISCAL_YEAR) pairs survive into
`input/district_audits.RDS` (37 of them disagree on BONDS OUTSTANDING); Stage B's second
dedup then picks arbitrarily.

**Fix sketch:** `order(common, zeros)` (keep fullest), and key the dedup on
`paste(District_ID, year(FY_END))`.

> Direction: Fix this. 

### H5. Committed outputs mix fit generations (known — Model 2 refit pending)
`scratch/recurrent_coxinla_*.RDS`, `output/model_estimates.csv`, `output/paper_facts.csv`

Model 1 was refit today 14:54 — *after* the 10:00 Model 2 fits that consume Model 1
posteriors as priors, and after every committed table. Consistent with the already-pending
Model 2 refit (by-year SDWIS denominator change), but until Model 2 + tables rerun in one
pass, every committed number mixes generations. Note: if H1/H3/H4 fixes are applied, the
refit should come **after** those so it only happens once.

> Direction: Let's make all the fixes, then we will re-run everything and update the results.

---

## MEDIUM — silent data loss, workflow traps

### M1. Fiscal `_P1` lags are previous-AVAILABLE-year, not previous-year
`code/01_combine/04_combine_district_fiscal_data.R:47-53`

Plain `shift()` within district: 6.0% of lagged rows (1,378/22,971) span a 2–5+ year filing
gap, so "last year's debt" can be 5+ years stale — precisely for districts with filing gaps
(unlikely missing-at-random). Also: one district has FISCAL_YEAR = 5 (mangled parse) and 33
rows have NA FISCAL_YEAR, 22 of which get *backwards-in-time* lags because NA sorts last.

> Direction: we dont' have to create p1 lags, we can use the fiscal_year_end_date values to do rolling joins against panel weeks limited to max = 2 year gap. Solve the mangled fiscal_year parses or figure out why those are wrong.

**Answer/plan:** good news — the panel ALREADY does exactly this: `build_recurrent_panel.R:356`
roll-joins fin on `fy_end` (the FY-end date) with `roll = 730` (max 2-year staleness), and the
`_P1` columns are created in `04_combine:51` but consumed NOWHERE downstream (grep-verified).
So the fix is pure deletion: drop the shift-based lag block from 04_combine, and repair/drop
the mangled FISCAL_YEAR rows (the FY=5 and NA-year rows — their `fy_end` dates feed the
SDWIS by-year denominator join at `build_recurrent_panel.R:334`, so bad parses still matter).

### M2. 304/4,807 in-window events (6.3%) silently dropped from the panel
`code/02_model/build_recurrent_panel.R:140`

Systems with zero rows in `pws_drought_weekly.RDS` contribute neither events nor at-risk
time, and nothing reconciles 4,807 → 4,441. Needs an explicit assertion + a decision on
whether those systems belong in the sample.

> Direction: Why are these dropped? Why are there systems with zero rows?

**Answer:** 131 event-systems (304 events) have **no polygon in the TCEQ service-area
shapefile**, so they never enter the county overlay (all 131 are absent from
`pws_county_overlaps.RDS`) and get no weekly drought series. They are NOT junk records:
121/131 are active in SDWIS through 2026, 95 are community water systems (one with 792k
connections), and **27 are district-linked** — so Model 2 loses them too.
**RESOLVED (with Tyler, 2026-07-23):** sample = **CWS only** (Model 1) and district-linked
CWS (Model 2). Correcting for `PWS Activity Code` (which the RDS didn't carry), active TX
CWS = **4,740**, of which **4,496 (94.9%) are mapped** in the TCEQ retail service-area
shapefile. The unmapped event systems are mostly wholesale-only (no retail service area to
map — pop served ≈ 0 despite thousands of connections) or no-longer-public; only 34 are
currently active CWS. Implemented: an explicit **CWS filter** in `build_recurrent_panel.R`
(SDWIS `pws_type_code`, any-year CWS) + a **reconciliation message** counting excluded
notices by reason (non-CWS vs unmapped CWS), with a warning if the excluded share ever
exceeds 10%. No county fallback — it would either select on the outcome (131 event-havers
only) or force county-level covariates onto 3,401 mostly-trivial systems.

### M3. `07_scrape_source_and_purchases.R` missing from run_all Stage A
`code/run_all.R:49-56` vs `build_recurrent_panel.R:168,210`

Its outputs (`pws_source.RDS`, `pws_purchase_edges.RDS`) feed `seller_restricted`,
`wholesaler`, `purchases_water`, `emergency_source` — a full `RESCRAPE=TRUE` rerun
refreshes everything *except* the network snapshot.

> Direction: What do we need to do here?

**Answer:** one line — add `run_step("00_assemble/07_scrape_source_and_purchases.R")` to
run_all.R Stage A (between 06 and 08). The script already exists and is resume-capable;
it was just never wired into the driver. Its retry behavior gets fixed under M7 anyway.

### M4. Storage GAL rows treated as MG (~1e6 unit error)
`input/storage_connections_data.txt` vs `build_recurrent_panel.R:203`

30 rows carry GAL units, but the builder assumes MG (its header claims no unit parsing is
needed) — a million-fold error in `storage_per_conn_g` for those systems.

> Direction: Build a little tool in the code to standardize/correct the units to make these consistent. clearly a unit parsing step is needed.

### M5. `seller_restricted` includes the declaration week itself
`code/02_model/build_recurrent_panel.R:176-179`

The [E, E+4) exposure window includes the interval containing the seller's declaration, so
a buyer declaring the same week as its wholesaler has its event "explained" by the
covariate. Conservative fix: start exposure at the *next* interval.

> Direction: If the notice date is on or after the same date, that makes sense. A buyer is likely to respond, perhaps even at the same time, to the seller's declaration.

### M6. Figure 1 hits the live UNL API and rewrites committed input on default runs
`code/02_model/02_make_figure1.R:58-95` + `config.R` (RESCRAPE default TRUE)

The self-cache guard is `file.exists(cache) && !isTRUE(RESCRAPE)`; with RESCRAPE defaulting
TRUE, every default model-only run fetches from the UNL API and rewrites
`input/statewide_drought_area.RDS`. This *did* close the old provenance gap (the file now
has an on-path writer), but it makes Stage C network-dependent and mutates git-tracked
input by default. Option: key the guard on its own flag, or on run_assemble.

> Direction: We don't need to re-download the UNL API every time. These data arent' going to change. All we would need to change is if we expanded the dates later, we would want to add the new dates.

### M7. DWV API failures become hard zeros
`code/00_assemble/06_htmlscrape_storage_interconnects.R`, `07_scrape_source_and_purchases.R`

`tryCatch(error = function(e) data.table())` makes "query failed" indistinguishable from
"queried, empty" — 2,636/4,752 systems show 0 interconnections and it's unknowable how many
are failures; the resume logic then treats them as done forever.

> Direction: Code needs to have a way of re-running or retrying failed queries. 

### M8. canon_key collapses IMPROVEMENT DISTRICT and IRRIGATION DISTRICT → "ID"
`code/00_assemble/build_tbrb_crosswalk.R:45-46`

If only one of a same-named pair exists in the audit universe, debt is misattributed at
HIGH confidence with no review flag.

> Direction: This should be fine, these are small and rare cases. IMPROVEMENT DISTRICTS usually occurs as part of WATER CONTROL AND IMPROVEMENT DISTRICT.

### M9. Analysis window opens 16 months before restriction data exists
Sources cover essentially nothing before Aug 2010 (FOIA sheets start 2011; earliest wayback
May 2011), but the window opens 2010-01-01 — that early at-risk time is structurally
event-free (exposure misclassification the RW1 baseline partially absorbs). Either confirm
pre-Aug-2010 records genuinely can't exist, or open the window later.

> Direction: When is the first observed restriction adoption? We probably want to start that month.

**Answer:** first mandatory notice is **2010-08-01** — but it's a single record, then
nothing until 2011-01 (35 notices, all dated exactly 2011-01-01, which smells like batch
placeholder dating in the early FOIA data), with steady real flow from Feb 2011 (16, then
73, 39, 198/mo into the 2011 drought). Per your "start that month" rule the new
`start_date` would be **2010-08-01**. If you'd rather not trust the two placeholder-looking
date clusters, 2011-02-01 is the defensible alternative. Defaulting to 2010-08-01 unless
you say otherwise.

### M10. Stale-fit hazards (no spec-staleness checks)
- `REUSE_MODEL1_FIT` reuses cached Model 1 with no check that the spec matches; the
  prior-transfer (`01_fit_recurrent_cox_inla.R:219`) silently gives default priors to any
  covariate missing from the cached fit.
- `03_model_results_table.R:47-59` `load_fit()` arbitrates slim-vs-full by mtime only.
- The panel guard (`build_recurrent_panel.R:96-97`) can't invalidate `panel_m2` or
  `NEIGHBOR_PERSIST` when their inputs change.

> Direction: build checks for this, but make sure it's not a recursive nightmare. it could be a check and warning/error message telling someone what they need to run first. Related to this, the rescrape functionality is really tricky. I think we should think about scraping and analysis separately -- scraping functions are additive: add to what is on file, retry failed queries, etc. analysis functions are clobber: fit new model, overwrite figure, regenerate the panel data frame.

### M11. Drought weights are area-based, not population-based (undocumented)
`code/01_combine/03_combine_district_and_drought.R:17-18` — a service area's DSCI is the
county-area-weighted mean; fine as a choice, but the paper should say so.

> Direction: Noted.

### M12. Latent `area_overlay` denominator bug with repeated x-ids
`code/01_combine/spatial_helpers.R:103-116` — takes the area of the *first* polygon per id;
`02_combine_pws_with_counties.R` passes the boundary layer **undissolved** (05 dissolves
first — asymmetric). Currently safe (shapefile verified unique on PWSId), but a future
multi-row release silently corrupts the weights. Cheap insurance: dissolve in 02 too, or
assert uniqueness in the helper.

> Direction: that's fine, dissolve in 02.

---

## LOW — hygiene, labels, docs

- **L1.** DSCI labeled "0–100" but the scale is 0–500 (`04_descriptive_stats_table.R:48`;
  the committed CSV itself shows max 500).
- **L2.** FOIA schema guard compares uppercase expected names *before* `toupper()` runs, so
  it warns every run and real drift would be invisible (`02_assemble_drought_restrictions.R:33-40`).
- **L3.** `03_assemble_audits.R` `expected_cols` guard omits BONDS OUTSTANDING though the
  panel builder consumes it.
- **L4.** 2.4 GB orphan `scratch/recurrent_coxinla_model1_appendix*.RDS` (+ committed slim
  twin) — nothing reads the "appendix" stem.
- **L5.** `explore/combine_districts_tracts_counties.R` writes via `committed()` — a casual
  explore run mutates git-tracked `input/`; its 3 overlap RDS outputs are orphans (already
  flagged for deletion in the foreign-intermediates cleanup).
- **L6.** TBRB XML filename `..._20260722.xml` hardcoded in both `04_assemble_debt.R:31`
  and `build_tbrb_crosswalk.R` — a refresh must touch both.
- **L7.** `01_combine_district_and_pws_ids.R`: hardcoded `mdy('01/01/2000')` dissolution
  cutoff; `grepl('MWA')` no word boundary; 1,605/2,770 crosswalk rows have `PWS_ID = NA`
  (latent NA-join hazard for future consumers).
- **L8.** `05_paper_facts.R:127` "largest service population among sample districts" is
  actually the max over sample *PWS systems*; `:138-140` `window_years = 15` for a 16-year
  inclusive window.
- **L9.** 12 PWS have NaN `Perc_Dem_Vote_Share`; 45 have NA `Median_Year_Structure_Built`
  in the panel. Demographics frozen at 2010 vintage through 2025 (presumably deliberate —
  undocumented).
- **L10.** `03_model_results_table.R:222,295` reports `exp(mean(log-HR))` labeled
  "posterior mean" (it's the geometric mean / median-ish under symmetry — labeling nit).
- **L11.** Figure 1A/2 `In_Sample` split is raw crosswalk membership, not the fiscal-model
  sample; the comment overstates ("the exact event set the model panel consumes" — event
  set yes, sample split no).
- **L12.** `format_pws_id` doesn't zero-pad TX-prefixed short ids.
- **L13.** `REBOOT_PLAN.MD` calls itself the authoritative pipeline map but describes the
  retired `code/restart/` layout and the pre-reboot 2010–2015 window.
- **L14.** Round(,2) sliver drop in `02_combine_pws_with_counties.R:32-33`; county weight
  sums verified 0.98–1.01 (benign today).
- **L15.** `04_combine_district_fiscal_data.R:50` `fvars` regex overreach; no inflation
  deflator on dollar covariates (2010–2025 span — may be a deliberate modeling choice).

> Direction: none for now, save these to address later.

---

## Checked clean (verified empirically, not assumed)

- Counting-process interval construction: partitions time correctly, left truncation OK,
  events attribute to the interval ending that week carrying the *prior* week's DSCI — no
  lookahead.
- Fiscal rolling join (roll=730): only uses audits ending on/before the interval.
- GO/REV split: all three coding rules implemented exactly as documented.
- SDWIS denominator (08_assemble): no dup keys, zero-connections → NA, asinh handles 0,
  dollar magnitudes plausible; resolves the old provenance gap.
- INLA coefficient extraction by rowname (no label/order mismatch possible); prior-transfer
  greps enforce exactly-one-match.
- All cross-stage column/key contracts verified against committed files; CRS uniform via
  shared loaders; id_crosswalk has zero PWS fan-out; debt aggregation has zero duplicate
  (District_ID, FiscalYear) and no double-counting.
- The uncommitted `02_make_figure1.R` diff (event-frequency panel) is itself correct —
  event set matches the panel builder's; it only inherits pre-existing H2/L11.

> Direction: none for now, save these to address later.
---

## Suggested sequencing (if fixing)

1. H1 + H4 (+ M1's garbage FISCAL_YEAR rows) — they change the **data**.
2. H3 census swap — changes demographics.
3. Rebuild Stage A/B outputs affected, then one clean Stage C pass: Model 1 → Model 2 →
   all tables/figures (clears H5 in the same run).
4. H2 legend fix rides along with the figure regeneration.
5. M-items as prioritized above; L-items opportunistically.

> Overall direction:

---

## Execution log (2026-07-23)

All directed fixes applied:

| Item | Fix | Where |
|---|---|---|
| H1 | case_when now maps `1/M1, 2/M2, 3/M3`; wayback reparsed | `02_assemble_drought_restrictions.R` |
| H2 | `breaks = rev(rib_cols)` added | `02_make_figure1.R` |
| H3 | Perc_Black → P006003/P003001 (any-part Black, total pop); rural/poverty semantics documented | `05_census_block_PWS.R` |
| H4 | dedup keys on district-YEAR, keeps FEWEST zeros (ties → later FY-end); `0005`→2005 repaired; 33 dateless rows dropped | `03_assemble_audits.R` |
| M1 | `_P1` lag block deleted (consumed nowhere; panel roll join is the lag) | `04_combine_district_fiscal_data.R` |
| M2 | CWS filter + excluded-event reconciliation (+>10% warning) | `build_recurrent_panel.R` |
| M3 | script 07 wired into Stage A | `run_all.R` |
| M4 | Unit column normalized to MG (GAL/1e6; unknown → NA with message) | `build_recurrent_panel.R` |
| M5 | no change per direction (same-week buyer response is plausible) | — |
| M6 | UNL cache guard keys on window coverage, not RESCRAPE | `02_make_figure1.R` |
| M7 | `dwv_widget_retry()` (3 attempts, backoff, NULL on failure); 06 records `Fetch_OK` + re-queues failures; 07 warns with failure count, failed `emergency_source` stays NA (no more coerced zeros) | `dwv_api_helpers.R`, scrapers 06/07 |
| M9 | `start_date` → 2010-08-01 (first observed mandatory notice) | `config.R` |
| M10 | cached-Model-1 spec check; prior-transfer completeness stop; Model1-newer-than-Model2 generation warning; panel guard also checks `panel_m2` fiscal cols. Principle adopted: scrape = additive, analysis = clobber (documented in scraper headers) | `01_fit`, `03_model_results_table.R`, `build_recurrent_panel.R` |
| M12 | boundaries dissolved to one polygon per PWS_ID before overlay | `02_combine_pws_with_counties.R` |

Verified after Stage A rerun: mandatory notices 2024: 228→**262**, 2025: 67→**95** (total
4,888); audits 20,197 rows, **0** duplicate district-years, **0** NA fiscal years.

Then: Stage B rerun (02→03→04→05), then one clean Stage C pass (Model 1 → Model 2 →
figures/tables), which also clears H5.
