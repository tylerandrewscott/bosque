# Data Architecture — Weekly Recurrent-Event Panel

How the drought-and-debt analysis dataset is assembled: how each raw source
(temporal and otherwise) maps onto the modeling observations, and where in the
code each step happens.

The canonical builder is
[`code/02_model/build_recurrent_panel.R`](../code/02_model/build_recurrent_panel.R),
sourced by both the frequentist and Bayesian/INLA fit scripts so the two
approaches fit the *exact* same data. Section numbers below (§1–§4) refer to that
file.

---

## Unit of observation

The panel is **week × PWS (public water system)**, structured as a
**counting-process (survival) panel** — *not* week × district. Each row is an
interval `(tstart, tstop]` measured in **weeks since the analysis start**, for a
single system, carrying that week's covariates and a `0/1` event flag.

District identity enters only as:

1. a **clustering / frailty label** attached to each system, and
2. the **join key** on which annual finances are merged.

So the precise description is *week-system observations, a subset of which are
district-linked*. If you need a genuinely district-level unit (systems
aggregated up per week), this panel does **not** provide it — it stays at the
system level and only *borrows* district identity.

### The clock

Defined once in [`code/config.R`](../code/config.R):

```
start_date = 2010-01-01
end_date   = 2025-12-31
```

Time is `weeks = (date − start_date) / 7`. Every temporal source is projected
onto this axis.

---

## The temporal skeleton — what defines the weeks

The weekly grid comes entirely from the **drought data**, built in two steps.

### 1. County-week DSCI — `00_assemble/01_assemble_drought_data.R`

Pulls **weekly county-level DSCI** (Drought Severity & Coverage Index) for all of
Texas from the U. Nebraska USDM API (2000 → today) → `dsci_measures.RDS`. The
USDM publishes one map per week, so a "week" is defined by the USDM map date
(`MapDate`, renamed `DroughtDate`).

### 2. System-week DSCI — `01_combine/03_combine_district_and_drought.R`

Converts county-weeks to **system-weeks**: each PWS's DSCI for a given
`DroughtDate` is the **area-weighted mean of the DSCI of every county it
overlaps**, weighted by `Prop_Over_County` (the share of the system's service
area falling in that county) → `pws_drought_weekly.RDS`, one row per
(`PWS_ID`, `DroughtDate`) with a continuous `DSCI`.

**Consequence:** the set of weeks in the panel = the set of USDM map dates, and
every system inherits a spatially interpolated DSCI for every week.

---

## §2 — Grid → intervals (counting-process skeleton)

For each system, ordered by date (`build_recurrent_panel.R` §2):

- `tstart = (DroughtDate − start_date) / 7`
- `tstop  = next week's tstart` (the lead); the **last** week runs to the window
  end (`end_time`).
- Zero/negative-length intervals are dropped (`tstop > tstart`).
- Each interval carries that week's `DSCI` and `DSCI_100 = DSCI / 100`.

The interval is the span over which that week's DSCI is treated as constant — a
standard piecewise-constant time-varying covariate.

---

## §1 — Events (mandatory restriction notices)

Defined once in `mandatory_restriction_events()`
([`code/ingest_helpers.R`](../code/ingest_helpers.R)) so the model panel and the
descriptive figure cannot drift apart.

- **Source:** `combined_restriction_records.RDS`.
- **Event definition:** a mandatory drought-restriction notice, `STAGE ∈ {M1,
  M2, M3}`, within the window.
- Collapsed to **one row per (`PWS_ID`, `event_date`)** using `NOTIFIED_YMD`, so
  the same notice scraped multiple times counts once; the same stage re-declared
  on a *different* date is a genuine new (recurrent) event and is kept.
- `event_time` = that date in weeks. An interval gets `event = 1` iff a notice's
  `event_time` falls in `(tstart, tstop]`. Multiple notices in one week collapse
  to a single `1`.

This is what makes the panel a **recurrent-event** counting process — a system
can contribute many event-weeks.

---

## §2b — Network covariate (`seller_restricted`)

A **time-varying** covariate flagging whether an upstream *seller* — a system the
PWS buys water from — is currently under a mandatory restriction.

- **Edges:** `pws_purchase_edges.RDS` (`00_assemble/07_scrape_source_and_purchases.R`,
  from DWV `DashPurchases.SELLERWSNUMBER`) — a **time-invariant** buyer→seller
  snapshot. Wholesale ties are sticky, and the DWV API exposes only current state.
- **Status:** the *seller's* restriction status reuses the same
  `mandatory_restriction_events()` notices as the event flag (§1). So the edges are
  fixed but the covariate varies in week-time.
- **Construction:** a seller notice at week `E` is "in effect" for `NEIGHBOR_PERSIST`
  weeks (default **4** ≈ a month; a restriction persists, the weekly grid is too
  fine for a single-week match, and recurrent re-declarations refresh the window). A
  buyer interval `(tstart, tstop]` gets `seller_restricted = 1` iff some seller
  window `[E, E + NEIGHBOR_PERSIST)` overlaps it (`tstart < win_end` AND
  `tstop > win_start`). Non-purchasers (no out-edges) are structurally `0`.
- Built on `panel` in §2b of `build_recurrent_panel.R`, so it carries into both
  `panel_m1` and `panel_m2`, and enters the model formula via `shared_vars`.
- `NEIGHBOR_PERSIST` is the one knob; set it before `source()`-ing the builder to
  vary how long an upstream restriction is treated as active.

Because it can only be `1` where a buys-from edge exists, `seller_restricted`
decomposes cleanly against the static `has_interconnect` control: the static flag
carries the "has a wholesale tie" baseline, and `seller_restricted` adds the
time-varying "and that tie is currently stressed."

---

## §3 — Time-invariant system controls (join on `PWS_ID`)

One value per system, broadcast onto every week of that system:

| Source file | Contributes |
|---|---|
| `pws_population.RDS` | `Connections`, `PopServed` (DWV master totals) |
| `storage_connections_data.txt` | total storage capacity `TSTC` → `storage_per_conn_g`; interconnection count → `has_interconnect` |
| `pws_source.RDS` | DWV source flags → `source_surface` (surface vs ground), `purchases_water` (wholesale is the primary source), `emergency_source` (an emergency-designated supply facility / interconnect) |
| `pws_demos_MR.RDS` | income, home value, structure age, % rural / Hispanic / Black, Dem vote share |

The three source flags come from `00_assemble/07_scrape_source_and_purchases.R`.
`source_surface` / `purchases_water` are derived from DWV's system-level
`D_FED_PRIM_SRC_CD` (GW / GWP / SW / SWP: the code's `S`-prefix = surface, `P`-suffix
= purchased). `emergency_source` is 1 when the system has a supply facility (well /
intake / consecutive-connection interconnect) whose `AVAILABILITY_CODE` is `E`
(emergency) in `DashSourceWater`. All three are treated as **time-invariant** (the
DWV API is a current snapshot; source infrastructure and wholesale ties are sticky).
Note this makes the model's pre-existing `has_interconnect` ("has any buys-from
edge", from `nrow(DashPurchases)`) the *broad* wholesale-tie flag, of which
`purchases_water` (primary-purchased) is a strict subset.

Merged into `controls`, transformed (logs, `asinh`, etc.), then joined to the
panel on `PWS_ID`. **`panel_m1`** keeps only rows with complete controls
(`complete.cases` over `ctrl_vars`), so the estimation sample is fixed across the
prime-time and appendix specifications.

Note: `median_structure_age` is pinned at the analysis-start year — the one
"control" evaluated *as of* the window open rather than being truly
time-invariant.

### District linkage

`id_crosswalk.RDS` attaches `District_ID` to each system via a left join.
Systems with **no** water district (city- or investor-owned) get `NA`: they
remain in Model 1 (frailty then clusters them individually) but are **excluded**
from Model 2.

---

## §4 — Time-varying finances (annual → weekly join)

The only covariate block that varies in system-time *and* originates at a
different temporal resolution (**annual**).

1. **`combined_and_lagged_finances.RDS`** (built in
   `01_combine/04_combine_district_fiscal_data.R`): audits + debt merged on
   (`District_ID`, `FISCAL_YEAR`), deduplicated, then **one-period lagged within
   district** (`shift()` ordered by year) — year *t*'s row carries year *t−1*'s
   fiscal values.
2. In the builder: filtered to the window years; each fiscal indicator computed
   per district-year — `debt_per_conn`, `fund_bal_per_conn`, `revenue_per_conn`,
   `operating_ratio` (all `asinh`-scaled), plus the binary `debt_svc_tax`.
3. **The join:** **`panel_m2`** = the district-linked subsample; each week gets
   `cal_year = year(start_date + tstart*7)`, and finances are merged on
   (`District_ID`, `cal_year` = `YEAR`). Every week in calendar year *Y* for a
   district inherits that district's year-*Y* (lag-of-*Y*−1) audit values — a
   step function that changes once per calendar year.
4. Only district-years with a matched audit are kept (`has_audit == 1`). Because
   the fiscal indicators are collinear, each is fit in its **own** model, and
   each fit drops only the rows missing *that* covariate — so a zero-expenditure
   audit (forced to `NA` operating_ratio) drops out of the operating-ratio model
   only.

---

## Summary — source → observation mapping

| Source | Native resolution | Mapped to week-system by… |
|---|---|---|
| USDM DSCI (`dsci_measures` → `pws_drought_weekly`) | weekly × county | area-weighted county overlap → weekly × system; **defines the weeks** |
| Mandatory restriction notices | event dates | interval-join: `event = 1` if date ∈ (tstart, tstop] |
| Population / connections / storage / demographics | one per system | broadcast to all weeks (join on `PWS_ID`) |
| Source flags (`pws_source.RDS`) | one per system | broadcast to all weeks (join on `PWS_ID`) → `source_surface`, `purchases_water`, `emergency_source` |
| Buys-from edges (`pws_purchase_edges.RDS`) | one per buyer→seller edge (time-invariant) | seller's §1 notices projected onto buyer-weeks via a persistence window → time-varying `seller_restricted` |
| District crosswalk | one per system | label join on `PWS_ID` (NA ⇒ Model-1-only) |
| Audits + debt (`combined_and_lagged_finances`) | annual × district, **lagged 1 yr** | step function: each week joins on (`District_ID`, calendar-year) |

## Outputs

- **`panel_m1`** — full system sample: drought skeleton + time-invariant
  controls, with district attached as a frailty label.
- **`panel_m2`** — district-linked subsample with the annual finances layered on
  as a per-calendar-year step function.
