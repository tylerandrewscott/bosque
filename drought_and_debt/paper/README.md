# Manuscript (`paper/`)

The manuscript lives here as a **Quarto source** (`manuscript.qmd`) whose numbers,
tables, and figures are pulled from the model pipeline's `output/*.csv` at render
time. Nothing is hand-typed — re-run the models, re-render, and every number
updates itself.

## Files

| File | Role |
| --- | --- |
| `manuscript.qmd` | **Source of truth.** Prose + inline `` `r ...` `` numbers + table chunks. Edit this. |
| `_setup.R` | Render-time data layer: loads the CSVs and defines the helper API below. Sourced by the setup chunk. |
| `manuscript.md` | Rendered output (numbers baked in). This is what you paste into Google Docs. Regenerated on every render — don't edit by hand. |

## Workflow

1. **Run the models** (writes the CSVs the paper reads):
   ```
   Rscript drought_and_debt/code/run_all.R      # Stage C fits + writes output/*.csv, output/paper_facts.csv
   ```
2. **Render the paper** (bakes the fresh numbers in):
   ```
   quarto render drought_and_debt/paper/manuscript.qmd        # -> manuscript.md and manuscript.docx
   ```
   Change a model spec → rerun step 1 → re-render → the paper's numbers follow.
3. **Share with co-authors:** open `manuscript.md`, copy it, and paste into a
   Google Doc (Docs converts Markdown natively; enable *Tools → Preferences →
   Enable Markdown*). Or import `manuscript.docx`. Pull their edits back with
   Docs' **Download → Markdown** and diff against `manuscript.qmd`.

> **Quarto is not yet installed on this machine.** Install it once:
> `brew install quarto` (or download from <https://quarto.org/docs/get-started/>).
> Until then, the R layer can be exercised with `knitr::knit("manuscript.qmd")`
> from this directory, which executes the same inline code and chunks.

## Helper API (used inside `manuscript.qmd`)

Defined in `_setup.R`. The model-column constants `M_DEBT`, `M_FUNDBAL`,
`M_REVENUE`, `M_JOINT`, `M_GLOBAL` name the fitted models.

| Call | Returns |
| --- | --- |
| `fact("key")` | A formatted scalar fact from `output/paper_facts.csv` (e.g. `fact("n_districts_sample")` → `613`). |
| `b("term", model)` | Coefficient (log-hazard) posterior mean. |
| `bci("term", model)` | Coefficient with 95% credible interval, e.g. `-0.14 (95% CrI: -0.16, -0.11)`. |
| `hr("term", model)` | Hazard ratio (exp of the coefficient). |
| `hrci("term", model)` | Hazard ratio with 95% credible interval. |
| `credible("term", model)` | `TRUE` if the 95% CrI excludes 0/1. |
| `results_table()` | Wide main fiscal-models table. |
| `appendix_table()` | Global Model 1 (prior-setting) table. |
| `descriptives_table()` | Grouped summary-statistics table. |

`term` accepts either the raw name (`"debt_go_per_conn"`) or its pretty label. A
missing fact renders as `??key??` and a missing estimate errors — both are
loud on purpose so gaps surface at render time.

## Facts

Scalar prose facts come from `output/paper_facts.csv`, written by
`code/02_model/05_paper_facts.R` from the shared analysis panel. To cite a new
number in the prose, add an `add_fact(...)` line there, rerun the pipeline, and
reference it with `` `r fact("your_key")` ``.

## Step-2 status (stale sections)

Blocks marked `<!-- STALE (step 2): ... -->` in `manuscript.qmd` describe the
**old** model and must be rewritten for the current spec (weekly 2010–2025, DSCI,
two iid frailties — district/system + county, no spatial/ICAR — repeated events,
730-day/FY-end-date audit rule, wholesaler control; demographics, consumption,
operating ratio, and the debt-service-tax interaction dropped). The wiring and
tables are already current; only the surrounding narrative needs updating.
