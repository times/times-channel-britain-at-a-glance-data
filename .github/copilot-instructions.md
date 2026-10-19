# Times Data — Britain at a Glance

This repo powers the sparklines at thetimes.com/uk and thetimes.com/data.
`script.R` fetches ~58 data series and writes `sparklines-page.json` and `sparklines-slice.json` (currently identical).

## "Update times data"
- **"update times data"** or **"full update times data"** → run `bash update.sh`. Runs `Rscript script.R` (fetches all ~58 series), checks whether the JSONs changed, commits and pushes to GitHub, which auto-deploys the sparklines.
- **"quick update times data"** → run `bash quick-update.sh`. Runs `Rscript script.R --quick`, which only refetches the fast-moving daily/weekly series (petrol, diesel, gilts, small boat crossings, BoE base rate, mortgage rates) and reuses the last full run's values for everything else. Needs an existing `sparklines-page.json` from a prior full run.

## Sync first
For any task that will edit files in this repo, first check the remote branch state and pull/rebase before making changes. If the branch is not up to date, sync it before editing so we do not build on stale local state.

## Reordering sparklines
Prompts like **"reorder: 1. rhdi, 2. small boats, 3. nhs waits"** mean: move those items to the top of the `bind_rows(list(...))` block in the STEP TWO section of `script.R`, leaving everything else in its current order. Then run the **full** `update.sh` — never `quick-update.sh`.

Reordering never works on a quick update: quick mode doesn't rebuild `position` from the list order, it just copies each label's last-known `position` straight out of the previous `sparklines-page.json` and only refreshes the values for its 6 fast-moving series. Any position change requires editing `bind_rows()` and running a full update so `position` gets recalculated for every label.

## Script structure
- **Part One**: each numbered section fetches one data series
- `quick <- '--quick' %in% commandArgs(trailingOnly = TRUE)` near the top gates most of Part One behind `if (!quick) { ... }` — only the 6 always-run sections (small boats, base rates, petrol/diesel, gilts, diesel-reuse, mortgage rates) execute in quick mode
- `compute_file()` (defined near the top) holds the shared "build file from a master tibble" logic (year-ago comparisons, colours, nesting into the JSON `data` shape) — used by both the full and quick paths
- **STEP TWO**: `if (!quick) { ... } else { ... }` — full branch does `bind_rows()` of all ~58 series (order here sets `position`) and calls `compute_file(master)`; quick branch builds a small `quick_master` of just the 6 fast series, calls `compute_file(quick_master)`, and merges it into the previous JSON's records (keeping their `position`, dropping only the refreshed labels)
- `safe_read_html()` and `safe_download()` wrappers at the top handle ONS rate-limiting (3s gap + retry)
- NHS financial year URL is dynamic (`nhs_wl_url` variable at top)
- Prison data section scrapes GOV.UK monthly ODS files for current + previous year

## Adding a new measure
1. Add a numbered fetch section in Part One
2. Add to `bind_rows()` in STEP TWO with `label`, `note`, `parent`, `up` (good/bad/neutral), `unit`
3. `up = 'good'` = green when rising; `up = 'bad'` = red when rising

## Key sources
ONS (most series), NHS England, Bank of England, Land Registry, Home Office, RAC, YouGov, GfK, FRED
