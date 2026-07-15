# Times Data — Britain at a Glance

This repo powers the sparklines at thetimes.com/uk and thetimes.com/data.
`script.R` fetches ~47 data series and writes `sparklines-page.json` and `sparklines-slice.json` (currently identical).

## "Update times data"
When the user says **"update times data"**, run `bash update.sh` in the terminal.
That script: runs `Rscript script.R`, checks whether the JSONs changed, commits and pushes to GitHub, which auto-deploys the sparklines.

## Reordering sparklines
Prompts like **"reorder: 1. rhdi, 2. small boats, 3. nhs waits"** mean: move those items to the top of the `bind_rows(list(...))` block in the STEP TWO section of `script.R`, leaving everything else in its current order. Then run `update.sh`.

## Script structure
- **Sections 1–47**: each fetches one data series
- **STEP TWO** (~line 720): `bind_rows()` assembles everything — the order here sets `position` in the JSON
- `safe_read_html()` and `safe_download()` wrappers at the top handle ONS rate-limiting (3s gap + retry)
- NHS financial year URL is dynamic (`nhs_wl_url` variable at top)
- Prison data (section 23) scrapes GOV.UK monthly ODS files for current + previous year

## Adding a new measure
1. Add a numbered fetch section in Part One
2. Add to `bind_rows()` in STEP TWO with `label`, `note`, `parent`, `up` (good/bad/neutral), `unit`
3. `up = 'good'` = green when rising; `up = 'bad'` = red when rising

## Key sources
ONS (most series), NHS England, Bank of England, Land Registry, Home Office, RAC, YouGov, GfK, FRED
