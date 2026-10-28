#!/bin/bash
# Times Data quick updater
# Run this (or say "quick update times data") to refresh only the fast-moving
# daily/weekly series (petrol, diesel, gilts, small boat crossings, BoE base
# rate, mortgage rates) and push. Everything else keeps its last full-run value.
# Requires a prior full `update.sh` run so sparklines-page.json has a position
# for every label.
set -euo pipefail

REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"

echo "--- Running script.R --quick ---"
Rscript script.R --quick

echo "--- Checking for changes ---"
if git diff --quiet sparklines-page.json sparklines-slice.json; then
  echo "No changes to JSON files — data unchanged since last run."
  exit 0
fi

echo "--- Committing and pushing ---"
git add sparklines-page.json sparklines-slice.json data/moneyfacts_mortgage_historic.csv
git commit -m "Quick update: $(date '+%Y-%m-%d %H:%M')"
# Note: in a sandboxed/CI shell, git push can fail with "Missing or invalid
# credentials" if the macOS Keychain credential helper can't run (needs full
# filesystem access) — commit still succeeds, just re-run `git push` unsandboxed.
git push
