#!/bin/bash
# Welfare at a glance updater
# Run this (or say "update welfare data") to refresh welfare_page.json and push to GitHub.
# Needs STATXPLORE_API_KEY in .Renviron (gitignored) for the DWP Stat-Xplore series.
set -euo pipefail

REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"

echo "--- Running welfare.R ---"
Rscript welfare.R

echo "--- Checking for changes ---"
if git diff --quiet welfare_page.json; then
  echo "No changes to welfare_page.json — data unchanged since last run."
  exit 0
fi

echo "--- Committing and pushing ---"
git add welfare_page.json welfare.R
git commit -m "Update welfare data: $(date '+%Y-%m-%d')"
# Note: in a sandboxed/CI shell, git push can fail with "Missing or invalid
# credentials" if the macOS Keychain credential helper can't run (needs full
# filesystem access) — commit still succeeds, just re-run `git push` unsandboxed.
git push

echo "--- Done. Welfare sparklines updated ---"
