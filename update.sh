#!/bin/bash
# Times Data updater
# Run this (or say "update times data") to refresh the sparklines and push to GitHub.
set -euo pipefail

REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"

echo "--- Running script.R ---"
Rscript script.R

echo "--- Checking for changes ---"
if git diff --quiet sparklines-page.json sparklines-slice.json; then
  echo "No changes to JSON files — data unchanged since last run."
  exit 0
fi

echo "--- Committing and pushing ---"
git add sparklines-page.json sparklines-slice.json script.R data/
git commit -m "Update data: $(date '+%Y-%m-%d')"
# Note: in a sandboxed/CI shell, git push can fail with "Missing or invalid
# credentials" if the macOS Keychain credential helper can't run (needs full
# filesystem access) — commit still succeeds, just re-run `git push` unsandboxed.
git push

echo "--- Done. Sparklines updated on thetimes.com ---"
