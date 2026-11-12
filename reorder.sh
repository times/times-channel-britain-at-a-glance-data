#!/bin/bash
# Reorder the sparklines without refetching data, then push to GitHub.
#   bash reorder.sh "NHS waiting list" "Real GDP per capita" "gdp growth" ...
set -euo pipefail

REPO="$(cd "$(dirname "$0")" && pwd)"
cd "$REPO"

if [ "$#" -eq 0 ]; then
  echo 'Usage: bash reorder.sh "Label one" "Label two" ...'
  exit 1
fi

echo "--- Reordering ---"
Rscript reorder.R "$@"

if git diff --quiet sparklines-page.json sparklines-slice.json script.R; then
  echo "Order unchanged — nothing to commit."
  exit 0
fi

echo "--- Committing and pushing ---"
git add sparklines-page.json sparklines-slice.json script.R
git commit -m "Reorder sparklines: $(date '+%Y-%m-%d %H:%M')"
git push

echo "--- Done. New order is live on thetimes.com ---"
