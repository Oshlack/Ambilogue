#!/usr/bin/env bash
# Fetches the citations per year for every method in Ambilogue.csv (OpenAlex)
# and writes the CSVs used by plot_citations.R.
#
# Usage (from anywhere):
#   bash analysis/update_citations_over_time.sh
#   bash analysis/update_citations_over_time.sh --some-extra-option

set -euo pipefail

# Run from the repository root so the paths below always work
cd "$(dirname "${BASH_SOURCE[0]}")/.."

python3 analysis/citations_over_time.py --csv Ambilogue.csv --outdir analysis "$@"
