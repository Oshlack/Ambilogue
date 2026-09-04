#!/usr/bin/env python3
"""
Appends pending_submission.csv to Ambilogue.csv, then wipes pending_submission.csv.
Usage: python scripts/pending_to_ambilogue.py
"""

import csv
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
PENDING_CSV = REPO_ROOT / "pending_submission.csv"
AMBILOGUE_CSV = REPO_ROOT / "Ambilogue.csv"

CSV_COLUMNS = [
    "Method", "Author", "Publication_DOI", "Publication_Date",
    "Ecosystem", "Availability", "Citations", "Requires_Empties",
    "Additional_Requirements", "Technology_Suitability", "Ambient_Basis",
    "Method_Category", "Approach_Description", "Benchmarking_Summary",
    "Can_Filter_Empties", "Can_Ambient_Estimate_Droplets",
    "Can_Ambient_Estimate_Genes", "Can_Ambient_Correct",
    "Can_Ambient_QC_Plot", "Can_Ambient_Simulate", "Other_Features",
    "Notes", "Bioconductor_Link", "GitHub_Link", "Notes_Publication_DOI",
]


def main():
    if not PENDING_CSV.exists():
        print(f"Error: {PENDING_CSV.name} not found. Run issue_to_pending.py first.")
        return

    # Read pending rows
    with open(PENDING_CSV, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        pending_rows = [row for row in reader if any(v.strip() for v in row.values())]

    if not pending_rows:
        print(f"{PENDING_CSV.name} is empty — nothing to add.")
        return

    # Show a summary and ask for confirmation
    print("About to add the following to Ambilogue.csv:")
    for row in pending_rows:
        print(f"  - {row.get('Method', '(unknown)')} by {row.get('Author', '(unknown)')}")
    confirm = input("\nProceed? [y/N] ").strip().lower()
    if confirm != "y":
        print("Aborted. No changes made.")
        return

    # Append to Ambilogue.csv (file already has a BOM; open in append mode without adding another)
    with open(AMBILOGUE_CSV, "a", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_COLUMNS)
        for row in pending_rows:
            writer.writerow(row)

    print(f"\nAdded {len(pending_rows)} row(s) to {AMBILOGUE_CSV.name}.")

    # Wipe pending_submission.csv (leave just the header)
    with open(PENDING_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_COLUMNS)
        writer.writeheader()

    print(f"{PENDING_CSV.name} wiped.")
    print("\nNext steps:")
    print("  1. git add Ambilogue.csv pending_submission.csv")
    print("  2. git commit -m 'Add <method name> to Ambilogue'")
    print("  3. git push")
    print("  4. Trigger the update_citations workflow on GitHub (Actions tab → Run workflow)")


if __name__ == "__main__":
    main()
