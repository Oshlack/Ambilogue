#!/usr/bin/env python3
"""
Merges pending_community_input.csv into the matching method's row in Ambilogue.csv
(only ever touching the Technology_Suitability_Community field of an existing row -
never adds a new row or changes a method's officially supported input types), then
wipes pending_community_input.csv.
Usage: python scripts/pending_community_input_to_ambilogue.py
"""

import csv
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
PENDING_CSV = REPO_ROOT / "pending_community_input.csv"
AMBILOGUE_CSV = REPO_ROOT / "Ambilogue.csv"

PENDING_COLUMNS = ["Method", "Technology_Suitability_Community", "Evidence", "Issue_Number", "Issue_URL"]


def split_terms(value):
    return [t.strip() for t in (value or "").split(";") if t.strip()]


def main():
    if not PENDING_CSV.exists():
        print(f"Error: {PENDING_CSV.name} not found. Run community_input_to_pending.py first.")
        return

    with open(PENDING_CSV, newline="", encoding="utf-8") as f:
        pending_rows = [row for row in csv.DictReader(f) if any(v.strip() for v in row.values())]

    if not pending_rows:
        print(f"{PENDING_CSV.name} is empty - nothing to add.")
        return

    with open(AMBILOGUE_CSV, newline="", encoding="utf-8-sig") as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames
        rows = list(reader)

    if not fieldnames or "Technology_Suitability_Community" not in fieldnames:
        print("Error: Ambilogue.csv has no Technology_Suitability_Community column yet.")
        return

    changed = []
    for pending in pending_rows:
        method = pending.get("Method", "").strip()
        new_terms = split_terms(pending.get("Technology_Suitability_Community", ""))
        if not method or not new_terms:
            continue

        matches = [r for r in rows if r.get("Method", "").strip().lower() == method.lower()]
        if not matches:
            close = [r["Method"] for r in rows if method.lower() in r.get("Method", "").lower()]
            print(f"Warning: no method named '{method}' found in Ambilogue.csv - skipped.")
            if close:
                print(f"  Did you mean: {', '.join(close)}?")
            continue
        if len(matches) > 1:
            print(f"Warning: more than one method matched '{method}' - skipped. Resolve manually.")
            continue

        row = matches[0]
        official = set(t.lower() for t in split_terms(row.get("Technology_Suitability", "")))
        existing_community = split_terms(row.get("Technology_Suitability_Community", ""))
        existing_lower = set(t.lower() for t in existing_community)

        added = []
        for term in new_terms:
            if term.lower() in official or term.lower() in existing_lower:
                continue
            existing_community.append(term)
            existing_lower.add(term.lower())
            added.append(term)

        if added:
            row["Technology_Suitability_Community"] = "; ".join(existing_community)
            changed.append((method, added, pending.get("Issue_URL", "")))
        else:
            print(f"Nothing new to add for '{method}' - all reported types are already listed.")

    if not changed:
        print("\nNo rows were changed.")
        return

    print("\nAbout to update Ambilogue.csv:")
    for method, added, issue_url in changed:
        suffix = f"  ({issue_url})" if issue_url else ""
        print(f"  - {method}: + {', '.join(added)}{suffix}")
    confirm = input("\nProceed? [y/N] ").strip().lower()
    if confirm != "y":
        print("Aborted. No changes made.")
        return

    with open(AMBILOGUE_CSV, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nUpdated {len(changed)} row(s) in {AMBILOGUE_CSV.name}.")

    with open(PENDING_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=PENDING_COLUMNS)
        writer.writeheader()

    print(f"{PENDING_CSV.name} wiped.")
    print("\nNext steps:")
    print("  1. git add Ambilogue.csv pending_community_input.csv")
    print("  2. git commit -m 'Add community-reported input type(s)'")
    print("  3. git push")


if __name__ == "__main__":
    main()
