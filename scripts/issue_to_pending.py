#!/usr/bin/env python3
"""
Fetches a GitHub issue from Oshlack/Ambilogue and writes it to pending_submission.csv.
Usage: python scripts/issue_to_pending.py <issue_number>
"""

import sys
import csv
import json
import re
import urllib.request
from pathlib import Path

REPO = "Oshlack/Ambilogue"
REPO_ROOT = Path(__file__).parent.parent
PENDING_CSV = REPO_ROOT / "pending_submission.csv"
AMBILOGUE_CSV = REPO_ROOT / "Ambilogue.csv"

# Maps issue template field labels to CSV column names
FIELD_MAP = {
    "Method Name": "Method",
    "Author(s) and Publication Year": "Author",
    "Publication DOI": "Publication_DOI",
    "Publication Date": "Publication_Date",
    "Preprint Date": "Preprint_Date",
    "Preprint DOI": "Preprint_DOI",
    "Software Ecosystem": "Ecosystem",
    "Software Availability": "Availability",
    "Requires Empty Droplets?": "Requires_Empties",
    "Additional Requirements": "Additional_Requirements",
    "Officially Supported Input Types": "Technology_Suitability",
    "Additional Input Types Used by the Community": "Technology_Suitability_Community",
    "Ambient Basis": "Ambient_Basis",
    "Method Category": "Method_Category",
    "Approach Description": "Approach_Description",
    "Benchmarking Summary": "Benchmarking_Summary",
    "Can filter empty droplets?": "Can_Filter_Empties",
    "Can estimate ambient contamination in droplets?": "Can_Ambient_Estimate_Droplets",
    "Can identify genes driving ambient contamination?": "Can_Ambient_Estimate_Genes",
    "Can correct ambient contamination?": "Can_Ambient_Correct",
    "Can generate ambient contamination QC plots?": "Can_Ambient_QC_Plot",
    "Can simulate datasets with ambient contamination?": "Can_Ambient_Simulate",
    "Other Features": "Other_Features",
    "Notes": "Notes",
    "Bioconductor Link": "Bioconductor_Link",
    "GitHub Link": "GitHub_Link",
    "Publication DOI Notes": "Notes_Publication_DOI",
}

def load_columns():
    """Use the column order of Ambilogue.csv, so the two files can never drift apart."""
    with open(AMBILOGUE_CSV, newline="", encoding="utf-8-sig") as f:
        return next(csv.reader(f))


# Fields rendered as checkboxes (multiple selections joined by "; ")
CHECKBOX_FIELDS = {"Ecosystem", "Availability", "Technology_Suitability", "Technology_Suitability_Community", "Method_Category"}

# Form option wording that differs from the value used in Ambilogue.csv
VALUE_MAP = {
    "Ecosystem": {"Cell Ranger": "Cell Ranger v3.0.0+"},
}

# "Other" free-text fields and which CSV column they append to
OTHER_FIELDS = {
    "Other: Software Ecosystem": "Ecosystem",
    "Other: Software Availability": "Availability",
    "Other: Officially Supported Input Types": "Technology_Suitability",
    "Other: Additional Input Types Used by the Community": "Technology_Suitability_Community",
    "Other: Method Category": "Method_Category",
}


def fetch_issue(issue_number):
    url = f"https://api.github.com/repos/{REPO}/issues/{issue_number}"
    req = urllib.request.Request(
        url,
        headers={
            "Accept": "application/vnd.github+json",
            "User-Agent": "Ambilogue-script",
        }
    )
    with urllib.request.urlopen(req) as response:
        return json.loads(response.read())


def parse_checkboxes(text):
    """Extract checked items from a checkbox section, excluding 'Other'."""
    checked = re.findall(r"- \[x\] (.+)", text, re.IGNORECASE)
    # Exclude the generic "Other" tick — the value comes from the free-text field
    return "; ".join(item.strip() for item in checked if item.strip().lower() != "other")


def parse_body(body):
    """Parse issue body into a dict of {label: value}."""
    sections = re.split(r"^### (.+)$", body, flags=re.MULTILINE)

    parsed = {}
    for i in range(1, len(sections), 2):
        label = sections[i].strip()
        content = sections[i + 1].strip() if i + 1 < len(sections) else ""

        if content == "_No response_":
            content = ""

        csv_col = FIELD_MAP.get(label)
        if csv_col and csv_col in CHECKBOX_FIELDS:
            content = parse_checkboxes(content)
            renames = VALUE_MAP.get(csv_col, {})
            content = "; ".join(renames.get(item, item) for item in content.split("; ") if item)

        parsed[label] = content

    return parsed


def main():
    if len(sys.argv) != 2:
        print("Usage: python scripts/issue_to_pending.py <issue_number>")
        sys.exit(1)

    issue_number = sys.argv[1]
    print(f"Fetching issue #{issue_number} from {REPO}...")

    issue = fetch_issue(issue_number)
    title = issue.get("title", "")
    body = issue.get("body", "")

    if not body:
        print("Error: issue body is empty.")
        sys.exit(1)

    print(f"Title: {title}")
    parsed = parse_body(body)

    # Build CSV row
    columns = load_columns()
    row = {col: "" for col in columns}
    for label, csv_col in FIELD_MAP.items():
        row[csv_col] = parsed.get(label, "")

    # Append any "Other" free-text values to their parent checkbox field
    for other_label, csv_col in OTHER_FIELDS.items():
        other_val = parsed.get(other_label, "").strip()
        if other_val:
            existing = row[csv_col]
            row[csv_col] = f"{existing}; {other_val}" if existing else other_val

    # Publication status has its own column; Citations is only the number
    pub_status = parsed.get("Publication Status", "").strip()
    row["Publication_Status"] = pub_status
    if pub_status == "Preprint":
        row["Citations"] = "0"
    else:
        row["Citations"] = ""  # auto-populated by the update_citations GitHub Action

    # Write to pending_submission.csv
    with open(PENDING_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=columns, extrasaction="ignore")
        writer.writeheader()
        writer.writerow(row)

    print(f"\nWritten to {PENDING_CSV.name}")
    print(f"  Method:  {row.get('Method', '(unknown)')}")
    print(f"  Author:  {row.get('Author', '(unknown)')}")
    print(f"  DOI:     {row.get('Publication_DOI', '(unknown)')}")
    print("\nReview and edit pending_submission.csv if needed, then run:")
    print("  python scripts/pending_to_ambilogue.py")


if __name__ == "__main__":
    main()
