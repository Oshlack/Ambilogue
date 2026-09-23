#!/usr/bin/env python3
"""
Fetches a "Report a Community-Used Input Type" GitHub issue from Oshlack/Ambilogue
and writes it to pending_community_input.csv.
Usage: python scripts/community_input_to_pending.py <issue_number>
"""

import sys
import csv
import json
import re
import urllib.request
from pathlib import Path

REPO = "Oshlack/Ambilogue"
REPO_ROOT = Path(__file__).parent.parent
PENDING_CSV = REPO_ROOT / "pending_community_input.csv"

CSV_COLUMNS = ["Method", "Technology_Suitability_Community", "Evidence", "Issue_Number", "Issue_URL"]


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
        parsed[label] = content
    return parsed


def main():
    if len(sys.argv) != 2:
        print("Usage: python scripts/community_input_to_pending.py <issue_number>")
        sys.exit(1)

    issue_number = sys.argv[1]
    print(f"Fetching issue #{issue_number} from {REPO}...")

    issue = fetch_issue(issue_number)
    title = issue.get("title", "")
    body = issue.get("body", "")
    issue_url = issue.get("html_url", "")

    if not body:
        print("Error: issue body is empty.")
        sys.exit(1)

    print(f"Title: {title}")
    parsed = parse_body(body)

    method = parsed.get("Method Name", "").strip()
    technology = parse_checkboxes(parsed.get("Input Type(s) Used", ""))
    other = parsed.get("Other: Input Type(s) Used", "").strip()
    if other:
        technology = f"{technology}; {other}" if technology else other
    evidence = parsed.get("Evidence", "").strip()

    if not method:
        print("Error: could not find a Method Name in the issue body.")
        sys.exit(1)
    if not technology:
        print("Error: no input types were selected in the issue body.")
        sys.exit(1)

    row = {
        "Method": method,
        "Technology_Suitability_Community": technology,
        "Evidence": evidence,
        "Issue_Number": issue_number,
        "Issue_URL": issue_url,
    }

    with open(PENDING_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_COLUMNS)
        writer.writeheader()
        writer.writerow(row)

    print(f"\nWritten to {PENDING_CSV.name}")
    print(f"  Method:      {method}")
    print(f"  Input types: {technology}")
    print(f"  Evidence:    {evidence or '(none provided)'}")
    print("\nReview pending_community_input.csv, then run:")
    print("  python scripts/pending_community_input_to_ambilogue.py")


if __name__ == "__main__":
    main()
