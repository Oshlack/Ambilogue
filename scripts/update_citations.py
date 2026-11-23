#!/usr/bin/env python3
"""
Update citation counts in Ambilogue.csv and benchmarking.csv using the OpenAlex API.
Runs daily via GitHub Actions.

OpenAlex docs: https://docs.openalex.org/
"""

import csv
import os
import time
import requests

# (file, column that names each row). Both files have Publication_DOI and Citations columns.
DATA_FILES = [
    (os.path.join(os.path.dirname(__file__), '..', 'Ambilogue.csv'), 'Tool'),
    (os.path.join(os.path.dirname(__file__), '..', 'benchmarking.csv'), 'Author'),
]
OPENALEX_BASE = 'https://api.openalex.org/works'

# Providing an email uses OpenAlex's "polite pool" — faster and more reliable.
# Set OPENALEX_EMAIL as a GitHub Actions secret (or leave blank for anonymous).
EMAIL = os.environ.get('OPENALEX_EMAIL', '')


def get_citation_count(doi_url: str) -> int | None:
    """Return cited_by_count from OpenAlex for a given DOI URL, or None on failure."""
    doi = (
        doi_url
        .replace('https://doi.org/', '')
        .replace('http://doi.org/', '')
        .strip()
    )
    if not doi:
        return None

    url = f'{OPENALEX_BASE}/doi:{doi}'
    params = {'mailto': EMAIL} if EMAIL else {}

    try:
        resp = requests.get(url, params=params, timeout=15)
        if resp.status_code == 200:
            return resp.json().get('cited_by_count')
        elif resp.status_code == 404:
            print(f'  [not found] {doi}')
        else:
            print(f'  [HTTP {resp.status_code}] {doi}')
    except requests.RequestException as e:
        print(f'  [error] {doi}: {e}')

    return None


def update_file(data_path: str, label_column: str):
    data_path = os.path.abspath(data_path)
    print(f'Reading {data_path}')

    # utf-8-sig reads the byte order mark off the first column name (and writes it back)
    with open(data_path, newline='', encoding='utf-8-sig') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames

    updated = 0
    skipped = 0

    for row in rows:
        label = row.get(label_column, '').strip()
        doi = row.get('Publication_DOI', '').strip()

        if not doi or not doi.startswith('http'):
            print(f'  [skip] {label} — no DOI')
            skipped += 1
            continue

        print(f'  Querying {label}...')
        count = get_citation_count(doi)

        if count is not None:
            old = row.get('Citations', '')
            new_val = str(count)
            if str(old).strip() != new_val:
                row['Citations'] = new_val
                updated += 1
                print(f'    {label}: {old!r} → {new_val!r}')
            else:
                print(f'    {label}: {count} (no change)')

        time.sleep(0.15)  # Stay within OpenAlex rate limits

    with open(data_path, 'w', newline='', encoding='utf-8-sig') as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f'\nDone — {updated} updated, {skipped} skipped (no DOI)\n')


def main():
    for data_path, label_column in DATA_FILES:
        update_file(data_path, label_column)


if __name__ == '__main__':
    main()
