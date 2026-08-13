#!/usr/bin/env python3
"""
Update citation counts in Ambilogue.csv using the OpenAlex API.
Runs daily via GitHub Actions.

OpenAlex docs: https://docs.openalex.org/
"""

import csv
import os
import time
import requests

DATA_FILE = os.path.join(os.path.dirname(__file__), '..', 'Ambilogue.csv')
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


def is_preprint(citation_str: str) -> bool:
    return '(preprint)' in str(citation_str)


def format_citations(count: int, preprint: bool) -> str:
    return f'{count} (preprint)' if preprint else str(count)


def main():
    data_path = os.path.abspath(DATA_FILE)
    print(f'Reading {data_path}')

    with open(data_path, newline='', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames

    updated = 0
    skipped = 0

    for row in rows:
        method = row.get('Method', '').strip()
        doi = row.get('Publication_DOI', '').strip()

        if not doi or not doi.startswith('http'):
            print(f'  [skip] {method} — no DOI')
            skipped += 1
            continue

        print(f'  Querying {method}...')
        count = get_citation_count(doi)

        if count is not None:
            old = row.get('Citations', '')
            preprint = is_preprint(old)
            new_val = format_citations(count, preprint)
            if str(old).strip() != new_val:
                row['Citations'] = new_val
                updated += 1
                print(f'    {method}: {old!r} → {new_val!r}')
            else:
                print(f'    {method}: {count} (no change)')

        time.sleep(0.15)  # Stay within OpenAlex rate limits

    with open(data_path, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f'\nDone — {updated} updated, {skipped} skipped (no DOI)')


if __name__ == '__main__':
    main()
