#!/usr/bin/env python3
"""
Check whether any method marked "Preprint" in Ambilogue.csv has now been published.

For each preprint it looks for a journal version:
  - bioRxiv / medRxiv preprints: the bioRxiv API lists the journal DOI once published.
  - everything else (e.g. Research Square): Crossref lists it as "is-preprint-of".

When one is found, the row is updated:
  Preprint_DOI       <- the old Publication_DOI (unless already filled in)
  Preprint_Date      <- the old Publication_Date (unless already filled in)
  Publication_DOI    <- the journal DOI
  Publication_Date   <- the date the journal article was first published online (D/M/YYYY)
  Publication_Status <- Published

The GitHub workflow runs this and opens a pull request with the changes, so they
are reviewed before they reach the website. A summary of what changed (and what to
double check) is written to --summary.

Usage:
  python scripts/check_preprints.py
  python scripts/check_preprints.py --dry-run
  python scripts/check_preprints.py --summary preprint_summary.md
"""

import argparse
import csv
import io
import os
import time
import requests

DATA_FILE = os.path.join(os.path.dirname(__file__), '..', 'Ambilogue.csv')
BIORXIV_API = 'https://api.biorxiv.org/details'
CROSSREF_API = 'https://api.crossref.org/works'
BIORXIV_PREFIXES = ('10.1101/', '10.64898/')

# Crossref asks for an email in the User-Agent (its "polite pool").
EMAIL = os.environ.get('OPENALEX_EMAIL', '')
HEADERS = {'User-Agent': f'Ambilogue-preprint-check (mailto:{EMAIL})' if EMAIL else 'Ambilogue-preprint-check'}


def bare_doi(doi_url: str) -> str:
    return (
        doi_url.replace('https://doi.org/', '')
        .replace('http://doi.org/', '')
        .replace('doi:', '')
        .strip()
    )


def get_json(url: str) -> dict | None:
    try:
        response = requests.get(url, headers=HEADERS, timeout=30)
    except requests.RequestException as error:
        print(f'  [error] {url}: {error}')
        return None
    if response.status_code != 200:
        print(f'  [HTTP {response.status_code}] {url}')
        return None
    try:
        return response.json()
    except ValueError:
        return None


def journal_doi_from_biorxiv(doi: str) -> str | None:
    for server in ('biorxiv', 'medrxiv'):
        data = get_json(f'{BIORXIV_API}/{server}/{doi}')
        collection = (data or {}).get('collection') or []
        if collection:
            published = str(collection[-1].get('published', '')).strip()
            return published if published and published.upper() != 'NA' else None
    return None


def journal_doi_from_crossref(doi: str) -> str | None:
    data = get_json(f'{CROSSREF_API}/{doi}')
    relation = ((data or {}).get('message') or {}).get('relation') or {}
    for item in relation.get('is-preprint-of', []):
        if item.get('id-type') == 'doi':
            return item.get('id')
    return None


def find_journal_doi(preprint_doi: str) -> str | None:
    doi = bare_doi(preprint_doi)
    found = None
    if doi.startswith(BIORXIV_PREFIXES):
        found = journal_doi_from_biorxiv(doi)
    if not found:
        found = journal_doi_from_crossref(doi)
    return found


def journal_date(doi: str) -> tuple[str, bool] | None:
    """
    Return (D/M/YYYY, day_known) for the date the article was first published,
    preferring the online date. If Crossref only has a month or year, the missing
    parts are set to 1 and day_known is False so the summary can flag it.
    """
    data = get_json(f'{CROSSREF_API}/{doi}')
    message = (data or {}).get('message') or {}
    for key in ('published-online', 'published', 'issued'):
        parts = (message.get(key) or {}).get('date-parts') or [[]]
        parts = parts[0]
        if parts and parts[0]:
            year = int(parts[0])
            month = int(parts[1]) if len(parts) > 1 and parts[1] else 1
            day = int(parts[2]) if len(parts) > 2 and parts[2] else 1
            return f'{day}/{month}/{year}', len(parts) > 2
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    parser.add_argument('--dry-run', action='store_true', help='report only, do not change the CSV')
    parser.add_argument('--summary', help='write a markdown summary of the changes to this file')
    args = parser.parse_args()

    data_path = os.path.abspath(DATA_FILE)
    with open(data_path, 'rb') as f:
        raw = f.read()
    has_bom = raw.startswith(b'\xef\xbb\xbf')
    line_break = '\r\n' if b'\r\n' in raw else '\n'
    has_final_line_break = raw.endswith(b'\n')
    rows = list(csv.DictReader(io.StringIO(raw.decode('utf-8-sig'), newline='')))
    fieldnames = list(rows[0].keys())

    for column in ('Publication_Status', 'Preprint_DOI', 'Preprint_Date'):
        if column not in fieldnames:
            raise SystemExit(f'Ambilogue.csv has no {column} column.')

    changes = []
    for row in rows:
        if row.get('Publication_Status', '').strip() != 'Preprint':
            continue
        method = row['Method'].strip()
        doi_url = row.get('Publication_DOI', '').strip()
        if not doi_url.startswith('http'):
            print(f'  [skip] {method} - no DOI')
            continue

        print(f'  Checking {method}...')
        journal = find_journal_doi(doi_url)
        time.sleep(0.2)
        if not journal:
            print(f'    {method}: still a preprint')
            continue

        date = journal_date(journal)
        time.sleep(0.2)
        new_date, day_known = date if date else ('', False)
        print(f'    {method}: now published -> {journal} ({new_date or "date not found"})')
        changes.append({
            'method': method,
            'old_doi': doi_url,
            'old_date': row.get('Publication_Date', ''),
            'journal_doi': f'https://doi.org/{journal}',
            'new_date': new_date,
            'day_known': day_known,
            'author': row.get('Author', ''),
        })

        if args.dry_run:
            continue
        if not row.get('Preprint_DOI', '').strip():
            row['Preprint_DOI'] = doi_url
        if not row.get('Preprint_Date', '').strip():
            row['Preprint_Date'] = row.get('Publication_Date', '')
        row['Publication_DOI'] = f'https://doi.org/{journal}'
        if new_date:
            row['Publication_Date'] = new_date
        row['Publication_Status'] = 'Published'

    print(f'\n{len(changes)} preprint(s) now published.')

    if changes and not args.dry_run:
        out = io.StringIO(newline='')
        writer = csv.DictWriter(out, fieldnames=fieldnames, lineterminator=line_break)
        writer.writeheader()
        writer.writerows(rows)
        text = out.getvalue()
        if not has_final_line_break:
            text = text[:-len(line_break)]
        with open(data_path, 'wb') as f:
            f.write((b'\xef\xbb\xbf' if has_bom else b'') + text.encode('utf-8'))

    if args.summary and changes:
        lines = ['These preprints now appear to be published. Please check each before merging.\n']
        for c in changes:
            lines.append(f'### {c["method"]}')
            lines.append(f'- Journal article: {c["journal_doi"]}')
            lines.append(f'- Moved to the preprint fields: {c["old_doi"]} ({c["old_date"]})')
            if not c['new_date']:
                lines.append('- **Journal date not found.** Publication_Date was left as the preprint date; please set it.')
            else:
                lines.append(f'- New Publication_Date: {c["new_date"]}')
                if not c['day_known']:
                    lines.append('- **Crossref only has the month or year**, so the missing day was set to 1; please check it.')
            lines.append(f'- Author field still reads "{c["author"]}"; update the year if it changed.')
            lines.append('')
        lines.append('The citation count refreshes in the next daily update.')
        with open(args.summary, 'w', encoding='utf-8') as f:
            f.write('\n'.join(lines) + '\n')

    # Lets the workflow know whether to open a pull request
    github_output = os.environ.get('GITHUB_OUTPUT')
    if github_output:
        with open(github_output, 'a') as f:
            f.write(f'changes={len(changes)}\n')


if __name__ == '__main__':
    main()
