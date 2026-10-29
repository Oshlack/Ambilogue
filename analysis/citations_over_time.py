#!/usr/bin/env python3
"""
Citations over time for the methods listed in Ambilogue.csv, using OpenAlex.

For every method with a DOI this script:
  1. resolves the DOI to an OpenAlex work ID,
  2. asks OpenAlex for the works that cite it, grouped by the citing work's
     publication year (filter=cites:<id>&group_by=publication_year),
  3. asks the same question once for ALL methods together, which gives the
     number of UNIQUE papers citing at least one ambient contamination tool
     (a paper citing SoupX and CellBender is counted once).

CSV outputs (written to --outdir, default: the folder containing this script).
Together these hold every number needed for the figures (drawn by
plot_citations.R):

  citations_by_year_long.csv     TIDY, one row per method x year
      Method, Group, OpenAlex_ID, Publication_DOI, Publication_Date,
      Preprint_Date, Publication_Year, Year, Years_since_publication, Citations,
      Cumulative_citations, Year_to_date, Retrieved
  citations_by_year_overall.csv  one row per year, all methods pooled
      Year, Group, Unique_citing_works, Sum_of_per_method_counts,
      Cumulative_unique, Cumulative_sum, Year_to_date, Retrieved

This script only retrieves data and writes CSVs. The figures are drawn from
these CSVs by plot_citations.R (run: Rscript plot_citations.R).

Unique_citing_works counts a paper once even if it cites several tools, so it
is <= Sum_of_per_method_counts. Citing works that OpenAlex dates outside the
plotted years (likely date errors) are left out of both files.

Read-only with respect to Ambilogue.csv (it is never modified).

Usage:
  python3 citations_over_time.py
  python3 citations_over_time.py --csv ../Ambilogue.csv --outdir ./out
  python3 citations_over_time.py --strict   # for automated runs

Optional environment variables:
  OPENALEX_EMAIL    sent as `mailto` (OpenAlex "polite pool")
  OPENALEX_API_KEY  sent as `api_key` if you have one

Notes:
  * Years are the publication year of the CITING work, so the current year is
    incomplete (flagged by the Year_to_date column).
  * Counts come from OpenAlex's own citation graph and will not match
    Google Scholar / Scopus exactly. Preprint and published versions of the
    same tool can be separate OpenAlex works; the CSV lists one DOI per tool.
"""

import argparse
import csv
import datetime as dt
import os
import sys
import time

import requests

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_CSV = os.path.join(HERE, '..', 'Ambilogue.csv')
BASE = 'https://api.openalex.org'

EMAIL = os.environ.get('OPENALEX_EMAIL', '')
API_KEY = os.environ.get('OPENALEX_API_KEY', '')

REQUEST_DELAY = 0.15  # seconds between calls


# --------------------------------------------------------------------------
# OpenAlex helpers
# --------------------------------------------------------------------------

def _params(extra=None):
    p = {}
    if EMAIL:
        p['mailto'] = EMAIL
    if API_KEY:
        p['api_key'] = API_KEY
    if extra:
        p.update(extra)
    return p


def api_get(url, extra=None, retries=4):
    """GET JSON from OpenAlex. Returns None on 404 or persistent failure."""
    for attempt in range(retries):
        try:
            resp = requests.get(url, params=_params(extra), timeout=30)
        except requests.RequestException as e:
            print(f'  [network error] {e}', file=sys.stderr)
            time.sleep(2 ** attempt)
            continue

        if resp.status_code == 200:
            return resp.json()
        if resp.status_code == 404:
            return None
        if resp.status_code in (429, 500, 502, 503, 504):
            wait = 2 ** attempt
            print(f'  [HTTP {resp.status_code}] retrying in {wait}s', file=sys.stderr)
            time.sleep(wait)
            continue

        print(f'  [HTTP {resp.status_code}] {url}', file=sys.stderr)
        return None
    return None


def bare_doi(doi_url):
    return (
        doi_url.strip()
        .replace('https://doi.org/', '')
        .replace('http://doi.org/', '')
    )


def resolve_work(doi_url):
    """DOI URL -> {'id': 'W123...', 'total': int} or None."""
    data = api_get(
        f'{BASE}/works/doi:{bare_doi(doi_url)}',
        {'select': 'id,doi,title,cited_by_count'},
    )
    if not data or 'id' not in data:
        return None
    return {
        'id': data['id'].rsplit('/', 1)[-1],
        'title': data.get('title') or '',
        'total': data.get('cited_by_count', 0),
    }


def citing_by_year(work_ids):
    """
    Number of works citing ANY of work_ids, grouped by citing-work publication
    year. Returns {year: count} or None on failure.
    """
    data = api_get(
        f'{BASE}/works',
        {'filter': 'cites:' + '|'.join(work_ids), 'group_by': 'publication_year'},
    )
    if data is None:
        return None
    out = {}
    for g in data.get('group_by', []):
        try:
            year = int(g['key'])
        except (KeyError, TypeError, ValueError):
            continue  # skip 'unknown'
        out[year] = out.get(year, 0) + g.get('count', 0)
    return out


# --------------------------------------------------------------------------
# Data collection
# --------------------------------------------------------------------------

def pub_year(date_str):
    """Ambilogue dates look like 22/3/2019."""
    try:
        return int(str(date_str).strip().split('/')[-1])
    except ValueError:
        return None


def pub_date_iso(date_str):
    """22/3/2019 -> 2019-03-22 ('' if it cannot be parsed)."""
    try:
        d, m, y = (int(p) for p in str(date_str).strip().split('/'))
        return dt.date(y, m, d).isoformat()
    except ValueError:
        return ''


def collect(csv_path):
    with open(csv_path, newline='', encoding='utf-8-sig') as f:
        rows = list(csv.DictReader(f))

    results, unresolved = [], []
    for row in rows:
        method = row.get('Method', '').strip()
        doi = row.get('Publication_DOI', '').strip()
        if not method:
            continue
        if not doi.startswith('http'):
            unresolved.append((method, 'no DOI'))
            continue

        print(f'  {method} ...', flush=True)
        work = resolve_work(doi)
        time.sleep(REQUEST_DELAY)
        if work is None:
            unresolved.append((method, 'DOI not found in OpenAlex'))
            continue

        by_year = citing_by_year([work['id']])
        time.sleep(REQUEST_DELAY)
        if by_year is None:
            unresolved.append((method, 'citation query failed'))
            continue

        results.append({
            'method': method,
            'group': 'Tool',
            'doi': doi,
            'pub_year': pub_year(row.get('Publication_Date', '')),
            'pub_date': pub_date_iso(row.get('Publication_Date', '')),
            'preprint_year': pub_year(row.get('Preprint_Date', '')),
            'preprint_date': pub_date_iso(row.get('Preprint_Date', '')),
            'category': row.get('Method_Category', '').strip(),
            'id': work['id'],
            'title': work['title'],
            'total': work['total'],
            'by_year': by_year,
        })

    overall_unique = {}
    if results:
        # OpenAlex allows up to 100 OR-ed values in one filter.
        ids = [r['id'] for r in results]
        for i in range(0, len(ids), 100):
            chunk = citing_by_year(ids[i:i + 100]) or {}
            time.sleep(REQUEST_DELAY)
            if len(ids) > 100:
                print('  [warn] >100 methods: unique count is per-chunk only',
                      file=sys.stderr)
            for y, c in chunk.items():
                overall_unique[y] = overall_unique.get(y, 0) + c

    return results, overall_unique, unresolved


def year_range(results, overall_unique, this_year):
    """
    Start at the earliest method publication year. OpenAlex occasionally lists
    citing works with impossible dates (e.g. 1991 or 2005 citing a 2020 tool);
    those are left out of the tables rather than stretching the year range.
    """
    pub_years = [r['pub_year'] for r in results if r['pub_year']]
    pub_years += [r['preprint_year'] for r in results if r['preprint_year']]
    if pub_years:
        first = min(pub_years)
    else:
        years = set(overall_unique)
        for r in results:
            years |= set(r['by_year'])
        first = min(years) if years else this_year
    return list(range(first, this_year + 1))


# --------------------------------------------------------------------------
# Output files
# --------------------------------------------------------------------------

def _cum(values):
    """Running total."""
    out, s = [], 0
    for v in values:
        s += v
        out.append(s)
    return out


def _bool(v):
    """TRUE/FALSE so R and Excel both read it as a logical."""
    return 'TRUE' if v else 'FALSE'


def write_csvs(results, overall_unique, years, outdir, this_year, retrieved):
    # --- tidy per-method, per-year table: the best starting point for R ----
    long_path = os.path.join(outdir, 'citations_by_year_long.csv')
    with open(long_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(['Method', 'Group', 'OpenAlex_ID', 'Publication_DOI',
                    'Publication_Date', 'Preprint_Date', 'Publication_Year', 'Year',
                    'Years_since_publication', 'Citations',
                    'Cumulative_citations', 'Year_to_date', 'Retrieved'])
        for r in results:
            counts = [r['by_year'].get(y, 0) for y in years]
            py = r['pub_year']
            for y, n, cn in zip(years, counts, _cum(counts)):
                w.writerow([r['method'], r['group'], r['id'], r['doi'],
                            r['pub_date'], r['preprint_date'], py or '', y, (y - py) if py else '',
                            n, cn, _bool(y == this_year), retrieved])

    # --- all methods pooled, one row per year ------------------------------
    overall_path = os.path.join(outdir, 'citations_by_year_overall.csv')
    uniq = [overall_unique.get(y, 0) for y in years]
    summed = [sum(r['by_year'].get(y, 0) for r in results) for y in years]
    with open(overall_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.writer(f)
        w.writerow(['Year', 'Group', 'Unique_citing_works',
                    'Sum_of_per_method_counts', 'Cumulative_unique',
                    'Cumulative_sum', 'Year_to_date', 'Retrieved'])
        for y, u, s, cu, cs in zip(years, uniq, summed, _cum(uniq), _cum(summed)):
            w.writerow([y, 'Tool', u, s, cu, cs, _bool(y == this_year), retrieved])

    return [long_path, overall_path]


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n\n')[0])
    ap.add_argument('--csv', default=DEFAULT_CSV, help='path to Ambilogue.csv')
    ap.add_argument('--outdir', default=HERE, help='where to write outputs')
    ap.add_argument('--strict', action='store_true',
                    help='exit with an error, before writing any citation '
                         'outputs, if a method with a DOI could not be '
                         'retrieved from OpenAlex (for automated runs)')
    args = ap.parse_args()

    csv_path = os.path.abspath(args.csv)
    outdir = os.path.abspath(args.outdir)
    os.makedirs(outdir, exist_ok=True)

    today = dt.date.today()
    this_year = today.year
    retrieved = today.isoformat()

    print(f'Reading {csv_path}')

    results, overall_unique, unresolved = collect(csv_path)

    # A method with no DOI is not a retrieval failure; anything else is.
    failures = [(m, why) for m, why in unresolved if why != 'no DOI']
    if args.strict and failures:
        print('\n--strict: could not retrieve these methods, so no citation '
              'outputs were written:', file=sys.stderr)
        for m, why in failures:
            print(f'  {m}: {why}', file=sys.stderr)
        sys.exit(1)

    if not results:
        print('No citation data retrieved - nothing was written.',
              file=sys.stderr)
        sys.exit(1)

    years = year_range(results, overall_unique, this_year)
    dropped = sum(c for r in results for y, c in r['by_year'].items()
                  if y < years[0] or y > years[-1])
    if dropped:
        print(f'\nNote: {dropped} citation(s) dated outside {years[0]}-'
              f'{years[-1]} were left out of the tables as likely '
              f'OpenAlex date errors.')
    written = write_csvs(results, overall_unique, years, outdir,
                         this_year, retrieved)

    print('\nWrote:')
    for p in written:
        print(f'  {p}')
    if unresolved:
        print('\nNot included:')
        for m, why in unresolved:
            print(f'  {m}: {why}')

    total_unique = sum(overall_unique.get(y, 0) for y in years)
    total_summed = sum(r['by_year'].get(y, 0) for r in results for y in years)
    print(f'\n{len(results)} methods; {total_unique:,} unique citing papers in total '
          f'({total_summed:,} summed per-method), {years[0]}-{years[-1]}.')


if __name__ == '__main__':
    main()
