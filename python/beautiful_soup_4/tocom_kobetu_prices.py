import bs4
import requests
import pandas

URLS = [ "https://www.tocom.or.jp/market/kobetu/east_base_elec.html",
         "https://www.tocom.or.jp/market/kobetu/west_base_elec.html",
         "https://www.tocom.or.jp/market/kobetu/east_peak_elec.html",
         "https://www.tocom.or.jp/market/kobetu/west_peak_elec.html",
       ]

def parse_html_table_with_header(t):
    rows = []

    for bits in t:
        x = parse_rows(bits)
        if x != []: rows += x

    header = [h.text.strip() for h in t.find('thead').find_all('th')]
    return (header, rows)

def parse_rows(x):
    rows = []

    if hasattr(x, 'find_all'):
        for row in x.find_all('tr'):
            cols = row.find_all('td')
            cols = [c.text.strip() for c in cols]
            this_row = [c for c in cols if c != []]
            if cols:
                rows.append(this_row)
    return rows

def scrape(url):
    tables = {}

    soup = bs4.BeautifulSoup(requests.get(url).content, features='html.parser')

    h3 = soup.find('h3')

    while h3 is not None:
        name  = h3.contents[0].strip()

        table0 = h3.find_next_sibling('table')
        table1 = h3.find_next_sibling('table').find_next_sibling('table')

        tables[name] = [parse_rows(table0), parse_html_table_with_header(table1)]

        h3 = h3.find_next_sibling('h3')

    for (name, [t0, t1]) in tables.items():
        [[session, _, prices_in]] = t0
        (header, rows)            = t1

        df = pandas.DataFrame(data=rows, columns=header)

        print(url)
        print()
        print(name)
        print(session)
        print(prices_in)
        print()
        print(df)
        print()
        print()

if __name__ == '__main__':
    for url in URLS: scrape(url)
