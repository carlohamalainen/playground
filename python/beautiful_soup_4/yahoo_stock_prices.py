import copy
import json
import time
import requests
import bs4
import re

def load_yahoo(base, ticker):
    url = f'{base}/{ticker}/history/'
    # https://stackoverflow.com/questions/39631386/how-to-understand-this-raw-html-of-yahoo-finance-when-retrieving-data-using-pyt
    x = requests.get(url).content
    soup = bs4.BeautifulSoup(x, features='html.parser')
    script = soup.find("script",text=re.compile("root.App.main")).text
    j = json.loads(re.search("root.App.main\s+=\s+(\{.*\})", script).group(1), parse_float=lambda x: x)
    return j

def to_time(t):
    if t is None:
        return None
    else:
        return time.gmtime(t)

def parse_dividend(x):
    p = copy.deepcopy(x)

    try:
        if p['type'] == 'DIVIDEND':
            p['date']   = to_time(p['date'])
            p['amount'] = p['amount']
            p['data']   = p['data']
            p['datetime'] = p.pop('date')
            p['dividend_amount'] = p.pop('amount')
            p['dividend_data'] = p.pop('data')
            return p
        else:
            return None
    except KeyError:
            return None

def parse_price(x):
    p = copy.deepcopy(x)

    try:
        p['date']     = to_time(p['date'])
        p['open']     = p['open']
        p['high']     = p['high']
        p['low']      = p['low']
        p['close']    = p['close']
        p['volume']   = p['volume']
        p['adjclose'] = p['adjclose']
        p['datetime'] = p.pop('date')
        p['type']     = 'PRICE'
        return p
    except KeyError:
        return None

def load_fixing(base, ticker):
    history = load_yahoo(base, ticker)

    x = history['context']['dispatcher']['stores']['StreamDataStore']['quoteData'][ticker]

    exchange_timezone_longname  = x['exchangeTimezoneName']

    close = x['regularMarketPreviousClose']

    regular_market_time_fmt = x['regularMarketTime']['fmt']
 
    return(ticker, exchange_timezone_longname, regular_market_time_fmt, float(close['raw']))

def load_prices(base, ticker):
    history = load_yahoo(base, ticker)

    prices = history['context']['dispatcher']['stores']['HistoricalPriceStore']['prices']                                        

    good = []
    bad  = []

    for p in prices:
        as_dividend = parse_dividend(p)
        as_price    = parse_price(p)

        if as_dividend is not None:
            good.append(as_dividend)
            continue

        if as_price is not None:
            good.append(as_price)
            continue

        bad.append(p)

    page = history['context']['dispatcher']['stores']['PageStore']['pageData']
    ccy  = history['context']['dispatcher']['stores']['StreamDataStore']['quoteData'][ticker]['currency']

    fmt  = history['context']['dispatcher']['stores']['StreamDataStore']['quoteData'][ticker]['regularMarketOpen']['fmt'] # e.g. '1.1940'
    fmt = '%.' + str(len(fmt.split('.')[1])) + 'f'

    if bad != []:
        print('Some failures:', bad)

    for g in good:
        g['ticker'] = page['symbol']
        g['title']  = page['title']
        g['ccy']    = ccy
        g['fmt']    = fmt

    good = sorted(good, key=lambda x: x['datetime'])

    return good

if __name__ == '__main__':
    base   = 'https://sg.finance.yahoo.com/quote'

    print('AUD to SGD:', load_fixing(base, 'AUDSGD=X'))
    print()

    tickers = ['BHP.AX', 'ES3.SI']

    for ticker in tickers:
        prices = load_prices(base, ticker)

        for p in prices:
            if p['type'] == 'PRICE' and p['close'] is not None:
                print(p['ticker'], p['ccy'], time.strftime('%F', p['datetime']), p['fmt'] % float(p['close']))

        for p in prices:
            if p['type'] == 'DIVIDEND':
                print(p['ticker'], p['ccy'], time.strftime('%F', p['datetime']), p['dividend_amount'], p['dividend_data'])

        print()
