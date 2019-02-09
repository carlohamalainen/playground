import pandas as pd

def get_data(url):
    if url == '/foo/bar/x':
        return 42 # in real life, talks to network, database, etc

    if url == '/foo/bar/y':
        return 30
    
    return None

def extend_get_data(x):
    if x['OP'] == 'GET_DATA':
        return get_data(x['DATA_SOURCE'])
    else:
        return None

def eval_rule(rule):
    s = []

    expr = []

    for (_, x) in rule.sort_values('ORDER').iterrows():
        op = x['OP']

        if op == 'GET_DATA':
            s.append(x['GET_DATA'])
            expr.append('(GET_DATA: ' + str(x['DATA_SOURCE']) + ')')

        elif op == 'CONSTANT':
            s.append(x['CONST'])
            expr.append(str(x['CONST']))

        elif op == 'MUL':
            b = s.pop()
            a = s.pop()
            s.append(a*b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '*' + b2 + ')')

        elif op == 'PLUS':
            b = s.pop()
            a = s.pop()
            s.append(a+b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '+' + b2 + ')')

        elif op == 'MINUS':
            b = s.pop()
            a = s.pop()
            s.append(a-b)

            b2 = expr.pop()
            a2 = expr.pop()
            expr.append('(' + a2 + '-' + b2 + ')')

        elif op == 'DIV':
            denominator = s.pop()
            numerator   = s.pop()
            s.append(numerator/denominator)

            denominator2 = expr.pop()
            numerator2   = expr.pop()
            expr.append('(' + numerator2 + '/' + denominator2 + ')')
        else:
            raise ValueError('Unknown operator: ' + op)

    if len(s) != 1:
        raise ValueError('Expected one item on the evaluation stack, but found: ' + str(s))

    if len(expr) != 1:
        raise ValueError('Expected one item on the expression stack, but found: ' + str(expr))

    return s[0], expr[0]

if __name__ == '__main__':
    df = pd.read_csv('ops.csv')

    df['GET_DATA'] = df.apply(extend_get_data, axis=1)

    by_rule = df.groupby('OUTPUT_ID')

    results = by_rule.apply(eval_rule)
    results = pd.DataFrame({'OUTPUT_ID':results.index, 'RESULT':results.values})

    for (_, row) in results.iterrows():
        print(row['OUTPUT_ID'])
        print(row['RESULT'][1])
        print(row['RESULT'][0])
        print()
