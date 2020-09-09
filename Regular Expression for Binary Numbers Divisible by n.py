j = lambda e: '|'.join(e)
s = lambda x: f"(?:{j(x)})" if len(x) > 1 else j(x)
u = lambda a, f, t, l: (a[f][1][t].append(l), a[t][0][f].append(l))
def i(b):
    d = __import__("collections").defaultdict
    a = d(lambda: d(lambda: d(list))) if b else []
    for n in range(b):
        a[n][1][n * 2 % b].append('0')
        a[n][1][(1 + n * 2) % b].append('1')
        a[(n * 2 + 1) % b][0][n].append('1')
        a[n * 2 % b][0][n].append('0')
    return a
def f(n, a, r=''):
    if n in a[n][1]:
        r = f"(?:{'|'.join(a[n][1][n])})*"
        a[n][1].pop(n)
        a[n][0].pop(n)
    return a, r
def g(r = ''):
    for f0, l0 in a[n][0].items():
        x = s(l0)
        [u(a, f0, f1, x + r + s(l1)) for f1, l1 in a[n][1].items()]
    return r
def r(a, b,z=''):
    for n in range(b, -1, -1):
        a, z = f(n, a)
        for h0, t0 in a[n][0].items():
            x = s(t0)
            [[a[h0][1][h1].append(x + z + s(t1)),a[h1][0][h0].append(x + z + s(t1))] for h1, t1 in a[n][1].items()]
    return z
def regex_divisible_by(n):
    return f"^{r(i(n),n)}$"