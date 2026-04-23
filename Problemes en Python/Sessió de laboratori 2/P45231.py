def fibs():

    a , b = 0 , 1

    while True:
        yield a
        a , b = b , a + b

def roots(x):

    f = x

    while True:
        yield f

        if f == 0:
            pass
        else:
            f = 0.5 * (f + x / f)

def primes():

    D = {}
    q = 2

    while True:
        if q not in D:
            yield q
            D[q * q] = [q]
        else:
            for p in D[q]:
                D.setdefault(p + q, []).append(p)
            del D[q]
        q += 1

def hammings():

    yield 1
    h = [1]
    i2 = i3 = i5 = 0 

    while True:
        n2 = h[i2] * 2
        n3 = h[i3] * 3
        n5 = h[i5] * 5
        next = min(n2, n3, n5)
        yield next
        h.append(next)

        if next == n2: i2 += 1
        if next == n3: i3 += 1
        if next == n5: i5 += 1