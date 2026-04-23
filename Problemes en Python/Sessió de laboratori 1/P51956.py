import functools as f
import operator as op

def myLength(L):

    return f.reduce(lambda acc, _ : acc + 1, L, 0)

def myMaximum(L):

    return f.reduce(lambda a, b : a if a > b else b, L)

def average(L):

    return sum(L) / myLength(L)

def buildPalindrome(L):

    return L[::-1] + L

def remove(L1, L2):

    func = lambda x : x not in L2
    return list(filter(func, L1))

def flatten(L):

    if not isinstance(L, list): 
        return [L]
    return f.reduce(op.add, map(flatten, L), [])

def oddsNevens(L):

    odd = lambda x : x % 2 == 1
    even = lambda x : x % 2 == 0
    return list(filter(odd, L)), list(filter(even, L))

def primeDivisors(n):

    isPrime = lambda p : p > 1 and not any(p % i == 0 for i in range(2, int(p ** 0.5) + 1))
    divisors = set(f.reduce(op.add, ([i, n // i] for i in range(1, int(n ** 0.5) + 1) if n % i == 0), []))
    return sorted(list(filter(isPrime, divisors)))