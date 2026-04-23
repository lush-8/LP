import functools as f
import operator as op

def evens_product(L):

    return f.reduce(op.mul, filter(lambda x : x % 2 == 0, L), 1)

def reverse(L):

    return f.reduce(lambda acc, x : [x] + acc, L, [])

def zip_with(f, L1, L2):

    return list(map(f, L1, L2))

def count_if(f, L):

    return len(list(filter(f, L)))