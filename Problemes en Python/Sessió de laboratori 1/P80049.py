import functools as f
import operator as op

def count_unique(L):

    return len(set(L))

def remove_duplicates(L):

    return list(set(L))

def flatten(L):

    return sum(L, [])

def flatten_rec(L):

    if not isinstance(L, list):
        return [L]
    return f.reduce(op.add, map(flatten_rec, L), [])