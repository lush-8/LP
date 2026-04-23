import math

def absValue(x):

    if x < 0:
        return -x
    return x

def power(x, p):

    return x ** p 

def isPrime(x):

    if x < 2:
        return False
    elif x == 2: 
        return True
    elif x % 2 == 0:
        return False
    
    for i in range(3, int(math.sqrt(x)) + 1, 2):
        if x % i == 0:
            return False
    return True

def slowFib(n):

    if n == 0 or n == 1: 
        return n
    return slowFib(n - 1) + slowFib(n - 2)

def quickFib(n):

    a , b = 0 , 1

    for _ in range(n):
        a , b = b , a + b
    return a