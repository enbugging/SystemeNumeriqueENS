from lib_carotte import *

def or_accumulator(a):
    sz = a.bus_size
    if sz == 1:
        return a[0]
    else:
        return or_accumulator(a[:sz//2]) | or_accumulator(a[sz//2:])

def n_and(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] & b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] & b[i])
    return (s, ~or_accumulator(s))

def n_or(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] | b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] | b[i])
    return (s, ~or_accumulator(s))

def n_xor(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] ^ b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] ^ b[i])
    return (s, ~or_accumulator(s))