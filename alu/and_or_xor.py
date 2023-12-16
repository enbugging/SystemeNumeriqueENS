from lib_carotte import *

def n_and(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] & b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] & b[i])
    return s

def n_or(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] | b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] | b[i])
    return s

def n_xor(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] ^ b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] ^ b[i])
    return s