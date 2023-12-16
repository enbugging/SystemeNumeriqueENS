from lib_carotte import *

def and_acculumator(a):
    sz = a.bus_size
    if sz == 1:
        return a[0]
    else:
        return and_acculumator(a[sz//2-1]) & and_acculumator(a[sz//2:])

def n_and(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] & b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] & b[i])
    return (s, ~and_acculumator(s))

def n_or(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] | b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] | b[i])
    return (s, ~and_acculumator(s))

def n_xor(a, b):
    assert(a.bus_size == b.bus_size)
    s = a[0] ^ b[0]
    for i in range(1, a.bus_size):
        s = s + (a[i] ^ b[i])
    return (s, ~and_acculumator(s))