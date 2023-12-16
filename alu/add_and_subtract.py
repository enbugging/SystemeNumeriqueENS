import sys  
sys.path.append("..")

from lib_carotte import *
import alu.constant as constant

def full_adder(a, b, c):
    assert(a.bus_size == 1)
    assert(b.bus_size == 1)
    assert(c.bus_size == 1)
    s = a ^ b ^ c
    c_out = (a & b) | (b & c) | (c & a)
    return (s, c_out)

def n_adder(a, b):
    assert(a.bus_size == b.bus_size)
    c = constant.z_1
    (s, c) = full_adder(a[0], b[0], c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        (s_i, c) = full_adder(a[i], b[i], c)
        s = s + s_i
    return s

def n_subtractor(a, b):
    assert(a.bus_size == b.bus_size)
    c = ~constant.z_1
    (s, c) = full_adder(a[0], ~b[0], c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        (s_i, c) = full_adder(a[i], ~b[i], c)
        s = s + s_i
    return s