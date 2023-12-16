from lib_carotte import *

def left_shift_1(a):
    assert(a.bus_size == 32)
    s = a[1:32] + Constant("0")
    return s

def left_shift_2(a):
    assert(a.bus_size == 32)
    s = a[2:32] + Constant("00")
    return s

def left_shift_4(a):
    assert(a.bus_size == 32)
    s = a[4:32] + Constant("0000")
    return s

def left_shift_8(a):
    assert(a.bus_size == 32)
    s = a[8:32] + Constant("00000000")
    return s

def left_shift_16(a):
    assert(a.bus_size == 32)
    s = a[16:32] + Constant("0000000000000000")
    return s

def n_sll(a, b):
    assert(a.bus_size == b.bus_size)
    for i in range(5, b.bus_size):
        a = Mux(b[i], a, Constant("00000000000000000000000000000000"))
    a = Mux(b[4], left_shift_16(a), a)
    a = Mux(b[3], left_shift_8(a), a)
    a = Mux(b[2], left_shift_4(a), a)
    a = Mux(b[1], left_shift_2(a), a)
    a = Mux(b[0], left_shift_1(a), a)
    return a