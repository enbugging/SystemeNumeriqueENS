from lib_carotte import *

def right_shift_1(a):
    assert(a.bus_size == 32)
    s = Constant("0") + a[0:31]
    return s

def right_shift_2(a):
    assert(a.bus_size == 32)
    s = Constant("00") + a[0:30]
    return s

def right_shift_4(a):
    assert(a.bus_size == 32)
    s = Constant("0000") + a[0:28]
    return s

def right_shift_8(a):
    assert(a.bus_size == 32)
    s = Constant("00000000") + a[0:24]
    return s

def right_shift_16(a):
    assert(a.bus_size == 32)
    s = Constant("0000000000000000") + a[0:16]
    return s

def n_srl(a, b):
    assert(a.bus_size == b.bus_size)
    for i in range(5, b.bus_size):
        a = Mux(b[i], a, Constant("00000000000000000000000000000000"))
    a = Mux(b[4], right_shift_16(a), a)
    a = Mux(b[3], right_shift_8(a), a)
    a = Mux(b[2], right_shift_4(a), a)
    a = Mux(b[1], right_shift_2(a), a)
    a = Mux(b[0], right_shift_1(a), a)
    return a
