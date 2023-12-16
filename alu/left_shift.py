import sys  
sys.path.append("..")

from lib_carotte import *
import alu.constant as constant

def left_shift_1(a):
    assert(a.bus_size == 32)
    s = a[1:32] + constant.z_1
    return s

def left_shift_2(a):
    assert(a.bus_size == 32)
    s = a[2:32] + constant.z_2
    return s

def left_shift_4(a):
    assert(a.bus_size == 32)
    s = a[4:32] + constant.z_4
    return s

def left_shift_8(a):
    assert(a.bus_size == 32)
    s = a[8:32] + constant.z_8
    return s

def left_shift_16(a):
    assert(a.bus_size == 32)
    s = a[16:32] + constant.z_16
    return s

def n_sll(a, b):
    assert(a.bus_size == b.bus_size)
    for i in range(5, b.bus_size):
        a = Mux(b[i], a, constant.z_32)
    a = Mux(b[4], left_shift_16(a), a)
    a = Mux(b[3], left_shift_8(a), a)
    a = Mux(b[2], left_shift_4(a), a)
    a = Mux(b[1], left_shift_2(a), a)
    a = Mux(b[0], left_shift_1(a), a)
    return a