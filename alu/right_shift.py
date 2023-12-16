import sys  
sys.path.append("..")

from lib_carotte import *
import alu.constant as constant

def right_shift_1(a):
    assert(a.bus_size == 32)
    s = constant.z_1 + a[0:31]
    return s

def right_shift_2(a):
    assert(a.bus_size == 32)
    s = constant.z_2 + a[0:30]
    return s

def right_shift_4(a):
    assert(a.bus_size == 32)
    s = constant.z_4 + a[0:28]
    return s

def right_shift_8(a):
    assert(a.bus_size == 32)
    s = constant.z_8 + a[0:24]
    return s

def right_shift_16(a):
    assert(a.bus_size == 32)
    s = constant.z_16 + a[0:16]
    return s

def n_srl(a, b):
    assert(a.bus_size == b.bus_size)

    c45 = b[5]
    c67 = b[6] | b[7]
    c89 = b[8] | b[9]
    c1011 = b[10] | b[11]
    c1213 = b[12] | b[13]
    c1415 = b[14] | b[15]
    c1617 = b[16] | b[17]
    c1819 = b[18] | b[19]
    c2021 = b[20] | b[21]
    c2223 = b[22] | b[23]
    c2425 = b[24] | b[25]
    c2627 = b[26] | b[27]
    c2829 = b[28] | b[29]
    c3031 = b[30] | b[31]
    c47 = c45 | c67
    c811 = c89 | c1011
    c1215 = c1213 | c1415
    c1619 = c1617 | c1819
    c2023 = c2021 | c2223
    c2427 = c2425 | c2627
    c2831 = c2829 | c3031
    c07 = c47
    c815 = c811 | c1215
    c1623 = c1619 | c2023
    c2431 = c2427 | c2831
    c015 = c07 | c815
    c1631 = c1623 | c2431
    c031 = c015 | c1631

    a = Mux(b[4], right_shift_16(a), a)
    a = Mux(b[3], right_shift_8(a), a)
    a = Mux(b[2], right_shift_4(a), a)
    a = Mux(b[1], right_shift_2(a), a)
    a = Mux(b[0], right_shift_1(a), a)
    a = Mux(c031, a, constant.z_32)
    return a
