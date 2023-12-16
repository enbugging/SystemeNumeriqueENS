import sys  
sys.path.append("..")

from lib_carotte import *
import alu.constant as constant

def full_adder(a, b, c):
    assert(a.bus_size == 1)
    assert(b.bus_size == 1)
    assert(c.bus_size == 1)
    s = a ^ b ^ c
    c_out = (a & b) | (c & (a ^ b))
    g = a & b
    p = a | b
    return (s, c_out, p, g)

def ripple_carry_adder(a, b, c):
    assert(a.bus_size == b.bus_size)
    assert(c.bus_size == 1)
    (s, carry, _, _) = full_adder(a[0], b[0], c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        (s_i, carry, _, _) = full_adder(a[i], b[i], carry)
        s = s + s_i
    return (s, carry)

def carry_lookahead_adder(a, b, c, stage=5):
    assert(a.bus_size == b.bus_size)
    assert(c.bus_size == 1)
    block_size = 1 << stage
    if stage == 0:
        (s, c_i, p_i, g_i) = full_adder(a[0], b[0], c)
        flag_nz = s
    else:
        (s, c_i, p_i, g_i, flag_nz) = carry_lookahead_adder(a[:block_size], b[:block_size], c, stage-1)
    p = p_i
    g = g_i
    for i in range(block_size, a.bus_size, block_size):
        if stage == 0:
            (s_i, c_i, p_i, g_i) = full_adder(a[i], b[i], g_i | (p_i & c_i))
            flag_nz_i = s_i
        else:
            (s_i, c_i, p_i, g_i, flag_nz_i) = carry_lookahead_adder(a[i:i+block_size], b[i:i+block_size], g_i | (p_i & c_i), stage-1)
        p = p & p_i
        g = g_i | (p_i & g)
        s = s + s_i
        flag_nz = flag_nz | flag_nz_i
    return (s, g | (p & c), p, g, flag_nz)

def adder(a, b):
    assert(a.bus_size == b.bus_size)
    #(s, _) = ripple_carry_adder(a, b, constant.z_1)
    (s, _, _, _, flag_nz) = carry_lookahead_adder(a, b, constant.z_1)
    return (s, ~flag_nz)

def subtractor(a, b):
    assert(a.bus_size == b.bus_size)
    #(s, _) = ripple_carry_adder(a, ~b, ~constant.z_1)
    not_b = ~b[0]
    for i in range(1, b.bus_size):
        not_b = not_b + (~b[i])
    (s, _, _, _, flag_nz) = carry_lookahead_adder(a, not_b, ~constant.z_1)
    return (s, ~flag_nz)