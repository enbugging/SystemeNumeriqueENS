'''
ALU: Arithmetic Logic Unit for the microprocessor project.

The encoding is as follows:
    000: ... (reserved)
    001: ADD
    010: SUB
    011: AND
    100: OR
    101: XOR
    110: SLL
    111: SRL

The ALU takes two 32-bit inputs and outputs a 32-bit result.
The ALU also outputs a zero flag, which is set to 1 if the
result of the operation is zero. Calculations are done in 
big-endian.
'''

from lib_carotte import *

def full_adder(a, b, c):
    assert(a.bus_size == 1)
    assert(b.bus_size == 1)
    assert(c.bus_size == 1)
    s = a ^ b ^ c
    c_out = (a & b) | (b & c) | (c & a)
    return (s, c_out)

def n_adder(a, b):
    assert(a.bus_size == b.bus_size)
    c = Constant("0")
    (s, c) = full_adder(a[0], b[0], c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        (s_i, c) = full_adder(a[i], b[i], c)
        s = s + s_i
    return s

def n_subtractor(a, b):
    assert(a.bus_size == b.bus_size)
    c = Constant("1")
    (s, c) = full_adder(a[0], ~b[0], c) # Treat the 0 case separately since variables have a bus size >= 1
    for i in range(1, a.bus_size):
        (s_i, c) = full_adder(a[i], ~b[i], c)
        s = s + s_i
    return s

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

def alu(a, b, op):
    assert(a.bus_size == 32)
    assert(b.bus_size == 32)
    assert(op.bus_size == 3)
    zero_flag = Constant("0")

    s_add = n_adder(a, b)
    s_sub = n_subtractor(a, b)
    s_and = n_and(a, b)
    s_or = n_or(a, b)
    s_xor = n_xor(a, b)
    s_sll = n_sll(a, b)
    s_srl = n_srl(a, b)

    s_2b1 = Mux(op[2], Constant("00000000000000000000000000000000"), s_add)
    s_2b2 = Mux(op[2], s_sub, s_and)
    s_2b3 = Mux(op[2], s_or, s_xor)
    s_2b4 = Mux(op[2], s_sll, s_srl)

    s_3b1 = Mux(op[1], s_2b1, s_2b2)
    s_3b2 = Mux(op[1], s_2b3, s_2b4)

    s = Mux(op[0], s_3b1, s_3b2)

    for i in range(0, s.bus_size):
        zero_flag = zero_flag | s[i]
    return (s, ~zero_flag)

def main():
    a = Input(32)
    b = Input(32)
    op = Input(3)

    (result, flag_z) = alu(a, b, op)
    result.set_as_output("result")
    flag_z.set_as_output("flag_z")

