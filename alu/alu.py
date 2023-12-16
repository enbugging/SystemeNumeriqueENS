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

import sys  
sys.path.append("..")

from lib_carotte import *
from alu.add_and_subtract import *
from alu.and_or_xor import *
from alu.left_shift import *
from alu.right_shift import *

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

