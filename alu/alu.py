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

A possible idea of calculating the flag is to do it for each
xor, or, and, left and right shift. The first three use standard
binary trees. The latter two are by muxing between if b > 5 and
b <= 5. If b > 5, then the flag is set. If b <= 5, then the flag
is computed as a is shifted with barrel shift.

The critical path, however, is still addition and subtraction.
'''

import sys  
sys.path.append("..")

from lib_carotte import *
import alu.constant as constant
from alu.add_and_subtract import *
from alu.and_or_xor import *
from alu.left_shift import *
from alu.right_shift import *

def alu(a, b, op):
    assert(a.bus_size == 32)
    assert(b.bus_size == 32)
    assert(op.bus_size == 3)
    
    (s_add, flag_z_add) = adder(a, b)
    (s_sub, flag_z_sub) = subtractor(a, b)
    (s_and, flag_z_and) = n_and(a, b)
    (s_or , flag_z_or ) = n_or(a, b)
    (s_xor, flag_z_xor) = n_xor(a, b)
    (s_sll, flag_z_sll) = n_sll(a, b)
    (s_srl, flag_z_srl) = n_srl(a, b)

    s_2b1 = Mux(op[2], constant.z_32, s_add)
    s_2b2 = Mux(op[2], s_sub, s_and)
    s_2b3 = Mux(op[2], s_or, s_xor)
    s_2b4 = Mux(op[2], s_sll, s_srl)
    s_3b1 = Mux(op[1], s_2b1, s_2b2)
    s_3b2 = Mux(op[1], s_2b3, s_2b4)
    s = Mux(op[0], s_3b1, s_3b2)

    f_2b1 = Mux(op[2], Constant("1"), flag_z_add)
    f_2b2 = Mux(op[2], flag_z_sub, flag_z_and)
    f_2b3 = Mux(op[2], flag_z_or, flag_z_xor)
    f_2b4 = Mux(op[2], flag_z_sll, flag_z_srl)
    f_3b1 = Mux(op[1], f_2b1, f_2b2)
    f_3b2 = Mux(op[1], f_2b3, f_2b4)
    f = Mux(op[0], f_3b1, f_3b2)

    return (s, f)

def main():
    a = Input(32)
    b = Input(32)
    op = Input(3)

    constant.z_1 = Constant("0")
    constant.z_2 = Constant("00")
    constant.z_4 = Constant("0000")
    constant.z_8 = Constant("00000000")
    constant.z_16 = Constant("0000000000000000")
    constant.z_32 = Constant("00000000000000000000000000000000")

    (result, flag_z) = alu(a, b, op)
    result.set_as_output("result")
    flag_z.set_as_output("flag_z")