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

    zero_flag = constant.z_1
    
    s_add = adder(a, b)
    s_sub = subtractor(a, b)
    s_and = n_and(a, b)
    s_or = n_or(a, b)
    s_xor = n_xor(a, b)
    s_sll = n_sll(a, b)
    s_srl = n_srl(a, b)

    s_2b1 = Mux(op[2], constant.z_32, s_add)
    s_2b2 = Mux(op[2], s_sub, s_and)
    s_2b3 = Mux(op[2], s_or, s_xor)
    s_2b4 = Mux(op[2], s_sll, s_srl)

    s_3b1 = Mux(op[1], s_2b1, s_2b2)
    s_3b2 = Mux(op[1], s_2b3, s_2b4)

    s = Mux(op[0], s_3b1, s_3b2)

    f01 = s[0] | s[1]
    f23 = s[2] | s[3]
    f45 = s[4] | s[5]
    f67 = s[6] | s[7]
    f89 = s[8] | s[9]
    f1011 = s[10] | s[11]
    f1213 = s[12] | s[13]
    f1415 = s[14] | s[15]
    f1617 = s[16] | s[17]
    f1819 = s[18] | s[19]
    f2021 = s[20] | s[21]
    f2223 = s[22] | s[23]
    f2425 = s[24] | s[25]
    f2627 = s[26] | s[27]
    f2829 = s[28] | s[29]
    f3031 = s[30] | s[31]
    f03 = f01 | f23
    f47 = f45 | f67
    f811 = f89 | f1011
    f1215 = f1213 | f1415
    f1619 = f1617 | f1819
    f2023 = f2021 | f2223
    f2427 = f2425 | f2627
    f2831 = f2829 | f3031
    f07 = f03 | f47
    f815 = f811 | f1215
    f1623 = f1619 | f2023
    f2431 = f2427 | f2831
    f015 = f07 | f815
    f1631 = f1623 | f2431
    f031 = f015 | f1631
    return (s, ~f031)

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

