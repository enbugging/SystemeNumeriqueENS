import subprocess
import time
import signal
import random
import re

def timeout_handler(signum, frame):
    raise TimeoutError("Solution execution timed out")

solution_command = ["../tp1/netlist_simulator.byte", "alu.net"]
num_tests = 10
num_ops = 10

def strip_prefix(string):
    match = re.match(r'^[^01]*', string)
    if match:
        return string[match.end():]
    return string

def gen_test_case():    
    a = random.randint(0, 2**32-1)
    b = random.randint(0, 2**32-1)
    op = random.randint(2, 2)
    if op == 0:
        s = 0
    elif op == 1:
        s = a + b
    elif op == 2:
        s = a - b
        if s < 0:
            s += 2**32
    elif op == 3:
        s = a & b
    elif op == 4:
        s = a | b
    elif op == 5:
        s = a ^ b
    elif op == 6:
        if b >= 32:
            s = 0
        else: 
            s = a << b
    elif op == 7:
        if b >= 32:
            s = 0
        else:
            s = a >> b 
    f = '1' if s == 0 else '0'
    a = '{:032b}'.format(a)
    b = '{:032b}'.format(b)
    s = '{:032b}'.format(s)
    if len(s) > 32:
        s = s[-32:]
    op = '{:03b}'.format(op)
    # inverse a, b, and s for little-endian
    a = a[::-1]
    b = b[::-1]
    s = s[::-1]
    return (a, b, op, s, f)

cnt = 0
for _ in range(num_tests):
    solution_process = subprocess.Popen(solution_command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    c1 = solution_process.stdout.readline()
    c2 = solution_process.stdout.readline()

    for _ in range(num_ops):
        # Generate a test case
        a, b, op, s, f = gen_test_case()
        judge_output = s

        # Send the test case to the solution process
        # reading and discarding the first two lines output by the solution process
        _ = solution_process.stdout.readline()
        solution_process.stdin.write(a + "\n")
        solution_process.stdin.write(b + "\n")
        solution_process.stdin.write(op + "\n")
        solution_process.stdin.flush()
        
        signal.signal(signal.SIGALRM, timeout_handler)
        signal.alarm(1)
        
        try:
            result = solution_process.stdout.readline().strip()
            flag_z = solution_process.stdout.readline().strip()
            result = strip_prefix(result)
            flag_z = strip_prefix(flag_z)
            # Perform judging logic (compare solution output with judge output)
            if result == judge_output and flag_z == f:
                cnt += 1
            else:
                print("Test case: a={}, b={}, op={}".format(a, b, op))
                print("Expected: s={}, f={}".format(judge_output, f))
                print("Received: s={}, f={}".format(result, flag_z))

        except TimeoutError as e:
            print("Solution timed out")
        finally:
            # Reset the alarm and continue to the next interaction
            signal.alarm(0)
    # Close subprocesses after all interactions for this test case
    solution_process.kill()
print(c1 + c2 + "Passed {} out of {} tests".format(cnt, num_tests * num_ops))