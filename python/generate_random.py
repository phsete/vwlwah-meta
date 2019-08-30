import argparse
import numpy as np

# using the example from https://docs.python.org/2/library/argparse.html
parser = argparse.ArgumentParser(description='Generate N lines of B digit bitstreams.')
parser.add_argument('lines', metavar='N', type=int, help='The number of lines to be generated.')
parser.add_argument('bits', metavar='B', type=int, help='The number bits per word.')
parser.add_argument('density', metavar='P', type=float, help='The probability for a bit to be 1.')

args = parser.parse_args()

for i in range(args.lines):
    bit_list = np.random.choice([0,1], size=(args.bits,), p=[1-args.density, args.density])
    print(''.join(map(str, bit_list)))
