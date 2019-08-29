import random
import argparse

# using the example from https://docs.python.org/2/library/argparse.html
parser = argparse.ArgumentParser(description='Generate N lines of B digit bitstreams.')
parser.add_argument('lines', metavar='N', type=int, help='The number of lines to be generated.')
parser.add_argument('bits', metavar='B', type=int, help='The number bits per word.')

args = parser.parse_args()

format_string = '0'+ str(args.bits) + 'b'

for i in range(args.lines):
    print(format(random.getrandbits(args.bits), format_string))
