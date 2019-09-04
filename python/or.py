import argparse
import math
import sys

current_fill_length = 0
current_fill_type = None
args = None

def main ():
    global current_fill_length
    global current_fill_type
    global args

    parser = argparse.ArgumentParser(description='Compress string represented bitstreams from stdin.')
    parser.add_argument('block_size', metavar='B', type=int, help='The number of bits in an uncompressed word.')

    args = parser.parse_args()

    for line in sys.stdin:
        symbols = list(line.strip())
        output = [];

        for idx in range(args.block_size):
            if symbols[idx] == symbols[idx + args.block_size + 1]:
                output += [symbols[idx]]
            else:
                output += ['1']

        print(''.join(output))

if __name__ == '__main__':
    main()
