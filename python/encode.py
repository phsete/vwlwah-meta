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

        if '0' in symbols and '1' in symbols:
            literal(symbols)
        elif '0' in symbols:
            fill('0')
        else:
            fill('1')
    finalize()

def literal (content):
    global current_fill_length
    global current_fill_type

    if current_fill_length > 0:
        finalize()

    print('0' + ''.join(content))

def fill (fill_type):
    global current_fill_length
    global current_fill_type

    if (fill_type != current_fill_type):
        finalize()

    current_fill_type = fill_type
    current_fill_length = current_fill_length + 1

def finalize ():
    global current_fill_length
    global current_fill_type
    global args

    payload_bits = args.block_size - 1

    if current_fill_length > 0:
        bits_needed = math.floor(math.log(current_fill_length, 2)) + 1
        words_needed = math.ceil(bits_needed / payload_bits)
        format_string = '0' + str(words_needed * payload_bits) + 'b'
        bitstring = format(current_fill_length, format_string)
        for word in range(words_needed):
            print('1' + current_fill_type + bitstring[int(word * payload_bits) : int((word + 1) * payload_bits)])

    current_fill_length = 0

if __name__ == '__main__':
    main()
