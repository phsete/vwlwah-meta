#ifndef SORT_HPP
#define SORT_HPP

#include <math.h>

/*
 * This version of WAH does not respect maximum fill lengths
 */
template <typename word_type>
int wah (word_type* array, int length) {
    const word_type content_mask = ((word_type) 1 << 31) - 1;

    bool active_zero_fill = false;
    bool active_one_fill = false;

    int word_count = 0;

    for (int i = 0; i < length; ++i) {
        if ((array[i] & content_mask) == 0) { // 0-fill
            word_count += !active_zero_fill;
            active_zero_fill = true;
            active_one_fill = false;
        } else if ((array[i] & content_mask) == content_mask) { // 1-fill
            word_count += !active_one_fill;
            active_zero_fill = false;
            active_one_fill = true;
        } else {
            ++word_count;
            active_zero_fill = false;
            active_one_fill = false;
        }
    }

    return word_count;
}

template <typename word_type>
struct size_converter {
    word_type* array;
    int length;
    int input_word_size;
    int compressed_word_size;
    size_t current_bit_pos;

    size_converter (word_type* array, int length, int compressed_word_size) {
        this->array = array;
        this->length = length;
        this-> input_word_size = sizeof(word_type) * 8;
        this->compressed_word_size = compressed_word_size;
        this->current_bit_pos = 0;
    }

    bool has_next () {
        return current_bit_pos + compressed_word_size <= length * input_word_size;
    }

    word_type get_next () {
        word_type begin_word = array[current_bit_pos/input_word_size];
        int starting_bit_index = current_bit_pos % input_word_size; // inclusive index
        current_bit_pos += compressed_word_size;
        word_type end_word = array[current_bit_pos/input_word_size];
        int end_bit_index = current_bit_pos % input_word_size + 1; // exclusive index

        int shift_distance;
        // shift first word left
        begin_word = begin_word << starting_bit_index;
        // shift first word right
        begin_word = begin_word >> (input_word_size - compressed_word_size);

        // shift second word right
        end_word = end_word >> (input_word_size - end_bit_index + 1);
        // shift second word left and back
        end_word = end_word << (input_word_size - compressed_word_size);
        end_word = end_word >> (input_word_size - compressed_word_size);

        return begin_word | end_word;
    }
};

/*
 * This version of VLWAH does respect maximum fill lengths
 */
template <typename word_type>
int vlwah (word_type* array, int length, int bits) {
    const word_type content_mask = ((word_type) 1 << bits) - 1;

    size_converter<word_type> size_conv(array, length, bits);

    word_type max_fill_length = pow(2,bits);
    long long active_max_fill_length = 1;
    word_type active_zero_fill_length = 0;
    word_type active_one_fill_length = 0;

    // encode active word
    int word_count = 2;

    while (size_conv.has_next()) {
        word_type current_word = size_conv.get_next();
        if ((current_word & content_mask) == 0) { // 0-fill
            // set back max fill length for new fills
            if (active_zero_fill_length == 0) {
                active_max_fill_length = 1;
            }
            active_one_fill_length = 0;
            ++active_zero_fill_length;
            if (active_zero_fill_length >= active_max_fill_length) { // new fill word needed
                ++word_count;
                active_max_fill_length *= max_fill_length;
            }
        } else if ((current_word & content_mask) == content_mask) { // 1-fill
            // set back max fill length for new fills
            if (active_one_fill_length == 0) {
                active_max_fill_length = 1;
            }
            active_zero_fill_length = 0;
            ++active_one_fill_length;
            if (active_one_fill_length >= active_max_fill_length) { // new fill word needed
                ++word_count;
                active_max_fill_length *= max_fill_length;
            }
        } else {
            ++word_count;
            active_zero_fill_length = 0;
            active_one_fill_length = 0;
        }
    }

    return word_count;
}

#endif
