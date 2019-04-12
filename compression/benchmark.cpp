#include <iostream>
#include "util.hpp"
#include "compression.hpp"

template <class word_type>
void generate_sequence (word_type* array, double probability, int count) {
    RandomProvider<word_type> rand_provider(probability);

    for (int i = 0; i < count; ++i) {
        array[i] = 0;
        rand_provider.generate_word(array[i]);
    }
}

template <class word_type>
void print_sequence (word_type* array, int count) {
    for (int i = 0; i < count; ++i) {
        print_bitwise<32>(array[i]);
    }
}

int main () {
    int count = 1000000;
    int block_size = 2;
    double probability = 0.0001;

    uint32_t* array = new uint32_t[count];
    generate_sequence(array, probability, count);
    // print_sequence(array, count);
    std::cout << std::endl;
    int compressed_size = vlwah(array, count, block_size);
    std::cout << compressed_size * (block_size + 1) << std::endl;

    delete[] array;
	return 0;
}
