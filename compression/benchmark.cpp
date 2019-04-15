#include <iostream>
#include "util.hpp"
#include "compression.hpp"

template <class word_type>
void generate_sequence (word_type* array, RandomProvider<word_type>& rand_provider, int count) {
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
    int count = 10;
    int block_size = 10;
    double probability = 0.1;
    double clustering = 3.0;

    uint32_t* array = new uint32_t[count];
    UniformRandom<uint32_t> rand_provider(probability);
    MarkovRandom<uint32_t> rand_provider2(probability, clustering);

    generate_sequence(array, rand_provider2, count);
    print_sequence(array, count);
    std::cout << std::endl;
    int compressed_size = vlwah(array, count, block_size);
    std::cout << compressed_size * (block_size + 1) << std::endl;

    delete[] array;
	return 0;
}
