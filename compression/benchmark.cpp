#include <iostream>
#include "util.hpp"
#include "compression.hpp"

void print_result (long long input_size, bool markov, int block_size, double density, double clustering, long long output_size) {
    std::cout << "RESULT";
    std::cout << "\t" << markov;
    std::cout << "\t" << input_size;
    std::cout << "\t" << block_size;
    std::cout << "\t" << density;
    std::cout << "\t" << clustering;
    std::cout << "\t" << output_size;
    std::cout << std::endl;
}

int main () {
    // prepare input array
    int word_count = 1000000;
    uint32_t* array = new uint32_t[word_count];

    // Uniform Random
    for (int exp = 1; exp <= 20; ++exp) {
        // generate input
        double density = pow(0.5, exp);
        UniformRandom<uint32_t> rand_provider(density);
        generate_sequence(array, rand_provider, word_count);

        // compute compression
        for (int block_size = 1; block_size <= 31; ++block_size) {
            int output_size = vlwah(array, word_count, block_size) * (block_size + 1);
            print_result(word_count * sizeof(array[0]) * 8, false, block_size, density, 0, output_size);
        }
    }

    /*
    // Markov Random
    for (int exp = 1; exp <= 20; ++exp) {
        // generate input
        double density = pow(0.5, exp);
        double clustering = 5.0;
        MarkovRandom<uint32_t> rand_provider(density, clustering);
        generate_sequence(array, rand_provider, word_count);

        // compute compression
        for (int block_size = 1; block_size <= 31; ++block_size) {
            int output_size = vlwah(array, word_count, block_size) * (block_size + 1);
            print_result(word_count * sizeof(array[0]) * 8, true, block_size, density, clustering, output_size);
        }
    }
    */

    delete[] array;
	return 0;
}
