#ifndef UTIL_HPP
#define UTIL_HPP
#include <random>
#include <bitset>

template <class word_type>
class RandomProvider {
    std::random_device rd; /* Seed */
    std::default_random_engine generator; /* Random number generator */
    std::uniform_int_distribution<uint64_t> distribution; /* Distribution on which to apply the generator */
    uint64_t probability_multiplicator = 1'000'000'000'000L;
    uint64_t threshold;

    public:
    RandomProvider (double bit_probability) {
        threshold = bit_probability * probability_multiplicator;
        generator = std::default_random_engine(rd());
        distribution = std::uniform_int_distribution<uint64_t>(0,probability_multiplicator);
    }

    word_type generate_word (word_type& rand_word) {
        int bits = sizeof(word_type) * 8;
        for (int i = 0; i < bits; ++i) {
            uint64_t random_value = distribution(generator);
            rand_word = (rand_word << 1) | (random_value <= threshold);
        }
        return rand_word;
    }
};

template <int N, class word_type>
void print_bitwise (word_type word) {
    std::bitset<N> bit_representation(word);
    std::cout << bit_representation << std::endl;
}

#endif
