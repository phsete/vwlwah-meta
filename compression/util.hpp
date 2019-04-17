#ifndef UTIL_HPP
#define UTIL_HPP
#include <random>
#include <bitset>

template <class word_type>
class RandomProvider {
    protected:
    std::random_device rd; /* Seed */
    std::default_random_engine generator; /* Random number generator */
    std::uniform_int_distribution<uint64_t> distribution; /* Distribution on which to apply the generator */
    uint64_t probability_multiplicator = 1'000'000'000'000L;

    public:
    RandomProvider () {
        generator = std::default_random_engine(rd());
        distribution = std::uniform_int_distribution<uint64_t>(0,probability_multiplicator);
    }

    word_type generate_word (word_type& rand_word) {
        int bits = sizeof(word_type) * 8;
        for (int i = 0; i < bits; ++i) {
            rand_word = (rand_word << 1) | next_bit();
        }
        return rand_word;
    }

    virtual bool next_bit () = 0;
};

template <class word_type>
class UniformRandom : public RandomProvider<word_type> {
    uint64_t threshold;

    public:
    UniformRandom (double bit_probability) : RandomProvider<word_type>() {
        threshold = bit_probability * this->probability_multiplicator;
    }

    bool next_bit () {
        uint64_t random_value = this->distribution(this->generator);
        return random_value <= threshold;
    }
};

template <class word_type>
class MarkovRandom : public RandomProvider<word_type> {
    uint64_t threshold[2];
    bool last_bit;

    public:
    MarkovRandom (double bit_density, double clustering_factor) : RandomProvider<word_type>() {
        double p = bit_density / ((1 - bit_density) * clustering_factor);
        double q = 1 / clustering_factor;
        threshold[0] = p * this->probability_multiplicator;
        threshold[1] = q * this->probability_multiplicator;

        // generate starting bit
        last_bit = (bit_density * this->probability_multiplicator) <= this->distribution(this->generator);
    }

    bool next_bit () {
        uint64_t random_value = this->distribution(this->generator);
        return last_bit = last_bit ^ random_value <= threshold[last_bit];
    }
};

template <int N, class word_type>
void print_bitwise (word_type word) {
    std::bitset<N> bit_representation(word);
    std::cout << bit_representation << std::endl;
}

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

#endif
