#ifndef UTILS_HPP
#define UTILS_HPP


#include <string>
#include <vector>

namespace util {

    class String {
    private:
        std::string data;
    public:
        String() {};
        String(std::string d) : data(d) {};
        std::string get_data();
        void print();
        std::vector<String> splitAt(std::string);
        int asint();
        bool equals(std::string);
    };

    std::vector<int> parse_ints(std::vector<String>);

    std::vector<int> diff(std::vector<int>);

    unsigned long long int n_tuples_or_triples(int);

}

#endif
