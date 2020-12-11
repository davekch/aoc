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

// here comes stuff that is too specific to be util::

enum Instruction {
    NOP,
    ACC,
    JMP
};

struct Command {
    Instruction instruction;
    int value;
};

Command str_to_command(std::string input);

enum BootStatus {
    DONE,
    INFLOOP
};

class BootLoader {
private:
    std::vector<Command> code;
    int pos;
    int acc;
    std::vector<int> history;
    // logging utility
    bool log = false;
public:
    BootLoader(std::vector<Command>);
    void reset();
    void set_code(std::vector<Command>);
    BootStatus run();
    int get_result();
    void set_command(int, Command);
    void set_log(bool);
};


#endif
