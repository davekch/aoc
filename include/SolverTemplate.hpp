#include <string>
#include <fstream>


template <typename T, typename U>
class AocSolver {
    protected:
        std::string raw_data;
        T parsed_data;
    public:
        void read_input(std::string);
        void set_input(std::string&); // to be able to also insert test data on the fly
        void parse_data();
        U solve1();
        U solve2();
};

template <typename T, typename U>
void AocSolver<T,U>::set_input(std::string &input) {
    raw_data = input;
}

template <typename T, typename U>
void AocSolver<T,U>::read_input(std::string path) {
    raw_data = "";
    std::string line;
    std::ifstream file(path);
    if (file.is_open()) {
        while (getline(file, line)) {
            raw_data += line + "\n";
        }
        file.close();
    }
};
