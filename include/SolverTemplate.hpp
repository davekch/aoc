#include <string>
#include <sstream>
#include <fstream>
#include <vector>


template <typename T, typename U>
class AocSolver {
    protected:
        std::string raw_data;
        std::vector<std::string> raw_data_lines;
        T parsed_data;
    public:
        void read_input(std::string);
        void read_input_lines(std::string);
        void set_input(std::string&); // to be able to also insert test data on the fly
        void set_input_lines(std::string&);
        void parse_data();
        U solve1();
        U solve2();
};

template <typename T, typename U>
void AocSolver<T,U>::set_input(std::string &input) {
    raw_data = input;
}

template <typename T, typename U>
void AocSolver<T,U>::set_input_lines(std::string &input) {
    std::vector<std::string> lines;
    std::istringstream iss(input);
    for (std::string line; std::getline(iss, line);) {
        lines.push_back(line);
    }
    raw_data_lines = lines;
}

template <typename T, typename U>
void AocSolver<T,U>::read_input(std::string path) {
    // read the contents in path into a single string
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

template <typename T, typename U>
void AocSolver<T,U>::read_input_lines(std::string path) {
    // read the lines in path into a vector of strings
    std::string line;
    std::ifstream file(path);
    if (file.is_open()) {
        while (getline(file, line)) {
            raw_data_lines.push_back(line);
        }
        file.close();
    }
};
