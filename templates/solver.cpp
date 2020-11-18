#include <iostream>

#include "solver.hpp"

/*
replace the type parameters for AocSolver such that the first parameter
represents the type of the parsed data and the second one the type of
the solutions
*/
using Solver = AocSolver<std::string, int>;

template<>
void Solver::parse_data() {
    // parse this->raw_data into this->parse_data
    return;
}

template<>
int Solver::solve1() {
    return 0;
}

template<>
int Solver::solve2() {
    return 0;
}


int main() {
    Solver solver = Solver();
    solver.read_input("input.txt");
    solver.parse_data();
    
    std::cout << "Part 1: " << solver.solve1() << std::endl;
    std::cout << "Part 1: " << solver.solve2() << std::endl;
    return 0;
}