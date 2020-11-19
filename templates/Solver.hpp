#include "SolverTemplate.hpp"


/*
replace the type parameters for AocSolver such that the first parameter
represents the type of the parsed data and the second one the type of
the solutions
*/
using Solver = AocSolver<std::string, int>;

template<>
void Solver::parse_data() {
    // parse this->raw_data into this->parsed_data
    parsed_data = raw_data;
}

template<>
int Solver::solve1() {
    return 0;
}

template<>
int Solver::solve2() {
    return 0;
}
