#include <iostream>
#include "SolverTemplate.hpp"
#include "Utils.hpp"


/*
replace the type parameters for AocSolver such that the first parameter
represents the type of the parsed data and the second one the type of
the solutions
*/
class Solver : public AocSolver<util::String, int> {
public:
    void parse_data();
    int solve1();
    int solve2();
};
