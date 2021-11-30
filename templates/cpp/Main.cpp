#include <iostream>
#include "Solver.hpp"


int main() {
    Solver solver = Solver();
    solver.read_input("input.txt");
    solver.parse_data();

    std::cout << "Part 1: " << solver.solve1() << std::endl;
    std::cout << "Part 2: " << solver.solve2() << std::endl;
    return 0;
}
