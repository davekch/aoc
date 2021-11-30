#include <cxxtest/TestSuite.h>
#include "Solver.hpp"


class SolverTestSuite : public CxxTest::TestSuite {
private:
    Solver testSolver;
public:
    void setUp() {
        testSolver = Solver();
        std::string testdata = "test test test";
        testSolver.set_input(testdata);
        testSolver.parse_data();
    }
    
    void test_solve1() {
        ETS_ASSERT_EQUALS(testSolver.solve1(), 0);
    }
    
    void test_solve2() {
        ETS_ASSERT_EQUALS(testSolver.solve2(), 0);
    }
};
