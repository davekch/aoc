#include <cxxtest/TestSuite.h>
#include "Solver.hpp"


class SolverTestSuite : public CxxTest::TestSuite {
private:
    Solver testSolver;
public:
    void setUp() {
        std::string testdata = "test test test";
        testSolver.set_input(testdata);
        testSolver.parse_data();
    }
    
    void test_solve1() {
        TS_ASSERT_EQUALS(testSolver.solve1(), 0);
    }
    
    void test_solve2() {
        TS_ASSERT_EQUALS(testSolver.solve2(), 0);
    }
};
