#include <cxxtest/TestSuite.h>
#include "Utils.hpp"


class UtilsTestSuite : public CxxTest::TestSuite {
public:
    void test_str_to_command1() {
        Command test = str_to_command("jmp 43");
        ETS_ASSERT_EQUALS(test.instruction, Instruction::JMP);
        ETS_ASSERT_EQUALS(test.value, 43);
    }

    void test_str_to_command2() {
        Command test = str_to_command("acc -4");
        ETS_ASSERT_EQUALS(test.instruction, Instruction::ACC);
        ETS_ASSERT_EQUALS(test.value, -4);
    }

    void test_diff() {
        std::vector<int> test{2,6,1,50,4};
        std::vector<int> truth{4,-5,49,-46};
        ETS_ASSERT_EQUALS(util::diff(test), truth);
    }

    void test_tuplecount() {
        ETS_ASSERT_EQUALS(util::n_tuples_or_triples(5), 13);
        ETS_ASSERT_EQUALS(util::n_tuples_or_triples(6), 24);
        ETS_ASSERT_EQUALS(util::n_tuples_or_triples(17), 19513);
    }
};
