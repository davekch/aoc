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
};
