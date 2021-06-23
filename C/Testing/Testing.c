#include "Testing.h"

int main() {
    HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
    TEST_TRUE(1, "Test1", hConsole);
    TEST_TRUE(21, "Test2", hConsole);
    TEST_EQ(22, 25, "Test3", hConsole);
    return 0;
}