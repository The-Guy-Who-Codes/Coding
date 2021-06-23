#include <stdio.h>
#include <string.h>
#include <Windows.h>

#define TextRed SetConsoleTextAttribute(hConsole, FOREGROUND_RED)
#define TextGreen SetConsoleTextAttribute(hConsole, FOREGROUND_GREEN)
#define TextWhite SetConsoleTextAttribute(hConsole, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE)



void TEST_TRUE(int value, char* testName, HANDLE* hConsole) {
    if (value == 1) {
        TextGreen;
        printf("[%s]: Success\n\n", testName);
        TextWhite;
    } else {
        TextRed;
        printf("[%s]: Fail\nExpected: 1\nRecieved: %d\n\n", testName, value);
        TextWhite;
    }
}

void TEST_FALSE(int value, char* testName, HANDLE* hConsole) {
    if (value == 0) {
        TextGreen;
        printf("[%s]: Success\n\n", testName);
        TextWhite;
    } else {
        TextRed;
        printf("[%s]: Fail\nExpected: 0\nRecieved: %d\n\n", testName, value);
        TextWhite;
    }
}

void TEST_EQ(int val1, int val2, char* testName, HANDLE* hConsole) {
    if (val1 == val2) {
        TextGreen;
        printf("[%s]: Success\n\n", testName);
        TextWhite;
    } else {
        TextRed;
        printf("[%s]: Fail\nValue 1: %x\nValue 2: %x\n(In Hex)\n\n", testName, val1, val2);
        TextWhite;
    }
}