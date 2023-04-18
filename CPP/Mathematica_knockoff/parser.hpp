#pragma once
#include <vector>
#include <string>
#include <cstdio>

std::vector<std::string> func_tokens = {"pow", "sqrt", "root", "sin", "cos", "tan"};
std::vector<int> func_num_params = {2, 1, 2, 1, 1, 1};

void error(const char* msg) {
    int i = 0;
    while (msg[i] != '\0') {
        printf("%c", msg[i]);
    }
}


