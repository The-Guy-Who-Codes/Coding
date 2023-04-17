#pragma once
#include <vector>
#include <string>
#include <cstdio>

void error(const char* msg) {
    int i = 0;
    while (msg[i] != '\0') {
        printf("%c", msg[i]);
    }
}

