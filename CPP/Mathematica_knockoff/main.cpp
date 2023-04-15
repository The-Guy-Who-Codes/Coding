#include <cstdio>
#include <cstdlib>
#include <vector>
#include <iostream>
#include "lexer.hpp"


void debugI(int a) {
    printf("DEBUG: %i\n", a);
}

int main(int argc, char* argv[]){
    while (1) {
        execute();
    }
    return 0;
}
