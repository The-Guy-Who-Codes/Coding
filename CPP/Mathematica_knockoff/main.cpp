#include <cstdio>
#include <cstdlib>
#include <vector>
#include <string>
#include <iostream>
#include "lexer.hpp"


void debugI(int a) {
    printf("DEBUG: %i\n", a);
}

int main(int argc, char* argv[]){
    while (1) {
        //execute();
        tokenise_input();
    }
    return 0;
}
