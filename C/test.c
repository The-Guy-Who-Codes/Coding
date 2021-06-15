#include <stdio.h>
#include <stdint.h>

typedef uint8_t BOOL;
enum boolean{false = 0, true = 1};

BOOL isEven(int num) {
    if (num % 2 == 0) {
        return true;
    } else {
        return false;
    }
}


int main() {
    int x = 246;
    if (isEven(x) == true) {
        printf("yay\n");
    }
    return 0;
}