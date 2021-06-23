#include <stdio.h>

int main() {
    int value = 0b00011001 ;
    printf("%d", (value & (1<<7)) >> 7);
    return 0;
}