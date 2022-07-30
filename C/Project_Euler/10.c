#include <stdio.h>
#include <stdint.h>

int is_prime(int val);

int main(void) {

    uint32_t sum = 0;

    for (int x = 1; x < 2000000; x++) {
        if (is_prime(x) == 1){
            sum += x;
        }
    }
    printf("%d", sum);
    return 0;
}

int is_prime(int val) {
    for (int i = 2; i < val; i++) {
        if (val % i == 0) {
            return 0;
        }
    }
    return 1;
}