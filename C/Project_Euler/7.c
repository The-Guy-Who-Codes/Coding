#include <stdio.h> // What is the 10 001st prime number

int is_prime(int num);

int main(void) {
    int primeval = 0;
    int prime;
    int i = 2;
    while (primeval != 10001) {
        if (is_prime(i) == 1) {
            primeval++;
        }
        if (primeval == 10001) {
            prime = i;
        }
        i++;
    }
    printf("%d\n", prime);
    return 0;
}

int is_prime(int num) {

    for (int i = 2; i < num; i++) {
        if (num % i == 0) {
            return 0;
        }
    }
    return 1;
}