#include <stdio.h> // Find the sum of all the primes below two million

int is_prime(int num) {

    for (int i = 2; i < num / 2; i++) {
        if (num % i == 0) {
            return 0;
        }
    }
    return 1;
}

int main() {
    const int upper = 2000000;
    int sum = 0;
    for (int i = 1; i < upper; i++) {
        if (is_prime(i) == 1) {
            sum += i;
        }
    }

    printf("%d", sum);
    return 0;
}