#include <stdio.h> // What is the largest prime factor of the number 600851475143

int is_prime(int num);

int main(void) {
    int largest = 0;
    long long number = 600851475143;
    for (int i = 3; i < number;) {
        if (is_prime(i) == 1 && number % i == 0) {
            largest = i;
            number /= i;
        } else {
            i++;
        }
    }

    printf("%d\n%d\n", largest, number);
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