#include <stdio.h> // By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

int main(void) {
    int fibb[1000];
    int sum = 0;
    fibb[0] = 1;
    fibb[1] = 2;

    for (int i = 2; i < 1000; i++) {
        fibb[i] = fibb[i-2] + fibb[i-1];
    }
    
    for (int i = 0; fibb[i] <= 4000000; i++) {
        if (fibb[i] % 2 == 0) {
            sum += fibb[i];
        }
    }
    printf("%d\n", sum);
    return 0;
}