#include <stdio.h> // Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

int square(int x) {
    return x * x;
}

int main(void) {
    int sumy;
    int x = 0;
    int y = 0;
    for (int i = 1; i < 101; i++) {
        x += square(i);
    }
    for (int i = 1; i < 101; i++) {
        y += i;
    }
    sumy = square(y);
    printf("%d\n", sumy-x);
    return 0;
}