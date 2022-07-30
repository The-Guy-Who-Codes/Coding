#include <stdio.h>
#include <math.h>

float e = 2.718281828;

double f(double x) {
    return -4 * x + 6 * exp(-x + 13) + 161;
}

double fp(double x) {
    return -4 - 6 * exp(-x + 13);
}

int main() {
    int i = 6;
    double r0 = 20;
    double r1 = 0;

    while (i > 0) {
        r1 = r0 - f(r0)/fp(r0);
        r0 = r1;
        i--;
    }

    printf("%.5f", r0);

    return 0;
}