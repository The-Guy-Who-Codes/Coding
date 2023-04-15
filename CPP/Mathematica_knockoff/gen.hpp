#pragma once

constexpr double pi = 3.141592654;


double pow(double a, int b) {
    if (b == 0) {
        return 1;
    }
    double tmp = a;
    for (int i = 0; i < b - 1; i++) {
        a *= tmp;
    }
    return a;
}

double sqrt(double a) {
    double xn, xn1;
    int i;
    for (i = 0; i < a; i++) {
        int v1 = pow(i, 2);
        int v2 = pow(i + 1, 2);

        if (v1 <= a && v2 >= a) {
            xn = i;
            break;
        }
    }
    for (i = 0; i < 4; i++) {
        xn1 = xn - (pow(xn, 2) - a) / (2 * xn);
        xn = xn1;
    }
    return xn1;
}

double root(double a, int b) {
    double xn, xn1;
    int i;
    for (i = 0; i < a; i++) {
        int v1 = pow(i, b);
        int v2 = pow(i + 1, b);

        if (v1 <= a && v2 >= a) {
            xn = i;
            break;
        }
    }
    for (i = 0; i < 6; i++) {
        xn1 = xn - (pow(xn, b) - a) / (b * pow(xn, b - 1));
        xn = xn1;
    }
    return xn1;
}

int factorial(int a) {
    int sum = a;
    for (int i = a - 1; i > 0; i--) {
        sum *= i;
    }
    return sum;
}