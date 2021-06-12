

double power(double value, int power){
    double sum = value;
    for (int i = 1; i < power; i++) {
        sum *= value;
    }
    return sum;
}



double mySQRT(int value) {
    double x0 = value / 2;
    double x1;

    for (int i = 0; i < value / 2; i++) {
        if (i * i < value && (i + 1) * (i + 1) > value) {
            x0 = i;
            break;
        }
    }

    for (int i = 0; i < 4; i++) {
        x1 = x0 - (((x0 * x0) - value) / (2 * x0));
        x0 = x1;
    }
    return x1;
}





double root(double value, double exponent) {
    double x0, x1;

    for (int i = 0; i < value / exponent; i++) {
        if (power(i, exponent) < value && power((i + 1), exponent) > value) {
            x0 = i;
            break;
        }
    }

    for (int i = 0; i < 4; i++) {
        x1 = x0 - ((power(x0, exponent) - value) / (exponent * power(x0, exponent - 1)));
        x0 = x1;
    }
    return x1;
}