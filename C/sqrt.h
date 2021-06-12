

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