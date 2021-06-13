

double power(double value, int power){
    double sum = 1;
    for (int i = 0; i < power; i++) {
        sum *= value;
    }
    return sum;
}



double mySQRT(int value) {
    double x0, x1;
    for (int i = 0; i < value; i++) {
        
        int t1 = power(i, 2);
        int t2 = power(i + 1, 2);
        
        if (t1 <= value && t2 > value) {
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

    for (int i = 0; i < value; i++) {
        int t1 = power(i, exponent);
        int t2 = power(i + 1, exponent);
        // printf("DEBUG: %d\n%d\n", t1, t2);
        if (t1 <= value && t2 > value) {
            x0 = i;
            break;
        }
    }
    //x0 = value / exponent;
    // printf("test: %f\n", x0);

    for (int i = 0; i < 4; i++) {
        x1 = x0 - ((power(x0, exponent) - value) / (exponent * power(x0, exponent - 1)));
        x0 = x1;
    }
    return x1;
}