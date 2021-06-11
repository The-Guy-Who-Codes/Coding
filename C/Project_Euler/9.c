#include <stdio.h> 
// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.


int isTriplet(int a, int b, int c) {
    if (a < b && b < c && ((a * a) + (b * b)) == (c * c)) {
        return 1;
    } else {
        return 0;
    }
}

int main() {
    const int sum = 1000;
    int a, b, c;


    for (a = 1; a < 700; a++) {

        for (b = 2; b < 700; b++) {

            for (c = 3; c < 700; c++) {

                if (isTriplet(a, b, c) == 1 && a + b + c == sum) {

                    printf("%d", a * b * c);
            }
        }
    }
    return 0;
}