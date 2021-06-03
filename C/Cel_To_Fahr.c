#include<stdio.h>

#define LOWER -50 /* the lower limit of the table */
#define UPPER 500 /* the upper limit of the table */
#define STEP 10 /* the step going up on the table */

int main() {
    int fahr;

    for (fahr = 0; fahr <= 500; fahr = fahr + 10) {
        printf("%3d\t%6.2f\n", fahr, (5.0/9.0) * (fahr - 32));
    }
    
}