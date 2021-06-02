#include <stdio.h>

void swap(int *px, int *py);

int main(){
    int Number;
    int *pNumber; /* "int" refers to data type of pointer and "*" shows that pNumber is a pointer 
                                                    (lowercase p signifies variable is a pointer) */
    Number = 23;
    pNumber = &Number;  /* "&" returns the adress of Number */

    int x, y;
    
    x = 3;
    y = 4;

    swap(&x, &y); /* passing in locations of x and y into function */

    printf("%d, %d\n", x, y);

    return 0;
}

void swap(int *px, int *py){ /* Functions in C cannot change the value of variables so pointers have to be used */
    int temp;
    temp = *px; /* in this case, the "*" is used as a dereferencer gving the value of the pointers pointing location */
    *px = *py;
    *py = temp;
}