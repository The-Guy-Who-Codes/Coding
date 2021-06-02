#include<stdio.h>


void main()
{
    const int len = 20;
    int fibb[len], i, temp;
    fibb[0] = 0;
    fibb[1] = 1;

    for (i = 2; i <= len; i++){
        temp = fibb[i - 2];
        fibb[i] = temp + fibb[i - 1];
    }
    for (i = 0; i <= 20; i++){
        printf("%6d\n", fibb[i]);
    }
}