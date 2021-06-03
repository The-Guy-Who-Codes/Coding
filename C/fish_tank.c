#include <stdio.h>

void main()
{
    float l, w, h, vol;
    printf("all inputs are to be in cm\n");

    printf("Enter the length of the tank: ");
    scanf("%f", &l);
    printf("Enter the width of the tank: ");
    scanf("%f", &w);
    printf("Enter the height of the tank: ");
    scanf("%f", &h);

    vol = (l * w * h) / 1000;
    printf("volume is %f liters", vol);
}