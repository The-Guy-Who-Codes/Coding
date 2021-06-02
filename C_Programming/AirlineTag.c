#include <stdio.h>

int strlength(char *string);

int main(){
    char departure[20];
    char arrival[20];
    char final[10];
    char *pdeparture, *parrival;

    printf("Enter your departure airport: ");
    scanf("%s", &departure);
    printf("Enter your arrival airport: ");
    scanf("%s", &arrival);

    pdeparture = &departure[0];
    parrival = &arrival[0];

    int deplength = strlength(pdeparture);
    int arrlength = strlength(parrival);

    int dep_change = 0;
    int arr_change = 0;

    if (deplength < 4) {
        for (int i = 0; i < deplength; i++) {
            final[i] = departure[i];
            dep_change = 1;
        }
    } else {
        for (int i = 0; i < 4; i++) {
            final[i] = departure[i];
        }
    }

    final[4] = '-';

    if (arrlength < 4) {
        for (int i = 5; i < (arrlength + 5); i++) {
            final[i] = arrival[i-5];
            arr_change = 1;
        }
    } else {
        for (int i = 5; i < 9; i++) {
            final[i] = arrival[i-5];
        }
    }

    if (dep_change != 0) {
        for (int i = 3; i >= (4 - deplength); i--) {
            final[i] = ' ';
        }
    }
    if (arr_change != 0) {
        for (int i = 8; i >= (arrlength + 5); i--) {
            final[i] = ' ';
        }
    }
    printf("%s\n", final);

    return 0;
}

int strlength(char *string){
    int n = 0;
    for (; *string != '\0'; string++){
        n++;
    }

    return n;
}