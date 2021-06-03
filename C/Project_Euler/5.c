# include <stdio.h> // What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

int main(void) {

    int i = 20;
    int isdivis = 0;
    while (isdivis == 0) {
        
        for (int j = 2; j < 21; j++) {
            if (i % j == 0) {
                isdivis = 1;
            } else {
                isdivis = 0;
                break;
            }
        }
        if (isdivis == 1) {
            printf("%d\n", i);
            break;
        }
        i++;
    }

    return 0;
}