#include <stdio.h> // Find the largest palindrome made from the product of two 3-digit numbers.

int power(int x, int y);
int * split(int number);
int pallendrome(int *numbers);

int num_length = 0;

int main(void) {
    
    int largest = 0;

    for (int x = 100; x < 1000; x++) {

        for (int y = 100; y < 1000; y++) {

            int *ptest = split(x * y);

            if (*ptest == 0) {
               if (pallendrome(ptest + 1) == 1 && (x * y) > largest) {
                    largest = x * y;
                } 
            } else {
                if (pallendrome(ptest) == 1 && (x * y) > largest) {
                    largest = x * y;
                }
            }

        }
    }

    printf("%d\n", largest);

    return 0;
}

int power(int x, int y) {
    int out = 1;
    for (int i = 0; i < y; i++) {
        out *= x;
    }
    return out;
}

int * split(int number) {
    static int numbers[6];
    int temp;
    for (int i = 0; i < 6; i++) {
        temp = number % power(10, i + 1);
        number -= temp;
        numbers[5 - i] = temp / power(10, i);
    }
    if (numbers[0] == 0) {
        num_length = 5;
    } else {
        num_length = 6;
    }
    return numbers;
}

int pallendrome(int *numbers) {
    int pallendrome[num_length];
    for (int i = (num_length - 1); i >= 0; i--) {
        pallendrome[i] = *numbers;
        numbers++;
    }
    numbers -= num_length;
    for (int i = 0; i < num_length; i++) {
        if (*numbers != pallendrome[i]) {
            return 0;
        } else {
            numbers++;
        }
    }
    return 1;
}