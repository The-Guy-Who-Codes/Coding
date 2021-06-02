#include <stdio.h>
#define len 5

int minimum(int array[], size_t length);

void main(){
    int numbers[len] = {2, 3, 4, -5, 5};
    printf("%d\n", minimum(numbers, len));;
}

int minimum(int array[], size_t length){
    int min = array[0];
    for (int i = 0; i <= length; i ++) {
        if (array[i] < min) {
            min = array[i];
        }
    }
    return min;
}