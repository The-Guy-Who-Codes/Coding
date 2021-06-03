#include <stdio.h>


int main(){
    int array[100];
    int *ptr = array;
    for(int x = 0; x < 100; x++){
        array[x] = x + 1;
    }
    for(int x = 0; x < 100; x++){
        printf("%d", *ptr);
        ptr++;
    }
    return 0;
}