#include <stdio.h>
#include <stdint.h>

// program to find the largest product of any amount of consecuative numbers in a file


void PrintArray(int *array, int arrayLen) {
    for (int i = 0; i < arrayLen; i++) {
        printf("%i\n", *array);
        array++;
    }
}

void FileToArray(int *array) {
    FILE *fp;
    fp = fopen("file.txt", "r");
    for (int i = 0; i < 1000; i++) {
        array[i] = ((int)getc(fp)) - 48; // getc automatically incriments the pointer and the (- 48) adjusts the ascii value of a number to their numerical value
    }
    fclose(fp);
}

int main() {
    
    const int ArrayLen = 1000;
    const int adjacent = 13;

    int numbers[ArrayLen];
    FileToArray(numbers);
    uint64_t largest = 0;
    
    for (int i = 0; i < (ArrayLen - adjacent); i++) {
        static uint64_t product;
        product = 1;
        for (int x = 0; x < adjacent; x++) {
            product *= numbers[i + x];
        }
        if (product > largest) {
            largest = product;
        }
    }
    printf("%I64u\n", largest);
    return 0;
}