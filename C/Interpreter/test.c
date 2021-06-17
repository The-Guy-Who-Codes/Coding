#include <stdio.h>

void printString(char *array, int arrayLen) {
    for (int i = 0; i < arrayLen; i++) {
        printf("%c", *array);
        array++;
    }
}


int main() {
    char array[20][10];
    FILE *fp;
    fp = fopen("test.txt", "r");
    int iterator1 = 0;
    int iterator2 = 0;
    char c;
    while (c = getc(fp) != EOF) {
        while (c != ' ') {
            array[iterator1][iterator2] = c;
            iterator2++;
        }
        iterator1++;
    }
    printString(array[2], 10);
    return 0;
}