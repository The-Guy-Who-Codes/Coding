
#include <stdio.h>
#include <stdlib.h>

int main() {
    int* arr = malloc(100 * sizeof(int));
    arr[0] = 23;
    arr[2] = 7;
    int n = arr[0] + arr[2];
    printf("%d", n);
    return 0;
}
