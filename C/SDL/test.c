#include <stdio.h>
#include <stdint.h>
#include <omp.h>

    
int main() {
    #pragma omp parallel for
    for (int i = 0; i < 16000; i++) {
        printf("%d\n", omp_get_thread_num());
    }
    return 0;
}