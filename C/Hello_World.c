#include <stdio.h>
#include <string.h>
 
void main(){
    char *msg[] = {"\x48\x65\x6c\x6c\x6f\x2c\x20\x57\x6f\x72\x6c\x64\x21"}; 
    int i = strlen(msg[0]), x;
    
    for (x = 0; x < i; x++ ){
        printf("%s", msg[x]);
    }
}