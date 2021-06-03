#include <stdio.h>

float dosage(float nitrate){
 
    if (nitrate > 10) {
        return 3.0;
    } else if (nitrate > 2.5) {
        return 2;
    } else if (nitrate > 1) {
        return 0.5;
    } else {
        return 1;
    }
}

int main(){
    float nitrate;
    printf("enter nitrate levels (1-50):\n");
    scanf("%f", &nitrate);
    printf("Dosage is %.1fml.", dosage(nitrate));
    return 0;
}