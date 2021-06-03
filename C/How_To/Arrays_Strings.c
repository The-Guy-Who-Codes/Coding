#include <stdio.h>

int main(){
    int a[10]; // defines an array of integers with length 10
    int b[5] = {1, 4, 2, 5, 6}; // alternate way of defining array
    int x;

    a[0] = 20; // assigns 1st element as value 20

    x = a[0]; // variable x is now = to 1st element in list a
    x = *a; // this does the same thing since the name of the array is a pointer

//      pa = &a[0]; and pa = a; are the same thing
//      a[i]; is also the same as *(a + i); [the latter being what c compiles the first to]

    int *pb = &b[0]; // defines a pointer to the first value of the list b

    pb++; // incriments the pointer to point at next value in list

    printf("The data point 1 after the pointer is: %d\n%d\n", *(pb + 1), x);
    // *(pb  + 1) gives the value held at the memory loction 1 after where the pointer is pointing to

//   When an array is passed into a function only the first location is passed to a pointer is needed
/*     [returns the length of a string]
        int strlen(char *s){
            int n;
            for (n = 0; *s != '\0'; s++){
                n++;
            }
            return n;
        }
    return 0;
*/ 
// ######################################################################################################################
//          Strings  [arrays of characters with a "\0" as the final element]

    char string[] = "Hi"; // creates a 3 value long array with "\0" (null character) at the end
    




}