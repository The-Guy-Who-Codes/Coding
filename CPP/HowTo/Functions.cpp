#include <iostream>

// identical to c with declaration

int multiply(int a, int b) {
    return a * b;
}

int main() {
    int x = multiply(4, 5);
    std::cout << x << std::endl;

}