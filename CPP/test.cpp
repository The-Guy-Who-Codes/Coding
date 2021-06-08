#include <iostream>
#include <complex>

int main() {
    std::complex<int> test (3, 5);
    std::complex<float> i (0, 1);
    std::cout << test << '\n' << i * i;

    return 0;
}