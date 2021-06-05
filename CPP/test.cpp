#include <iostream>
using namespace std;

int main() {
    int x = 1;
    x++;
    int * px = &x;
    cout << px << '\n' << x;
    return 0;
}