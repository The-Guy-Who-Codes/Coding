#include <cstdio>
#include <vector>

int ascii_sum() {
    int a = getchar();
    int sum = 0;
    std::vector<char> inp;
    while (a != 0x0A) {
        inp.push_back(a);
        a = getchar();
    }
    for (int i = 0; i < inp.size(); i++) {
        sum += inp[i];
    }
    return sum;
}

int main() {
    while (1) {
        printf("%x\n\n", ascii_sum());
    }
    return 1;
}