#include "6502.h"

int main() {
    Mem mem;
    CPU cpu;
    reset(cpu, mem);
    return 0;
}