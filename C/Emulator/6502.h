#include <stdio.h>
#include <stdint.h>
const int MaxMem = 1024 * 64; // defines the maximum amount of bits the 6502 can handle in memory

typedef struct Mem {
    uint8_t Data[MaxMem];
} Mem;


typedef struct CPU {
    // info about registers [http://www.obelisk.me.uk/6502/registers.html]

    uint16_t PC; // program counter
    uint16_t SP; // stack pointer

    uint8_t A, X, Y; // registers

    // status flags using bit fields
    uint8_t C : 1; // carry
    uint8_t Z : 1; // Zero
    uint8_t I : 1; // interrupt Disable
    uint8_t D : 1; // Decimal mode
    uint8_t B : 1; // Break command
    uint8_t O : 1; // Overflow
    uint8_t N : 1; // negative
    

} CPU;

void memInit(struct Mem mem) {
    for (int i = 0; i < MaxMem; i++) {
        mem.Data[i] = 0;
    }
}

void reset(struct CPU cpu, struct Mem mem) { // resets the state of the CPu as it were to be booted
    cpu.PC = 0xFFFC; // the reset vector in memory for the 6502 [https://www.c64-wiki.com/wiki/Reset_(Process)] 
    cpu.SP = 0x0100; 
    cpu.D = 0; // clear decimal flag
    cpu.C = cpu.Z = cpu.I = cpu.B = cpu.O = cpu.N = 0; // clear all flag registers
    memInit(mem);

}