#include <stdio.h>
#include <stdint.h>
#define MaxMem (1024 * 64) // defines the maximum amount of bytes the 6502 can handle in memory

// opcodes

#define INS_LDA_IM 0xA9
#define INS_LDX_IM 0xA2
#define INS_LDY_IM 0xA0


typedef uint8_t Byte;
typedef uint16_t Word;

typedef struct Mem {
    uint8_t Data[MaxMem];
} Mem;


typedef struct CPU {
    // info about registers [http://www.obelisk.me.uk/6502/registers.html]

    Word PC; // program counter
    Word SP; // stack pointer

    Byte A, X, Y; // registers

    // status flags using bit fields
    Byte C : 1; // carry
    Byte Z : 1; // Zero
    Byte I : 1; // interrupt Disable
    Byte D : 1; // Decimal mode
    Byte B : 1; // Break command
    Byte O : 1; // Overflow
    Byte N : 1; // negative
    

} CPU;
