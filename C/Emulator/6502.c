#include "6502.h"

void memInit(struct Mem mem) { // initialises memory by resetting all memory values to 0
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

Byte fetch(uint32_t* cycles, struct Mem mem, struct CPU cpu) {
    Byte Data = mem.Data[cpu.PC]; // fetches byte which PC is pointing to
    printf("%x\n", Data);
    cpu.PC++; // increment PC
    *(cycles)--; // decriments cycle counter as fetching takes 1 cycle
    return Data;
}

 void execute(uint32_t cycles, struct CPU cpu, struct Mem mem) {
     while (cycles > 0) {
        Byte Instruction = fetch(&cycles, mem, cpu);
        switch (Instruction) {

            // data from [http://www.obelisk.me.uk/6502/instructions.html]

            case INS_LDA_IM: {
                Byte Value = fetch(&cycles, mem, cpu);
                cpu.A = Value;
                cpu.Z = (cpu.A == 0); // set if A == 0
                cpu.N = (cpu.A & 0b10000000) > 0; // set if Bit 7 of A is set
            } break;


            default: printf("[ERROR]: Instruction Not Handled: %d\n", Instruction); break;
        }
        cycles--;
     }
 }


int main() {
    Mem mem;
    CPU cpu;
    reset(cpu, mem);
    printf("%x\n", cpu.PC);
    // test program
    mem.Data[0xFFFC] = INS_LDA_IM;
    mem.Data[0xFFFD] = 0x42;
    // End Test
    execute(2, cpu, mem);
    return 0;
}