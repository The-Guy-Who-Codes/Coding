#!/bin/bash

nasm -f elf -o test.o test.asm

ld -m elf_i386 -o a test.o

./a
