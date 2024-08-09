
section .data

msg: db 'Write a 4 letter word:', 10
len: dd $ - msg
input_len: dd 5

section .text
global _start

_start:
    ; print intro message
    mov eax, 4
    xor ebx, ebx
    mov ecx, msg
    mov edx, [len] 
    int 0x80
    
    mov ebx, [input_len] ; stack base offset
    dec ebx ; make space for the \n value

    mov ebp, esp
    sub esp, [input_len] ; reserve bytes to stack

.input:
    mov eax, 3

    ; offset base pointer used as buffer to store character
    
    push ebp
    sub ebp, ebx
    mov ecx, ebp
    pop ebp
    

    push ebx

    xor ebx, ebx
    mov edx, 1
    int 0x80

    pop ebx
    
    dec ebx

    cmp ebx, 0
    jne .input

    mov [ebp], byte 10



.print: ; print string from offsetted base-pointer to the base-pointer
    mov eax, 4
    xor ebx, ebx
    
    push ebp
    sub ebp, [input_len]
    mov ecx, ebp
    pop ebp
    
    mov edx, [input_len]
    inc edx
    int 0x80

.end:
    ; close program
    xor ebx, ebx
    mov eax, 1
    int 0x80
