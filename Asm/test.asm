global _test

_test:
    push rbp
    mov rbp, rsp

    mov rax, rsi
    cmp rdi, rsi
    cmovl rax, rdi

    mov rdi, rdx
    cmp rax, rdi
    cmovg rax, rdi
    
    mov rsp, rbp
    pop rbp
    ret