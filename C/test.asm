global add20

add20:
    push rbp
    mov rbp, rsp

    mov rax, rdi
    add rax, 20

    mov rsp, rbp
    pop rbp
    ret