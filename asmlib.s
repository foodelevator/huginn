bits 64

extern main

section .text

global _start
_start:
    call main
    mov rdi, 0
    mov rax, 0x3c ; sys_exit
    syscall

; FIXME: crashes when trying to print -9223372036854775808 (= i64 min)
global print
print: ; System V calling conv
    xor r9d, r9d
    mov r8d, 1

    mov rax, rdi
    neg rax

    cmovle r8d, r9d ; r8 = rax < 0
    cmovle rax, rdi ; rax = abs(rax)

    mov ecx, 10
    mov rdi, lf
begin_loop:
    cqo
    div rcx              ; (rdx, rax) = (rax % ecx, rax / ecx)
    add rdx, '0'         ; to ascii value
    dec rdi
    mov byte [rdi], dl ; write to buffer
    test rax, rax
    jnz begin_loop

    test r8, r8
    jz write_syscall

    dec rdi
    mov byte [rdi], '-'
write_syscall:
    inc rax       ; rax = sys_write
    mov rsi, rdi  ; buf
    mov rdi, 1    ; fd  = stdout
    mov rdx, bend
    sub rdx, rsi ; count
    syscall
    ret

section .data

buf: db 20 dup 0
lf: db 10
bend:
