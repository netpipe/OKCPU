# OKCPU
new opensource cpu design and research repository

WIP untested still


--task design a cpu for nanoOS
design a cpu in chisel language for a multicore 64 bit processor that can do these opcodes 

; Instructions: mov, add, or, and, sub, xor, cmp, shl, shr, sar,
;               jmp, je/jz, jne/jnz, jl, jle, jg, jge, jb, jbe, ja, jae, js, jns,
;               call, ret, syscall, db
; Registers:    rax rcx rdx rbx rsp rbp rsi rdi r8..r15, and al cl dl bl
; Operands:     reg · imm32 · label
;               [reg] · [reg + N] · [reg - N] · [rip + label] · [label]
; Byte access:  mov al, [mem] and mov [mem], al (also cl, dl, bl). Required:
;               a 1-byte store cannot be synthesised from 8-byte operations
;               without touching memory the program may not own.

and maybe its own imul idiv if its faster than doing it by software with shl shr

in under 2000 lines of code if you can






To turn this into a fuller OS-capable multicore processor, you would typically add:
caches and cache coherence,
interrupts and exceptions,
CSR/control registers,
virtual memory / page tables,
proper 64-bit immediates or instruction bundles,
unaligned access support,
a real syscall/trap ABI.


ACPI features
uses:
timer interrupt,
inter-processor interrupt,
UART RX activity,
GPIO interrupt,
ACPI power button,
DMA completion,
device interrupt controller.
Important practical note

This gives you an ACPI-like control model, but true zero power requires physical implementation support:
clock gating cells,
power switches,
isolation cells,
retention registers,
power-good signals,
level shifters if using multiple power domains,
firmware to save/restore state if power gating.

