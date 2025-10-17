# set architecture i8086
target remote localhost:1234
break *0x7c00
display/40xb 0x6000
layout asm
layout regs
focus cmd
continue
stepi

