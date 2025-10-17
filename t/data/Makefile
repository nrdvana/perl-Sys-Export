i386-pc-mbr-gpt.img: i386-pc-mbr-gpt.o i386-pc-mbr-gpt.ld
	ld -m elf_i386 -T i386-pc-mbr-gpt.ld i386-pc-mbr-gpt.o -o $@

i386-pc-mbr-gpt.o: i386-pc-mbr-gpt.S
	as --32 $< -o $@

