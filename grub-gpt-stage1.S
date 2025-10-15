/*
 *  GPT-aware GRUB Boot Loader - Using full 440 bytes
 *  No BPB padding - this is for MBR boot sector, not FAT partition
 */

.equ GRUB_BOOT_MACHINE_KERNEL_ADDR, 0x8000
.equ GRUB_BOOT_MACHINE_KERNEL_SEG, 0x800
.equ GRUB_BOOT_MACHINE_BUFFER_SEG, 0x7000
.equ GRUB_BOOT_MACHINE_STACK_SEG, 0x2000

	.section .boot, "ax"
	.code16
	.globl _start

_start:
	cli
	
	/* Normalize CS:IP to 0000:7c00 */
	ljmp	$0, $start

start:
	/* Set up segments and stack */
	xorw	%ax, %ax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss
	movw	$GRUB_BOOT_MACHINE_STACK_SEG, %sp
	sti

	/* Save boot drive */
	pushw	%dx

	/* Print "GRUB " */
	movw	$msg, %si
	call	print

	/* Setup disk address packet */
	movw	$dap, %si

	/* Read GPT header (LBA 1) */
	movw	$0x0010, (%si)		/* packet size */
	movw	$1, 2(%si)		/* 1 sector */
	movw	$0, 4(%si)		/* buffer offset */
	movw	$GRUB_BOOT_MACHINE_BUFFER_SEG, 6(%si)
	movl	$1, 8(%si)		/* LBA 1 */
	movl	$0, 12(%si)		/* LBA high */

	call	read_disk

	/* Extract partition entries LBA from GPT header offset 0x48 */
	movw	$GRUB_BOOT_MACHINE_BUFFER_SEG, %ax
	movw	%ax, %es
	movl	%es:0x48, %ebx

	/* Read 4 sectors of partition entries (16 entries) */
	movw	$4, 2(%si)
	movl	%ebx, 8(%si)
	call	read_disk

	/* Search for GUID "Hah!IdontNeedEFI" in partition entries */
	movw	$GRUB_BOOT_MACHINE_BUFFER_SEG, %ax
	movw	%ax, %es
	xorw	%bx, %bx

search_loop:
	/* Check if past 16 entries (16 * 128 = 2048 bytes) */
	cmpw	$2048, %bx
	jae	error

	/* Compare 16-byte GUID */
	movw	%bx, %di
	movw	$guid, %si
	movw	$16, %cx
	pushw	%es
	pushw	%ds
	popw	%es
	repe cmpsb
	popw	%es
	je	found_partition

	/* Next entry (128 bytes) */
	addw	$128, %bx
	jmp	search_loop

found_partition:
	/* Get first LBA (offset 32) and last LBA (offset 40) */
	movl	%es:32(%bx), %eax	/* first LBA */
	movl	%es:40(%bx), %ecx	/* last LBA */
	
	/* Calculate sector count = last - first + 1 */
	subl	%eax, %ecx
	incl	%ecx
	
	/* Limit to 220 sectors (~110KB) */
	cmpl	$220, %ecx
	jbe	load_partition
	movl	$220, %ecx

load_partition:
	/* Load partition to memory starting at 0x8000 */
	movw	$GRUB_BOOT_MACHINE_KERNEL_SEG, %di

load_loop:
	/* Check if done */
	testl	%ecx, %ecx
	jz	boot_stage2

	/* Determine chunk size (max 127 sectors for BIOS) */
	movl	%ecx, %ebx
	cmpl	$127, %ebx
	jbe	chunk_ok
	movl	$127, %ebx

chunk_ok:
	/* Setup disk address packet */
	movw	$dap, %si
	movw	$0x0010, (%si)
	movw	%bx, 2(%si)		/* sectors */
	movw	$0, 4(%si)		/* offset */
	movw	%di, 6(%si)		/* segment */
	movl	%eax, 8(%si)		/* LBA */
	movl	$0, 12(%si)

	/* Save registers and read */
	pushw	%ax
	pushw	%cx
	call	read_disk
	popw	%cx
	popw	%ax

	/* Update LBA */
	addl	%ebx, %eax

	/* Update remaining count */
	subl	%ebx, %ecx

	/* Update destination segment (512 * sectors / 16 = sectors * 32) */
	shlw	$5, %bx
	addw	%bx, %di

	jmp	load_loop

boot_stage2:
	/* Restore boot drive and jump to stage 2 */
	popw	%dx
	ljmp	$0, $GRUB_BOOT_MACHINE_KERNEL_ADDR

/*
 * Read disk using INT 13h AH=42h (LBA)
 * Input: %si = disk address packet
 * Uses: boot drive from stack
 */
read_disk:
	popw	%dx			/* get drive from stack */
	pushw	%dx			/* put it back */
	movb	$0x42, %ah
	int	$0x13
	jc	error
	ret

/*
 * Print null-terminated string
 * Input: %si = string pointer
 */
print:
	lodsb
	testb	%al, %al
	jz	print_done
	movw	$0x0001, %bx
	movb	$0xe, %ah
	int	$0x10
	jmp	print
print_done:
	ret

/*
 * Error handler - print '!' and halt
 */
error:
	movb	$'!', %al
	movw	$0x0001, %bx
	movb	$0xe, %ah
	int	$0x10
	int	$0x18			/* BIOS boot failure */
halt:
	jmp	halt

/* Data section */
msg:
	.asciz "GRUB "

/* GRUB BIOS boot partition GUID: "Hah!IdontNeedEFI" */
guid:
	.byte 0x48, 0x61, 0x68, 0x21	/* "Hah!" */
	.byte 0x49, 0x64, 0x6F, 0x6E	/* "Idon" */
	.byte 0x74, 0x4E, 0x65, 0x65	/* "tNee" */
	.byte 0x64, 0x45, 0x46, 0x49	/* "dEFI" */

/* Disk address packet (16 bytes) */
	.align 4
dap:
	.fill 16, 1, 0

/* Pad to 440 bytes, then add disk signature area */
	. = 0x1b8
disk_signature:
	.long 0
	.word 0

/* Partition table (filled by partitioning tools) */
	. = 0x1be
partition_table:
	.fill 64, 1, 0

/* Boot signature */
	.word 0xaa55
