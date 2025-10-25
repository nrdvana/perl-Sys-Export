; This MBR boot loader is a replacement for GRUB stage 1
; This loader assumes a BIOS capable of loading blocks by LBA, and a GPT
; partition label.   It reads the GPT partition label looking for the EFI
; System Partition (FAT16/32) then loads the first sector of that partition
; and jumps to it.  The ESP filesystem will need a boot loader installed into
; its sector 0 in order to continue booting.
;
; This implementation supports reading the partition from a 64-bit LBA, and
; provides some status feedback as it loads.
;
; It first loads sector 1 of the boot disk at address 0x8000
; It then verifies it is looking at GPT, then loads the partition table.
; It iterates partition entries until it finds the ESP partition type GUID.
; It then loads the first sector of that partition to address 0x8000 and
; executes it, which is presumably a stage 1.5 installed in the FAT16/32
; first sector.

BOOT_KERNEL_ADDR equ 0x8000

; Frame pointer offsets for local variables
BOOT_STACK_OFS equ 0x6000 ; local vars upward, stack downward from here
BOOT_DRIVE     equ 0x00   ; initial value of DX
ENT_STEP       equ 0x02   ; size of one GPT entry / 16
ENT_STOP       equ 0x04   ; size of entire GPT partition table / 16
DIGIT_BUF      equ 0x08   ; Buffer for int-to-string conversion

; int 13h DiskAddressParams struct fields
DAP_SIZE      equ  0 ; dap.size, constant 0x0010
DAP_COUNT     equ  2 ; dap.count, number of sectors to read
DAP_BUF_OFS   equ  4 ; dap.buf_ofs
DAP_BUF_SEG   equ  6 ; dap.buf_seg
DAP_LBA       equ  8 ; dap.lba (64-bit)

; GPT header fields
GPT_HEADER_TBL_LBA   equ 0x48 ; (64-bit)
GPT_HEADER_ENT_COUNT equ 0x50 ; (32-bit)
GPT_HEADER_ENT_SIZE  equ 0x54 ; (32-bit)

; GPT entry fields
GPT_ENTRY_LBA_FIRST  equ 0x20 ; (64-bit)
GPT_ENTRY_LBA_LAST   equ 0x28 ; (64-bit)

[BITS 16]
[ORG 0x7C00]

_start:
   cli
   jmp 0:start ; Normalize CS:IP to 0000:7c00
start:
   ; Set up segments and stack
   xor ax, ax
   mov ds, ax
   mov ss, ax
   mov bp, BOOT_KERNEL_ADDR ; es will point to the data we load with IN13h AH=42h
   mov es, bp
   mov sp, BOOT_STACK_OFS   ; Stack downward from BOOT_STACK_OFS
   mov bp, sp               ; Local vars upward from BOOT_STACK_OFS
   sti
   
   ; Save boot drive
   mov [bp+BOOT_DRIVE], dx
   ; Print boot drive "$N:MBR" to show we started, and from which BIOS disk
   mov ax, dx
   call print_ax 
   mov si, msg_MBR
   call print
   
   ; Read GPT header (LBA 1)
   ; construct this first DAP struct backward on stack
   xor edx, edx  ; LBA high = 0
   xor eax, eax
   inc ax        ; LBA low  = 1
   mov cx, ax    ; count = 1
   mov si, msg_GPT ; "$N:MBR GPT" <-- 'GPT' means loaded sector 1
   call read_disk
   ; read_disk always loads cx sectors to es:0
   
   ; Sanity check, must start with "EFI PART"
   mov di, 0
   mov si, efi_part_magic
   mov cx, 8
   repe cmpsb
   jne error
   
   ; Calculate sectors needed for partition entries
   mov eax, [es:GPT_HEADER_ENT_COUNT] ; number of entries
   mov ecx, [es:GPT_HEADER_ENT_SIZE]  ; size of entry
   mul ecx                            ; = total bytes needed
   ; sanity check, total size of table must be under 19 bits
   mov ebx, eax
   shr ebx, 19
   jnz error
   ; prepare loop variables and calculate count of sectors
   shr eax, 4
   shr cx, 4
   mov [bp+ENT_STEP], cx       ; save entry size / 16
   mov [bp+ENT_STOP], ax       ; save total bytes / 16
   add ax, 0x1F                ; round up
   shr ax, 5                   ; = number of 512 sectors needed

   ; Read partition table
   mov cx, ax                         ; sector count
   mov eax, [es:GPT_HEADER_TBL_LBA]   ; LBA low
   mov edx, [es:GPT_HEADER_TBL_LBA+4] ; LBA high
   mov di, msg_ENT                    ; "$N:MBR GPT:" <-- colon means we loaded the table
   call read_disk
   ; read_disk always loads cx sectors to es:0
   ; and sets si and di to zero
   
   ; for (es= 0; es < ent_stop; es += ent_step) {
   push es
   xor ax, ax
search_loop:
   cmp ax, [bp+ENT_STOP]
   jae error
   ; Compare 16-byte GUID
   mov es, ax
   xor di, di
   mov si, efi_type_guid
   mov cx, 16
   repe cmpsb
   je found_partition
   ; next entry
   add ax, [bp+ENT_STEP]
   jmp search_loop

found_partition:
   ; Print decimal number of partition entry
   xor dx, dx              ; zero dx before division
   div word [bp+ENT_STEP]
   call print_ax
   
   ; Read first LBA of partition
   mov eax, [es:GPT_ENTRY_LBA_FIRST]   ; LBA-low
   mov edx, [es:GPT_ENTRY_LBA_FIRST+4] ; LBA-high
   mov cx, 1                           ; 1 sector
   pop es ; restore it after using as loop variable
   mov di, msg_LOADED ; final message reads "$n:MBR GPT:$i LOADED\r\n"
   call read_disk

boot_stage2:
   ; Jump to stage 1.5 with boot drive in DX
   push word 0
   pop es ; in case stage 1.5 cares
   mov dx, [bp+BOOT_DRIVE]
   jmp 0:BOOT_KERNEL_ADDR

; Read disk using INT 13h AH=42h (LBA) and store at es:0
; Input:  edx:eax = LBA Address to load
;         cx = nonzero total sector count (16-bit)
;         si = address of message to display on success
read_disk:
   push si
   ; Initialize DAP struct on stack
   push edx            ; LBA-high
   push eax            ; LBA-low
   push es             ; segment=es
   push word 0         ; ofs=0
   push dword 0x400010 ; count=64, reserved=0, size=16
   mov si, sp          ; DAP struct is at this stack pos

read_loop:
   ; Determine chunk size (max 32K = 64 sectors)
   cmp cx, 64               ; default is copying 64 sectors; is cx less than that?
   jae chunk_ok             ; if cx >= 64, use 64
   mov [si+DAP_COUNT], cx   ; cx < 64, final loop iteration
chunk_ok:
   push cx
   
   ; Perform read
   mov dx, [bp+BOOT_DRIVE]
   mov ah, 0x42
   int 0x13
   jc error

   pop cx
   mov si, sp               ; DAP struct is at this stack pos again
   sub cx, 64               ; update remaining count
   jle read_done
   
   ; LBA += 64  (32K)
   add dword [si+DAP_LBA], 64
   adc dword [si+DAP_LBA+4], 0
   ; Destination segment += 2048 (32K)
   add word [si+DAP_BUF_SEG], 2048
   jmp read_loop

read_done:
   add sp, 16  ; remove DAP struct
   pop si
   ; fall into print routine to print success message

; Print null-terminated string
; Input: si = string pointer
print:
print_next_ch:
   lodsb
   test al, al
   jz print_done
   mov bx, 0x0001
   mov ah, 0x0e
   int 0x10
   jmp print_next_ch

print_done:
   ret

; Print each digit of AX in decimal
print_ax:
   movzx eax, ax
; Print each digit of EAX in decimal.
print_eax:
   lea si, [bp+DIGIT_BUF+11]  ; si points to end of digit_buf[10 digits + NUL]
   mov byte [si], 0           ; store NUL terminator
   mov ebx, 10

print_eax_next_digit:
   xor edx, edx
   div ebx                    ; divide EAX by 10; quotient in EAX, remainder in EDX
   add dl, '0'                ; convert remainder to ASCII
   dec si
   mov [si], dl
   test eax, eax
   jnz print_eax_next_digit
   jmp print                  ; print null-terminated string in si

; Error handler - print '!' and halt
error:
   mov si, msg_ERR
   call print
   int 0x18                   ; BIOS boot failure

halt:
   jmp halt

; Data section
msg_MBR:     db ":MBR", 0
msg_GPT:     db " GPT", 0
msg_ENT:     db ":", 0
msg_LOADED:  db " LOADED", 13, 10, 0
msg_ERR:     db "!", 13, 10, 0

efi_part_magic: db "EFI PART"
; EFI System Partition Type GUID: C12A7328-F81F-11D2-BA4B-00A0C93EC93B
efi_type_guid:
   db 0x28, 0x73, 0x2A, 0xC1, 0x1F, 0xF8, 0xD2, 0x11,
   db 0xBA, 0x4B, 0x00, 0xA0, 0xC9, 0x3E, 0xC9, 0x3B

; Pad to 440 bytes, then add disk signature area
times 0x1b8-($-$$) db 0

disk_signature:
   dd 0
   dw 0

; Partition table (filled by partitioning tools)
times 0x1be-($-$$) db 0

partition_table:
   times 64 db 0

; Boot signature
   dw 0xaa55
