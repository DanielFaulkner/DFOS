; DFOS inital boot loader (Stage 1)
; The main task of this file is to transfer control to the boot loader stage 2.
;  This arrangement should allow me to create a larger and more impressive boot loader
; Author: Daniel Rowell Faulkner
; Date: 2nd July 2002
; Aim: This part has a size limit so it must be kept below 510 bytes
; Hence precedures instead of macros.

; This bootloader is for 1.44 meg standard floppy disks only!!!
; The limitation is due to the hard set details within the fat12 table!!!
; If you use it on any other drive I'm unsure as to what will happen. But it may not be nice.

; Version 0.4 aims to have a FAT12 kernel file loaded rather than a next sector file

[BITS 16]               ; 16 bit code generation
[ORG 0x7C00]            ; This code will be loaded into: Segment 0x0000 Position:0x7C00

; Table at the start of the bootloader so that the floppy can be read by
; FAT12 floppy drivers. (Most modern OS's)

;;; FAT12 Table - MUST remain in this position for FAT12 filesystem compatability

        jmp main                ; Jump to the start of the boot loader code
        OEM_ID                  db      "DFOS    "      ; 8 char sys ID
        BytesPerSector          dw      512             ; Sector size in bytes
        SectorsPerCluster       db      1               ; Sector per cluster
        ReservedSectors         dw      4               ; Reserved sectors (This I think for this should become 2 for the boot strap, was 1)
        TotalFATs               db      2               ; Number of fats
        MaxRootEntries          dw      224             ; Root directory entries
        TotalSectorsSmall       dw      2880            ; Total Sectors
        MediaDescriptor         db      0F0h            ; Format ID (FAT12 ID number)
        SectorsPerFAT           dw      9               ; Sectors per FAT
        SectorsPerTrack         dw      18              ; Sectors per track
        NumHeads                dw      2               ; Number of heads (2 as double sided floppy)
        HiddenSectors           dd      0               ; Special hidden sectors
        TotalSectorsLarge       dd      0               ; More sectors
        DriveNumber             db      0               ; Drive Number (Primary Floppy is normally 0)
        Flags                   db      0               ; Reserved
        Signature               db      41              ; Boot signature
        VolumeID                dd      435101793       ; Volume serial number
        VolumeLabel             db      "NO NAME    "   ; Volume label (11 bytes)
        SystemID                db      "FAT12   "      ; File system (8 bytes)

;;; End of FAT12 table - Start of main program

; Main program code

main:                   ; Main part of the program

        ; Grap the drive used to boot from fast before something fiddles with it
        mov [BootDrv],dl

        ; Set up the DS segment register
        mov ax, 0x0000  ; Load AX register with current segment
        mov ds, ax      ; Load DS using the value just put into AX (remember DS doesn't take immediate data)

        ;;; The stack set up code

        ; We need to setup SS and SP registers at this point (the stack)
        cli             ; Clear interrupts while we setup a stack
        mov ax,0x9000   ; this seems to be the typical place for a stack
        mov ss,ax       ; Remember the segment registers can't handle immediate data
        mov sp,0xffff   ; Use the whole segment.
        sti             ; Turn the interrupts back on

        ; Could we put cs onto the stack and then pop it off as ds?
        ;  This would be far more effective as a universal method no
        ;  matter what location style.

        ;;; Welcome message

        ; Show a welcome message on the screen
        mov si, Welcome ; Write Welcome to the screen (by placing it's address in SI)
        call PrintMsg   ; Run Procedure

        ;;; Segment Registers

        ; Set up the ES segment now we know we have a moden CPU
        push    cs      ; Put CS on the stack
        pop     es      ; Take CS off the stack into ES
	; Remember to set up the other segment registers at a later stage
	
        ; Load the second stage boot strap process
        mov cx,4        ; <- Number of sectors (Increase as the boot loader size increases)
        mov ax,1        ; <- Input location start
        mov bx,0x1000   ; <- Output location es:bx (1000:0000)
	mov es,bx
	mov bx,0x0000	; Offset
        call ReadSectors      ; Start the boot strap procedure (2nd stage)

        ; Jump to the second stage location and setup the segment registers
	mov dl,[BootDrv]
        mov ax,0x1000           ; <- Setup DS
	mov ds,ax
        jmp 0x1000:0x0000       ; <- Location of next stage

        ; ** Should never get past this point
        ;  (at least if put together correctly)

; -*-*-*-*-*-*-*-*-*-*-*-*-*-* End of main program -*-*-*-*-*-*-*-*-*-*-*-*-
; ******************************* Procedures *******************************

; Print message procedure
;  Input: Msg to display should be in DS:SI (End string with 0)
;  Output: Message displayed on the screen

PrintMsg:
        mov ah, 0x0E    ; Teletype Mode
        mov bh, 0x00    ; Page Number
        mov bl, 0x07    ; Text attribute (Colour 07 = Black and White)
.nextchar:
        lodsb           ; Load [si] into al and increment si
        or al,al        ; Set the Zero Flag if al = 0
        jz .return      ; If the Zero Flag is set, jump to Break
        int 0x10        ; Call BIOS Video Function
        jmp .nextchar   ; Loop around to write next character
.return
        ret             ; Return

; Reboot procedure
;  Input: None
;  Output: Reboot/Shutdown

reboot:
        mov si, rebootMsg       ; Put reboot msg into SI
        call PrintMsg           ; Run the print message procedure
        call getkey             ; Wait for key press
        db 0Eah                 ; Machine language for reboot: (FFFF:0000)
        dw 0000h
        dw 0FFFFh

; Wait for key press procedure
;  Input: None
;  Output: Pauses till key press

getkey:
        mov ah, 0       ; Wait for key function
        int 016H        ; Call interrupt
        ret             ; return

; Procedure ReadSectors - Reads sectors from the disk.
;  Input: cx - Number of sectors; ax - Start position
;  Output: Loaded file into: es:bx (bx initial value must be set)

ReadSectors:
.MAIN:                          ; Main Label
        mov di, 5               ; Loop 5 times max!!!
.SECTORLOOP:
        push ax                 ; Save register values on the stack
        push bx
        push cx
        push bx
	; AX must have the LBA sector value. (Correct)
	call LBAtoCHS             ; Change the LBA addressing to CHS addressing
	; AX=Sector, CX=Cylinder, BX=Head
        ; The code to read a sector from the floppy drive
        mov ch, cl                      ; Cylinder to read
        mov cl, al                      ; Sector to read
        mov dh, bl                      ; Head to read
        mov dl, BYTE [BootDrv]          ; Drive to read (Could be setup before loop to optimise)
        mov ah, 02              	; BIOS read sector function
        mov al, 01              	; read one sector
        pop bx
        int 0x13                	; Make the BIOS call
	; Loop round stuff
        jnc .SUCCESS
        dec di                  ; Decrease the error counter
        pop cx                  ; Restore the register values
        pop bx
        pop ax
        jnz .SECTORLOOP         ; Try the command again incase the floppy drive is being annoying
        call ReadError          ; Call the error command in case all else fails
.SUCCESS
        pop cx                  ; Restore the register values
        pop bx
        pop ax
        add bx, WORD [BytesPerSector]   ; Queue next buffer (Adjust output location so as to not over write the same area again with the next set of data)
        inc ax                          ; Queue next sector (Start at the next sector along from last time)
        ; I think I may add a status bar thing also. A # for each sector loaded or something.
        ; Shouldn't a test for CX go in here???
        dec cx                          ; One less sector left to read
        jz .ENDREAD                     ; Jump to the end of the precedure
        jmp .MAIN                      ; Read next sector (Back to the start)
.ENDREAD:                       ; End of the read procedure
        ret                     ; Return to main program

; Procedure to change LBA address system into CHS address system
;  Description: Gets absolute values from logical values. (Logical = Consecutive sectors; Absolute = Sector:Track:Head)
;  Calculations done:
;   absolute sector = (LBA mod SPT) + 1
;   absolute head = (LBA / SPT) MOD Heads
;   absolute cylinder = (LBA / SPT) / Heads
;  Input: AX - LBA value (And the FAT table of course)
;  Output: AX - Sector; BX - Head; CX - Cylinder

LBAtoCHS:
 PUSH dx			; Save the value in dx
 XOR dx,dx			; Zero dx
 MOV bx, [SectorsPerTrack]	; Move into place STP (LBA all ready in place)
 DIV bx				; Make the divide (ax/bx -> ax,dx)
 inc dx				; Add one to the remainder (sector value)
 push dx			; Save the sector value on the stack

 XOR dx,dx			; Zero dx
 MOV bx, [NumHeads]		; Move NumHeads into place (NumTracks all ready in place)
 DIV bx				; Make the divide (ax/bx -> ax,dx)

 MOV cx,ax			; Move ax to cx (Cylinder)
 MOV bx,dx			; Move dx to bx (Head)
 POP ax				; Take the last value entered on the stack off.
 POP dx				; Restore dx, in case something was there to start with
 RET				; Return to the main function

; Message procedures

ReadError:
        mov si, ReadErrorMsg    ; Move the message address
        call PrintMsg           ; Run the procedure
        call reboot             ; Run the reboot procedure

; ********************************** Data **********************************

; Numbers/Other data:
BootDrv         db      0       ; Used to store the bootup drive

; Messages:
Welcome: db "DFOS Boot Loader Ver 0.5", 13, 10, 0
rebootMsg: db "Press any key to reboot"
ReadErrorMsg: db "Error Reading Sector (Possible cause is corruption, or program error)",13,10,0

; ************** End matter (needed only for boot loaders) *****************

times 510-($-$$) db 0   ; Loads of zeros
dw 0xAA55               ; 0x55 and 0xAA are the last bytes

; Possible future option is to have the next stage included here.
