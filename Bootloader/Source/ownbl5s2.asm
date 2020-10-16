; Second stage part of the boot loader
; The advantage of two stages is I have more space to play with
;  in this stage. :)
; Version 0.4
; This is the last version with no file system or pmode support
; Made by DFTECH: Daniel Rowell Faulkner
; URL: www.dftech.cwc.net

[BITS 16]	; 16 bit code generation
[ORG 0x0000]    ; Origin offset of 0

; ----------------------- Main ------------------------
main:
	; Grab the boot drive
        mov [BootDrv],dl

	; Make sure the main 2 data segments are set ok
	mov ax, 0x1000
	mov ds, ax
	mov es, ax

	; Hi message
	mov si,Welcome
	call PrintMsg
 
	; All the registers were setup in the first stage except gs and fs

	; * Hardware checks *
	
	; Uncomment the below line to enable the 386+ check
	;call checkCPU

	; -*- FAT12 file system stuff -*-

        ; Calculations

        ; Root Dir Size:
        ; ([MaxRootEntries] * 32 bytes per entry) / [BytesPerSector]
	xor ax, ax			; Zero registers
	xor cx, cx
        mov ax,[MaxRootEntries] 	; Move value to register to work on. ax=Arithmatic
        mov cx, 32      		; Move value to multiply into register
        mul cx          		; Multiply
        div WORD [BytesPerSector]    	; Divide
        mov [RootSize], ax      	; Put the value into a nice storage area for a bit

        ; Root Dir Start Location
        ; RootStart = Reserved sectors + fat sectors
        ; So RootStart = [ReservedSectors] + ([TotalFATs]*[SectorsPerFAT])
	xor ax,ax			; Zero AX for next calculation
        mov al, BYTE [TotalFATs]        ; Load up number of FAT tables (/info) into AL
        mul WORD [SectorsPerFAT]        ; Multiply AL by the number of sectors per FAT table (/info)
        add ax, WORD [ReservedSectors]  ; Add to the FAT total (AX) the number of reserved sectors
        mov [RootStart], ax             ; Put the start of the root address into RootStart variable
        
	; Now add to the total the root directory so giving the start of the data
        ; DataStart = Reserved Sectors + FAT sectors + Root size
        mov cx,[RootSize]               ; Mov the root size into CX
        add ax, cx			; Add ax (RootStart) to cx (RootSize)
	mov [DataStart], ax             ; Move the answer into DataStart

	; Loading and searching the root dir

	; Read root directory into memory
	; Print the loading message
	mov si, LoadingRootMsg
	call PrintMsg
	; Location: (0x1000:0x1000)
	mov ax,[RootStart]		; Start location of the root directory
	mov cx,[RootSize]		; Number of sectors to load
	mov bx,0x1000			; Offset of location to write to (es:bx)

	call ReadSectors		; <- Read root directory sectors

	mov si, SearchMsg		; Display a searching message
	call PrintMsg

	; Browse the root directory for the binary image
	; Loop to start of each entry to check the name
	; Input: FAT table (And some immediate values)
	; Output: DI - Contains the address of the kernel image (Root Entry Wise)
	; Preset values:
	mov cx, WORD [MaxRootEntries]	; Load the loop counter
	mov di, 0x1000			; First root entry (Offset)
	SearchLoop:
	push cx				; Save counter value
	mov cx, 0x000B			; Eleven character name (Num of times for rep to repeat)
	mov si, KernelName		; Kernel image to find (Load into SI the string to compare to DI)
	push di				; Save DI (Modified by cmpsb command)
	rep cmpsb			; Repeat internally the compare string block instruction (DS:SI to ES:DI) CX times
	pop di				; Restore DI
	je FoundKernel			; If equal jump to load kernel.
	pop cx				; Restore the counter value
	add di, 0x0020			; Add 32 to the value in DI (Next FAT block start)
	loop SearchLoop			; Loop dec's cx by one and jmps, unless cx == 0 then it stops looping.

	jmp SearchError			; Jump to the search error message

	; Load the FAT system and store the kernel image details
	;  Unsure of importance of loading the FAT system, as seems to be unused

	FoundKernel:
        mov     dx, WORD [di + 0x001A]
        mov     WORD [KernelAddress], dx                  ; file’s first cluster
	mov si, SearchDoneMsg		; Display the successful search message
	call PrintMsg

	; Work out size of FAT
	; [TotalFATs] * [SectorsPerFAT] = [FATsize]
	xor ax,ax		; Zero AX
	mov al, BYTE [TotalFATs]; Move TotalFAT's into position
	mul WORD [SectorsPerFAT]; Multiply by SectorsPerFAT
	mov WORD [FATsize], ax	; Move into memory variable

	; Work out start of FAT
	; [ReservedSectors] = [FATstart]
	xor ax,ax			; Zero AX
	mov ax, WORD [ReservedSectors]	; Move ReservedSectors into ax (This is the FATstart location)

	; Read FAT into memory
	mov cx, WORD [FATsize]	; FAT table size
	mov bx,0x1000		; Offset of memory location to load to
	call ReadSectors	; Read sectors procedure
	
	; Load the kernel image into memory :)

	; PrintMsg Messes up ax and bx
	mov si, LoadingMsg		; Display the kernel loading message
	call PrintMsg

	; Inital values:
	; The output location: (0x3000:0x0000)
	push es		; Save es
	mov bx, 0x3000	; Destination location
	mov es, bx	; Segment
	mov bx, 0x0000	; Offset
	push bx		; Save bx

	LoadKernelImage:
	xor ax, ax
        mov     ax, WORD [KernelAddress]                  ; cluster to read
        pop     bx                                  ; buffer to read into
        call    FATtoLBA                          ; convert cluster to LBA
	mov [KernelAddressLBA], ax

	mov ax, [KernelAddressLBA]
        xor     cx, cx
        mov     cl, BYTE [SectorsPerCluster]        ; sectors to read
        call    ReadSectors
        push    bx

     	; compute next cluster
	; Reading the FAT
	xor ax, ax
	mov ax, WORD [KernelAddress]	; Current Location
	call NextCluster		; Work out the next cluster
	mov WORD [KernelAddress], ax		; The new cluster value is stored in the variable.

	; Test to see what value the next cluster contains.
	cmp ax,0000h		; Free cluster (Empty)
        je .EmptyError          ; Error message
	cmp ax,0ff7h		; Is it a bad cluster?
        je .BadClusterError     ; Error message

	; Test to see if this is the end of the cluster chain:
	cmp ax,0x0fff		; End of chain? (0fff)
	je KernelJmp		; Jump to the loaded kernel

	jmp LoadKernelImage	; Loop back round to start

        .BadClusterError:               ; Short jumps needed initally.
                jmp BadClusterError     ; Jump to error handler
        .EmptyError:
                jmp EmptyError          ; Jump to error handler

	KernelJmp:			; Resume the program

	; * Segment registers *

	; Setup all the extra segment registers (Uncomment if wanted)
	;  or add the below lines to the end of the 386 procedure.
	;mov ax,cx
	;mov ds,ax	;<- Normally all ready setup by the 1st stage
	;mov es,ax
	;mov fs,ax
	;mov gs,ax

	; * Kernel Jump *
	mov si, KernJmpMsg	; Display the kernel jump message
	call PrintMsg

        ; Jump to the kernel location and setup the segment registers
        mov ax,0x3000           ; <- Setup DS
	mov ds,ax
        jmp 0x3000:0x0000       ; <- Location of next stage


; --------------------- Procedures --------------------

; * Procedures to display video output *

; Print message procedure
;  Input: Msg to display should be in DS:SI (End string with 0)
;  Output: Message displayed on the screen


PrintMsg:
        mov bl, 0x07    ; Text attribute (Colour 07 = Black and White)
PrintColor:
        mov ah, 0x0E    ; Teletype Mode
        mov bh, 0x00    ; Page Number
.nextchar:
        lodsb           ; Load [si] into al and increment si
        or al,al        ; Set the Zero Flag if al = 0
        jz .return      ; If the Zero Flag is set, jump to Break
        int 0x10        ; Call BIOS Video Function
        jmp .nextchar   ; Loop around to write next character
.return
        ret             ; Return

; * Hardware check procedures *

; Check for 386 procedure
;  Input: none
;  Output: Message and either a reboot or nothing

checkCPU:
        pushf           ; Save flags (first use of the stack :-) )
        xor     ah,ah   ; clear high byte (ah=0)
        push    ax      ; push AX onto the stack (with flags 12-15 clear)
        popf            ; pop this value into the flag register
        pushf           ; push flags back onto the stack
        pop     ax      ; ...and get flags into AX
        and     ah,0f0h ; try to set the high nibble
        cmp     ah,0f0h ; the high nibble is never 0f0h on a
        je      .no386  ; 8088/8086 present NOT 386
        ; Check for 286 (bits 12-15 clear)
        mov     ah,0f0h ; set bits 12-15
        push    ax      ; copy ax onto the flags
        popf
        pushf           ; copy the flags into AX
        pop     ax
        and     ah,0f0h ; check if bits 12-15 are clear
        jz      .no386  ; is no 80386 installed
        call    a386    ;  Success message
        popf            ; restore the flags
        ret             ; ... and return
.no386:
        mov si, no386msg        ; Move the message address into the register
        call PrintMsg           ; Run the print message procedure
        call reboot             ; Putting the CPU into a never ending loop prob isn't the best of ideas (jmp $)
                                ;  so now ending the process and rebooting instead.


; * Loading kernel procedures *

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
        push bx		; This value is used rather than saved
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
	; PrintMsg messes up ax and bx!
	;push ax		; \
	;push bx
	;mov si, LoadingHash	;  Un comment this to print a hash per sector loaded
	;call PrintMsg
	;pop bx
	;pop ax			; /
	; Test CX
        ;dec cx                          ; One less sector left to read
        ;jz .ENDREAD                     ; Jump to the end of the precedure
        loop .MAIN                      ; Read next sector (Back to the start)
	;jmp .MAIN			; Read next sector
.ENDREAD:                       ; End of the read procedure
	;mov si, CRLF		; <- Un comment to put a blank line after the #'s
	;call PrintMsg
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

; FATtoLBA - Procedure to translate a FAT cluster address into an LBA address.
; LBA = (Cluster - 2) * sectors per cluster	| Also you have to add to this the datastart location
; Input: AX - FAT cluster ; FAT table (SectorsPerCluster) And data sector address
; Output: AX - LBA address

FATtoLBA:
	sub ax, 0x0002				; Subtract 2 from ax (Not sure why yet)
	xor cx, cx				; Zero CX
	mov cl, BYTE [SectorsPerCluster]	; Move SPC to cl
	mul cx					; Multiply AX by CX (FAT*SectorsPerCluster)
	add ax, WORD [DataStart]		; Base data sector
	ret					; Return

; Next cluster procedure
; * (Multiply by 3 * Divide result by 2) or get 1.5 by (Divide by 2 * Add 1) * Read WORD at resulting address * Even keep low 12 bytes * Odd shift right by 4 bits
; Input: AX - CurrentCluster
; Output: AX - NextCluster

NextCluster:
	mov cx, ax			; Copy current cluster
	mov dx, ax			; Ditto again

	shr dx, 0x0001			; Divide dx by 2
	add cx, dx			; CX = 1.5

	mov bx, 0x1000			; Load the FAT location

	add bx, cx			; Add the calculated offset to the FAT location (Index into FAT) bx = FAT+calculated offset
	mov dx, WORD [bx]		; Read two bytes from FAT (a word)

	; Odd even test
	test ax, 0x0001			; Test to see if the cluster was odd or even (Seems to be the old cluster rather than the just calculated value!)
	jnz .OddCluster			; If not a zero ending cluster:
	.EvenCluster:
		and dx, 0x0fff		; Mask out the top 4 bits (0000111111111111b)
		jmp .Done		; Carry on to next section
	.OddCluster:
		shr dx, 0x0004		; So shift right by 4 bits. (1111111111110000b -> 0000111111111111b)
	.Done:
		mov ax, dx		; Move result to ax
		ret			; Return
	

; * Reboot procedures *

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

; * Procedures to display error messages *

ReadError:
        mov si, ReadErrorMsg    ; Move the message address
        call PrintMsg           ; Run the procedure
        call reboot             ; Run the reboot procedure

SearchError:
	mov si, SearchErrorMsg	; Move the message address
	call PrintMsg		; Run the procedure
	call reboot		; Run the reboot procedure

BadClusterError:
	mov si, BadClusterMsg	; Move the message address
	call PrintMsg		; Run the procedure
	call reboot		; Run the reboot procedure

EmptyError:
	mov si, EmptyMsg	; Move the Empty Cluster error message address
	call PrintMsg		; Run the procedure
	call reboot		; Run the reboot procedure

a386:
        mov si, the386msg       ; Same as for all messages
        call PrintMsg           ; Put the message on the screen procedure
        ret                     ; Return to caller

; ----------------------- Tables ----------------------

; ------------------------ Data -----------------------

; * Numbers *

; FAT table stuff

; (May well be able to use the values in the first stage boot loader)

BootDrv         db      0       ; Used to store the bootup drive
BytesPerSector	dw	512	; Number of bytes per sector
SectorsPerTrack dw	18	; Number of sectors per track
NumHeads	dw	2	; Number of heads
;FAT specific
SectorsPerCluster       db      1               ; Sector per cluster
ReservedSectors         dw      4               ; Reserved sectors (Size of boot loader)
TotalFATs               db      2               ; Number of fats
MaxRootEntries          dw      224             ; Root directory entries
SectorsPerFAT           dw      9               ; Sectors per FAT


; Own variables stuff
RootSize	dw	0	; Root Directory Size
RootStart	dw	0	; Root Directory Start Location
DataStart	dw	0	; Data Directory Start Location
KernelName	db	"KERNEL  COM"	; Must be 11 chars long!
KernelAddress	dw	0	; Start of the kernel image (cluster rather than LBA)
KernelAddressLBA	dw	0	; Start of the kernel image (LBA rather than cluster)
FATsize		dw	0	; FAT system size
FATstart	dw	0	; FAT system start

; * Messages *

Welcome 	db "Boot loader second stage starting",13,10,0
LoadingRootMsg	db "Loading root directory",13,10,0
LoadingMsg	db "Loading Kernel",13,10,0
LoadingHash	db "#",0
CRLF		db 13,10,0
no386msg: 	db "No 386 CPU found. 386 minimum is needed!",13,10,0 ; 13 and 10 is character return and line feed characters.
the386msg: 	db "386 or above CPU found. Boot process continuing ...",13,10,0
rebootMsg: 	db "Press any key to reboot",0
ReadErrorMsg: 	db 13,10,"Error Reading Sector (Possible cause is corruption, or program error)",13,10,0
SearchMsg	db "Seaching for kernel...",13,10,0
SearchDoneMsg:	db "Found the kernel image",13,10,0
SearchErrorMsg:	db 13,10,"Error can not find kernel image on disk!",13,10,0
BadClusterMsg	db 13,10,"Bad Cluster Error!",13,10,0
EmptyMsg	db 13,10,"Empty cluster!",13,10,0
KernJmpMsg	db "Jumping to kernel",13,10,0