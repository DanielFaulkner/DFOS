; FAT12 driver for DF-DOS
;
; Made by DFTECH: Daniel Rowell Faulkner
; Compile using nasm16 0.98

; Related interrupts:
;  Int 0x26  (CX-NumSectors, ES:BX-Output, AL-Drive, DX-Start sector)  <-- Write sectors
;  Int 0x25  (CX-NumSectors, ES:BX-Output, AL-Drive, DX-Start sector)  <-- Read sectors

; Related procedures:
;  LBAtoCHS (AX-LBA value, Output: AX-Sector, BX-Head, CX-Cylinder)
;  FATtoLBA (AX-FAT cluster, Output: AX-LBA address)
;  NextCluster (AX-CurrentCluster, Output: AX-NextCluster)

; Memory locations of the system at startup.
; Boot loader 2nd stage - 0x1000 | Root & FAT loaded - 0x1000:0x1000 (aka 0x2000) | Kernel - 0x3000

; Note: There needs to be some code addded so there is a return header
;  at the start of the binary segments!

; -------------------------- Command stuff ----------------------------

SearchFile:
	; This is called by the console command section currently.

	; Search for the file
	call FAT12LocCalcs	; Work out the root directory location
	call LoadRoot		; Load the root directory
	call FileSearch		; Search for the file

	xor bx,bx		; Return code
	cmp dx,bx		; <- Failed
	je .SearchEnd

	mov dx,0x01		; <- Worked
	
	; Return to console
	.SearchEnd:
	ret			; Return

RunFile:
	call FAT12LocCalcs	; Calc root size and location
	call LoadRoot		; Load root dir
	call FileSearch		; Search for file address

	xor bx,bx		; Set compare values
	cmp dx,bx
	je .RunEnd		; Check error and jump if needed

	; Found

	call FATCalcs		; Calc FAT size and location
	call LoadFAT		; Load FAT

	mov ax, 0x5000
	mov cx, 0x0100
	call LoadHeader		; Setup return stuff
	call LoadFile		; Load file

	xor bx,bx		; Set compare values
	cmp dx,bx
	je .RunEnd		; Check error and jump if needed

	; Found

	mov ax, 0x5000
	mov cx, 0x0100
	jmp FileJump		; Jump to file

	; Return to console
	.RunEnd:
	xor ax,ax
	int 0x21

; List/Directory style command (Specific to the text console)
TxtConListFiles:
	; Set up the variables/details needed for the procedure
	call FAT12LocCalcs	; Work out the root directory location
	call LoadRoot		; Load the root directory
	call FileList		; List the files

	; Return to console
	.ListEnd:
	xor ax,ax
	int 0x21	

; Write file command (Mainly for interrupt)


; ------------------------Common Procedures ---------------------------

; -*- Transfer control stuff -*-

; Procedure to jump to the loaded file and transfer control.
;  Input: AX-Segment location, CX-Offset location
FileJump:
        ; Jump to the kernel location and setup the segment registers
	mov ds,ax		; Setup segment registers
	mov es,ax
	mov fs,ax
	mov gs,ax
	; Value below is required in order to allow a program to 'ret' back to the OS using code at the start of the .com file.
	xor bx,bx		; Zero BX
	push bx			; Push return offset location
	push ax			; Push new segment location
	push cx			; Push new offset location
	xor ax,ax		; Zero all the general registers
	xor bx,bx
	xor cx,cx	; <- In MS-DOS not zero
	xor dx,dx
	xor di,di
	xor si,si
	xor bp,bp	; <- Unsure of value in MS-DOS
        retf			; Far return (takes CS and IP from the stack)

; -*- Searching stuff -*-

; Procedure to search the root directory for a file,
;  once located it should return the files address.
FileSearch:
	; Browse the root directory for the binary image
	; Loop to start of each entry to check the name
	; Input: FAT table (And some immediate values)
	; Output: DI - Contains the address of the kernel image (Root Entry Wise)
	; Preset values:
	mov cx, WORD [MaxRootEntries]	; Load the loop counter
	mov di, 0x1000			; First root entry (Offset)
	.SearchLoop:
	push cx				; Save counter value
	mov cx, 0x000B			; Eleven character name (Num of times for rep to repeat)
	mov si, FileName		; Kernel image to find (Load into SI the string to compare to DI)
	push di				; Save DI (Modified by cmpsb command)
	rep cmpsb			; Repeat internally the compare string block instruction (DS:SI to ES:DI) CX times
	pop di				; Restore DI
	pop cx				; Restore the counter value
	je .FoundFile			; If equal jump to load kernel.
	add di, 0x0020			; Add 32 to the value in DI (Next FAT block start)
	loop .SearchLoop		; Loop dec's cx by one and jmps, unless cx == 0 then it stops looping.

	jmp .SearchError		; Jump to the search error message

	; Error message and quit section
	.SearchError:
	mov si, SearchErrorMsg	; Move the message address
	call Print		; Run the procedure

	xor dx,dx		; Error code

	ret			; Return

	.FoundFile:
	; Save the Files address in the FAT table
        mov     dx, WORD [di + 0x001A]
        mov     WORD [FileAddress], dx	; file’s first cluster

	xor dx,dx		; Error/Success code
	mov dx, 0x01

	ret				; Return

; -*- Listing stuff -*-

; Procedure to list the files in the root directory
; Will I need to compare the root table with the FAT table as a filter? (Will add extra time to complete if so)

FileList:
	; Browse the root directory for the binary image
	; Loop to start of each entry to check the name
	; Input: FAT table (And some immediate values)
	; Output: Print output to text console (Eventually a version will be made which returns the values in a memory location or variable)
	; Preset values:
	; - Initial values
	mov cx, WORD [MaxRootEntries]	; Load the loop counter
	mov di, 0x1000			; First root entry (Offset)

	; - List loop
	.ListLoop:
	push cx				; Save entry counter value
					; -Filter out non alphabet characters!
	cmp byte [di], 0x41		; The characters below Cap Letters in the Table
	jb .FilteredEntry		; Jump to filtered entry. (Not a viable character)
	cmp byte [di], 0x5A		; The characters above Cap Letters in Table.
	ja .FilteredEntry		; Jump if above to filtered entry.
					; Finalise this code later so that the filter keeps in other viable filename characters.
					; At later date set up extra filter code here which cross references the entries with the FAT table data.

	mov cx, 0x000B			; Eleven character name (Num of times for rep to repeat) - Name length counter!
	; -*- The actual name displaying/processing code.
	.NameLoop			; Add the characters to a memory variable.(Or screen or something)

	; Display the data
        mov al, [DI]			; Move the ASCII code to correct register
	call putchar			; Print character
	inc di				; Move to next character
	loop .NameLoop			; Loop 11 times (CX value)
	mov si,CRLF			; Print newline
	call Print

	; Setup ready for next loop
	sub di,0xb			; Subtract the amount added by file name pointer. (Want to combine with below instruction at some point!)
	add di,0x0020			; Add 32 to value in DI to move to next entry
	pop cx				; Restore the root entry counter value
	loop .ListLoop			; Loop round to next entry
	ret				; End procedure

	; -*- Process of filtered entries (aka discarding them)
	.FilteredEntry
	add di, 0x0020			; Add 32 to the value in DI (Next root entry block start)
	pop cx				; Restore cx. (Number of entries)
	loop .ListLoop			; Loop round too next entry
	ret				; End procedure

	; -*- End of the list command section - Currently non used, will be used to return list in variable or similar.
	.ListEnd			; Return label
	ret				; Return

; -*- Loading stuff -*-

; Procedure to read the root directory into memory
LoadRoot:
	; Read root directory into memory
	; Location: (0x1000:0x1000) - This uses the same as the boot loader.
	mov al,[DriveNumber]		; Drive to load from
	mov dx,[RootStart]		; Start location of the root directory
	mov cx,[RootSize]		; Number of sectors to load
	mov bx,0x1000			; Offset of location to write to (es:bx)

	int 0x25			; Call read sectors interrupt

	ret				; Return

; Procedure to load the FAT into memory.
LoadFAT:
	mov al, [DriveNumber]	; Drive to load from
	mov dx, [FATstart]	; Start of FAT table
	mov cx, [FATsize]	; FAT table size
	mov bx,0x1000		; Offset of memory location to load to

	int 0x25		; Read sectors interrupt

	ret			; Return

; Procedure to load the file into memory
;  Input: AX-Start segment, CX-Start offset
;	  [FileAddress], FAT table variables
LoadFile:
	; Inital values:
	; The output location: (AX:CX -> ES:BX)
	push es		; Save es
	mov bx, ax	; Destination location
	mov es, bx	; Segment
	mov bx, cx	; Offset
	push bx		; Save bx

	.LoadFileImage:
	; !*! FAT -> LBA conversion
	xor ax,ax
        mov     ax, [FileAddress]		; cluster to read
	pop bx
        call    FATtoLBA			; convert cluster to LBA
	mov [FileAddressLBA], ax

	; !*! Move to correct int registers:
	mov ax, [FileAddressLBA]
	mov 	dx, ax				; Start location

        xor     cx, cx
        mov     cl, BYTE [SectorsPerCluster]	; sectors to read

	xor 	ax,ax
	mov	al, [DriveNumber]		; Drive number (from FAT table)

        ;pop     bx				; buffer to read into

        int 0x25				; Read sectors interrupt

	xor ax,ax
	mov al, BYTE [SectorsPerCluster]
	mov cx, [BytesPerSector]
	mul cx
	add bx, ax

        push    bx

     	; !*! compute next cluster
	xor ax,ax
	mov ax, WORD [FileAddress]	; Current Location
	call NextCluster		; Work out the next cluster
	mov WORD [FileAddress], ax	; The new cluster value is stored in the variable.

	; !*! Test to see what value the next cluster contains.
	cmp ax,0000h		; Free cluster (Empty)
        je .EmptyError          ; Error message
	cmp ax,0ff7h		; Is it a bad cluster?
        je .BadClusterError     ; Error message

	; !*! Test to see if this is the end of the cluster chain:
	cmp ax,0x0fff		; End of chain? (0fff)
	je .FileLoaded		; Jump to end of procedure. (Finished)

	jmp .LoadFileImage	; Loop back round to start

        .BadClusterError:
                mov si,BadClusterErrorMsg	; Display error msg
		call Print
		pop bx				; Empty stack
		pop es
		xor dx,dx			; Error code (0)
		ret				; Return
        .EmptyError:
                mov si,EmptyClusterErrorMsg	; Display error msg
		call Print
		pop bx				; Empty stack
		pop es
		xor dx,dx			; Error code (0)
		ret				; Return
	
	.FileLoaded:
		pop bx				; Empty stack
		pop es
		xor dx,dx			; Error code (1)
		mov dx,0x01
		ret				; Return

; Load the return stuff to the start of the segment.
;  (allows the applications to be ended with a ret if small enough)
; Input: AX - Segment start

LoadHeader:
	pusha					; Save segment register
	push es					; Save all general registers
	xor cx,cx				; Zero cx
	mov cx, WORD [HeaderStuff]		; Move instructions into a register
	mov es,ax				; Setup data segment
	xor bx,bx				; Setup the offset location
	mov [es:bx], cx				; Move to memory
	pop es					; Restore all general registers
	popa					; Restore segment register
	ret					; Return

HeaderStuff:
	int 0x20			; Program terminate function

; -*- Calculations -*-

; Procedure which recalculates the root directory size, location and data location.
;  Input: FAT table variables
;  Output: [RootSize], [RootStart], [DataStart]
FAT12LocCalcs:
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

	ret		; <- Return	        

; Works out the start and size of the FAT system.	
;  Input - FAT table variables
;  Output - [FATsize], [FATstart]
FATCalcs:
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
	mov [FATstart], ax		; Move to variable

	ret	; <-- Return
