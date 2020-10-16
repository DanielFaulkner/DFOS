; This file contains all the common conversions used.

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
