; Interrupt 26 functions

Int26:
	; Write sectors function
	
	; Should be similar to the read sector code (I hope)

	; BIOS register inputs are the same as read sector. (function 3)

; Input = CX-Number of sectors, AL-Logical drive number, ES:BX-Output location (DOS is normally DS:BX), DX-Start sector
; Output = AH and AL DON'T currently in this version return an error code.
;		If error a print is put on the screen. Else sectors are loaded.

WriteSectors:

push ax			; Save register values for return
push bx
push cx
push dx

mov [RSDrive], AL	; Save the drive number.
mov ax, DX		; Start sector

.MAIN:                          ; Main Label
        mov di, 5               ; Loop 5 times max!!!
.SECTORLOOP:
        push ax                 ; Save register values on the stack
        push bx
        push cx

        push bx			; This value is used rather than saved
	; AX must have the LBA sector value. (Correct)
	call LBAtoCHS             ; Change the LBA addressing to CHS addressing
	; AX=Sector, CX=Cylinder, BX=Head
        ; The code to read a sector from the floppy drive
        mov ch, cl                      ; Cylinder to write to
        mov cl, al                      ; Sector to write to
        mov dh, bl                      ; Head to write to
        mov dl, BYTE [RSDrive]          ; Drive to write to (Could be setup before loop to optimise)
        mov ah, 03              	; BIOS write sector function
        mov al, 01              	; Write one sector
        pop bx
        int 0x13                	; Make the BIOS call
	; Loop round stuff
        jnc .SUCCESS
        dec di                  ; Decrease the error counter
        pop cx                  ; Restore the register values
        pop bx
        pop ax
        jnz .SECTORLOOP         ; Try the command again incase the floppy drive is being annoying
        jmp .WriteError          ; Call the error command in case all else fails
.WriteError:
	; Note should later return an error code! (But not in this version)
	push ds			; Save the data segment
	mov ax, cs		; Setup the data segment so I can access the error message
	mov ds, ax
	mov si, WriteErrorMsg	; Read Error Message
	call Print		; Display error message
	pop ds			; Restore the data segment
	jmp .ENDWRITE
.SUCCESS
        pop cx                  ; Restore the register values
        pop bx
        pop ax
        add bx, WORD [BytesPerSector]   ; Queue next buffer (Adjust output location so as to not over write the same area again with the next set of data)
        inc ax                          ; Queue next sector (Start at the next sector along from last time)
        loop .MAIN                      ; Read next sector (Back to the start)
.ENDWRITE:	                        ; End of the read procedure
	pop dx
	pop cx
	pop bx
	pop ax
	iret	; <- Return from interrupt