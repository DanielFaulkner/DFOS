; Interrupt 25 functions
; Read sectors

Int25:

; Input = CX-Number of sectors, AL-Logical drive number, ES:BX-Output location (DOS is normally DS:BX), DX-Start sector
; Output = AH and AL DON'T currently in this version return an error code.
;		If error a print is put on the screen. Else sectors are loaded.

ReadSectors:

push ax			; Save register values for return
push bx
push cx
push dx
push ds

push ax			; Set data segment so the kernel variables can be used
mov ax, cs
mov ds, ax
pop ax

mov [RSDrive], AL	; Save the drive number.
xor ax,ax
mov ax, DX		; Start sector
xor dx,dx

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
        mov ch, cl                      ; Cylinder to read
        mov cl, al                      ; Sector to read
        mov dh, bl                      ; Head to read
        mov dl, BYTE [RSDrive]          ; Drive to read (Could be setup before loop to optimise)
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
        jmp .ReadError          ; Call the error command in case all else fails
.ReadError:
	; Note should later return an error code! (But not in this version)
	mov si, ReadErrorMsg	; Read Error Message
	call Print		; Display error message
	jmp .ENDREAD
.SUCCESS
        pop cx                  ; Restore the register values
        pop bx
        pop ax
        add bx, WORD [BytesPerSector]   ; Queue next buffer (Adjust output location so as to not over write the same area again with the next set of data)
        inc ax                          ; Queue next sector (Start at the next sector along from last time)
        loop .MAIN                      ; Read next sector (Back to the start)
.ENDREAD:                       ; End of the read procedure
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	iret	; <- Return from interrupt