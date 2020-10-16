; This file should contain the common interrupt procedures.
; Note most functions should respond to Ctrl-Break by jumping to int23

; This procedure should terminate a program and return control back to
; the operating system.
; Ints - Int 20; Int 21,0
INT_ProgramTerminate:
	; Instead of clearing the stack re-initalise the stack instead
	xor bx, bx		; Zero BX
	mov sp, 0xffff		; Setup the stack pointer
	mov bx, 0x9000		; Setup the stack segment
	mov ss, bx
	; Setup the registers
	xor bx, bx		; Zero bx (ax used for passing back error codes)
	mov bx, cs		; Move into bx the code segment address
	mov ds, bx		; Move the value now in bx into the other segment registers.
	mov es, bx
	mov fs, bx
	mov gs, bx
	; Jump to console
	xor bx, bx		; Zero bx
	mov bx, console		; Move into bx console IP location
	pushf			; <- Must be there as last item iret takes off the stack is the flags
	push WORD cs		; <- Must be there for iret to take the code segment location off the stack (To return to)
	push WORD bx		; <- Must be there for iret to take the Instrution Pointer off the stack (To return to)
	iret

; This procedure should get a char and echo it then return the char in al
; Ints - Int 21,1
INT_KeyboardEcho:
        push bx                 ; Save registers
	xor ax,ax		; <- BIOS function number (Get char)
	int 16h			; <- BIOS interrupt (VIDEO)
	call putchar		; <- Display the character
	pop bx			; Restore registers
        mov ah, 0x01
	iret			; <- Interrupt return

; This procedure should take a ASCII character and display it.
; Back space should work like an arrow key (but not in this)
; Ints - Int 21,2
INT_CharOut:
        push bx                 ; Save registers
	mov al, dl		; Move the ASCII char to the right location
	call putchar		; Call the print char procedure
	pop bx			; Restore registers
        mov ah, 0x02            ; Restore ah
	iret			; Int return

; Advanced version of the input/output without echo
; Ints - Int 21,6
INT_NoEchoIO:
	cmp dl, 0xff		; Compare dl to the input function
	je INT_Keyboard		; Jump if equal to the keyboard input function
	iret			; Int return

; Non echo version of the keyboard input
; Ints - Int 21,7; Int 21,8
INT_Keyboard:
        push ax                 ; Save register
	xor ax,ax		; AX function
	int 16h			; BIOS get character interrupt.
        pop ax                  ; Restore register
	iret			; Int return

; Print string, DS:DX = string location. (String ends with "$")
; Ints - Int 21,9
INT_PrintStr:
	push ax		; Save registers
	push bx
	mov si, dx	; Move into the correct register the pointer
	mov bl, 0x07	; Text Attribute (Black and white)
        mov ah, 0x0E    ; Teletype Mode
        mov bh, 0x00    ; Page Number
.nextchar:
        lodsb           ; Loads [si] into al and increment si
        cmp al, 0x24    ; Compare to 0x24 ($)
        je .return      ; If equal
        int 0x10        ; Call BIOS Video Function
        jmp .nextchar   ; Loop around to write next character
.return:
	pop bx		; Restore registers
	pop ax
	iret		; Int return
