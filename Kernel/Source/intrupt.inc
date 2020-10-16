; Part of DFOS: DF-DOS

; By DFTECH: Daniel Rowell Faulkner

; I hope to keep the interrupts mostly compatable with DOS.
; So to do that I need to setup the following int's:
; Int 20 - Program terminate
; Int 21 - DOS function Dispatcher (119 functions)
; Int 22 - Program terminate
; Int 23 - Ctl-Break exit address
; Int 24 - Critical error handler address
; Int 25 - Absolute disk read
; Int 26 - Absolute disk write
; Int 27 - Terminate but stay resident
; Int 28 - DOS idle loop/scheduler
; Int 29 - Fast chahacter output
; Int 2E - Execute command using base level command.com
; Int 2F - Multiplex interrupt (10 functions)

; Initalise Interrupt
Initalise_int:
	push ax			; Save registers
	push es

	; Setup the segments for the Interrupt Vector Table (0000:0000)
	xor ax, ax		; Zero ax
	mov es, ax		; Setup es to equal sx (0000)

	; Int 20
	mov WORD [es:0x20*4], Int20	; Add the Interrupt procedure location to the IVT
	mov WORD [es:0x20*4+2], cs	; Segment containing the procedure

	; Int 21
	mov WORD [es:0x21*4], Int21	; Procedure
	mov WORD [es:0x21*4+2], cs	; Segment

	; Int 22
        mov WORD [es:0x22*4], Int22    ; Procedure
	mov WORD [es:0x22*4+2], cs	; Segment

	; Int 23
        mov WORD [es:0x23*4], Int23    ; Procedure
	mov WORD [es:0x23*4+2], cs	; Segment

	; Int 24
	mov WORD [es:0x24*4], Int24	; Procedure
	mov WORD [es:0x24*4+2], cs	; Segment

	; Int 25
        mov WORD [es:0x25*4], Int25    ; Procedure
	mov WORD [es:0x25*4+2], cs	; Segment

	; Int 26
        mov WORD [es:0x26*4], Int26    ; Procedure
	mov WORD [es:0x26*4+2], cs	; Segment

	; Int 27
	mov WORD [es:0x27*4], Int27	; Procedure
	mov WORD [es:0x27*4+2], cs	; Segment

	; Int 28
        mov WORD [es:0x28*4], Int28    ; Procedure
	mov WORD [es:0x28*4+2], cs	; Segment

	; Int 29
        mov WORD [es:0x29*4], Int29    ; Procedure
	mov WORD [es:0x29*4+2], cs	; Segment

	; Int 2E
        mov WORD [es:0x2e*4], Int2e    ; Procedure
	mov WORD [es:0x2e*4+2], cs	; Segment

	; Int 2F
        mov WORD [es:0x2f*4], Int2f    ; Procedure
	mov WORD [es:0x2f*4+2], cs	; Segment

	pop es
	pop ax
	ret
