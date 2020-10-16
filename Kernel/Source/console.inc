; This file includes the work done to set up the console.
; Seperated so that I can update the console without
;  risking the whole OS.

; By DFTECH: Daniel Rowell Faulkner

; This function is used to display the prompt and await an input

OSPrompt:
	; Display the prompt
	mov si, Prompt
	call Print

	; Setup the registers for the scanf call
	mov di, CmdStr		; Location to store keyboard input
	mov dx, 40		; Max size
	xor cx, cx		; 0 = Display char
	call Scanf		; Make the call

	;push bx		; \
	;mov cx, bx		;  \ Test code to display string length
	;call PrintNumH		;  /  value.
	;pop bx			; /

	ret

; Console compare string 

ConsoleCompareStr:
	; Value put in to the register
	mov di, CmdStr		; Move the command entered into DI
	mov cx, bx		; Number of chars to compare
	mov bx, 1		; Compare based upon length passed 
	jmp CommandCompare	; Call the command compare procedure

; --------------------------- Command compare ------------------------

; At this stage ConsoleCompareString should have setup DI with the
;  prompt input, CX with the length, BX with the compare type.

CommandCompare:
; Insert a null entry handler here.
.nullCmd
	pusha			; Save register values
	xor ax,ax		; Zero ax
	cmp ax,cx		; Compare 0 to cx
	popa			; Restore register values
	jne .nullCmdEnd

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.nullCmdEnd:
	; Command end

.about
	; About command
	pusha			; Save register values

	mov si, Com_About	; Setup the CompareStr command
	call CompareStr		; Compare the two strings

        xor bx, bx              ; Zero register
        cmp bx, cx              ; True if equal
	popa
	jne .aboutend		; Jump of equal
	
	call about		; Run command

	xor ax,ax		; Program terminate function
	int 0x21		; OS interrupt

.aboutend
	; End of this command

.ver
	pusha			; Save registers

	mov si, Com_Ver		; Load the command for the CompareStr command
	call CompareStr		; Compare the strings

	xor bx,bx		; Zero bx
	cmp bx,cx		; Compare the registers
	popa			; Restore registers
	jne .verend		; If equal go to the ver command

	call ver		; Run the command

	xor ax,ax		; Program terminate function
	int 0x21		; OS interrupt
	
.verend
	; End of the command

.help
	pusha			; Save registers

	mov si, Com_Help	; Setup the compare procedure
	call CompareStr		; Compare the strings

	xor bx, bx		; Zero bx
	cmp bx, cx		; Compare registers
	popa			; Restore registers
        jne .helpend            ; If equal jump to the help procedure

	call help		; Run command

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.helpend
	; Command end

.reboot
	pusha			; Save registers

	mov si, Com_Reboot	; Setup the compare procedure
	call CompareStr		; Compare the strings

	xor bx, bx		; Zero bx
	cmp bx, cx		; Compare registers
	popa			; Restore registers
        jne .rebootend          ; If equal jump to the reboot procedure

	call reboot		; Run command

	xor ax,ax		; Function 0 (Prog terminate)
	int 0x21		; OS int

.rebootend
	; Command end

.cls
	pusha

	mov si, Com_Cls 	; Setup the compare procedure
	call CompareStr		; Compare the strings

	xor bx, bx		; Zero bx
	cmp bx, cx		; Compare registers
	popa			; Restore registers
        jne .clsend             ; If equal jump to the clear screen procedure

	call cls		; Run command

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.clsend
	; Command end

.echo
	pusha			; Save registers

	;mov cx, 4		; Length to compare to (For some reason I managed to make it not count 0)
	mov si, Com_Echo 	; Setup the compare procedure
	xor cx,cx		; Reset the num of chars to compare
	mov cx,0x04
	call CompareStr		; Compare the strings

	xor ax, ax		; Zero ax
	cmp ax, cx		; Compare registers
	popa			; Restore registers
        jne .echoend            ; If equal jump to the clear screen procedure

	call echo

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.echoend
	; Command end

.exist
	pusha			; Save registers

	mov si, Com_Exist 	; Setup the compare procedure
	xor cx,cx		; Reset the number of chars to compare
	mov cx,0x05
	call CompareStr		; Compare the strings

	xor ax, ax		; Zero ax
	cmp ax, cx		; Compare registers
	popa			; Restore registers
        jne .existend		; If equal jump to the clear screen procedure

.existrun
	call existCmd

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.existend
	; Command end	

.execute
	pusha			; Save registers

	mov si, Com_Execute 	; Setup the compare procedure
	xor cx,cx		; Reset the number of chars to compare
	mov cx,0x03
	call CompareStr		; Compare the strings

	xor ax, ax		; Zero ax
	cmp ax, cx		; Compare registers
	popa			; Restore registers
        jne .executeend		; If equal jump to the clear screen procedure

.executerun
	call executeCmd

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.executeend
	; Command end

.ls
	pusha

	mov si, Com_Ls	 	; Setup the compare procedure
	call CompareStr		; Compare the strings

	xor bx, bx		; Zero bx
	cmp bx, cx		; Compare registers
	popa			; Restore registers
        jne .lsend              ; If equal jump to the clear screen procedure

	call ls			; Run command

	xor ax,ax		; Prog terminate function
	int 0x21		; OS int

.lsend
	; Command end

.nonCmd
	call nonCmd		; Run the no command procedure
	
	xor ax,ax		; Prog terminate function
	int 0x21		; OS int