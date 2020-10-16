; This assembly include file will contain all the macros/procedures
; Used for text console based Input/Output.
; This is distrobuted under the license that comes with the file.
; By DFTECH: Daniel Rowell Faulkner

; ------------------------- Print String -----------------------------

; Print - Black and white console print function.
;       -  accepts the string location in SI
; PrintColour - You can set bl to an alternative color as well as SI

Print:
	; Set text attribute
	mov bl, 0x07	; Text Attribute (Black and white)
PrintColour:
        ; Setup the registers
        mov ah, 0x0E    ; Teletype Mode
        mov bh, 0x00    ; Page Number
.nextchar:
        lodsb           ; Loads [si] into al and increment si
        or al, al       ; Set the Zero Flag if al = 0
        jz .return      ; If the Zero Flag is set, jump to Break
        int 0x10        ; Call BIOS Video Function
        jmp .nextchar   ; Loop around to write next character
.return:
        ret             ; Return

; ----------------------------- Put Character ------------------------

; al = Passed in digit/ASCII character
putchar:
	; Setup registers
	mov bl, 0x07	; Text attribute
	mov ah, 0x0E	; Teletype function
	mov bh, 0x00	; Page number
	; Call the interrupt
	int 0x10
	ret		; Return

; ------------------------------- ScanF ------------------------------

; Scanf - Get a string from the keyboard, until the <Return> key is pressed
;	  DI must be set before running command to the command variable. (CmdStr)
;	  Output the keyboard value to the variable in DI.
;	  Limited to dx chars, and will start at the start of the string.
;	  If CX contains a 1 then the string is not to be displayed
;	  Backspace support (Done)
;	  Arrow key support (In progress)

Scanf:
	xor bx,bx	; Zero BX (string location counter/pointer)
.start
        ; Make the BIOS call
        xor ax,ax       ; Function 0, read keyboard char
        int 16h		; Keyboard interrupt (outputs char to al)
	
	; Check to see if the return key was pressed
	cmp al, 0x0D	; Compare to ASCII <Return> char code
	je .return	; If equal make the jump

	; Check to see if the back space key was pressed.
	cmp al, 0x08	; Compare to ASCII <BSpace> char code
	je .bspace	; If equal move the pointers back by one

	; Unsure if arrow keys have ascii value. May have to use alternative reference code.
	; Check to see if the right arrow key was pressed

	; Check to see if the left arrow key was pressed

	; Save the character to a location
        mov [es:di+bx],al       ; String Location + Num chars from start
        inc bx                  ; Increase BX for next char 

	; Max string size
	cmp bx, dx	; Compare the length with the max size entered
	je .return	; No error message, just act as though return pressed

	; Display or not?
	push bx		; Save bx
	xor bx,bx
	cmp bx,cx	; Compare cx to see what value was passed
	je .putchar
.putcharReturn
        ;pop bx          ; Restore bx (Done before reaching this point!)

	jmp .start	; Loop for next char input
.putchar
	pop bx		; Restore bx
	push ax		; Save the registers
	push bx
        xor ah,ah
        mov si,ax       ; Move the ASCII char to the register
	call putchar	; Call the procedure
	pop bx		; Restore the registers
	pop ax
	jmp .putcharReturn	; Return
.return
	xor al,al		; 0 to indicate the end of the string
	mov [es:di+bx],al
	ret			; Return
.bspace
	; Check for the start of the string.
	cmp bx,0x00
	je .start	; If the start has been reached then skip the decrease.

	; Decrease bx and sort out the memory variable
	mov [es:di+bx], BYTE 0x00	; Put a 0 in the old location (wipe out current char)
	dec bx				; Decrease the pointer by 1

	; -*- Sort out the screen and display the change
	pusha			; Save registers
	; Move cursor back:
	call .CursorBack
	; * Put a blank char in the location	
	mov al, 0x20		; Move space ASCII code into al
	call putchar		; Put character on the screen
	; * Move cursor back by one (again) for the next character to be printed.
	call .CursorBack
	popa			; Restore registers

	jmp .start		; Return to the start

.CursorBack:
	; * Get current cursor position
	mov ah,0x0f		; Video state function
	int 0x10		; Video BIOS interrupt
	; ^ returns page number in BH
	mov ah,0x03		; Get current position function
	int 0x10		; Video BIOS interrupt
	; ^ Returns column in dl, row in dh
	; * Move cursor back one
	dec dl			; Column location minus 1
	mov ah,0x02		; Set current position function
	int 0x10		; Video BIOS interrupt
	ret			; Return

.CursorForward:
	; * Get current cursor position
	mov ah,0x0f		; Video state function
	int 0x10		; Video BIOS interrupt
	; ^ returns page number in BH
	mov ah,0x03		; Get current position function
	int 0x10		; Video BIOS interrupt
	; ^ Returns column in dl, row in dh
	; * Move cursor forward one
	inc dl			; Column location plus 1
	mov ah,0x02		; Set current position function
	int 0x10		; Video BIOS interrupt
	ret			; Return

.LeftArrow:
	; Check for start of buffer.
	cmp bx,0x00
	je .start	; If equal skip as can't go left.

	; Move cursor position in string
	; Prob order of stuff:
	; *Mem variable with current location in string
	; *Mem variable to show if at end of string or not
	; *Mem variable to store the end of the string if needed.

	; *** Add here some code to allow the movement through the string ***

	; Move cursor position on screen
	call .CursorBack ; No need to put a character on screen so this ok for left arrow

	jmp .start	; Return to start for next character

.RightArrow:
	; check to see if the end of the string has been reached.

	; Move cursor poistion in string
	; Move cursor position on screen
	call .CursorForward ; Move the cursor forward one character

	jmp .start	; Return to the start for the next character

; ------------------------- Compare string ---------------------------

; This function compares to strings and returns a value
;  if the two strings are the same.
; Also a value returned if the space char is found as the last
;  same value.
; CompareStr - Input compare values into DI & SI, length in CX
;	     - Output CX = 0 if equal.

CompareStr:
	; The value is compared
	; repe - Repeat in CPU internally, cx times
	; cmpsb - Compares si and di (both in segment es)
	mov ax, cx	; Move the number of chars to compare
	xor cx, cx	; Zero CX (Set initial return value)
	xor bx, bx	; Zero BX (Set loop compare value)

.ComStart:

	cmpsb		; Compare char (DS:SI, ES:DI)
	jne .Diff	; If different end
	cmp bx,ax	; Else compare with counter end
	je .End

	dec ax		; Move the ax counter down by one
	jmp .ComStart	; Go to start

.Diff:
	; Return values in registers
	mov cx, 1	; Set return value
	ret		; Return
.End:
	; Return values in registers
	ret		; Return

; --------------------- Compare string with arguments ----------------

; Modified Compare String procedure which also looks for space's
;  and to consider them as the end of the command
; DI contains the string to compare (aka the Command the User entered)
; SI contains the string to compare DI too. (aka the Command)
; Output - CX = 0 if equal, CX = 1 if not equal
;	   BX = 0 if no arguments, BX = 1 if a space char found
; Note CX and BX should never both be equal to 1

; ASCII code for space = 20h

; Problem I found was too remember to compare the ASCII code NOT
;  the character address

CompareArgumentStr:
	mov ax, cx	; Move counter to an alternative register
	xor cx, cx	; Zero CX (Set initial return value)
	xor bx, bx	; Zero BX (Set the loop compare value)
.start:
	; The value is compared
	; repe - Repeat in CPU internally, cx times <- Now done externally.
	; cmpsb - Compares si and di (both in segment es)

	cmpsb		; Compare the char then move to the next char
	jne .Diff	; Jump if different
	; If counter compare is done first character 0 will never be compared.
	cmp bx, ax	; Compare the two values: 0 and counter
	je .End		; Jump to the end if equal

.LoopEnd
	dec ax		; Subtract 1 from the loop counter
	;inc si		; Load the next two chars to compare.
	;inc di
	jmp .start	; Loop to the start for the next character

.Diff:
	; Not equal: So the values differ, as a result at this point.
	dec di		; Move pointer back to last char

	; Remember to move the value into a register for the compare!

	; Check for the return character. (ASCII return code - 0x0D)
	cmp di, 0x0D	; Compare user entered command with return code
	je .End		; Go to the end of the procedure
	; Check for the space character. (ASCII space code - 0x20)
	cmp di, 0x20	; Compare with space code
	je .Space	; Space character detected
	; Check for a null character. (code - 0)
	cmp di, 0	; Compare with 0
	je .End		; Count it as the end of the command and jump to the end

	inc di		; Move pointer forward to next char

	; Return values in registers
	mov cx, 1	; Set return value
	jmp .LoopEnd	; Go back to the compare procedure loop
.Space:
	inc bx		; Set the space detected register
			; Then end
.End:
	; Return values in registers
	; Set return value at the start, changed by .Diff if different
	ret		; Return

; ----------------------------- Pause --------------------------------

; Pause procedure - Inputs nothing, outputs nothing.
;		  - However save any info in ax!

Pauser:
	mov si, PauseMsg	; Tell the user that we are paused
	call Print
	mov ah, 0		; Function 0
	int 16h			; Wait for key press
	ret		; Return

; ---------------------------- Clear screen --------------------------

; Clear screen procedure - Inputs nothing, outputs nothing.
;			 - However save any info in ax!

Cls:
	mov ah, 0fh	; Function to detect current video state
	int 10h		; Video interrupt

			; AL = Video mode (returned from first call for second call)

	mov ah, 0	; Function to re-set the video mode
	int 10h		; Video interrupt

	ret		; Return

; ------------------------- Print Hex Number -------------------------

; PrintNumH - Prints a hex number on the screen.
;	      Number passed in CX.
;	      (Num size limited to a 16bit register)
;	      Uses PrintChar to display the character.

; Small fault is that this version puts the numbers the wrong way round.
;  aka mirrored.

PrintNumH:
	mov bx, HexDigits	; Move the table into memory
	mov si, HexReturnDigit	; Returned string value
	call .NumChar		; 0-4
	mov [si+3], al		; Put the value into the string
	shr cx, 4		; Move the number down by 4 bits
	call .NumChar		; 4-8
	mov [si+2], al		; Put value into the string, 3rd along
	shr cx, 4		; Move the number down by 4 bits
	call .NumChar		; 8-12
	mov [si+1], al		; Put the number into the string, 2nd along
	shr cx, 4		; Move the number down by 4 bits
	call .NumChar		; 12-16
	mov [si+0], al		; Put the number into the string, 1st char

	; SI all ready contains the return string.
	call Print

	ret			; Return
.NumChar:
	push cx			; Save CX for the next digit
	push bx			;  save bx which contains the table
	and cx, 000fh		; Mask out the high 12 bytes
	xor di, di		; Zero DI
	mov di, cx		; Move CX, into DI
	mov al, BYTE [bx+di]	; Move into al the char from the table
	pop bx			; Restore the table address
	pop cx			;  restore the original char
	ret			; Return

; ------------------------- Parse file address -----------------------

; Converts an address from 'abc.123' -> 'ABC     123'
;  Input: DS:SI - String to parse
;  Output: ES:DI - Location of output variable
;  Note: No handler for dodgy files with less than 3 char extension

ParseFile:
	; -*- Put 11 spaces into the variable (reset it)
	pusha				; Save registers
	xor cx, cx			; Zero CX
	mov cx, 0x0B			; Put 11 into cx counter
	mov si, FileNameTemplate	; Load template
	.Reset
		mov ax,[si]		; Copy the template over the output variable
		mov [di],ax
		inc di			; Increment for next char
		inc si
	loop .Reset			; Loop till cx = 0
	popa				; Restore registers

	; -*- Initial first name details
	xor bx,bx		; Zero bx

	; -*- Start of name
	.FirstName:
	lodsb			; Load si into al and inc si
	
	; Compare the character with a full stop.
	cmp al,0x2E		; Full stop ASCII code
	je .Extension		; Jump to the extension

	mov [es:di+bx],al	; Move the character into FileName


	cmp bx,0x08		; Compare the counter to 8
	je .Extension		; If 8 drop out to end

	inc bx			; Add 1 to bx

	jmp .FirstName		; Loop back to start

	; -*- Extension
	.Extension:
	xor bx,bx		; Reset bx for the extension (3 chars from end)
	mov bx,0x08
	; -*- Add the next characters until a nul (or 3 chars reached) to the end. 
	.ExtensionLoop:
	lodsb			; Skip the '.' character
				; Loads si into al - Increments si
	
	cmp al,0x00		; If nothing end
	je .FileParseEnd
	cmp al,0x20		; If space character end
	je .FileParseEnd

	mov [es:di+bx],al	; Add the extension to the file

	cmp bx, 0x0A		; If end of extension loop jump to end.
	je .FileParseEnd

	inc bx			; Add one to bx

	jmp .ExtensionLoop	; Loop round if not finished

	.FileParseEnd:
	ret			; Return
