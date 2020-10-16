; This compares the inputed string with the command string.
; And it contains the built-in prompt commands

;Order to run in:
; ConsoleCompareString (This will start this automaticly)
; CommandCompare (This include file)
; CompareStr (Check if it matchs the next file)

; -------------------------- Command procedures ----------------------

; Procedure to check if a file exists or not. (Command = exist)
;  existCmd - Gets the file name as well
;  exist - Just checks the name

existCmd:
	; -*- Get hold of the file name -*-
	; CmdStr - "exist abc.123"
	; So load CmdStr + 6 chars = "abc.123"
	xor ax, ax	; Zero ax
	mov ax, CmdStr	; Load address to a register
	add ax, 6	; Add 6 to delete the command from the start
	; -*- Move to the correct register for the action needed:
	mov si, ax	; Move into the register used by the file name convert procedure

exist:
	pusha
	mov si, CRLF		; Move to new line
	call Print
	popa

	; -*- Parse the file name -*-	
	mov di, FileName	; Output variable location
	call ParseFile		; Parse the file name

	; -*- Make the compare -*-
	call SearchFile		; See if the file exist's or not.
	xor ax,ax		; Zero ax
	cmp dx,ax
	je .existEnd		; If returned 0 - Failed go to end

	mov si, Found		; Print found message (Success)
	call Print

	.existEnd:
	ret			; Return

; Procedure to run a binary file
executeCmd:
	; -*- Get hold of the file name -*-
	; CmdStr - "run abc.123"
	; So load run + 4 chars = "abc.123"
	xor ax, ax	; Zero ax
	mov ax, CmdStr	; Load address to a register
	add ax, 4	; Add 6 to delete the command from the start
	; -*- Move to the correct register for the action needed:
	mov si, ax	; Move into the register used by the file name convert procedure

execute:
	pusha
	mov si, CRLF		; Move to new line
	call Print
	popa

	; -*- Parse the file name -*-	
	mov di, FileName	; Output variable location
	call ParseFile		; Parse the file name

	; -*- Make the compare -*-
	call RunFile		; See if the file exist's or not.
	xor ax,ax		; Zero ax
	cmp dx,ax
	je .executeEnd		; If returned 0 - Failed go to end

	; Run this code now the file has been parsed and found.

	.executeEnd:
	ret			; Return

about:
	mov si, CRLF		; Display a new line
	call Print
	mov si, About		; Display the about message
	call Print
	ret			; Return

ver:
	mov si, CRLF		; New line
	call Print
	mov si, Version		; Version string
	call Print
	ret			; Return

help:
	mov si, CRLF		; New line
	call Print
	mov si, Help		; Help intro message
	call Print
	mov si, HelpCmdList	; List of commands
	call Print
	ret			; Return

reboot:
        db 0Eah                 ; Machine language for reboot: (FFFF:0000)
        dw 0000h		; Could just use jmp ffff:0000 I suspect
        dw 0FFFFh
	; We are rebooting so no need to put a return here.

cls:
	call Cls
	ret			; Return

echo:
	mov si, CRLF		; Move the new line string into si
	call Print		; Display it
	mov si, CmdStr		; Move the command string into si
	add si,5		; Move the command along a bit to avoid printing the echo
	call Print		; Display it
	ret			; Return

ls:
	mov si,CRLF		; Print newline in preparation for text output
	call Print
	call TxtConListFiles	; List files procedure. (Simple version)
	ret			; Return

nonCmd:
	mov si, InvalidCmd	; Display the invalid command error
	call Print
	ret			; Return
