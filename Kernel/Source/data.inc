; This include file contains all the data used within the kernel

%include "FATtable.inc"		;<- Include the FAT table values. (Will be replaced by some code to get details from boot loader later)

; -+- Variables -+-
CmdStr	db	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	; 41 blanks for scan command
HexReturnDigit	db	0,0,0,0,0	; Hexadecimal number will be outputted to this string.
RSDrive		db 0x00			; ReadSectors procedure drive variable.
; FAT variables
RootSize	dw	0	; Root Directory Size
RootStart	dw	0	; Root Directory Start Location
DataStart	dw	0	; Data Directory Start Location
FATsize		dw	0	; FAT system size
FATstart	dw	0	; FAT system start

FileAddress	dw	0	; FAT address of file
FileAddressLBA	dw	0	; LBA address of file
FileName	db	"           "	; Name of file (8 digits + 3 digit extension = 11)
FileNameTemplate	db	"           "	; File name variable template (Used to reset the variable before each use)
TestName	db	"TEST.COM"

; -+- Messages -+-
Welcome db      13,10,"Welcome to DFOS",13,10,13,10,0
Prompt	db	13,10,"DFOS> ",0
CmdStrTemplate	db	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	; 41 blanks for scan command
PauseMsg	db	13,10,"Paused, press any key to continue...",13,10,0
Initint         db      "Initalising Interrupt handler",13,10,0
Found		db	"Found",13,10,0

; Hard ware check messages
no386msg: 	db "No 386 CPU found.",13,10,0 ; 13 and 10 is character return and line feed characters.
the386msg: 	db "386 or above CPU found.",13,10,0


; Prompt command messages
About	db	"DFOS version: 0.04a",13,10,"A DFTECH product",13,10,"Author: Daniel Rowell Faulkner",13,10,0
Version	db	"DFOS: DF-DOS prototype pre 0.1 (0.04a)",13,10,0
Help	db	"Help - command list:",13,10,0
HelpCmdList     db      "about",13,10,"help",13,10,"ver",13,10,"reboot",13,10,"cls",13,10,"exist <file>",13,10,"run <file>",13,10,"echo <Text>",13,10,"ls",13,10,0

; Error messages
InvalidCmd	db	13,10,"Invalid command",13,10,0
ReadErrorMsg	db	"Error: Unable to read sector",13,10,0
WriteErrorMsg	db	"Error: Unable to write sector",13,10,0
SearchErrorMsg	db	"Error: Unable to find file",13,10,0
BadClusterErrorMsg	db	"Error: Bad cluster",13,10,0
EmptyClusterErrorMsg	db	"Error: Empty cluster",13,10,0

; Misc
CRLF	db	13,10,0
Hash	db	"#",0
Dot	db	".",0

; Tables
HexDigits db	"0123456789abcdef"	; Number table

; -+- Commands -+-
; No Arguments required commands
Com_About	db	"about",0
Com_Help	db	"help",0
Com_Ver		db	"ver",0
Com_Reboot	db	"reboot",0
Com_Cls		db	"cls",0
Com_Echo	db	"echo ",0
Com_Exist	db	"exist ",0
Com_Execute	db	"run ",0
Com_Ls		db	"ls",0