; This is a simple kernel

; By Daniel Rowell Faulkner

[BITS 16]
[ORG 0x0000]

jmp main

; %include "txtio.mac" ;<- Macro version removed for now
%include "txtio.inc"            ;<- Print/Scan style commands
%include "console.inc"          ;<- UI stuff
%include "commands.inc"		;<- Commands possible
%include "data.inc"		;<- Data (Messages variables etc)
%include "convers.inc"		;<- Common conversion procedures.
%include "HWtests.inc"		;<- Hard ware checks/test procedures.
%include "FAT12drv.inc"		;<- FAT12 driver file.
%include "intrupt.inc"		;<- Interrupts Initalising section
%include "IntFuncs.inc"		;<- Common Interrupt functions/procedures

; Include the interrupt functions:
%include "int20.inc"		; Include the int 20 functions
%include "int21.inc"		; Include the int 21 functions
%include "int22.inc"		; Include the int 22 functions
%include "int23.inc"		; Include the int 23 functions
%include "int24.inc"		; Include the int 24 functions
%include "int25.inc"		; Include the int 25 functions
%include "int26.inc"		; Include the int 26 functions
%include "int27.inc"		; Include the int 27 functions
%include "int28.inc"		; Include the int 28 functions
%include "int29.inc"		; Include the int 29 functions
%include "int2e.inc"		; Include the int 2e functions
%include "int2f.inc"		; Include the int 2f functions

main:

	xor bx, bx
	; - Stuff normally done in the boot loader -
KernInit:
	; Setup the registers
	mov ax, 0x3000
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax

	;call reboot

	; Setup the stack
	mov sp, 0xffff
	mov ax, 0x9000
	mov ss, ax

	cmp bx, 0x01
	je console

	; Display the welcome message
	mov si, Welcome
	call Print	

	; -*- Kernel init messages -*-

	; Display the interrupt start message
	mov si, Initint		; Print Message
	call Print

	; Initalise software interrupts
	call Initalise_int

	; -*- About message -*-

	mov si, CRLF		; Put blank line
	call Print

	; Display the about message
	mov si, About
	call Print

	; Start the console
console:
	;mov ax,cs
	;mov ds,ax
	;mov es,ax

	; Get the user input
	call OSPrompt

	; Compare the command and do the appropriet action
	jmp ConsoleCompareStr		; Uses int's to get return.

	; Temporarily echo input:
	;mov si, CRLF	; New line 	;\
	;call Print			; \
	;mov si, CmdStr	; The input	;  \ Echo input string
	;call Print			;  / Test
	;mov si, CRLF	; New line	; /
	;call Print			;/

	jmp console

KernEnd:
	Hlt	; Halt

; --------------------------- Procedures ---------------------------

; ------------------------------ Data ------------------------------

; The data has been moved to the data include file.
