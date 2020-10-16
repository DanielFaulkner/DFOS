; Interrupt 21 functions

Int21:
	; Contains many functions - Function code passed in AL

	; 0 - Program terminate
	cmp ah, 0x00
        je .INT_ProgramTerminate

	; -*- Character functions -*-
	; 1 - Keyboard input with echo (al-Inputed ASCII char)
	cmp ah, 0x01
	je .INT_KeyboardEcho

	; 2 - Display output (dl-Input ASCII char)
	cmp ah, 0x02
	je .INT_CharOut

	; Skip the Auxiliary input device functions

	; 3 - Auxiliary input, from serial port (al-Outputed ASCII char)

	; 4 - Auxiliary output, from serial port (dl-Character to send to serial port)

	; 5 - Printer output (dl-Character to send to parrelled port)
	; I hope to be able to set this up in the near future

	; 6 - Advanced input/output procedure. (DL=FF for input, else char to output in al)
	cmp ah, 0x06
	je .INT_NoEchoIO

	; 7 - Wait for direct console input without echo (al-Char entered)
	cmp ah, 0x07
	je .INT_Keyboard	;<- General non echo input procedure

	; 8 - Keyboard input without echo (al-Char entered) (Difference is it checks for Ctrl+C)
	cmp ah, 0x08
	je .INT_Keyboard

	; 9 - Print string (DS:DX-String to display)
	cmp ah, 0x09
	je .INT_PrintStr

	; A - Buffered keyboard input (DS:DX-Location of input buffer, copies 1 line into buffer)
	;     Input buffer format: Byte1-Buffer size, Byte2-Num chars inputed, Byte3-Data
	; B - Check standard keyboard input status, determines if character available (Returns al:0-Not ready,0FFh-ready)
	; C - Clear keyboard buffer, invoke keyboard function (al-Input opcode of 0 1 6 7 or 8, al-Returns inputed char if opcode 1 6 7 8)

	; -*- Disk functions -*-
	; D - Disk reset (Copies all file buffers to disk, think seen done in a boot loader)
	; E - Select disk (Sets the default drive, dl-Drive Num input, al-Output number of drives) Logical order-0=A,1-B,2=C...

	; Skip some functions here

	; 19 - Get default drive (al-outputs drive number, 0=A,1=B...)
	; 1A - Set disk transfer area address (ds:dx-Disk transfer area address, Sets address for obsolete file I/O and Find First/Find Next commands)
	; 1B - Get default drive data (al-Outputs sectors/cluster, cx-Bytes per sector, dx-# of clusters, ds:bx-Media descriptor byte pointer.
	;  See also funtion 36h, Media descriptor byte: 0F0h-3.5, 0F8h-Hard disk,0F9h-720k 3.5 or 1.2mb 5.25,0FAh-320k 5.25, 0FBh-640k 3.5, 0FCh-180k 5.25
	;  0FDh-360k 5.25, 0FEh-160k 5.25, 0FFh-320k 5.25
	; 1C - Same as 1B but you can specify a drive (dl-Drive number, 0=A,1=B...)

	; 1D skipped

	; 1F - Get DPB (Default disk parameter block) (al-returns the pointer to the block, 0FFh if error)
	;    - Contains Drive(Byte) - Logical number, Unit(byte) - Unit number for driver, SectorSize(word) - # bytes/sector, ClusterMask(byte) - Sectors/cluster -1
	;    - Cluster2(byte) - 2clusters/sector

	; 31 - Terminate process and remain resident
	; For now this version will use the normal terminate process function (which does not leave the program resident)
	cmp ah,0x31
        je .INT_ProgramTerminate

	; 4C - Program terminate with return code (Preserve AL basicly)
	; For this version AL will not be preserved and this will use the same command as the other terminate ints
	; The above can be fixed by replacing the use of ax with bx in the function.
	cmp ah,0x4C
        je .INT_ProgramTerminate

	;... F8
	mov ah, 0x99	; Error number (specific to DF-DOS)
	iret		; If function not found return

.INT_ProgramTerminate:
        jmp INT_ProgramTerminate

.INT_KeyboardEcho:
	jmp INT_KeyboardEcho

.INT_CharOut:
	jmp INT_CharOut

.INT_NoEchoIO
	jmp INT_NoEchoIO

.INT_Keyboard
	jmp INT_Keyboard

.INT_PrintStr
	jmp INT_PrintStr