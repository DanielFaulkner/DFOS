; Interrupt 20 functions (Not commonly used in newer DOS versions)

Int20:
	; Program terminate interrupt
	jmp INT_ProgramTerminate	;<- Go to the interrupt function