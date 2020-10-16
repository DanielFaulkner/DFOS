; * Hardware check procedures *

; Check for 386 procedure
;  Input: none
;  Output: Message and either a reboot or nothing

checkCPU:
        pushf           ; Save flags (first use of the stack :-) )
        xor     ah,ah   ; clear high byte (ah=0)
        push    ax      ; push AX onto the stack (with flags 12-15 clear)
        popf            ; pop this value into the flag register
        pushf           ; push flags back onto the stack
        pop     ax      ; ...and get flags into AX
        and     ah,0f0h ; try to set the high nibble
        cmp     ah,0f0h ; the high nibble is never 0f0h on a
        je      .no386  ; 8088/8086 present NOT 386
        ; Check for 286 (bits 12-15 clear)
        mov     ah,0f0h ; set bits 12-15
        push    ax      ; copy ax onto the flags
        popf
        pushf           ; copy the flags into AX
        pop     ax
        and     ah,0f0h ; check if bits 12-15 are clear
        jz      .no386  ; is no 80386 installed
        call    a386    ;  Success message
        popf            ; restore the flags
        ret             ; ... and return
.no386:
        mov si, no386msg        ; Move the message address into the register
        call Print              ; Run the print message procedure
        call reboot             ; Putting the CPU into a never ending loop prob isn't the best of ideas (jmp $)
                                ;  so now ending the process and rebooting instead.

a386:
        mov si, the386msg       ; Same as for all messages
        call Print              ; Put the message on the screen procedure
        ret                     ; Return to caller
