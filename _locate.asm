; LOCATE display file address for a given line/column reference
; ENTRY: B=LINE, C=COLUMN
; EXIT: HL=address, DE=attr address, A=attr value

;DFCC:		EQU	0x5C84		; current print location

LOCATE:		LD	A,B
		AND	0x18
		LD	H,A
		SET	6,h
		RRCA
		RRCA
		RRCA
		OR	0x58
		LD	D,A
		LD	A,B
		AND	7
		RRCA
		RRCA
		RRCA
		ADD	A,C
		LD	L,A
		LD	E,A
		;LD	A,(DE)
		;LD	(DFCC),HL
		RET

; Get_Char_Address by Dean Bellfield
; http://www.breakintoprogram.co.uk/computers/zx-spectrum/assembly-language/z80-tutorials/print-in-assembly-language/2
;
; Get screen address from a character (X,Y) coordinate
; B = Y character position (0-23)
; C = X character position (0-31)
; Returns screen address in HL
;
Get_Char_Address:       LD A,B
                        AND %00000111
                        RRA
                        RRA
                        RRA
                        RRA
                        OR C
                        LD C,A
                        LD A,B
                        AND %00011000
                        OR %01000000
                        LD B,A
			LD HL,BC
                        RET                             ; Returns screen address in HL


;;; Move HL down one pixel line
Pixel_Address_Down:     INC H		; Go down onto the next pixel line
                        LD A,H		; Check first three bits of high byte (Y0-Y2)
                        AND 7
                        RET NZ		; If any bits are set (we are inside a char line), we are done
;;; Move HL down one character line
Next_Char_Line:
                        LD A,L		; we crossed the char line ((Y0-Y2)=0 i.e Y6=1)
                        ADD A,32	; (move down 1 char line) add the extra bit into (Y3-Y5)
                        LD L,A
                        RET NC		; Check for a carry bit (crossed a third), if C is set we are done
;crossed a third
                        LD A,H		; increase bits Y6&7 (i.e. screen-third 0, 1 or 2);
                        ADD %00001000	; 
			AND %01011111
                        LD H,A
        
	                RET		; we are done

;move up one line
Pixel_Address_Up:
	ld a,h			;check if we’re within 7 bottom lines of a char row
	and 7			;one of the three lowest bits will not be 0
	jr nz,done		;if so, it is safe to decrement H and move one line up
Prev_Char_Line	
	ld a,l			;else, load A with L
	sub $20			;subtract 32 from it,
	ld l,a			;and put it back in L
	jr nc,done		;if SUB $20 results in an overflow, we’re on the next line up
				;of char row
	ld a,h
	;add a,8			;now add 8 to H
	SUB %00001000	; 
	ld h,a			;to move to the next char row

done	;dec h			

	ret

