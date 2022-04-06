; Repopulate the buffer
; based on the masked sprite routine
; but just populates the buffer
; e.g. use when the screen changes under the icon

BUFF16x16:	
		push hl
		ld iy, BUFFER
		call BUFF16x8		; top two quadrants
		pop hl
		push hl
		call Next_Char_Line
		call BUFF16x8		; bottom two quadrants
		pop hl
		ret

BUFF16x8:	ld b,8
_BUFF16x8:	call BUFF8x8
		inc l
		call BUFF8x8
		inc h
		dec l
		djnz _BUFF16x8
		ret

BUFF8x8:	ld c, (hl)		; get screen data in C
		ld (iy), c		; copy screen data to buffer
		inc ix			; move to next byte, and repeat; etc.
		inc iy
		ret
