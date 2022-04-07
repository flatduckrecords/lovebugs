	;include _rebuffer.asm

;; Render 16x16 masked sprite ;;
MSKD24x32:	
		push hl
		call MSKD24x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD24x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD24x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD24x8		; bottom two quadrants
		pop hl
		ret
MSKD32x16:	
		push hl
		call MSKD32x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD32x8		; bottom two quadrants
		pop hl
		ret
MSKD24x16:	
		push hl
		call MSKD24x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD24x8		; bottom two quadrants
		pop hl
		ret
MSKD16x24:	
		push hl
		call MSKD16x8		; top two
		pop hl
		call Next_Char_Line
		push hl
		call MSKD16x8		; middle two
		pop hl
		call Next_Char_Line
		push hl
		call MSKD16x8		; bottom two
		pop hl
		ret
MSKD16x16:	
		push hl
		call MSKD16x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD16x8		; bottom two quadrants
		pop hl
		ret

MSKD32x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
		
_MSKD32x8:	call MSKD8x1
		inc l
		call MSKD8x1
		inc l
		call MSKD8x1
		inc l
		call MSKD8x1
		inc h
		dec l
		dec l
		dec l
		djnz _MSKD32x8
		ret
MSKD24x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
		
_MSKD24x8:	call MSKD8x1
		inc l
		call MSKD8x1
		inc l
		call MSKD8x1
		inc h
		dec l
		dec l
		djnz _MSKD24x8
		ret

MSKD16x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
_MSKD16x8:	call MSKD8x1
		inc l
		call MSKD8x1
		inc h
		dec l
		djnz _MSKD16x8
		ret

MSKD8x8:
		push hl
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		inc h
		call MSKD8x1
		pop hl
		ret



MSKD8x1:	ld c, (hl)		; get screen data in C
		ld a, (ix+1)		; get mask data in A
		AND c			; AND the mask over the screen data (i.e. punch out a hole)
		;set 5,h
		;ld (hl), c		; copy screen data to buffer
		;res 5,h
		ld c,(ix)		; get sprite data in C
		xor c			; XOR the sprite over the masked screen data (fill the hole with sprite)
		ld (hl),a		; update screen [1]
		inc ix			; move to next byte, and repeat; etc.
		inc ix			; skip over interwoven mask bytes
		;inc iy
		ret

SPR64x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
_SPR64x8:	call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc h
		dec l
		dec l
		dec l
		dec l
		dec l
		dec l
		dec l
		djnz _SPR64x8
		ret

SPR64x64:	ld b,8
_SPR64x64:	push bc
		push hl
		call SPR64x8
		pop hl
		call Next_Char_Line
		pop bc
		djnz _SPR64x64
		ret


SPR24x32:	
		push hl
		call SPR24x8		; top three blocks
		pop hl
		call Next_Char_Line
		push hl
		call SPR24x8		; second row
		pop hl
		call Next_Char_Line
		push hl
		call SPR24x8		; third row
		pop hl
		call Next_Char_Line
		push hl
		call SPR24x8		; last row
		pop hl
		ret


SPR24x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
_SPR24x8:	call SPR8x1
		inc l
		call SPR8x1
		inc l
		call SPR8x1
		inc h
		dec l
		dec l
		djnz _SPR24x8
		ret


SPR16x16:	
		push hl
		call SPR16x8		; top two quadrants
		pop hl
		push hl
		call Next_Char_Line
		call SPR16x8		; bottom two quadrants
		pop hl
		ret

SPR16x8:	ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
_SPR16x8:	call SPR8x8
		inc l
		call SPR8x8
		inc h
		dec l
		djnz _SPR16x8
		ret
SPR8x8:		ld c,(ix)		; get sprite data in C
		ld a,(hl)		; get screen data in A
		xor c
		ld (hl),a		; update screen
		inc ix			; move to next byte
		ret

UND24x32:	
		push hl
		call UND24x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND24x8		; 2nd two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND24x8		; 3rd two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND24x8		; bottom two quadrants
		pop hl
		ret
UND32x16:	
		push hl
		call UND32x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND32x8		; bottom two quadrants
		pop hl
		ret
UND24x16:	
		push hl
		call UND24x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND24x8		; bottom two quadrants
		pop hl
		ret

UND32x8:
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		ret

UND24x8:
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		ret

UND16x24:	
		push hl
		call UND16x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND16x8		; middle two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND16x8		; bottom two quadrants
		pop hl
		ret
UND16x16:	
		push hl
		call UND16x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call UND16x8		; bottom two quadrants
		pop hl
		ret

UND16x8:	;ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;
_UND16x8:
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc l
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

;		djnz _UND16x8
		ret


SPR8x1:		ret
UND8x8:
		push hl

		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		inc h
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]

		pop hl
		ret

SPR8x1b:
		set 5,h
		ld a,(hl)		; get sprite data in C
		res 5,h
		ld (hl),a		; update screen [1]
		ret
