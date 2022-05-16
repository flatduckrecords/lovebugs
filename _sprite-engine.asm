	;include _rebuffer.asm

;; Render 16x16 masked sprite ;;

MSKD_BB_64x64:

		push hl
		call MSKD_BB_64x8		; top two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_64x8		; bottom two quadrants
		pop hl
		ret

MSKD_BB_24x16:
		push hl
		call MSKD_BB_24x8
		pop hl
		call Next_Char_Line
		push hl
		call MSKD_BB_24x8
		pop hl
		ret


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

MSKD_BB_24x8:	ld b,8

		; on entry HL is pointing to display file
		set 7, h		; HL now pointing to buffer
		ld de, DBLBFFR-BUFFER	; DE=$300 (buffer length)
		add hl, de		; HL now pointing to back buffer
		
_MSKD_BB_24x8:	call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc h
		dec l
		dec l
		djnz _MSKD_BB_24x8
		ret
MSKD_BB_64x8:	ld b,8

		; on entry HL is pointing to display file
		set 7, h		; HL now pointing to buffer
		ld de, DBLBFFR-BUFFER	; DE=$300 (buffer length)
		add hl, de		; HL now pointing to back buffer
		
_MSKD_BB_64x8:	call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc l
		call MSKD_BB_8x1
		inc h
		dec l
		dec l
		dec l
		dec l
		dec l
		dec l
		dec l
		djnz _MSKD_BB_64x8
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

SPR24x16:
		push hl
		call SPR24x8
		pop hl
		call Next_Char_Line
		call SPR24x8
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


MSKD_BB_8x1:
		ld c, (hl)		; get screen data in C
		ld a, (ix+1)		; get mask data in A
		AND c			; AND the mask over the screen data (i.e. punch out a hole)
		ld c,(ix)		; get sprite data in C
		xor c			; XOR the sprite over the masked screen data (fill the hole with sprite)
		ld (hl), a		; copy screen data to buffer
		inc ix			; move to next byte, and repeat; etc.
		inc ix			; skip over interwoven mask bytes

		ret
MSKD8x1:
		set 7,h
		ld c, (hl)		; get screen data in C
		ld a, (ix+1)		; get mask data in A
		AND c			; AND the mask over the screen data (i.e. punch out a hole)
		ld c,(ix)		; get sprite data in C
		xor c			; XOR the sprite over the masked screen data (fill the hole with sprite)
		ld (hl), a		; copy screen data to buffer
		res 7,h
		;ld (hl),a		; update screen [1]
		inc ix			; move to next byte, and repeat; etc.
		inc ix			; skip over interwoven mask bytes
		;inc iy
		ret
;SCRNMSKD8x8:
;		push hl
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		inc h
;		call SCRNMSKD8x1
;		pop hl
;		ret


;SCRNMSKD8x1:
;		ld c, (hl)		; get screen data in C
;		ld a, (ix+1)		; get mask data in A
;		AND c			; AND the mask over the screen data (i.e. punch out a hole)
;		ld c,(ix)		; get sprite data in C
;		xor c			; XOR the sprite over the masked screen data (fill the hole with sprite)
;		set 7,h
;		ld (hl), a		; copy screen data to buffer
;		res 7,h
;		;ld (hl),a		; update screen [1]
;		inc ix			; move to next byte, and repeat; etc.
;		inc ix			; skip over interwoven mask bytes
;		;inc iy
;		ret

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

;SPR24x32:	
;		push hl
;		call SPR24x8		; top three blocks
;		pop hl
;		call Next_Char_Line
;		push hl
;		call SPR24x8		; second row
;		pop hl
;		call Next_Char_Line
;		push hl
;		call SPR24x8		; third row
;		pop hl
;		call Next_Char_Line
;		push hl
;		call SPR24x8		; last row
;		pop hl
;		ret


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

SCRN16x8:	call SCRN8x8
		inc l
		call SCRN8x8
		ret

SCRN8x8:
		push hl
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		inc h
		call SCRN8x1
		pop hl
		ret	

remainder:		db 0x00

SCOREBOARD:	

		push bc
		push de
		push hl

		ld a, (SCORE)
		cp 192
		jp nc, 5F

		ld e, a

		srl a
		srl a
		srl a
		srl a				; divide by 16
		ld c, a				; greatest common divisor in C

		sla a
		sla a
		sla a
		sla a				; multiply by 16
		sub e
		neg
		ld (remainder), a
		ld a,0
		adc c				; carry set if remainder > 0 (I think!)
		;ld a, c



		ld hl, DBLBFFR + 31
		

		ld b, 12				; we have 12 segments to fill
		sub b
		cp 0					; any empty segments?
		jr z, 2F				; no, skip
		neg
		ld b, a					; set loop for number of fully empty segements
1:		ld de, segment 
		call TTLSR8x8
		call Next_Char_Line_buff
		call TTLSR8x8
		call Next_Char_Line_buff
		djnz 1B

2:		
		ld de, full
		ld a, (remainder)
		srl a					; divide by two becuase 8 into 16
		cp 0					; check for partial fill
		jr z, 3F				; skip if fully full

		push bc

		ld de, fill
		ld b, a					; calculate sprite offset
		xor a
		sub a, 16
4:		add a, 16
		djnz 4B
		ld b,0
		ld c,a
		ex de, hl
		add hl, bc
		ex de, hl
		pop bc

3:		call TTLSR8x8
		call Next_Char_Line_buff
		call TTLSR8x8
		call Next_Char_Line_buff

		
		ld a, c
		cp 0
		jr z, 2F
		ld de, full			; sprite for a filled segment
		ld b, a
1:		push de
		call TTLSR8x8
		call Next_Char_Line_buff
		call TTLSR8x8
		call Next_Char_Line_buff
		pop de
		djnz 1B


2:		ld hl, 0xE8FF
		ld de, metrebot
		call TTLSR8x8

		ld de, metretop 
		ld hl, DBLBFFR + 31
		call TTLSR8x1

		ld hl, DBLBFFR + 31
		ld de, 0x401F
		ld b, 48
3:		push bc
		ld b, 0
		ld c, 32
		ld a, (hl)
		ld (de), a
		add hl, bc		; add 32 to HL
		ex de, hl
		add hl, bc
		ex de, hl		; add 32 to DE
		ld a, (hl)
		ld (de), a
		add hl, bc		; add 32 to HL
		ex de, hl
		add hl, bc
		ex de, hl		; add 32 to DE
		ld a, (hl)
		ld (de), a
		add hl, bc		; add 32 to HL
		ex de, hl
		add hl, bc
		ex de, hl		; add 32 to DE
		ld a, (hl)
		ld (de), a
		add hl, bc		; add 32 to HL
		ex de, hl
		add hl, bc
		ex de, hl		; add 32 to DE
		pop bc
		djnz 3B



5:		pop hl
		pop de
		pop bc
		ret

TOTALISER:
		call SCOREBOARD
		ret

TTLSR8x8:
;		push de
		push hl
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc h			; move to the next line down
		inc de			; move to next byte
		call TTLSR8x1
		inc de
		pop hl
;		pop de
		ret
TTLSR8x1:	
		ld a,(de)		; get sprite data in A
		ld (hl),a		; update screen
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
UND24x16:	
		push hl
		call UND24x8		; top two quadrants
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
;UND24x16:	
;		push hl
;		call UND24x8		; top two quadrants
;		pop hl
;		call Next_Char_Line
;		push hl
;		call UND24x8		; bottom two quadrants
;		pop hl
;		ret

UND32x8:
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l
		dec l

		ret

UND24x8:
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
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
;UND16x16:	
;		push hl
;		call UND16x8		; top two quadrants
;		pop hl
;		call Next_Char_Line
;		push hl
;		call UND16x8		; bottom two quadrants
;		pop hl
;		ret

UND16x8:	;ld b,8			;; ?? can we do the loop in a way that doesn't trash B? would save on push/pops! ?? ;;

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc l
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		dec l

;		djnz _UND16x8
		ret


SCRN8x1:
		ld c, (hl)		; get screen data in C
		ld a, (ix+1)		; get mask data in A
		AND c			; AND the mask over the screen data (i.e. punch out a hole)
		ld c,(ix)		; get sprite data in C
		or c			; XOR the sprite over the masked screen data (fill the hole with sprite)
		ld (hl),a		; update screen [1]

		inc ix			; move to next byte, and repeat; etc.
		inc ix			; skip over interwoven mask bytes
		ret

HEAL24x16:

		; [ 8][16][24]
		; [ 8][16][24]

		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24

		call Next_Char_Line

		call HEAL8x8		; 8
		dec l
		call HEAL8x8		; 16
		dec l
		call HEAL8x8		; 24

		pop hl
		ret
HEAL32x16:

		; [ 8][16][24][32]
		; [ 8][16][24][32]

		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24
		inc l
		call HEAL8x8		; 32
		;pop hl
		call Next_Char_Line
		;push hl
		call HEAL8x8		; 8
		dec l
		call HEAL8x8		; 16
		dec l
		call HEAL8x8		; 24
		dec l
		call HEAL8x8		; 32
		pop hl
		ret

;HEAL24x16:
;
;		; [ 8][16][24]
;		; [ 8][16][24]
;
;		push hl
;		call HEAL8x8		; 8
;		inc l
;		call HEAL8x8		; 16
;		inc l
;		call HEAL8x8		; 24
;		pop hl
;		call Next_Char_Line
;		push hl
;		call HEAL8x8		; 8
;		inc l
;		call HEAL8x8		; 16
;		inc l
;		call HEAL8x8		; 24
;		pop hl
;

HEAL24x32: 	
		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24
		pop hl
		call Next_Char_Line
		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24
		pop hl
		call Next_Char_Line
		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24
		pop hl
		call Next_Char_Line
		push hl
		call HEAL8x8		; 8
		inc l
		call HEAL8x8		; 16
		inc l
		call HEAL8x8		; 24
		pop hl
		ret

HEAL16x24:

		push hl
		call HEAL8x8		
		inc l
		call HEAL8x8
		pop hl
		call Next_Char_Line
		push hl
		call HEAL8x8
		inc l
		call HEAL8x8
		pop hl
		call Next_Char_Line
		push hl
		call HEAL8x8
		inc l
		call HEAL8x8
		pop hl
		ret
HEAL16x8:
		push hl
		call HEAL8x8		
		inc l
		call HEAL8x8
		pop hl
		ret
BLANK16x24:

		push hl
		call BLANK8x8		
		inc l
		call BLANK8x8
		pop hl
		call Next_Char_Line
		push hl
		call BLANK8x8
		inc l
		call BLANK8x8
		pop hl
		call Next_Char_Line
		push hl
		call BLANK8x8
		inc l
		call BLANK8x8
		pop hl
		ret

;HEAL16x16:
;		push hl
;		call HEAL8x8
;		inc l
;		call HEAL8x8
;		pop hl
;		call Next_Char_Line
;		push hl
;		call HEAL8x8
;		inc l
;		call HEAL8x8
;		pop hl
;		ret

BLANK8x8:
		push hl

		ld a, 0x00
		set 7,h			; HL=BUFFER

		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h
		ld (hl),a
		inc h

		pop hl
		ret

; 'heal' the buffer at sprite x,y
HEAL8x8:
		push bc
		push hl

		ld bc, DBLBFFR-BUFFER	; BC=0x300
		set 7,h			; HL=BUFFER
		ld d, h			;
		ld e, l			; DE=BUFFER
		add hl, bc		; HL=DBLBUFFER

		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
		inc h
		inc d
		ld a, (hl)
		ld (de), a
	
		pop hl
		pop bc
		ret
;SCRN8x8:
;
;		push hl
;
;		set 7,h			; HL=BUFFER
;		ld de, 0x4000		; DE=SCREEN
;		ex de,hl
;
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;		inc h
;		inc d
;		ld a, (hl)
;		ld (de), a
;	
;		pop hl
;		ret

SHOW16x16:
		push hl
		call SHOW8x8		; top two quadrants
		inc l
		call SHOW8x8
		pop hl
		call Next_Char_Line
		push hl
		call SHOW8x8		; bottom two quadrants
		inc l
		call SHOW8x8
		pop hl
		ret
SHOW8x8:
		push hl

		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]
		inc h
		set 7,h
		ld a,(hl)		; get sprite data in C
		res 7,h
		ld (hl),a		; update screen [1]

		pop hl
		ret

SPR8x1:
		ld a,(ix)		; get sprite data in C
		ld (hl),a		; update screen [1]
		inc ix			; move to next byte
		ret
