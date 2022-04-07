			DEVICE ZXSPECTRUM48  ; sjasmplus directive for SAVESNA (at end)
			SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION

	DEFINE EMULATOR 1
Stack_Top:		EQU 0xFFF0

	IFDEF EMULATOR
Code_Start:		EQU $8000
			ORG Code_Start
			JP ENTRY
	ELSE
Code_Start:		EQU $8000-4
			ORG Code_Start
			DW $8000
			DW ENTRY
	ENDIF

ENTRY
		EI
		call ShowTitle

;waitk		ld a,(23560)			; read keyboard.
;		cp 32				; is SPACE pressed?
;		jr nz,waitk			; no, wait.
		;jr PLAY

		DI
		LD SP,Stack_Top
		LD A,0x00
		OUT (254),A
		;CALL Clear_Screen
		LD HL,Interrupt
		LD IX,0xFFF0
		LD (IX+04h),0xC3           	; Opcode for JP
		LD (IX+05h),L
		LD (IX+06h),H
		LD (IX+0Fh),0x18           	; Opcode for JR; this will do JR to FFF4h
		LD A,0x39
		LD I,A
		LD SP,0xFFF0

		EXX
		ld hl, FRAMES
		EXX

		IM 2
		EI

		;jr waitk

PLAY:		call ShowBG
		call SetBuffer
		;call PJ1
		;call Pnt1
		;call ShowDrutt


LOOP:			HALT				; Wait for NMI
			;HALT
			;HALT
			

			;di
			;call _hellaPlot
			;call Timer
			;call Clicker
			;call z, CheckForHit


			ld ix, BUGS_TABLE
			ld iy, SPRITE_TABLE
			ld b, BT_LENGTH

bugloop			
			push ix
			;halt
;			ld a,b
;			and 7
;			out (254), a
			call show
		;halt
			pop ix
			ld de, BT_WIDTH
			add ix, de
			djnz bugloop
;			ld a,7
;			out (254), a


;			ld a,7
;			out (254), a

			;call PJ1


			;call Peejay

;			ld a,2
;			out (254), a

;			call Pointer


			;ei
		
			JR LOOP				; And loop



Interrupt:		
			DI
			ex af,af'
			EXX

			call Mouse		; get Mouse_Buttons and Mouse_Coords
			call _print		; translate coords into screen row/column
						xor a
			out (254), a
			call Reticule
			ld a,7
			out (254), a

			; update FRAMES. Similar to ROM but only 2 bytes
			ld hl, (FRAMES)
			inc hl	
			ld (FRAMES), hl	

			EXX
			ex af,af'
			EI
			RET


Clicker:		ld a, (Mouse_Buttons)
			cpl
			and 3
			out (254), a
			cp 2
			ret
Timer: 			
			;ld a, (FRAMES)
			;and %00000100
			;ret z
			ld a, (PJ_XY)
			inc a
			ld (PJ_XY),a
			ret
CheckForHit:		ret
show:
			ld a,(ix)		; bug table
			cp 255			; is bug dead?
			ret z			; if yes skip this one
			push bc
			cp 254
			jr nz, 1F
			call d1
			pop bc
			ret
1:			call display
			pop bc
			ret

move:			ld a,(ix+1)		; alien movement direction.
			rra			; rotate low bit into carry.
			jr nc,movav		; no carry ∴ 0 or 2, must be vertical.
						; direction is 1 or 3 so it's horizontal.
			rra			; rotate next bit into carry for test.
			jr nc,movar		; direction 1 = move alien right.

; Move alien left.
moval			ld a,(ix+3)		; get y coordinate.
			sub 1			; move up.
			ld (ix+3),a		
			cp (ix+4)		; reached mimimum yet?
			jr z,movax		; yes - change direction.
			jr c,movax		; oops, gone past it.
			ret
; Move alien right.
movar			ld a,(ix+3)		; get y coordinate.
			add a,1			; move down.
			ld (ix+3),a
			cp (ix+5)		; reached maximum yet?
			jr nc,movax		; yes - change direction
			ret

; Move alien vertically.
movav			rra			; test direction.
			jr c,movad		; direction 2 is down.

; Move alien up.
movau			ld a,(ix+2)		; get x coordinate.
			sub 1			; move left.
			ld (ix+2),a
			cp (ix+4)		; reached mimimum yet?
			jr z,movax		; yes - change direction.
			ret
; Move alien down.
movad			ld a,(ix+2)		; get x coordinate.
			add a,1			; move right.
			ld (ix+2),a		; new coordinate.
			cp (ix+5)		; reached maximum yet?
			jr nc,movax		; yes - change direction.
			ret

; Change alien direction.
movax			ld a,(ix+1)		; direction flag.
			xor 2			; switch direction, either
						; horizontally or vertically.
			ld (ix+1),a		; set new direction.
			ret

ShowTitle:
		ld hl, LB3
		ld de, 0x4000
		ld bc, 6912
		ldir
		ret
ShowBG:
		ld hl, BRICK
		ld de, 0x4000
		ld bc, 6912
		ldir
		ret
SetBuffer:
		ld hl, BRICK
		ld de, 0x6000
		ld bc, 6144
		ldir
		ret
ShowBuffer:
		ld hl, 0x6000
		ld de, 0x4000
		ld bc, 6144
		ldir
		ret

Reticule:
			ld bc,(RET_XY+2)		; OLD position
			call Get_Char_Address		; get screen address in HL
			call UND24x16			; HL will be preserved
			ld bc,(RET_XY)			; NEW position
			ld (RET_XY+2),bc		; update OLD position
			call Get_Char_Address		; get screen address in HL
			ld ix, hearticon		; pointer to sprite
			ld a, (RET_XY+4)		; get x-postion offset
			and 7				; clip to 7 max
			jr z,2F				; if zero skip to the end
			ld b,a				; number of offsets
			ld de, 96			; amount per offset
1:			add ix, de			; move sprite pointer by offet amount
			djnz 1B				; repeat until done
2:			call MSKD24x16			; draw sprite
			ret

display:	

			ld b,(ix+3)		; The previous coords
			ld c,(ix+2)		; The previous coords
;			LD A,(ix+2)		; Current coords
;			SUB C			; Subtract from the old coordinate
;			RET Z			; If zero, then skip
			ld a, c
			srl a
			srl a
			srl a
			ld c, a
			call Get_Char_Address		; get screen address in HL

			
			ld de, udr			; stack the return address as if we 
			push de				; were doing call to a routine.
			ld e, (ix+11)			; 
			ld d, (ix+12)			; DE e.g. call UND16x16
			push de				; stack the sprite routine jump addr
			;CALL UND24x32
			ret
udr:						
			;ld a, (FRAMES)
			;and a
			;call po, move
			call move
			
d1:							; entry point for non-moving sprites


			
			ld de, dr			; stack the return address as if we 
			push de				; were doing call to a routine.
			ld e, (ix+9)			; 
			ld d, (ix+10)			; DE e.g. call MSKD16x16
			push de				; stack the sprite routine jump addr
;			
			ld b,(ix+3)			; NEW position
			ld c,(ix+2)			; NEW position

			ld a, c
			srl a
			srl a
			srl a
			ld c, a
			push bc				; stack the columnised position
			sla a
			sla a
			sla a
			ld c,(ix+2)
			sub c
			neg
			and 7
			ld de,0				; 
			jr z,2F				; if zero skip to the end
			ld b,a				; number of offsets
			ld hl, 0
			ld e, (ix+6)			; amount per offset
1:			add hl, de			; add up sprite pointer offet amount
			djnz 1B				; repeat until done
			ex de,hl			; DE=total offset

2:			pop bc				; BC=row/column
			call Get_Char_Address		; HL=screen address
			ld c, (ix+7)			; 
			ld b, (ix+8)			; BC=sprite addr
			ld ixh, b			; 
			ld ixl, c			; IX=sprite addr
			add ix, de			; IX=sprite addr + animation frame

			ret				; use RET to simulate "call SPRITE_ROUTINE"
dr:							; return from jump
			ret				; actual RET for this routine
Pointer:	
			LD BC,(POSXY+2)		; The previous coords
			LD A,(POSXY)		; Current coords
			SUB C			; Subtract from the old coordinate
			RET Z			; If zero, then skip

			ld bc,(POSXY+2)			; OLD position
			call Get_Char_Address		; get screen address in HL
			CALL UND16x16			; HL will be preserved
			
Pnt1:			ld bc,(POSXY)			; NEW position
			ld (POSXY+2),bc			; update OLD position
			call Get_Char_Address		; get screen address in HL
			ld ix, drutt			; pointer to sprite
			call MSKD16x16			; draw sprite
			ret
ShowDrutt:	
			LD BC,(ENEMY+2)		; The previous coords
			LD A,(ENEMY)		; Current coords
			SUB C			; Subtract from the old coordinate
			RET Z			; If zero, then skip

			ld bc,(ENEMY+2)			; OLD position
			call Get_Char_Address		; get screen address in HL
			CALL UND16x16			; HL will be preserved
			
Drutt1:			ld bc,(ENEMY)			; NEW position
			ld (ENEMY+2),bc			; update OLD position
			call Get_Char_Address		; get screen address in HL
			ld ix, drutt			; pointer to sprite
			call MSKD16x16			; draw sprite
			ret

Peejay	:		
			LD BC,(PJ_XY+2)		; The previous coords
			LD A,(PJ_XY)		; Current coords
			SUB C			; Subtract from the old coordinate
			RET Z			; If zero, then skip

			;ld bc,(PJ_XY+2)			; OLD position
			call Get_Char_Address		; get screen address in HL
			CALL UND24x32			; HL will be preserved
PJ1:			ld bc,(PJ_XY)			; NEW position
			ld (PJ_XY+2),bc			; update OLD position
			call Get_Char_Address		; get screen address in HL
			ld ix, pj		; pointer to sprite
			call MSKD24x32			; draw sprite
			ret



FRAMES		equ 0x5C78
PJ_XY		dw 0x1400, 0x4001
ENEMY		dw 0x5A00, 0x5A00

POSXY		dw 0x4840, 0x0000
RET_XY		dw 0x0000, 0x0000, 0x0000
X_PositionBits: defb 128,64,32,16,8,4,2,1

SPR_BYTES	EQU 16*16/8
;SPR_BYTES	EQU 24*32/8

		;ix+1 = direction 0=left, 1=up, 2=right, 3=down

BT_WIDTH	EQU 13
BT_LENGTH	EQU 5
BUGS_TABLE:	defb 0x00,0x03,32,3,0,20,0
		defw drutt, MSKD16x16, UND16x16

		defb 0x00,0x00,12,17,0,255-24,128
		defw droog, MSKD32x16, UND32x16

		defb 0x00,0x00,1,20,0,255-24,128
		defw droog, MSKD32x16, UND32x16

		defb 0xFF,0x03,15,1,0,20,0
		defw drutt, MSKD16x16, UND16x16

petesprite	defb 0xFE,0x02,0,20,0,29,0
		defw pj, MSKD24x32, UND24x32
		
		defb 0xFF,0,0,0,0,0
		defw 0x0000, 0x0000


SPRITE_TABLE:	DEFW drutt, pj
BUFFER:		EQU 0X6000	;BLOCK 	32*192/8,0xFF	; 768 bytes to store screen display buffer
		;BLOCK 	32,0xFF		; 32 bytes to store 16x16 display buffer




;Byte2Attr:	
;		push hl
;		ld hl, $580F
;		;ld a,(DE)			; [7t]
;		ld b,2				; [7t]
;1		rrca				; [4t]
;		jr c, 3F			; [t12 or t7]
;		ld (hl), %00010000		; [t10]
;		jr 4F				; [t12]
;3		ld (hl), %00111111		; [t10]
;4		dec l				; [4t]
;		djnz 1B				; [13t]
;		pop hl
;		ret



_print:
;		push bc
		push de
;		ld de,stringx		; location of string
;		ld bc,eostrx-stringx	; length of string
;		call 8252		; print the string
		ld de,(Mouse_Coords)
		;ld c,d
		;call 6683
		ld a,e			; x-coord (0-255)
		srl a
		srl a
		srl a
		cp 30			; divide by 8 to give screen column (max 29)
		jr nc,1F
		ld (RET_XY),a
		sla a
		sla a
		sla a
		sub e
		neg
		ld (RET_XY+4), a
;		ld b,0
;		ld c,a
;		call 6683
;		ld de,stringy		; location of string
;		ld bc,eostry-stringy	; length of string
;		call 8252		; print the string
		;ret
1		ld de,(Mouse_Coords)	; y-coord
		ld a,d
		srl a
		srl a
		srl a
		cp 23			; divide by 8 to give screen row (max 21)
		jr nc,2F
		ld (RET_XY+1),a
		sla a
		sla a
		sla a
		sub e
		neg
		ld (RET_XY+5), a
;		ld b,0
;		ld c,a
;		call 6683
		;ld bc,(PJ_XY)		; number to print (up to 9999)
		;ld bc,(Mouse_Coords)
		;call 11563 ; stack number in bc.
		;call 11747 ; display top of calc. stack.
2		pop de
;		pop bc
		ret

stringx defb 22,2,11,'x:'
eostrx  equ $
stringy defb 13,22,21,11,'y:'
eostry  equ $

	include _locate.asm
	include _sprite-engine.asm

	include _mouse.asm
	include _hellaplot.asm

	include gfx/hearticon.asm
	include gfx/hand.asm
	include gfx/pj.asm
	include gfx/drutt.asm
	include gfx/drutt_v.asm
	include gfx/droog.asm
	include gfx/drtest.asm
LB3	incbin gfx/lovebugs3.scr
BRICK	incbin gfx/brick.scr


Code_Length:	EQU $-Code_Start+1

	DISPLAY "Code_Length: ",/D,Code_Length
	;SAVETAP "plot.tap", Code_Start
	SAVESNA "main.sna", Code_Start
	IF 3 == __PASS__
		DEVICE NONE : ORG 0
		OUTPUT "file.sna",r
		FPOS $00 : DB $FE       ; I = $FE (IM2 table starts at $FE00)
		FPOS $13 : DB (1<<2)    ; (bit 2) IFF2 = 1 -> "enable interrupts"
		FPOS $19 : DB 2         ; IM2 mode
		FPOS $1A : DB 2         ; border = 2
		OUTEND
	ENDIF