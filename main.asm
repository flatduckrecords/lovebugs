			DEVICE ZXSPECTRUM48  ; sjasmplus directive for SAVESNA (at end)
			SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION

Stack_Top:		EQU 0xFFF0

Code_Start:		EQU $5CCB-4
			ORG Code_Start

			DW $5CCB, $5CCB

			EI
			EXX
			ld hl, FRAMES
			EXX

			ld a,7
			out (254),a

			DI
			LD SP,Stack_Top
			LD HL,Interrupt
			LD IX,Stack_Top
			LD (IX+04h),0xC3           	; Opcode for JP
			LD (IX+05h),L
			LD (IX+06h),H
			LD (IX+0Fh),0x18           	; Opcode for JR; this will do JR to FFF4h
			LD A,0x39
			LD I,A
			IM 2
			EI

		call ShowTitle

wait:		call 0x028E
		ld a,e
		cp 0xFF
		jr z, wait

		di

		call 0x0d6b	; CLS routine

        	ld hl, STRING
		call TXTLOOP
        	ld hl, STRING2
		call TXTLOOP
        	ld hl, STRING3
		call TXTLOOP

		call Mouse		; initialise Mouse_Buttons and Mouse_Coords
		call _print


		ei

		ld ix, TITLE_TABLE
		ld b,  ST_LENGTH	; TT is the same width as ST

		jr wait2

TXTLOOP:   ld      a, (hl)         ; for each character of this string...
        cp      0xFF
	ret	z               ; check string terminator
        push    hl              ; preserve HL
        call    FZX_START       ; to print character (the label is defined in FZXdriver.asm)
        pop     hl              ; recover HL
        inc     hl
        jr      TXTLOOP
wait2:		
		nop
		push ix
		call healeach
		pop ix
		push ix
		call show
		pop ix
		halt
		halt
		push ix
		call show2
		pop ix

		call 0x028E
		ld a,e
		cp 0xFF
		jr z, wait2

		call triggerbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt
		call showbgfx
		halt

PRELOOP:	
		call TOTALISER
		call ShowBG
		call SetBuffer

;		di
;		halt
LOOP:			
			

			;di
			;call _hellaPlot
			;call Timer
			call Clicker

			ld ix, SPRITE_TABLE
			ld b, ST_LENGTH

healloop		; healloop
				push ix
				call healeach
				pop ix
				ld de, ST_WIDTH
				add ix, de
			djnz healloop

			; heal the reticule
			call Mouse		; get Mouse_Buttons and Mouse_Coords
			call _print		; translate coords into screen row/column

			ld ix, SPRITE_TABLE
			ld b, ST_LENGTH
bugloop			; bugloop
				push ix

				call show
				pop ix
				ld de, ST_WIDTH
				add ix, de
			djnz bugloop

;			call Reticule


			ld ix, SPRITE_TABLE
			ld b, ST_LENGTH

			;;;;
			;halt
			;;;;
bugloop2		;bugloop2
				push ix

				;ld a,b
				;out (254),a
				call show2
				pop ix
				ld de, ST_WIDTH
				add ix, de
			djnz bugloop2
;			call ReticuleB

			call BgFxService


			JR LOOP

; Check (l, h) for collision with (c, b), strict enforcement.

BgFxService:
			ld a, (BGFX_TABLE)
			and a
			ret z
			ld ix, BGFX_TABLE
			call showbgfx
			ret


Collision:

			ld a, (Mouse_Buttons)
			cpl
			and 3
			cp 2
			ccf
			ret nz

			ld hl, (Mouse_Coords)
			ld b,(ix+3)		; The previous coords
			ld c,(ix+2)		; The previous coords

			ld a, l
			sub c
			add a, 15
			cp 31
			ret nc		; miss
			ld a, h		; y-coord
			sub b
			add a, 15
			cp 31		; carry flag set if there's a collision.
			ret


Interrupt:		
			DI                                      ; Disable interrupts 
;			PUSH AF                                 ; Save all the registers on the stack
;			PUSH BC                                 ; This is probably not necessary unless
;			PUSH DE                                 ; we're looking at returning cleanly
;			PUSH HL                                 ; back to BASIC at some point
			PUSH IX
			EXX
			EX AF,AF'
;			PUSH AF
;			PUSH BC
;			PUSH DE
;			PUSH HL
;			PUSH IY



			; update FRAMES. Similar to ROM but only 2 bytes
			ld hl, (FRAMES)
			inc hl	
			ld (FRAMES), hl
			
			call Timeoutservice

;			ld b, (0x5AFF)
;			ld a, b
;			cpl
;			and %00111111
;			and b
;;			ld a, %00101010
;			ld (0x5AFF), a

			


;			POP IY                                  ; Restore all the registers
;			POP HL
;			POP DE
;			POP BC
;			POP AF
			EXX
			EX AF,AF'
			POP IX
;			POP HL
;			POP DE
;			POP BC
;			POP AF
			EI                                      ; Enable interrupts
			RET                                     ; And return

Timeoutservice:
			ld hl, (TIMEOUT)
			ld a, h
			or l
			ret z
			dec hl
			ld (TIMEOUT), hl
			ret

Clicker:		
			ld a, (Mouse_Buttons)
			cpl
			and 3
			cp 2
			ret nz
			call z, triggerbgfx
			ld a, (SCORE)
			add 2
			ld (SCORE), a
			call SCOREBOARD
;			ld a, 0x01
;			ld (petesprite), a
;			ret z
;			ld a, 0xFE
;			ld (petesprite), a
;			ret

CheckForHit:		ret
healeach:
			ld a,(ix)		; bug table
			cp 255			; is bug dead?
			ret z			; if yes skip this one
			push bc

			ld b,(ix+3)		; The current y-coords
			ld c,(ix+2)		; The current x-coords

			ld (ix+5),b		; The previous y-coords
			ld (ix+4),c		; The previous x-coords

			call healing		; CALL a healing routine

			pop bc
			ret
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
1:			call prerender
			pop bc
			ret
show2:
			ld a,(ix)		; bug table
			cp 255			; is bug dead?
			ret z			; if yes skip this one
			push bc
			call postrender
			pop bc
			ret


triggerbgfx:
			ld a,(BGFX_TABLE)
			and a
			ret nz
			inc a
			ld (BGFX_TABLE), a
			ld hl, FX_TABLE		; 
			ld a, (hl)		; Effect low byte
			ld (BGFX_TABLE+1),a
			inc hl
			ld a, (hl)		; Effect high byte
			ld (BGFX_TABLE+2),a
			ret
showbgfx:
			ld de, (BGFX_TABLE+1)	; DE=SOURCE
			ld hl, ATTR		; HL=DEST (normally would be DE)
			ld b, 96		; 96x8=768

fxbyte:		push bc
			ld b, 8
			ld a, (de)		; from SOURCE
fxbit:			rlca
			ld c, %00111000
			jp nc, nextbit
			ld c, %00010111
nextbit:		ld (hl), c		; to DEST ( no such thing as `LD(DE), c` )
			inc hl			; next DEST
			djnz fxbit
			inc de			; next SOURCE
		pop bc
			djnz fxbyte

			ld bc, (BGFX_TABLE+3)
			ld a, c
			inc a
			cp b
			ld (BGFX_TABLE+3),a	; update frame counter
			ld (BGFX_TABLE+1), de	; save source pointer for next frame
			ret c			; return if not reached frame max
			ld hl, BGFX_TABLE
			xor a
			ld (hl), a		; reset status
			inc l
			ld (hl), a		; reset animation-l
			inc l
			ld (hl), a		; reset animation-h
			inc l
			ld (hl), a		; reset frame counter
			inc l

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
			cp (ix+6)		; reached mimimum yet?
			jr z,movax		; yes - change direction.
			jr c,movax		; oops, gone past it.
			ret
; Move alien right.
movar			ld a,(ix+3)		; get y coordinate.
			add a,1			; move down.
			ld (ix+3),a
			cp (ix+7)		; reached maximum yet?
			jr nc,movax		; yes - change direction
			ret

; Move alien vertically.
movav			rra			; test direction.
			jr c,movad		; direction 2 is down.

; Move alien up.
movau			ld a,(ix+2)		; get x coordinate.
			sub 1			; move left.
			ld (ix+2),a
			cp (ix+6)		; reached mimimum yet?
			jr z,movax		; yes - change direction.
			ret
; Move alien right (not down).
movad			ld a,(ix+2)		; get x coordinate.
			add a,1			; move right.
			ld (ix+2),a		; new coordinate.
			cp (ix+7)		; reached maximum yet?
			jr nc,movax		; yes - change direction.
			ret

drop:			
			ld a, (ix+3)		; get y-coord
			inc a			; move down
			inc a			; move down
			ld (ix+3),a
			cp 191-16		; reached floor?
			jr nc, stopal
			ret	

; Change alien direction.
movax			ld a,(ix+1)		; direction flag.
			xor 2			; switch direction, either
						; horizontally or vertically.
			ld (ix+1),a		; set new direction.
			ret

stopal:
			ld a, 255
			ld (ix), a
;			call ChangeLevel
;			call SetBuffer
;			call ShowBG
			ret

ShowTitle:
		ld hl, LB3
		ld de, 0x4000
		call dzx0_standard
		ret
ShowBG:
		ld hl, DBLBFFR
		ld de, 0x4000
		ld bc, 6144
		ldir
		ret

SetBuffer:
		ld hl, DBLBFFR
		ld de, 0xC000
		ld bc, 6144
		ldir
		ret

ChangeLevel:
;		ld hl,meme3
;		ld de, DBLBFFR
;		call dzx0_standard
		ret

_print:
			push de
			ld de,(Mouse_Coords)
			ld (cursor+2),de
			pop de
			ret

healing:
			ld a, c
			srl a
			srl a
			srl a
			ld c, a
			ld a, b
			srl a
			srl a
			srl a
			ld b, a
			call Get_Char_Address		; get screen address in HL
			ld de, Return	; stack the return address as if we 
			push de		; were doing call to a routine.
			ld e, (ix+15)	; 
			ld d, (ix+16)	; DE=UND16x16 (e.g.)
			push de		; stack the routine addr
			ret		; and trigger a jump to it
Return:			ret

repaint:
			ld a, c
			srl a
			srl a
			srl a
			ld c, a
			ld a, b
			srl a
			srl a
			srl a
			ld b, a
			call Get_Char_Address		; HL=screen address
			ld de, udr2			; stack the return address as if we 
			push de				; were doing call to a routine.
			ld e, (ix+13)			; 
			ld d, (ix+14)			; DE e.g. call UND16x16
			push de				; stack the sprite routine jump addr
			ret				; trigger the jump
udr2:			ret

prerender:	

			ld a, (ix)
			cp 0
			call z, move
			cp 253
			call z, drop		
d1:			; entry point for non-moving sprites	
			ld de, dr			; stack the return address as if we 
			push de				; were doing call to a routine.
			ld e, (ix+11)			; 
			ld d, (ix+12)			; DE e.g. call MSKD16x16
			push de				; stack the sprite routine jump addr
;			
			ld b,(ix+3)			; NEW position Y
			ld c,(ix+2)			; NEW position X

			ld a, c
			srl a
			srl a
			srl a
			ld c, a
			ld a, b
			srl a
			srl a
			srl a
			ld b, a
			push bc				; stack the columnised position

			ld de,0				; 

			ld a, (ix)		; check status again
			cp 254
			jp z, noffset


			ld a,(ix+1)		; alien movement direction.
			rra			; rotate low bit into carry.
			jr c,d_get_y		; no carry ∴ 0 or 2, must be vertical.

			; get x offest
			ld a, c
			sla a
			sla a
			sla a
			ld c,(ix+2)
			sub c
			neg
			and 7
			jr z,noffset			; if zero skip to the end
			ld b,a				; number of offsets
			ld hl, 0
			ld e, (ix+8)			; amount per offset
1:			add hl, de			; add up sprite pointer offet amount
			djnz 1B				; repeat until done
			ex de,hl			; DE=total offset
			jr noffset

d_get_y:		; get y offest
			ld a, b
			sla a
			sla a
			sla a
			ld b,(ix+3)
			sub b
			neg
			and 7
			ld de,0				; 
			jr z,noffset			; if zero skip to the end
			ld b,a				; number of offsets
			ld hl, 0
			ld e, (ix+8)			; amount per offset
1:			add hl, de			; add up sprite pointer offet amount
			djnz 1B				; repeat until done
			ex de,hl			; DE=total offset



noffset:		pop bc				; BC=row/column
			call Get_Char_Address		; HL=screen address
			ld c, (ix+9)			; 
			ld b, (ix+10)			; BC=sprite addr
			ld ixh, b			; 
			ld ixl, c			; IX=sprite addr
			add ix, de			; IX=sprite addr + animation frame

			ret				; use RET to simulate "call SPRITE_ROUTINE"
dr:							; return from jump
			ret				; actual RET for this routine


;_offsetTemp		defb 0x00
;GetCursorOffset:
;			ld a, c
;			sla a
;			sla a
;			sla a
;			ld c,(ix+2)
;			sub c
;			neg
;			and 7
;			jr z,2F				; if zero skip
;			ld b,a				; number of offsets
;			ld hl, 0
;			ld e, (ix+8)			; amount per offset
;1:			add hl, de			; add up sprite pointer offet amount
;			djnz 1B				; repeat until done
;			ex de,hl			; HL= x-offset
;			ld (_offsetTemp), hl
			

			; get y offest
;2:			ld a, b
;			sla a
;			sla a
;			sla a
;			ld b,(ix+3)
;			sub b
;			neg
;			and 7
;			jr z,3F				; if zero skip
;			ld b,a				; number of offsets
;			ld hl, 0
;			ld e, (ix+8)			; amount per offset
;			ccf
;			rl c
;			rl c
;			rl c
;1:			add hl, de			; add up sprite pointer offet amount
;			djnz 1B				; repeat until done
;			ex de,hl			; DE= y-offset

;3:			ld hl, (_offsetTemp)
;			add hl, de
;			ex de, hl			; DE= total offset			
;
;			jr noffset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
postrender:	
			ld b,(ix+5)			; The previous y-coords
			ld c,(ix+4)			; The previous x-coords
			call repaint
			
			ld b,(ix+3)			; The current y-coord
			ld c,(ix+2)			; The current x-coord
			call repaint


			ld a, (ix)
			and a
			call z, Collision
			ret nc				; actual RETurn from this routine

			call triggerbgfx

			ld a, (SCORE)
			add 16
			ld (SCORE),a

			call SCOREBOARD

			ld a, (ACTIVE)
			dec a
			ld (ACTIVE), a

			call z, End_of_Level

			ld b,(ix+3)		; The current y-coords
			ld c,(ix+2)		; The current x-coords
			push bc
			call healing
			pop bc
			call repaint

			ld hl, heart_v
			ld (ix),   0xFD			; drop
			ld (ix+1), 0x01			; Direction=down
			ld (ix+7), 0xAF			; Y-coord max (192-height)
			ld (ix+8), 0x60			; change animation offset
			ld (ix+9), l			; change sprite (l)
			ld (ix+10),h			; change sprite (h)
			ld hl, MSKD16x24
			ld (ix+11), l			; change sprite (l)
			ld (ix+12),h			; change sprite (h)
			ld hl, UND16x24
			ld (ix+13), l			; change sprite (l)
			ld (ix+14),h			; change sprite (h)
			ld hl, HEAL16x24
			ld (ix+15), l			; change sprite (l)
			ld (ix+16),h			; change sprite (h)
			ret

End_of_Level:		
			push ix
			ld ix, peejay
			ld (ix), 0xFE		; STOP
			pop ix
			ret

NOTHING:		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ATTR		equ 0x5800
FRAMES		equ 0x5C78

TIMEOUT		dw 0x0000

	;	   X/Y    Old X/Y   Offset Hasmoved (low byte)
		;    0  1    2   3   4  5   6   7
RET_XY		dw 0x0000, 0x0000, 0x0000, 0x0000


		
BGFX_TABLE	;	STATUS	ANIMl	ANIMh	FRAME	FRAMES
		;		+1	+2	+3	+4
		db 	0x00,	0x00,	0x00,	0x00,	0x09
FX_TABLE	defw bgfx;, specattr, specattr,heartattr

SCORE:		defb 0x00

ACTIVE: 	defb 0x06
ST_WIDTH	EQU 17
ST_LENGTH	EQU 8

		;ix+1 = direction 0=left, 1=up, 2=right, 3=down

TITLE_TABLE:	

		defb 0x00,0x03, 30*8,21*8, 0,1, 162,169, 96
		;defb 0x00,0x03, 255-8,191-16, 255-8,191-16, 0,8, 96
		defw drutt_v2, MSKD16x24, UND16x24, BLANK16x24

SPRITE_TABLE:	

		defb 0xFE,0x03, 32,3, 0,0, 0,8*8, 96
		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24

		defb 0x00,0x01, 25*8,8, 0,0, 0,191-16, 96
		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24

		defb 0x00,0x00, 25*8,6*8, 0,0, 0,255-24, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16

		defb 0x00,0x00, 20*8,8*8, 0,0, 0,255-24, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16

		defb 0x00,0x00, 12*8,17*8, 0,0, 0,255-24, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16

		defb 0x00,0x02, 1*8,13*8, 0,0, 0,191-16, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16
		
peejay:		defb 0xFE,0x02, 0,20*8, 0,0, 0,255-24, 192
		defw pj_v, MSKD24x32, UND24x32, HEAL24x32

cursor		defb 0xFE,0xFF, 0,0, 0,0, 0,255-16, 64
		defw pointer, MSKD16x8, UND16x8, HEAL16x8

;		defb 0xFF,0,0,0,0,0,0,0
;		defw 0x0000, 0x0000, 0x0000, 0x0000



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



;stringx defb 22,2,11,'x:'
;eostrx  equ $
;stringy defb 13,22,21,11,'y:'
;eostry  equ $

		include _dzx0.asm
		include _locate.asm
		include _sprite-engine.asm
		include _mouse.asm
		include _fzx.asm
		;include _double-buffer.asm
		;include _hellaplot.asm

		include gfx/pointer_v.asm
		include gfx/pj_v.asm
		include gfx/drutt_v2.asm
		include gfx/droog.asm
		include gfx/heart_v.asm
		include gfx/bgfx.asm
		include gfx/metretop.asm
		include gfx/metrebot.asm
		include gfx/segment.asm
		include gfx/fill.asm


		;include gfx/drtest.asm
		;include gfx/hearticon.asm
		;include gfx/hand.asm
		;include gfx/pj.asm
		;include gfx/drutt.asm

;heartattr:	incbin gfx/heart.attr.bin
;specattr:	incbin gfx/spec.bin
;trumattr:	incbin gfx/trum.bin

LB3	incbin gfx/lovebugs3.zx0

;memeA:		incbin gfx/meme1.zx0
meme2:		incbin gfx/meme2.zx0
;meme3:		incbin gfx/meme3.zx0



STRING: defb 0x16, 0x08, 0x08, " OH NO! ", 13
	defb " ======= ", 13, 13
	defb "It's March 2022 and Spectrum Computing has", 13, "lost its Love! ",13,13
	defb "Bugs have appeared and are siphoning off all of", 13
	defb "the positive energy. Oh no! Oh the memes!!", 13
	defb 0xFF
STRING2:defb 13, 13
	defb "Help PJ to clear out the nasty bugs and restore", 13
	defb "the like button (and memes) to the forum!  <3 <3 <3"
	defb 0xFF ;0x16, 0x64, 0x00, 
STRING3:defb 13, 13
	defb "_________________________________________", 13, 13
	defb "(i) A Kempston Mouse is required", 13
	defb "(!) Contains flashing images    ", 13, 13
	defb "_________________________________________", 13, 13
	defb "  PRESS THE ANY KEY TO CONTINUE  ";, 13, 13
	defb 0xFF ;0x16, 0x64, 0x00, 

		org 0xC000
BUFFER:		BLOCK 	32*192,0x00	; 6144 bytes to store screen display buffer
DBLBFFR		incbin gfx/brick.scr
		;BLOCK 	32*192,0xCC	; 6144 bytes to store screen display buffer
		;BLOCK 	32,0xFF		; 32 bytes to store 16x16 display buffer

		;org 0xE000

FONT		incbin _prefect.fzx

Code_Length:	EQU $-Code_Start+1

	DISPLAY "Code_Length: ",/D,Code_Length
	SAVETAP "main.tap", Code_Start
	SAVESNA "main.sna", Code_Start
;	IF 3 == __PASS__
;		DEVICE NONE : ORG 0
;		OUTPUT "main.sna",r
;		FPOS $00 : DB $FE       ; I = $FE (IM2 table starts at $FE00)
;		FPOS $13 : DB (1<<2)    ; (bit 2) IFF2 = 1 -> "enable interrupts"
;		FPOS $19 : DB 2         ; IM2 mode
;		FPOS $1A : DB 2         ; border = 2
;		OUTEND
;	ENDIF