	DEVICE ZXSPECTRUM128  ; sjasmplus directive for SAVESNA (at end)
	SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION

		ORG 0x4000				; load screen as part of the SNApshot only,
		incbin "gfx/lovebugs3.scr"		; it won't be included in the BIN files.
Stack_Top:              EQU 0x0000                              ; Stack at top of RAM
IM2_Table:              EQU 0xFE00                              ; 256 byte page (+ 1 byte) for IM2
IM2_JP:                 EQU 0xFDFD
Code_Start:		EQU 0x5E00				; 23734
			DISPLAY "Code_Start: ",/D,Code_Start	; 
			ORG Code_Start
ENTRY			EQU $
			DISPLAY "Entry point: ",/A,ENTRY	; 23775
			EI
			EXX
			LD HL, FRAMES
			EXX

Initialise_Interrupt:   DI
                        LD DE, IM2_Table                        ; The IM2 vector table (on page boundary)
                        LD HL, IM2_JP                           ; Pointer for 3-byte interrupt handler
                        LD A, D                                 ; Interrupt table page high address
                        LD I, A                                 ; Set the interrupt register to that page
                        LD A, L                                 ; Fill page with values
1:                      LD (DE), A 				; Loop
                        INC E					; Loop
                        JR NZ, 1B:				; Loop
                        INC D                                   ; In case data bus bit 0 is not 0, we
                        LD (DE), A                              ; put an extra byte in here
			LD DE, Interrupt			; DE= interrupt routine address
                        LD (HL), 0xC3                           ; Write out the interrupt handler, a JP instruction
                        INC L					; …
                        LD (HL), E                  		; Low byte of the interrupt routine address
                        INC L					; …
                        LD (HL), D				; High byte of the interrup routine address
                        IM 2                                    ; Set the interrupt mode
			EI					; Re-enable interrupts

			jp wait					; jump past the data to the actual code
FONT		incbin "_prefect.fzx"				; font data
BrickZX0:	incbin "gfx/brick.zx0"				; ZX0 compressed pattern data


INTROTEXT:
		defb 0x16, 0x08, 0x08, " OH NO! ", 13
		defb " ======= ", 13, 13
		defb "It's March 2022 and the Spectrum Computing ", 13
		defb "forums have lost the like button! ",13,13
		defb "Bugs have appeared and are siphoning off all", 13
		defb "the positive energy. Oh no! Oh the memes!!"
		defb 13, 13
		defb "Help PJ to clear out the nasty bugs and restore", 13
		defb "the like button (and memes) to the forum!  <3 <3 <3"
		defb 13, 13
		defb "_________________________________________", 13, 13
		defb "(i) A Kempston Mouse is required", 13
		defb "(!) Contains flashing images    ", 13, 13
		defb "_________________________________________", 13, 13
		defb "  PRESS THE ANY KEY TO CONTINUE  ";, 13, 13
		defb 0xFF

NOMOUSE:
		defb 0x16, 0x78, 0xA0, " [NOT DETECTED!] ", 0xFF

BOSSTEXT:
		defb 0x16, 0x48, 0x40, "B O S S   F I G H T ! !"
		defb 0x16, 0x68, 0x28, "press any key when you are ready", 0xFF

ENDTEXT:
		defb 0x16, 0x48, 0x28, "C O N G R A T U L A T I O N S ! !"
		defb 0x16, 0x68, 0x28, "The bugs have been defeated!"
		defb 0x16, 0x78, 0x28, "Let the memes flow once again!"
		defb 0x16, 0x98, 0x28, "[any key to continue]", 0xFF

CREDITS:
		defb 0x16, 0x00, 0x00, "THANK YOU FOR PLAYING!", 13, 13
		defb "Made for CSSCGC 2022 ",13,13
		defb "Special greets go out to Spectrum Computing",13
		defb "and the wonderful forum community. xxx", 13, 13
		defb "The development of this software made use of:", 13, 13
		defb "- How to Write ZX Spectrum Games by J. Cauldwell", 13
		defb "- KMouse driver by B. Versteeg and D. Belfield", 13
		defb "- Headerless loading routine by A. Grussu", 13
		defb "- FZX ",0x7F,"2013 E. Saukas and A. Owen", 13
		defb "- ZX0 by E. Saukas and Urusergi", 13
		defb "- and some memes off of that Internet", 13,13
		defb "with SjASMPlus, DeZog, ZEsarUX, ZXBlockeditor",13
		defb "ZXPaintbrush, SevenuP and Visual Studio Code.",13
		defb "_________________________________________", 13, 13
		defb "Wishing the ZX Spectrum a very happy 40th Birthday",13,13
		defb "        Visit spectrumcomputing.co.uk today!"
		defb 0xFF

wait:		call	0x028E
		ld	a,e
		cp	0xFF
		jr	z, wait

intro: 		call sfxhi
		call	0x0d6b	; CLS routine again just to be really sure

		ld	hl, INTROTEXT
		di
		call	TXTLOOP
		ei

		call	MouseDetect

		ld ix, TITLE_TABLE
		ld b,  ST_LENGTH	; TT is the same width as ST
1:		
		call healeach
		call show
		halt
		halt
		call show2

		call 0x028E
		ld a,e
		cp 0xFF
		jr z, 1B

		call triggerbgfx
		call sfxlo
		halt
		call showbgfx
		call sfxlo
		halt
		call showbgfx
		call sfxlo
		halt
		call showbgfx
		call sfxmid
		halt
		call showbgfx
		call sfxmid
		halt
		call showbgfx
		call sfxmid
		halt
		call showbgfx
		call sfxhi
		halt
		call showbgfx
		call sfxhi
		halt
		call showbgfx
		call sfxhi
		halt
		call showbgfx
		call sfxhi
		halt
	
		call TOTALISER
		call ShowBG
		call SetBuffer

Part1_End:	equ $
Part1_Length:	equ Part1_End - Code_Start

	SAVEBIN "_part1.bin",Code_Start,Part1_Length

	ALIGN 16384					; lets get out of contended memory now
Part2_Start:	equ $					; should be 0x8000 
LOOP:							; Main game loop start
			call Clicker


			ld a, (SCORE)
			cp 192
			call nc, ChangeLevel

			ld a, (ACTIVE)
			cp 0
			jr nz,1F

			call HealButton
			call HealGoal

1:			ld ix, SPRITE_TABLE
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

			ld a, (ACTIVE)
			cp 0
			jr nz,1F

			call ShowButton
			call ShowGoal

1:			ld ix, SPRITE_TABLE
			ld b, ST_LENGTH


bugloop2		;bugloop2
			push ix

			call show2
			pop ix
			ld de, ST_WIDTH
			add ix, de
			djnz bugloop2

			call BgFxService


			ld a, (ACTIVE)
			cp 0
			jr nz,LOOP

			ld a,(SMASHING)
			cp 0
			jp z,SmashIt

			call WaneScore


			jr LOOP

BOSSFIGHT		

			call	0x0d6b	; CLS routine
			ld	hl, BOSSTEXT
			di
			call	TXTLOOP
			ei

1:			call	0x028E
			ld	a,e
			cp	0xFF
			jr	z, 1B


			call	0x0d6b	; CLS routine

			xor a
			ld (ACTIVE), a
			ld (SCORE), a

			ld hl,BrickZX0
			ld de, DBLBFFR
			call dzx0_standard

			call TOTALISER

			ld b, 8
			ld c, 12
			call Get_Char_Address
			ld ix, cow
			call MSKD_BB_64x64

			call SetBuffer
			call ShowBG

			ld ix, cursor
			
BossLoop		call Clicker

			ld a, (SCORE)
			cp 192
			jp nc, ending

			call HealGoal
			call HealButton

			;; update the cursor ;;
			call healeach
			call Mouse		; get Mouse_Buttons and Mouse_Coords
			call _print		; translate coords into screen row/column
			call show
			call show2
			;;;;;;;;;;;;;;;;;;;;;;;

			call BgFxService

			call WaneScore
			call ShowGoal
			call ShowButton

			jr BossLoop

TXTLOOP:  
			ld a, (hl)		; for each character of this string...
			cp 0xFF
			ret z			; check string terminator
			push hl			; preserve HL
			call FZX_START		; to print character (the label is defined in FZXdriver.asm)
			pop hl			; recover HL
			inc hl
			jr TXTLOOP

SmashIt:
			ld b, 8
			ld c, 12
			call Get_Char_Address
			ld ix, smash2
			call MSKD_BB_64x64
			call ShowBG
			ld a, 1
			ld (SMASHING), a
			jp LOOP

ending:

			call	0x0d6b	; CLS routine
			ld	hl, ENDTEXT
			di
			call	TXTLOOP
			ei

1:			call	0x028E
			ld	a,e
			cp	0xFF
			jr	z, 1B

			call	0x0d6b			; clear screen
			ld	hl, CREDITS
			di
			call	TXTLOOP

			halt				; freeze game

;			ei

;			call ResetGame

;			jp wait


sfxlo:
			ld hl,1000 ; load pitch into hl
			ld de,2 ; load duration into de;
			call 949 ; call the process to play the sound
			ret
sfxmid:
			push af
			push bc
			push de
			push hl
			push ix
			ld hl,497 ; load pitch into hl
			ld de,2 ; load duration into de;
			call 949 ; call the process to play the sound
			pop ix
			pop hl
			pop de
			pop bc
			pop af
			ret
sfxhi:
			ld hl,400 ; load pitch into hl
			ld de,4 ; load duration into de;
			call 949 ; call the process to play the sound
			ret

BgFxService:
			ld a, (BGFX_TABLE)
			and a
			ret z
			push ix
			ld ix, BGFX_TABLE
			call showbgfx
			pop ix
			ret

MouseDetect:
			call DETECT_M
			xor a
			cp c
			jr z, SetMouseNop
			ret

SetMouseNop
			ld a, 0xC9		; code for RET
			ld (MNOP),a		; effectively disable mouse
		       	ld hl, NOMOUSE
			call TXTLOOP
			ret


WaneScore:		
			ld hl, PreviousTime	; previous time.
			ld a, (FRAMES)		; curent time.
			sub (hl)		; difference.
			cp 16			; have 100 frames elapsed yet?
			ret c			; return if not.
			ld a, (FRAMES)		; if so, record the new
			ld (hl), a		; time setting.
			ld a, (SCORE)		; get the score
			and a			; check it's not already zero
			ret z			;
			sub 2			; and deduct two
			ld (SCORE),a
			call TOTALISER		; update the totaliser
			ret
PreviousTime:		defb 0x00
HealGoal:
			ld b, 0
			ld c, 28
			call Get_Char_Address
			call HEAL24x16
			ret

ShowGoal:
			push ix
			ld b, 0
			ld c, 28
			call Get_Char_Address

			ld a, (FRAMES)
			RLCA
			RLCA
			RLCA
			RLCA
			RLCA
			jr c,ShowGoalOdd
ShowGoalEven:		
			ld ix, goal
			push hl
			call MSKD24x16
			pop hl
			call UND24x16
			pop ix
			ret

ShowGoalOdd:
			ld ix, goal + 96
			push hl
			call MSKD24x16
			pop hl
			call UND24x16
			pop ix
			ret
HealButton:
			ld b, 22
			ld c, 28
			call Get_Char_Address
			call HEAL24x16
			ret
ShowButton:
			push ix
			ld b, 22
			ld c, 28
			call Get_Char_Address
			push hl
			call HEAL24x16
			pop hl

			ld a, (mouseWas)
			and a
			jr nz,ShowButtonOdd
ShowButtonEven:		
			ld ix, like
			push hl
			call MSKD24x16
			pop hl
			call UND24x16
			pop ix
			ret

ShowButtonOdd:
			ld ix, like + 96
			push hl
			call MSKD24x16
			pop hl
			call UND24x16
			pop ix
			ret

Collision:
; Check (l, h) for collision with (c, b), strict enforcement.
			ld a, (Mouse_Buttons)
			cpl
			and 3
			cp 2
			scf
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
			
;			call Timeoutservice

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


mouseWas		defb 0x00
Clicker:		
			ld a, (Mouse_Buttons)		; get mouse button state
			cpl				; invert all the bits
			and 3				; we only need 3 bottom bits
			cp 2				; does it equal 2?
			jr nz, mouseup			; if not, primary button is not being pressed. jump

			ld a,(mouseWas)			; load previous state was mousedown?
			cp 1				; (i.e. is it being HELD down?)
			ret z				; exit if so.

			ld a, (ACTIVE)			; are there any remaining
			and a				; active enemies on screen?
			ret nz				; exit if so.


			call sfxmid			; play sound
			call triggerbgfx		; start the effect
			ld a, (SCORE)			; increase the score
			add 2
			ld (SCORE), a
			call SCOREBOARD			; update the scoreboard

			ld a, 1				; record button state
			ld (mouseWas), a		; as down


			ret

mouseup:
			ld a, 0				; record button state
			ld (mouseWas), a		; as NOT down
			ret


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
			ld c, %00111000		; white paper black ink
			jp nc, nextbit
;			ld a, (FRAMES)
;			and %00111000
			ld c, (ix+5)		; white paper red ink
;			and c
;			ld c, a

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

movepj:
			ld a, (ix+2)		; get PJ's x-coord
			cp 0
			jr nz, move		; move if not at home position
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
			push hl
			push ix

			ld a, (ix+2)
			;sub 16
			ld ix, peejay
			ld (ix+7),a		; set PJ's max to enemy x-coord
			ld a, (ix+2)
			inc a			; move PJ right one block (which will kickoff
			ld (ix+2), a		; his movement untill he comes back to 0,0)

			push de
			call sfxlo
			pop de
			
			pop ix
			pop hl
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

			ld a, (LEVEL)
			cp 3
			jp nc, BOSSFIGHT
			inc a
			ld (LEVEL), a
			ld a, 8
			ld (SCORE), a
			ld a, 6
			ld (ACTIVE), a
			xor a
			ld (SMASHING), a

			ld bc, 17*7
			ld hl, LEVEL2
			ld de, SPRITE_TABLE
			ldir

			call INITBB		; load a new meme from tape

			call TOTALISER		; redraw the score-o-matic
			call SetBuffer		; copy new meme into first buffer
			call ShowBG		; copy first buffer to live screen
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
			cp 1
			call z, movepj
			cp 253
			call z, drop		
d1:			; entry point for non-moving sprites
			push ix				; IX will be modified later so save it on stack
							; before we start to manipulate it...	
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
			jp z, d_get_x


			ld a,(ix+1)		; alien movement direction.
			rra			; rotate low bit into carry.
			jr c,d_get_y		; no carry ∴ 0 or 2, must be vertical.

			; get x offest
d_get_x			ld a, c
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
			pop ix				; retore that value of IX we talked about
			ret				; actual RET for this routine


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
			call sfxmid


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
			;ld (ix), 0xFE		; STOP
			ld hl, pj_celebrate
			ld (ix+9), l
			ld (ix+10), h
			pop ix
			ret

;ResetGame:
;			ld a, 8
;			ld (SCORE), a
;			ld a, 6
;			ld (ACTIVE), a
;			ld a, 1
;			ld (LEVEL), a
;			ret

;NOTHING:		ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ATTR		equ 0x5800
FRAMES		equ 0x5C78

TIMEOUT		dw 0x0000

	;	   X/Y    Old X/Y   Offset Hasmoved (low byte)
		;    0  1    2   3   4  5   6   7
RET_XY		dw 0x0000, 0x0000, 0x0000, 0x0000


		
BGFX_TABLE	;	STATUS	ANIMl	ANIMh	FRAME	FRAMES	COLOUR
		;		+1	+2	+3	+4      +5
		db 	0x00,	0x00,	0x00,	0x00,	0x09,    %00111010
FX_TABLE	defw bgfx	;there were gonna be more!
SCORE:		defb 0x08

LEVEL:		defb 0x01
ACTIVE: 	defb 0x05
SMASHING:	defb 0x00
ST_WIDTH	EQU 17
ST_LENGTH	EQU 8

		;ix+1 = direction 0=left, 1=up, 2=right, 3=down
TITLE_TABLE:	
		defb 0x00,0x03, 30*8,21*8, 0,1, 162,169, 96
		defw drutt_v2, MSKD16x24, UND16x24, BLANK16x24

SPRITE_TABLE:	
		defb 0x00,0x03, 16,8, 0,0, 8,16, 96
		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24

		defb 0x00,0x01, 19*8,72, 0,0, 72,191-16, 96
		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24

		defb 0x00,0x00, 108,16, 0,0, 88,128, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16

		defb 0x00,0x00, 20*8,8*8, 0,0, 0,255-24, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16

		defb 0x00,0x03, 224,24, 0,0, 24,32, 96
		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24

		defb 0xFF,0x02, 1*8,13*8, 0,0, 0,191-16, 128
		defw droog, MSKD32x16, UND32x16, HEAL32x16
peejay:		defb 0x01,0x02, 0,20*8, 0,0, 0,7, 192
		defw pj_v, MSKD24x32, UND24x32, HEAL24x32

cursor		defb 0xFE,0xFF, 0,0, 0,0, 0,255-16, 32
		defw pointer, MSKD16x8, UND16x8, HEAL16x8

LEVEL2:
		defb 0x00,0x03, 32,3, 0,0, 0,8*8, 96
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

		defb 0x01,0x02, 0,20*8, 0,0, 0,255-24, 192
		defw pj_v, MSKD24x32, UND24x32, HEAL24x32

;LEVEL3:
;		defb 0x00,0x03, 32,3, 0,0, 0,8*8, 96
;		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24
;
;		defb 0x00,0x01, 25*8,8, 0,0, 0,191-16, 96
;		defw drutt_v2, MSKD16x24, UND16x24, HEAL16x24
;
;		defb 0x00,0x00, 25*8,6*8, 0,0, 0,255-24, 128
;		defw droog, MSKD32x16, UND32x16, HEAL32x16
;
;		defb 0x00,0x00, 20*8,8*8, 0,0, 0,255-24, 128
;		defw droog, MSKD32x16, UND32x16, HEAL32x16
;
;		defb 0x00,0x00, 12*8,17*8, 0,0, 0,255-24, 128
;		defw droog, MSKD32x16, UND32x16, HEAL32x16
;
;		defb 0x00,0x02, 1*8,13*8, 0,0, 0,191-16, 128
;		defw droog, MSKD32x16, UND32x16, HEAL32x16
;
;		defb 0x01,0x02, 0,20*8, 0,0, 0,255-24, 192
;		defw pj_v, MSKD24x32, UND24x32, HEAL24x32

		include _dzx0.asm
		include _locate.asm
		include _sprite-engine.asm
		include _mouse.asm
		include _fzx.asm

		include gfx/pointer_v.asm
		include gfx/pj_v.asm
		include gfx/pj_celebrate.asm
		include gfx/drutt_v2.asm
		include gfx/droog.asm
		include gfx/heart_v.asm
		include gfx/bgfx.asm
		include gfx/metretop.asm
		include gfx/metrebot.asm
		include gfx/segment.asm
		include gfx/fill.asm
		include gfx/smash2.asm
		include gfx/goal.asm
		include gfx/like.asm
		include gfx/cow_m.asm

		defb "This is the end"
		
Code_End:	EQU $
Part2_Length	EQU $ - 0x8000
Code_Length:	EQU Code_End-Code_Start

	SAVEBIN "_part2.bin",Part2_Start,Part2_Length

		ALIGN 16384
BUFFER:		
	BLOCK 	32*192,0xCC	; 6144 bytes to store screen display buffer
DBLBFFR:	incbin "gfx/meme1"	;BLOCK 	32*192,0x00	; 6144 bytes to store screen display buffer
LOADER:		include "loader.asm"

	;DISPLAY "LOOP: ",/D,LOOP
	DISPLAY "Part1 Start: ",/A,Code_Start
	DISPLAY "Part1_Length: ",/A,Part1_Length
	DISPLAY "Part1_End: ",/A,Part1_End
	DISPLAY "Part2 Start: ",/A,Part2_Start
	DISPLAY "Part2_Length: ",/A,Part2_Length
	DISPLAY "Entry: ",/A,ENTRY
	DISPLAY "LOOP: ",/A,LOOP
	DISPLAY "Space before buffer: ",/D,0xC000-Code_End
	DISPLAY "BUFFER: ",/A,BUFFER
	DISPLAY "DBLBFFR: ",/A,DBLBFFR
	DISPLAY "LOADER: ",/A,LOADER
	DISPLAY "INITBB: ",/A,INITBB
	DISPLAY "Free Stack space: ",/D,0xFFFF-$
	DISPLAY "Code_Start: ",/A,Code_Start
	DISPLAY "Code_Length: ",/A,Code_Length

	SAVESNA "main.sna", Code_Start
	SAVETAP "main.tap", Code_Start
