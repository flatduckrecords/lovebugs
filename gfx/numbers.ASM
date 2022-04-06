; ASM source file created by SevenuP v1.20
; SevenuP (C) Copyright 2002-2006 by Jaime Tejedor Gomez, aka Metalbrain

;GRAPHIC DATA:
;Pixel Size:      ( 24,  16)
;Char Size:       (  3,   2)
;Frames:             4
;Sort Priorities: Mask, X char, Char line, Y char, Frame number
;Data Outputted:  Gfx
;Interleave:      Line
;Mask:            Yes

numbers:
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$C7
	DEFB	$00,$FF,$00,$FF,$10,$C7,$00,$FF
	DEFB	$00,$FF,$10,$C7,$00,$FF,$00,$FF
	DEFB	$10,$C7,$00,$FF,$00,$FF,$10,$C7
	DEFB	$00,$FF,$00,$FF,$10,$C7,$00,$FF
	DEFB	$00,$FF,$10,$C7,$00,$FF,$00,$FF
	DEFB	$10,$C7,$00,$FF,$00,$FF,$10,$C7
	DEFB	$00,$FF,$00,$FF,$10,$C7,$00,$FF
	DEFB	$00,$FF,$10,$C7,$00,$FF,$00,$FF
	DEFB	$10,$C7,$00,$FF,$00,$FF,$10,$C7
	DEFB	$00,$FF,$00,$FF,$00,$C7,$00,$FF
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$F0
	DEFB	$00,$1F,$00,$FF,$07,$F0,$C0,$1F
	DEFB	$00,$FF,$00,$F0,$40,$1F,$00,$FF
	DEFB	$00,$FF,$40,$1F,$00,$FF,$00,$FF
	DEFB	$40,$1F,$00,$FF,$00,$F0,$40,$1F
	DEFB	$00,$FF,$07,$F0,$C0,$1F,$00,$FF
	DEFB	$04,$F0,$00,$1F,$00,$FF,$04,$F1
	DEFB	$00,$FF,$00,$FF,$04,$F1,$00,$FF
	DEFB	$00,$FF,$04,$F1,$00,$FF,$00,$FF
	DEFB	$04,$F0,$00,$1F,$00,$FF,$07,$F0
	DEFB	$C0,$1F,$00,$FF,$00,$F0,$00,$1F
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FE
	DEFB	$00,$03,$00,$FF,$00,$FE,$F8,$03
	DEFB	$00,$FF,$00,$FE,$08,$03,$00,$FF
	DEFB	$00,$FF,$08,$E3,$00,$FF,$00,$FF
	DEFB	$08,$E3,$00,$FF,$00,$FF,$08,$83
	DEFB	$00,$FF,$00,$FF,$38,$83,$00,$FF
	DEFB	$00,$FF,$08,$83,$00,$FF,$00,$FF
	DEFB	$08,$E3,$00,$FF,$00,$FF,$08,$E3
	DEFB	$00,$FF,$00,$FF,$08,$E3,$00,$FF
	DEFB	$00,$FE,$08,$03,$00,$FF,$00,$FE
	DEFB	$F8,$03,$00,$FF,$00,$FE,$00,$03
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$F8,$00,$FF,$00,$FF,$02,$F8
	DEFB	$00,$FF,$00,$FF,$02,$F8,$00,$FF
	DEFB	$00,$FF,$02,$F8,$00,$FF,$00,$FF
	DEFB	$02,$F8,$00,$1F,$00,$FF,$02,$F8
	DEFB	$40,$1F,$00,$FF,$02,$F8,$40,$03
	DEFB	$00,$FF,$03,$F8,$F8,$03,$00,$FF
	DEFB	$00,$F8,$40,$03,$00,$FF,$00,$FF
	DEFB	$40,$1F,$00,$FF,$00,$FF,$40,$1F
	DEFB	$00,$FF,$00,$FF,$40,$1F,$00,$FF
	DEFB	$00,$FF,$40,$1F,$00,$FF,$00,$FF
	DEFB	$00,$1F,$00,$FF,$00,$FF,$00,$FF