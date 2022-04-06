; ASM source file created by SevenuP v1.20
; SevenuP (C) Copyright 2002-2006 by Jaime Tejedor Gomez, aka Metalbrain

;GRAPHIC DATA:
;Pixel Size:      ( 16,  16)
;Char Size:       (  2,   2)
;Sort Priorities: X char, Char line, Y char, Mask
;Data Outputted:  Gfx
;Interleave:      Sprite
;Mask:            Yes, before graphic

reticule:
	DEFB	$61,$0E,$06,$70,$C1,$3E,$03,$7C
	DEFB	$81,$7E,$01,$7E,$01,$7E,$80,$7E
	DEFB	$00,$FF,$00,$FF,$04,$F9,$20,$9F
	DEFB	$02,$F9,$40,$9F,$10,$0F,$0F,$F0
	DEFB	$F0,$0F,$08,$F0,$02,$F9,$40,$9F
	DEFB	$04,$F9,$20,$9F,$00,$FF,$00,$FF
	DEFB	$01,$7E,$80,$7E,$80,$7E,$81,$7E
	DEFB	$C0,$3E,$83,$7C,$60,$0E,$86,$70