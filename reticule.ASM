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
	DEFB	$0E,$70,$3E,$7C,$7E,$7E,$7E,$7E
	DEFB	$FF,$FF,$F9,$9F,$F9,$9F,$0F,$F0
	DEFB	$0F,$F0,$F9,$9F,$F9,$9F,$FF,$FF
	DEFB	$7E,$7E,$7E,$7E,$3E,$7C,$0E,$70
	DEFB	$61,$06,$C1,$03,$81,$01,$01,$80
	DEFB	$00,$00,$04,$20,$02,$40,$10,$0F
	DEFB	$F0,$08,$02,$40,$04,$20,$00,$00
	DEFB	$01,$80,$80,$81,$C0,$83,$60,$86