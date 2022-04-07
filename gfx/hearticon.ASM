; ASM source file created by SevenuP v1.20
; SevenuP (C) Copyright 2002-2006 by Jaime Tejedor Gomez, aka Metalbrain

;GRAPHIC DATA:
;Pixel Size:      ( 24,  16)
;Char Size:       (  3,   2)
;Frames:             9
;Sort Priorities: Mask, X char, Char line, Y char, Frame number
;Data Outputted:  Gfx
;Interleave:      Line
;Mask:            Yes

hearticon:
	DEFB	$00,$F8,$00,$1F,$00,$FF,$06,$E0
	DEFB	$60,$07,$00,$FF,$1F,$C0,$78,$03
	DEFB	$00,$FF,$3F,$80,$7C,$01,$00,$FF
	DEFB	$3F,$80,$FC,$01,$00,$FF,$7F,$00
	DEFB	$FE,$00,$00,$FF,$7F,$00,$FE,$00
	DEFB	$00,$FF,$7F,$00,$FE,$00,$00,$FF
	DEFB	$3F,$80,$FC,$01,$00,$FF,$3F,$80
	DEFB	$FC,$01,$00,$FF,$1F,$C0,$F8,$03
	DEFB	$00,$FF,$0F,$E0,$F0,$07,$00,$FF
	DEFB	$07,$F0,$E0,$0F,$00,$FF,$03,$F8
	DEFB	$C0,$1F,$00,$FF,$01,$FC,$80,$3F
	DEFB	$00,$FF,$00,$FE,$00,$7F,$00,$FF

hi2:	DEFB	$00,$FC,$00,$0F,$00,$FF,$03,$F0
	DEFB	$30,$03,$00,$FF,$0F,$E0,$BC,$01
	DEFB	$00,$FF,$1F,$C0,$BE,$00,$00,$FF
	DEFB	$1F,$C0,$FE,$00,$00,$FF,$3F,$80
	DEFB	$FF,$00,$00,$7F,$3F,$80,$FF,$00
	DEFB	$00,$7F,$3F,$80,$FF,$00,$00,$7F
	DEFB	$1F,$C0,$FE,$00,$00,$FF,$1F,$C0
	DEFB	$FE,$00,$00,$FF,$0F,$E0,$FC,$01
	DEFB	$00,$FF,$07,$F0,$F8,$03,$00,$FF
	DEFB	$03,$F8,$F0,$07,$00,$FF,$01,$FC
	DEFB	$E0,$0F,$00,$FF,$00,$FE,$C0,$1F
	DEFB	$00,$FF,$00,$FF,$00,$3F,$00,$FF

	DEFB	$00,$FE,$00,$07,$00,$FF,$01,$F8
	DEFB	$98,$01,$00,$FF,$07,$F0,$DE,$00
	DEFB	$00,$FF,$0F,$E0,$DF,$00,$00,$7F
	DEFB	$0F,$E0,$FF,$00,$00,$7F,$1F,$C0
	DEFB	$FF,$00,$80,$3F,$1F,$C0,$FF,$00
	DEFB	$80,$3F,$1F,$C0,$FF,$00,$80,$3F
	DEFB	$0F,$E0,$FF,$00,$00,$7F,$0F,$E0
	DEFB	$FF,$00,$00,$7F,$07,$F0,$FE,$00
	DEFB	$00,$FF,$03,$F8,$FC,$01,$00,$FF
	DEFB	$01,$FC,$F8,$03,$00,$FF,$00,$FE
	DEFB	$F0,$07,$00,$FF,$00,$FF,$60,$0F
	DEFB	$00,$FF,$00,$FF,$00,$9F,$00,$FF
	DEFB	$00,$FF,$00,$03,$00,$FF,$00,$FC
	DEFB	$CC,$00,$00,$FF,$03,$F8,$EF,$00
	DEFB	$00,$7F,$07,$F0,$EF,$00,$80,$3F
	DEFB	$07,$F0,$FF,$00,$80,$3F,$0F,$E0
	DEFB	$FF,$00,$C0,$1F,$0F,$E0,$FF,$00
	DEFB	$C0,$1F,$0F,$E0,$FF,$00,$C0,$1F
	DEFB	$07,$F0,$FF,$00,$80,$3F,$07,$F0
	DEFB	$FF,$00,$80,$3F,$03,$F8,$FF,$00
	DEFB	$00,$7F,$01,$FC,$FE,$00,$00,$FF
	DEFB	$00,$FE,$FC,$01,$00,$FF,$00,$FF
	DEFB	$78,$03,$00,$FF,$00,$FF,$30,$87
	DEFB	$00,$FF,$00,$FF,$00,$CF,$00,$FF
	DEFB	$00,$FF,$00,$81,$00,$FF,$00,$FE
	DEFB	$66,$00,$00,$7F,$01,$FC,$F7,$00
	DEFB	$80,$3F,$03,$F8,$F7,$00,$C0,$1F
	DEFB	$03,$F8,$FF,$00,$C0,$1F,$07,$F0
	DEFB	$FF,$00,$E0,$0F,$07,$F0,$FF,$00
	DEFB	$E0,$0F,$07,$F0,$FF,$00,$E0,$0F
	DEFB	$03,$F8,$FF,$00,$C0,$1F,$03,$F8
	DEFB	$FF,$00,$C0,$1F,$01,$FC,$FF,$00
	DEFB	$80,$3F,$00,$FE,$FF,$00,$00,$7F
	DEFB	$00,$FF,$7E,$00,$00,$FF,$00,$FF
	DEFB	$3C,$81,$00,$FF,$00,$FF,$18,$C3
	DEFB	$00,$FF,$00,$FF,$00,$E7,$00,$FF
	DEFB	$00,$FF,$00,$C0,$00,$FF,$00,$FF
	DEFB	$33,$00,$00,$3F,$00,$FE,$FB,$00
	DEFB	$C0,$1F,$01,$FC,$FB,$00,$E0,$0F
	DEFB	$01,$FC,$FF,$00,$E0,$0F,$03,$F8
	DEFB	$FF,$00,$F0,$07,$03,$F8,$FF,$00
	DEFB	$F0,$07,$03,$F8,$FF,$00,$F0,$07
	DEFB	$01,$FC,$FF,$00,$E0,$0F,$01,$FC
	DEFB	$FF,$00,$E0,$0F,$00,$FE,$FF,$00
	DEFB	$C0,$1F,$00,$FF,$7F,$00,$80,$3F
	DEFB	$00,$FF,$3F,$80,$00,$7F,$00,$FF
	DEFB	$1E,$C0,$00,$FF,$00,$FF,$0C,$E1
	DEFB	$00,$FF,$00,$FF,$00,$F3,$00,$FF
	DEFB	$00,$FF,$00,$E0,$00,$7F,$00,$FF
	DEFB	$19,$80,$80,$1F,$00,$FF,$7D,$00
	DEFB	$E0,$0F,$00,$FE,$FD,$00,$F0,$07
	DEFB	$00,$FE,$FF,$00,$F0,$07,$01,$FC
	DEFB	$FF,$00,$F8,$03,$01,$FC,$FF,$00
	DEFB	$F8,$03,$01,$FC,$FF,$00,$F8,$03
	DEFB	$00,$FE,$FF,$00,$F0,$07,$00,$FE
	DEFB	$FF,$00,$F0,$07,$00,$FF,$7F,$00
	DEFB	$E0,$0F,$00,$FF,$3F,$80,$C0,$1F
	DEFB	$00,$FF,$1F,$C0,$80,$3F,$00,$FF
	DEFB	$0F,$E0,$00,$7F,$00,$FF,$06,$F0
	DEFB	$00,$FF,$00,$FF,$00,$F9,$00,$FF
	DEFB	$00,$FF,$00,$F0,$00,$3F,$00,$FF
	DEFB	$0C,$C0,$C0,$0F,$00,$FF,$3E,$80
	DEFB	$F0,$07,$00,$FF,$7E,$00,$F8,$03
	DEFB	$00,$FF,$7F,$00,$F8,$03,$00,$FE
	DEFB	$FF,$00,$FC,$01,$00,$FE,$FF,$00
	DEFB	$FC,$01,$00,$FE,$FF,$00,$FC,$01
	DEFB	$00,$FF,$7F,$00,$F8,$03,$00,$FF
	DEFB	$7F,$00,$F8,$03,$00,$FF,$3F,$80
	DEFB	$F0,$07,$00,$FF,$1F,$C0,$E0,$0F
	DEFB	$00,$FF,$0F,$E0,$C0,$1F,$00,$FF
	DEFB	$07,$F0,$80,$3F,$00,$FF,$03,$F8
	DEFB	$00,$7F,$00,$FF,$00,$FC,$00,$FF
	DEFB	$00,$FF,$00,$F8,$00,$1F,$00,$FF
	DEFB	$06,$E0,$60,$07,$00,$FF,$1F,$C0
	DEFB	$78,$03,$00,$FF,$3F,$80,$7C,$01
	DEFB	$00,$FF,$3F,$80,$FC,$01,$00,$FF
	DEFB	$7F,$00,$FE,$00,$00,$FF,$7F,$00
	DEFB	$FE,$00,$00,$FF,$7F,$00,$FE,$00
	DEFB	$00,$FF,$3F,$80,$FC,$01,$00,$FF
	DEFB	$3F,$80,$FC,$01,$00,$FF,$1F,$C0
	DEFB	$F8,$03,$00,$FF,$0F,$E0,$F0,$07
	DEFB	$00,$FF,$07,$F0,$E0,$0F,$00,$FF
	DEFB	$03,$F8,$C0,$1F,$00,$FF,$01,$FC
	DEFB	$80,$3F,$00,$FF,$00,$FE,$00,$7F