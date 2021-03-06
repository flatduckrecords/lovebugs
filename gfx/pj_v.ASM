; ASM source file created by SevenuP v1.20
; SevenuP (C) Copyright 2002-2006 by Jaime Tejedor Gomez, aka Metalbrain

;GRAPHIC DATA:
;Pixel Size:      ( 24,  32)
;Char Size:       (  3,   4)
;Frames:             8
;Sort Priorities: Mask, X char, Char line, Y char, Frame number
;Data Outputted:  Gfx
;Interleave:      Line
;Mask:            Yes

pj_v:
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$DF,$00,$FF,$00,$E4,$20,$0F
	DEFB	$00,$FF,$1B,$C0,$F0,$03,$00,$FF
	DEFB	$3F,$80,$FC,$00,$00,$FF,$7F,$00
	DEFB	$FF,$00,$00,$7F,$7F,$00,$FF,$00
	DEFB	$80,$3F,$FF,$00,$FF,$00,$80,$3F
	DEFB	$7D,$00,$FF,$00,$80,$3F,$F8,$00
	DEFB	$7F,$00,$80,$3F,$F0,$00,$03,$00
	DEFB	$80,$3F,$F3,$00,$19,$00,$80,$3F
	DEFB	$64,$00,$E7,$00,$80,$3F,$FD,$00
	DEFB	$AD,$00,$80,$3F,$43,$00,$19,$00
	DEFB	$80,$3F,$60,$00,$01,$00,$00,$7F
	DEFB	$11,$80,$C2,$00,$00,$FF,$18,$C0
	DEFB	$0C,$01,$00,$FF,$0E,$E0,$30,$03
	DEFB	$00,$FF,$03,$F0,$C0,$0F,$00,$FF
	DEFB	$0E,$E0,$60,$07,$00,$FF,$17,$C0
	DEFB	$38,$03,$00,$FF,$20,$80,$CC,$01
	DEFB	$00,$FF,$10,$C0,$64,$01,$00,$FF
	DEFB	$0F,$E0,$B4,$01,$00,$FF,$0F,$E0
	DEFB	$DC,$01,$00,$FF,$0F,$E0,$EC,$01
	DEFB	$00,$FF,$0F,$E0,$F6,$00,$00,$FF
	DEFB	$0E,$E0,$73,$00,$00,$0F,$0E,$E0
	DEFB	$77,$08,$F0,$07,$1C,$C1,$75,$00
	DEFB	$50,$07,$1C,$C1,$70,$08,$00,$0F
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$EF,$00,$FF,$00,$F2,$10,$07
	DEFB	$00,$FF,$0D,$E0,$F8,$01,$00,$FF
	DEFB	$1F,$C0,$FE,$00,$00,$7F,$3F,$80
	DEFB	$FF,$00,$80,$3F,$3F,$80,$FF,$00
	DEFB	$C0,$1F,$7F,$00,$FF,$00,$C0,$1F
	DEFB	$3E,$80,$FF,$00,$C0,$1F,$7C,$00
	DEFB	$3F,$00,$C0,$1F,$78,$00,$01,$00
	DEFB	$C0,$1F,$79,$00,$8C,$00,$C0,$1F
	DEFB	$32,$80,$73,$00,$C0,$1F,$7E,$00
	DEFB	$D6,$00,$C0,$1F,$21,$80,$8C,$00
	DEFB	$C0,$1F,$30,$80,$00,$00,$80,$3F
	DEFB	$08,$C0,$E1,$00,$00,$7F,$0C,$E0
	DEFB	$06,$00,$00,$FF,$07,$F0,$18,$01
	DEFB	$00,$FF,$01,$F8,$E0,$07,$00,$FF
	DEFB	$07,$F0,$30,$03,$00,$FF,$0B,$E0
	DEFB	$9C,$01,$00,$FF,$10,$C0,$66,$00
	DEFB	$00,$FF,$08,$E0,$32,$00,$00,$FF
	DEFB	$07,$F0,$DA,$00,$00,$FF,$07,$F0
	DEFB	$EE,$00,$00,$FF,$07,$F0,$F6,$00
	DEFB	$00,$FF,$07,$F0,$FB,$00,$00,$7F
	DEFB	$07,$F0,$39,$00,$80,$07,$07,$F0
	DEFB	$3B,$04,$F8,$03,$0E,$E0,$3A,$80
	DEFB	$A8,$03,$0E,$E0,$38,$84,$00,$07
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$F7,$00,$FF,$00,$F9,$08,$03
	DEFB	$00,$FF,$06,$F0,$FC,$00,$00,$FF
	DEFB	$0F,$E0,$FF,$00,$00,$3F,$1F,$C0
	DEFB	$FF,$00,$C0,$1F,$1F,$C0,$FF,$00
	DEFB	$E0,$0F,$3F,$80,$FF,$00,$E0,$0F
	DEFB	$1F,$C0,$7F,$00,$E0,$0F,$3E,$80
	DEFB	$1F,$00,$E0,$0F,$3C,$80,$00,$00
	DEFB	$E0,$0F,$3C,$80,$C6,$00,$60,$0F
	DEFB	$19,$C0,$39,$00,$E0,$0F,$3F,$80
	DEFB	$6B,$00,$60,$0F,$10,$C0,$C6,$00
	DEFB	$60,$0F,$18,$C0,$00,$00,$40,$1F
	DEFB	$04,$E0,$70,$00,$80,$3F,$06,$F0
	DEFB	$03,$00,$00,$7F,$03,$F8,$8C,$00
	DEFB	$00,$FF,$00,$FC,$F0,$03,$00,$FF
	DEFB	$03,$F8,$98,$01,$00,$FF,$05,$F0
	DEFB	$CE,$00,$00,$FF,$08,$E0,$33,$00
	DEFB	$00,$7F,$04,$F0,$19,$00,$00,$7F
	DEFB	$03,$F8,$ED,$00,$00,$7F,$03,$F8
	DEFB	$F7,$00,$00,$7F,$03,$F8,$FB,$00
	DEFB	$00,$7F,$03,$F8,$FD,$00,$80,$3F
	DEFB	$03,$F8,$9C,$00,$C0,$1F,$03,$F8
	DEFB	$98,$02,$60,$03,$03,$F8,$9D,$00
	DEFB	$FC,$01,$03,$F8,$C1,$00,$54,$01
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FB,$00,$FF,$00,$FC,$04,$81
	DEFB	$00,$FF,$03,$F8,$7E,$00,$00,$7F
	DEFB	$07,$F0,$FF,$00,$80,$1F,$0F,$E0
	DEFB	$FF,$00,$E0,$0F,$0F,$E0,$FF,$00
	DEFB	$F0,$07,$1F,$C0,$FF,$00,$F0,$07
	DEFB	$0F,$E0,$BF,$00,$F0,$07,$1F,$C0
	DEFB	$0F,$00,$F0,$07,$1E,$C0,$00,$00
	DEFB	$70,$07,$1E,$C0,$63,$00,$30,$07
	DEFB	$0C,$E0,$9C,$00,$F0,$07,$1F,$C0
	DEFB	$B5,$00,$B0,$07,$08,$E0,$63,$00
	DEFB	$30,$07,$0C,$E0,$00,$00,$20,$0F
	DEFB	$02,$F0,$38,$00,$40,$1F,$03,$F8
	DEFB	$01,$00,$80,$3F,$01,$FC,$C6,$00
	DEFB	$00,$7F,$00,$FE,$78,$01,$00,$FF
	DEFB	$01,$FC,$CC,$00,$00,$FF,$02,$F8
	DEFB	$E7,$00,$00,$7F,$04,$F0,$19,$00
	DEFB	$80,$3F,$02,$F8,$0C,$00,$80,$3F
	DEFB	$01,$FC,$F6,$00,$80,$3F,$01,$FC
	DEFB	$FB,$00,$80,$3F,$01,$FC,$FD,$00
	DEFB	$80,$3F,$01,$FC,$FE,$00,$C0,$1F
	DEFB	$01,$FC,$CE,$00,$60,$0F,$01,$FC
	DEFB	$CC,$01,$30,$01,$01,$FC,$CE,$00
	DEFB	$FE,$00,$01,$FC,$E0,$00,$AA,$00
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FD,$00,$FF,$00,$FE,$02,$40
	DEFB	$00,$FF,$01,$FC,$BF,$00,$00,$3F
	DEFB	$03,$F8,$FF,$00,$C0,$0F,$07,$F0
	DEFB	$FF,$00,$F0,$07,$07,$F0,$FF,$00
	DEFB	$F8,$03,$0F,$E0,$FF,$00,$F8,$03
	DEFB	$07,$F0,$DF,$00,$F8,$03,$0F,$E0
	DEFB	$87,$00,$F8,$03,$0F,$E0,$00,$00
	DEFB	$38,$03,$0F,$E0,$31,$00,$98,$03
	DEFB	$06,$F0,$4E,$00,$78,$03,$0F,$E0
	DEFB	$DA,$00,$D8,$03,$04,$F0,$31,$00
	DEFB	$98,$03,$06,$F0,$00,$00,$10,$07
	DEFB	$01,$F8,$1C,$00,$20,$0F,$01,$FC
	DEFB	$80,$00,$C0,$1F,$00,$FE,$E3,$00
	DEFB	$00,$3F,$00,$FE,$3C,$00,$00,$FF
	DEFB	$01,$FC,$E6,$00,$00,$7F,$02,$F8
	DEFB	$77,$00,$80,$3F,$02,$F8,$3E,$00
	DEFB	$C0,$1F,$01,$FC,$02,$00,$C0,$1F
	DEFB	$00,$FE,$83,$00,$40,$1F,$00,$FE
	DEFB	$FB,$00,$40,$1F,$00,$FE,$FD,$00
	DEFB	$80,$3F,$00,$FE,$FE,$00,$80,$3F
	DEFB	$00,$FE,$E6,$00,$C0,$1F,$00,$FE
	DEFB	$E7,$00,$60,$03,$00,$FE,$E7,$00
	DEFB	$FC,$01,$00,$FE,$F1,$00,$54,$01
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FE,$00,$FF,$00,$FF,$01,$20
	DEFB	$00,$7F,$00,$FE,$DF,$00,$80,$1F
	DEFB	$01,$FC,$FF,$00,$E0,$07,$03,$F8
	DEFB	$FF,$00,$F8,$03,$03,$F8,$FF,$00
	DEFB	$FC,$01,$07,$F0,$FF,$00,$FC,$01
	DEFB	$03,$F8,$EF,$00,$FC,$01,$07,$F0
	DEFB	$C3,$00,$FC,$01,$07,$F0,$80,$00
	DEFB	$1C,$01,$07,$F0,$98,$00,$CC,$01
	DEFB	$03,$F8,$27,$00,$3C,$01,$07,$F0
	DEFB	$ED,$00,$6C,$01,$02,$F8,$18,$00
	DEFB	$CC,$01,$03,$F8,$00,$00,$08,$03
	DEFB	$00,$FC,$8E,$00,$10,$07,$00,$FE
	DEFB	$C0,$00,$60,$0F,$00,$FF,$71,$00
	DEFB	$80,$1F,$00,$FF,$1E,$00,$00,$7F
	DEFB	$00,$FE,$F3,$00,$00,$3F,$01,$FC
	DEFB	$3B,$00,$C0,$1F,$01,$FC,$1F,$00
	DEFB	$60,$0F,$00,$FE,$81,$00,$60,$0F
	DEFB	$00,$FF,$41,$00,$A0,$0F,$00,$FF
	DEFB	$7D,$00,$A0,$0F,$00,$FF,$7E,$00
	DEFB	$C0,$1F,$00,$FF,$7F,$00,$40,$1F
	DEFB	$00,$FF,$73,$00,$60,$0F,$00,$FF
	DEFB	$73,$00,$B0,$01,$00,$FF,$73,$00
	DEFB	$FE,$00,$00,$FF,$78,$00,$AA,$00
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FF,$00,$7F,$00,$FF,$00,$90
	DEFB	$80,$3F,$00,$FF,$6F,$00,$C0,$0F
	DEFB	$00,$FE,$FF,$00,$F0,$03,$01,$FC
	DEFB	$FF,$00,$FC,$01,$01,$FC,$FF,$00
	DEFB	$FE,$00,$03,$F8,$FF,$00,$FE,$00
	DEFB	$01,$FC,$F7,$00,$FE,$00,$03,$F8
	DEFB	$E1,$00,$FE,$00,$03,$F8,$C0,$00
	DEFB	$0E,$00,$03,$F8,$CC,$00,$66,$00
	DEFB	$01,$FC,$93,$00,$9E,$00,$03,$F8
	DEFB	$F6,$00,$B6,$00,$01,$FC,$0C,$00
	DEFB	$66,$00,$01,$FC,$80,$00,$04,$01
	DEFB	$00,$FE,$47,$00,$08,$03,$00,$FF
	DEFB	$60,$00,$30,$07,$00,$FF,$38,$80
	DEFB	$C0,$0F,$00,$FF,$0F,$C0,$00,$3F
	DEFB	$00,$FF,$39,$80,$80,$1F,$00,$FF
	DEFB	$4C,$00,$E0,$0F,$00,$FE,$2F,$00
	DEFB	$D0,$07,$00,$FF,$20,$00,$D0,$07
	DEFB	$00,$FF,$1E,$80,$D0,$07,$00,$FF
	DEFB	$3E,$80,$D0,$07,$00,$FF,$3E,$80
	DEFB	$D0,$07,$00,$FF,$3E,$80,$D0,$07
	DEFB	$00,$FF,$3A,$80,$C0,$0F,$00,$FF
	DEFB	$38,$80,$C0,$07,$00,$FF,$73,$00
	DEFB	$F8,$03,$00,$FF,$72,$00,$A8,$03
	DEFB	$00,$FF,$00,$FF,$00,$FF,$00,$FF
	DEFB	$00,$FF,$00,$BF,$00,$FF,$00,$C8
	DEFB	$40,$1F,$00,$FF,$37,$80,$E0,$07
	DEFB	$00,$FF,$7F,$00,$F8,$01,$00,$FE
	DEFB	$FF,$00,$FE,$00,$00,$FE,$FF,$00
	DEFB	$FF,$00,$01,$FC,$FF,$00,$FF,$00
	DEFB	$00,$FE,$FB,$00,$FF,$00,$01,$FC
	DEFB	$F0,$00,$FF,$00,$01,$FC,$E0,$00
	DEFB	$07,$00,$01,$FC,$E6,$00,$33,$00
	DEFB	$00,$FE,$C9,$00,$CF,$00,$01,$FC
	DEFB	$FB,$00,$5B,$00,$00,$FE,$86,$00
	DEFB	$33,$00,$00,$FE,$C0,$00,$02,$00
	DEFB	$00,$FF,$23,$00,$84,$01,$00,$FF
	DEFB	$30,$80,$18,$03,$00,$FF,$1C,$C0
	DEFB	$60,$07,$00,$FF,$07,$E0,$80,$1F
	DEFB	$00,$FF,$1C,$80,$C0,$0F,$00,$FF
	DEFB	$26,$00,$70,$07,$00,$FF,$17,$00
	DEFB	$E8,$03,$00,$FF,$10,$80,$68,$03
	DEFB	$00,$FF,$0F,$C0,$68,$03,$00,$FF
	DEFB	$1F,$C0,$68,$03,$00,$FF,$1F,$C0
	DEFB	$68,$03,$00,$FF,$1F,$C0,$68,$03
	DEFB	$00,$FF,$1D,$C0,$60,$07,$00,$FF
	DEFB	$1C,$C0,$60,$03,$00,$FF,$39,$80
	DEFB	$FC,$01,$00,$FF,$39,$80,$54,$01