_hellaPlot:          ; plot d = x-axis, e = y-axis
	push bc
	push de
	push hl
	ld a, (Mouse_Coords)
	ld d, a
	ld a, (Mouse_Coords+1)
	ld e, a
	;ld de, (Mouse_Coords+2)
	ld a,7
	and d
	ld b,a
	inc b
	ld a,e

	rra
	scf
	rra
	or a
	rra

	ld l,a
	xor e
	and 248
	xor e
	ld h,a
	ld a,d
	xor l
	and 7
	xor d
	rrca
	rrca
	rrca

	ld l,a  ;$4f $69


	LD A, D
	AND 7

	LD DE, X_PositionBits
	ADD A,E
	LD E,A
	LD A,(DE)

	;output to screen
	or (hl)
	ld (hl),a

	pop hl
	pop de
	pop bc
	ret