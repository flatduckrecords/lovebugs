; ASM source file created by SevenuP v1.20
; SevenuP (C) Copyright 2002-2006 by Jaime Tejedor Gomez, aka Metalbrain
; Target Assembler: TASM

;GRAPHIC DATA:
;Pixel Size:      ( 16,  16)
;Char Size:       (  2,   2)
;Frames:             8
;Sort Priorities: X char, Char line, Y char, Frame number
;Data Outputted:  Gfx+Attr
;Interleave:      Sprite
;Mask:            No

heartray:
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  1,  0
	.BYTE	  0,128,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  3, 96,  3,224
	.BYTE	  1,192,  1,128,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  3, 96,  7,240,  7,240
	.BYTE	  3,224,  3,192,  1,128,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  3, 96,  7,240, 15,248, 15,248
	.BYTE	  7,240,  7,224,  3,192,  1,128
	.BYTE	  0,  0,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  7,112
	.BYTE	 15,120, 31,252, 31,252, 30,124
	.BYTE	 30,124, 15,248,  7,240,  3,192
	.BYTE	  1,128,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  7,112
	.BYTE	 15,120, 31,252, 28, 60, 24, 28
	.BYTE	 24, 28, 12, 56,  6,112,  3,192
	.BYTE	  1,128,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  0,  0,  0,  0,  7,112
	.BYTE	 13, 88, 25,204, 16,  4, 16,  4
	.BYTE	 24, 12, 12, 24,  6,112,  3,192
	.BYTE	  1,128,  0,  0,  0,  0,  0,  0
	.BYTE	  0,  0,  5, 36, 16,  2,  4, 16
	.BYTE	 64,  1,  0,132, 80,  1,  0,  0
	.BYTE	 64,  1,  4, 16, 32,  2,  8,144
	.BYTE	  0,  0,  2, 32,  0,128,  0,  0
	.BYTE	 56, 56, 56, 56, 56, 56, 56, 56
	.BYTE	 56, 56, 56, 56, 56, 56, 56, 56
	.BYTE	 56, 56, 56, 56, 56, 56, 56, 56
	.BYTE	 56, 56, 56, 56, 56, 56, 56, 56