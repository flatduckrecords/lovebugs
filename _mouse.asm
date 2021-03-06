; Title:	ZX Spectrum Next Mouse Routine
; Author:	Ben Versteeg
; Link:		http://www.benophetinternet.nl/hobby/kmt.htm
; Modified By:	Dean Belfield
; Created:	22/08/2020
; Last Updated:	22/08/2020
;
; Requires:
;
; Modinfo:
;

; Kempston mouse routine
;
Mouse:
MNOP			NOP
			PUSH BC
			PUSH DE
			PUSH HL
			LD BC,0xFADF		; Read the buttons
			IN A,(C)
			AND %00000011
			;CALL Byte2Attr
			;cp %00000001
			;ret nz
			LD (Mouse_Buttons),A
			LD HL,(Mouse_Coords)	; The current coords
			LD BC,0xFBDF		; The mouse port for X
			LD DE,(Mouse_Coords+2)	; The previous coords
			IN A,(C)		; Get the X coordinate
			LD (Mouse_Coords+2),A	; Store
			SUB E			; Subtract from the old coordinate
			JR Z,NM_X		; If zero, then skip
			JP P,MX_PL
			ADD A,L
			JR C,ZER_X
			XOR A
ZER_X:			LD L,A
			JR NM_X
MX_PL:			ADD A,L
			JR C,BEX_Z
			CP 0xF7 		; MAXIMUM X
			JR C,BEX_B
BEX_Z:			LD A,0xF7 		; MAXIMUM X
BEX_B:			LD L,A

NM_X:			LD B,0xFF		; The mouse port for Y
			IN A,(C)		; Get the Y coordinate
			LD (Mouse_Coords+3),A	; Store
			SUB D			; Subtract from the old coordinate
			JR Z,NM_Y		; If zero then skip
			NEG
			JP P,MY_PL
			ADD A,H
			JR C,ZER_Y
			XOR A
ZER_Y:			LD H,A
			JR NM_Y
MY_PL:			ADD A,H
			JR C,BEY_Z
			CP 0xB8 		; MAXIMUM Y
			JR C,BEY_B
BEY_Z:			LD A,0xB8 		; MAXIMUM Y
BEY_B:			LD H,A

NM_Y:			LD A,H
			CP 0xFF
			JR C,BIGY
			LD H,0xFF
BIGY:			CP 0x00 		; MINIMUM Y
			JR NC,SMALY
			LD H,0x00 		; MINIMUM Y

SMALY:			LD A,L
			CP 0xFF
			JR C,DIRY
			LD L,0xFF
DIRY:			CP 0x00 		; MINIMUM X
			JR NC,DIMENS
			LD L,0x00 		; MINIMUM X
DIMENS:			LD (Mouse_Coords),HL

			POP HL
			POP DE
			POP BC
			RET

Mouse_Buttons:		DEFB 0
Mouse_Coords:		DEFB 0,0,0,0	

;~~~~ detect master mouse ~~~~~
         
;this routine enable interrupt
;and use HALT
;After return BC=0(off)/1(on)
;1=mouse detected/0=no detected
         
DETECT_M 
         push af
;mask for master mouse ports
         ld   a,$FF
;         jr   DETECTOR
         
;DETECT_S 
;         push af
;mask for slave mouse ports
;         ld   a,$0F
         
         
;DETECTOR 
	push de
         push hl
         
;E=mask MA/SL
         ld   e,a
         
         ei   
         halt 
         
         ld   bc,$FADF
         ld   a,b
         and  e
         ld   b,a
         
;H=buttons port
         in   h,(c)
         inc  b
;L=x-axis port
         in   l,(c)
         ld   a,b
         or   $0F
         ld   b,a
;L=y-axis port
         in   a,(c)
         
;BC=1 for detected mouse
         ld   bc,1
         
         cp   h
         jr   nz,DETECT
         cp   l
         jr   nz,DETECT
         cp   255
         jr   nz,DETECT
;BC=0 - mouse not detected
NODETECT dec  bc
         
DETECT   pop  hl
         pop  de
         pop  af
         ret  