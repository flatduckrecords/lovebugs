			org 0xF230	; 62000

			; Reset colours, clear the screen
			ld a, %00111000	; black ink white paper
			ld (23624),a	; set ATTR P
			ld (23693),a	; set BORDCR
			call 0x0d6b	; CLS routine

			; Loading screen	
			ld ix,0x4000	; block address
			ld de,0x1B00	; block length
			scf		; load A with 255 to signify signify
			sbc a,a		; data bytes (as opposed to header)
			call 0x0556 	; 1366	; LD_BYTES; loads the block

			; MAIN CODE Part 2 (uncondended RAM)
			ld ix,0x8000	; block address (#32768)
			ld de,0x34B7	; block length
			scf		; load A with 255 to signify signify
			sbc a,a		; data bytes (as opposed to header)
			call 0x0556 	; 1366	; LD_BYTES; loads the block

			; MAIN CODE Part 1 (contended RAM)
			ld ix,0x5E00	; block address (#23734)
			ld de,0x0921	; block length
			scf		; load A with 255 to signify signify
			sbc a,a		; data bytes (as opposed to header)
			call 0x0556 	; 1366	; LD_BYTES; loads the block

			call INITBB	; Load the first screen

			jp 0x5E00	; JUMP TO MAIN CODE

INITBB:			; INITIALISE THE BACK BUFFER
			ld ix,0xD800	; block address
			ld de,0x1800	; block length
			scf		; load A with 255 to signify signify
			sbc a,a		; data bytes (as opposed to header)
			call 0x0556 	; 1366	; LD_BYTES; loads the block
			ret