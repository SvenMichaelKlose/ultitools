;;; @file banktest.s
;;; Banking register test for the Vic Flash Plugin
;;; @author Marko Mäkelä (marko.makela@iki.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2010,2011 Marko Mäkelä (marko.makela@iki.fi)
;;;
;;;     This program is free software; you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation; either version 2 of the License, or
;;;     (at your option) any later version.
;;;
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;;
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program; if not, write to the Free Software
;;;     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

	.word $1001
	*=$1001
prg	.word nextln
	.word 2011
	.byte $9e		; SYS
	.byte $30 + (start / 1000)
	.byte $30 + ((start - (start / 1000 * 1000)) / 100)
	.byte $30 + ((start - (start / 100 * 100)) / 10)
	.byte $30 + (start - (start / 10 * 10))
	.byte 0
nextln	.word 0

bankreg	= $9800	; ROM bank switching register (A20..A13, default value $00)
cfgreg	= $9801	; configuration register (default value $40)
	;; b7 == 1 => I/O2 disabled until RESET
	;; b6 == 1 => BLK5 write protect (default)
	;; b5 == 1 => RAM at BLK5 (instead of ROM)
	;; b4 => 0=3k (RAM1-3), 1=8k (BLK1)
	;; b3 == 1 => master RAM enable (BLK2,BLK3 is always enabled)
	;; b0 => A21
cfg3k	= $28			; select 3k RAM
cfg8k	= $38			; select 8k+ RAM
bankdef	= $00			; default Flash ROM bank
cfgdef	= $40			; default configuration

strout	= $cb1e			; output a NUL-terminated string
chrout	= $ffd2			; output a character
#define printmsg(x) lda#<x:ldy#>x:jsr strout

crsrchr	= $d1			; cursor position in screen memory
crsr_x	= $d3			; current cursor column
crsrcol	= $f3			; cursor position in color memory
txtcolor= 646			; current text color

	;; 16-bit division
	;; .A = NUM % DIV
	;; .Y = 0
	;; NUM /= DIV
#define div16(NUM,DIV) ldy #17	; n_bits+1:\
	lda #0			; remainder:.(:\
	beq entry		; entry point (branch always):\
loop	rol:\
	cmp #127:\
	bcc entry		; remainder > divisor?:\
	sbc #127		; yes, subtract the divisor:\
entry				; shift the quotient:\
	rol NUM:\
	rol NUM+1:\
	dey:\
	bne loop:.)

	;; Generate a 16-bit random number using a binary Galois LFSR
	;; with the primitive polynomial
	;; x^16 + x^14 + x^13 + x^11 + 1
	;; (taps corresponding to $b400)
	;; from http://en.wikipedia.org/wiki/Linear_feedback_shift_register
	;; RNG = new random value (never 0)
	;; .A = trashed
#define rng16(RNG) .(:\
	lsr RNG+1:\
	ror RNG:\
	bcc done:\
	lda RNG+1:\
	eor #$b4:\
	sta RNG+1:\
done	.)

ram123	= $0400			; 3k RAM
blk1	= $2000			; 8k RAM
blk23	= $4000			; 16k RAM, always enabled
blk5	= $a000			; 8k RAM or ROM

cnt	= $ff			; loop counter
rng	= $fb			; random number generator
dst	= $fd

start	sei
	lda bankreg
	sta defbank
	lda cfgreg
	sta defcfg
	lda #cfg8k
	sta cfgreg
	;; copy a 127-byte array to the memory (32768 = 258 * 127 + 2)
	lda #>blk23
	ldy #0
	sty dst
	sta dst+1
	sta rng			; nonzero initial value for the pseudo rng
	sta rng+1
copy1	ldx #1
copy	lda pattern-1,x
	sta (dst),y
	iny
	bne dstok
	inc dst+1
	bpl dstok
	bmi last8k
dstok	inx
	bpl copy
	bmi copy1
last8k	lda #>blk5
	sta dst+1
dstok2	inx
	bpl copy2
	ldx #1
copy2	lda pattern-1,x
	sta (dst),y
	iny
	bne dstok2
	inc dst+1
	bit dst+1
	bvc dstok2		; until $c000 reached

	ldy #0			; compare RAM1-3 to BLK1
	lda #4
	sta cmphi1
	lda #$24
	sta cmphi2
	ldx #cfg8k
cmploop	lda #cfg3k
	sta cfgreg
cmphi1	= *+2
	lda $400,y
	stx cfgreg
cmphi2	= *+2
	cmp $2400,y
	bne cmpfail
	iny
	bne cmploop
	inc cmphi1
	inc cmphi2
	lda cmphi1
	and #$f
	bne cmploop
	beq ram123_ok
cmpfail	tya
	pha
	lda cmphi1
	lsr
	lsr
	clc
	adc #$30
	sta ram123_fail1
	printmsg(ram123_fail)
	lda cmphi1
	jsr hex2
	pla
hex2_exit
	jsr hex2
	lda #$0d
	jsr chrout
	jmp exit
ram_fail
	printmsg(blk_fail)
	lda dst+1
	jsr hex2
	lda dst
	jmp hex2_exit
ram123_ok
	;; switch off RAM1..3; check that it does not compare to BLK1
	;; switch off BLK1, BLK5, ditto
	;; copy BLK5 flash bank 0 to RAM, compare to other flash banks (complain on match)
	
	;; verify BLK1, BLK2, BLK3, BLK5 at random addresses
	ldx #0
	stx cnt			; 256*256 rounds
	lda #$20		; reset the progress indicator
	jsr $ffd2
	dec crsr_x

blk_l	rng16(rng)
	lda rng+1		; load 15 bits to the address
	and #$7f
	sta dst+1
	pha
	lda rng
	sta dst
	pha
	div16(dst,127)
	tay			; address % 127
	pla
	sta dst
	pla
	clc
	adc #>blk1
	bpl addr_ok
	eor #>(blk5-$8000)
addr_ok	sta dst+1
	lda pattern,y
	ldy #0
	eor (dst),y
	bne ram_fail
	dex
	bne blk_l
	lda cnt			; update the on-screen progress display
	and #15
	tay
	lda bar,y
	ldy crsr_x
	sta (crsrchr),y
	dec cnt
	bne blk_l
	printmsg(ram_ok)
exit
defbank	= *+1
	lda #bankdef
	sta bankreg
defcfg	= *+1
	lda #cfgdef
	sta cfgreg
	cli
	rts

	;; print two hexadecimal digits in .A
hex2
	tax
	lsr
	lsr
	lsr
	lsr
	jsr hex1
	txa
	;; print one hexadecimal digit in .A
hex1
	and #$f
	ora #$30
	cmp #$3a
	bcc doprint
	adc #$41-$3a-1
doprint
	jmp chrout

;; 127 bytes of test data
pattern	.byte $01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10
	.byte $aa,$55,$cc,$33,$11,$22,$44,$88,$33,$66,$cc,$99,$00,$ff,$c3,$3c
	.byte $03,$1c,$2a,$35,$40,$5f,$61,$7e,$82,$9d,$a3,$bc,$c4,$db,$e5,$fa
	.byte $10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0,$01
	.byte $30,$c1,$a2,$53,$04,$f5,$16,$e7,$28,$d9,$3a,$cb,$4c,$bd,$5e,$af
	.byte $c0,$ff,$ee,$de,$ad,$be,$ef,$c0,$de,$b1,$de,$37,$73,$f0,$0f,$09
	.byte $b1,$40,$23,$a2,$85,$74,$97,$66,$a9,$38,$db,$4c,$cd,$35,$df,$2e
	.byte $ef,$df,$cf,$bf,$af,$9f,$8f,$7f,$6f,$5f,$4f,$3f,$2f,$1f,$0f
ram123_fail
ram123_fail1 = *+3
	.byte "RAM1<>BLK1 AT ", 0
blk_fail
	.byte "BLK MISMATCH AT ", 0
ram_ok
	.byte "MEMORY OK", 13, 0
;; progress bar
bar
	.byte $20,$65,$74,$75,$61,$f6,$ea,$e7,$a0,$e5,$f4,$f5,$76,$6a,$67,$20
