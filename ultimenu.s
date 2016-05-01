;;; @file ultimenu.s
;;; Menu program for the Vic UltiMem
;;; @author Marko Mäkelä (marko.makela@iki.fi)
;;; The PuCrunch decompressor is based on code by Pasi Ojala (albert@cs.tut.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2001,2003,2005,2009,2010,2015 Marko Mäkelä (marko.makela@iki.fi)
;;; Copyright © 1997-2000 Pasi 'Albert' Ojala (a1bert@iki.fi)
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

NAMEBYTES = 16			; 16 bytes per file name
DIRENTSIZE	= 6+NAMEBYTES	; size of a directory entry in bytes
cBg		= 8	; background colour
cBgS		= $6e	; background for selected entry
cFg		= 1	; foreground colour
cFgD		= 7	; foreground colour for subdirectory entry

LZPOS	= $9e		; 2 ZeroPage temporaries
table	= $200		; RLE table

mout	= $d1			; pointer for writing menu characters
cout	= $c3			; pointer for colourising menu entries
mbits	= $f7			; bit store for menu characters
typefl	= $f9			; directory entry type
tmpos	= $f9			; temporary position, 24 bits
menupos	= $fc			; menu position, 24 bits
pagesiz	= $ff			; page size in bytes is MENULINES / 2

ultimem	= $9bf0			; UltiMem registers
ultimem_ioram = ultimem		; 00|I/O3|I/O2|RAM123 config
ultimem_blk = ultimem + 1	; BLK5|BLK3|BLK2|BLK1 config
ultimem_cfg = ultimem + 2	; UltiMem configuration register
ultimem_cfg_dis = $40		; disable the UltiMem registers
ultimem_cfg_led = 1		; UltiMem LED
ultimem_ram = ultimem + 4	; RAM123 address register (lo/hi)
ultimem_io = ultimem + 6	; I/O2 and I/O3 address register (lo/hi)
ultimem_blk1 = ultimem + 8	; BLK1 address register (lo/hi)
ultimem_blk2 = ultimem + 10	; BLK2 address register (lo/hi)
ultimem_blk3 = ultimem + 12	; BLK3 address register (lo/hi)
ultimem_blk5 = ultimem + 14	; BLK5 address register (lo/hi)

*	= $a000
image_start
	.word reset		; RESET vector
	.word nmi		; NMI vector
	.byte $41,$30,$c3,$c2,$cd; cartridge signature a0CBM
freevec				; pointer to freemem, a singly linked list
				; of free memory, terminated by $ffffff
				; (meaning that all memory following the
				; $ffffff is free)
	.byte 0, 0, 1		; Exclude the first 64k for the menu software,
				; so that it can be update without rewriting
				; the menu items.
;	.word freemem-$a000
;	.byte 0
menuvec
	.byte 3, 0, 1		; The freemem pointer takes 3 bytes.
;	.word rootmenu-$a000	; 24-bit vector to the first menu entry
;	.byte 0

;;; PuCrunch decruncher code starts
block_stack
bitstr	= $f8
esc	= $f9
*	= $fa
block_stack_

OUTPOS = *+1		; ZP
putch	sta $aaaa	; ** parameter
	inc OUTPOS	; ZP
	bne putch0
	inc OUTPOS+1	; ZP
	pha
OUTWARP = *+1
	lda #$aa	; ** parameter
	eor OUTPOS+1
	bne putch00
	lda #$a0
	sta OUTPOS+1
putch00	pla
putch0	dex
	rts

newesc	ldy esc		; remember the old code (top bits for escaped byte)
escB0	ldx #2		; ** PARAMETER	0..8
	jsr getchkf	; get & save the new escape code
	sta esc
	tya		; pre-set the bits
	; Fall through and get the rest of the bits.
noesc	ldx #6		; ** PARAMETER	8..0
	jsr getchkf
	jsr putch	; output the escaped/normal byte
	; Fall through and check the escape bits again
decrunch
	ldy #0		; Reset to a defined state
	tya		; A = 0
escB1	ldx #2		; ** PARAMETER	0..8
	jsr getchkf	; X = 0
	cmp esc
	bne noesc
	; Fall through to packed code

	jsr getval	; X = 0
	sta LZPOS	; xstore - save the length for a later time
	lsr		; cmp #1	; LEN == 2 ? (A is never 0)
	bne lz77	; LEN != 2	-> LZ77
	;tya		; A = 0

	jsr getbit	; X = 0
	bcc lz77_2	; A=0 -> LZPOS+1
	; e..e01
	jsr getbit	; X = 0
	bcc newesc	; e..e010
	; e..e011
	iny		; Y is 1 bigger than MSB loops
	jsr getval	; Y is 1, get len, X = 0
	sta LZPOS	; xstore - Save length LSB
mg1	cmp #64		; ** PARAMETER 63-64 -> C clear, 64-64 -> C set..
	bcc chrcode	; short RLE, get bytecode

longrle	ldx #2		; ** PARAMETER	111111xxxxxx
	jsr getbits	; get 3/2/1 more bits to get a full byte, X = 0
	sta LZPOS	; xstore - Save length LSB

	jsr getval	; length MSB, X = 0
	tay		; Y is 1 bigger than MSB loops

chrcode	jsr getval	; Byte Code, X = 0
	tax		; this is executed most of the time anyway
	lda table-1,x	; Saves one jump if done here (loses one txa)

	cpx #32		; 31-32 -> C clear, 32-32 -> C set..
	bcc dorle0	; 1..31, we got the right byte from the table

	; Ranks 32..64 (11111°xxxxx), get byte..
	txa		; get back the value (5 valid bits)
	ldx #3
	jsr getbits	; get 3 more bits to get a full byte, X = 0

dorle0	ldx LZPOS	; xstore - get length LSB
	inx		; adjust for cpx#$ff;bne -> bne
dorle	jsr putch	;+dex
	bne dorle	; xstore 0..255 -> 1..256
	dey
	bne dorle	; Y was 1 bigger than wanted originally
mainbeq	beq decrunch	; reverse condition -> jump always

bankdef	= 0			;FIXME
bankreg = ultimem_blk
cfgdef = 0
cfgreg = ultimem_blk

eof	ldx #bankdef
	stx bankreg	; A20..A13=0
	ldx #cfgdef
	stx cfgreg	; A21=0
	rts

lz77	jsr getval	; X = 0
mg21	cmp #127	; ** PARAMETER	Clears carry (is maximum value)
	beq eof

	sbc #0		; C is clear -> subtract 1  (1..126 -> 0..125)
elzpb	ldx #0		; ** PARAMETER (more bits to get)
	jsr getchkf	; clears Carry, X = 0

lz77_2	sta LZPOS+1	; offset MSB
	ldx #8
	jsr getbits	; clears Carry, X = 0
	; Note, it is already eored in the compressor..
	;eor #255	; offset LSB 2's complement -1 (i.e. -X = ~X+1)
	adc OUTPOS	; -offset -1 + curpos (C is clear)
	ldx LZPOS	; xstore = LZLEN (read before it's overwritten)

	sta LZPOS
	lda OUTPOS+1
	sbc LZPOS+1	; takes C into account
	sta LZPOS+1	; copy X+1 number of chars from LZPOS to OUTPOS
	;ldy #0		; Y was 0 originally, we don't change it

	inx		; adjust for cpx#$ff;bne -> bne
lzloop	lda (LZPOS),y	; using abs,y is 3 bytes longer, only 1 cycle/byte faster
	iny		; Y does not wrap because X=0..255 and Y initially 0
	jsr putch	;+dex
	bne lzloop	; X loops, (256,1..255)
	beq mainbeq

; getval => Gets a 'static huffman coded' value
; ** Scratches X, returns the value in A **
getval	inx		; X <- 1
	txa		; set the top bit (value is 1..255)
gv0	jsr getbit
	bcc getchk	; got 0-bit
	inx
mg	cpx #7		; ** PARAMETER unary code maximum length + 1
	bne gv0
	beq getchk	; inverse condition -> jump always

; getbits => Gets X bits from the stream
; ** Scratches X, returns the value in A **
getbits	jsr getbit
	rol
getchk	dex
getchkf	bne getbits
	clc
	rts
block_stack_end
*	= block_stack_end - block_stack_ + block_stack
block_stack2
*	= block_stack_end
block_stack2_
getbit	asl bitstr
	bne gbend
getnew	pha
INCFG = *+1
	lda #cfgdef
	sta cfgreg	; select Flash ROM at BLK3
INPOS = *+1
	lda $aaaa	; ** PARAMETER
INPOSr	rts;rol		; Shift out the next bit and
			; shift in C=1 (last bit marker)
	sta bitstr	; bitstr initial value = $80 == empty
	inc INPOS	; increment bits A0..A12
	bne getnew0
	inc INPOS+1
	bmi getnewpage	; A0..A12 wrapped around => increment A13..A23
getnew0
INCFG1 = *+1
	lda #cfgdef
	sta cfgreg	; select RAM at BLK3
	pla
gbend	rts
block_stack2_end
*	= block_stack2_end - block_stack2_ + block_stack2

block_temp
*	= table+31
block_temp_
	;; get a new page of data
getnewpage
	lda #>$6000	; wrap around to the start of BLK3
	sta INPOS+1
	inc ultimem_blk3; increment the bank
	bne getnew0
	inc ultimem_blk3+1
	bne getnew0	; branch always
;;; PuCrunch decruncher code ends
block_temp_end
*	= block_temp_end - block_temp_ + block_temp

;;; Trampoline code for starting the decompressed program
block_reset
*	= $100
block_reset_
reset_blk = *+1
	lda #0
	sta ultimem_blk
reset_cfg = *+1
	lda #0
	sta ultimem_cfg		; change the memory configuration
	rti
block_reset_end
*	= block_reset_end - block_reset_ + block_reset

;;; Constants for the menu interface, for both PAL and NTSC
FIRST_LINE	= 28	; first visible raster line
FIRST_COLUMN_P	= 5	; leftmost visible screen coordinate, PAL
LAST_COLUMN_P	= 65	; leftmost right border coordinate that is not visible
LINES_P		= 312	; number of scanlines
LINECYCLES_P	= 71	; number of bus cycles per scanline

FIRST_COLUMN_N	= 1	; leftmost visible screen coordinate, NTSC
LAST_COLUMN_N	= 55	; leftmost right border coordinate that is not visible
LINES_N		= 261	; number of scanlines
LINECYCLES_N	= 65	; number of bus cycles per scanline

VIA_RELOAD_TIME	= 2	; reloading a VIA timer takes 2 bus clock cycles
TIMER_VALUE_P	= LINES_P * LINECYCLES_P - VIA_RELOAD_TIME
TIMER_VALUE_N	= LINES_N * LINECYCLES_N - VIA_RELOAD_TIME

MENUWIDTH	= 16	; number of characters per menu entry
MENULINES_P	= 31	; number of menu entries on the screen, PAL
MENULINES_N	= 29	; number of menu entries on the screen, NTSC
RASTERLINE_P	= (FIRST_LINE + LINES_P) / 4 - 2 * MENULINES_P + 4 * (MENULINES_P / 2) - 5
SCREENTOP_P	= (FIRST_LINE + LINES_P) / 4 - 2 * MENULINES_P
SCREENLEFT_P	= (LAST_COLUMN_P + FIRST_COLUMN_P) / 2 - MENUWIDTH

SCREENTOP_N	= (FIRST_LINE + LINES_N) / 4 - 2 * MENULINES_N
SCREENLEFT_N	= (LAST_COLUMN_N + FIRST_COLUMN_N) / 2 - MENUWIDTH
RASTERLINE_N	= (FIRST_LINE + LINES_N) / 4 - 2 * MENULINES_N + 4 * (MENULINES_N / 2) - 6

MENUSTART	= $1e00	; start of menu in screen memory
COLORSTART	= $9600	; start of menu in color memory
MENUEND_P	= MENULINES_P * MENUWIDTH + MENUSTART
MENUEND_N	= MENULINES_N * MENUWIDTH + MENUSTART

bend	= $2d		; end of basic program text
membot	= $282		; start page of BASIC RAM
memtop	= $284		; end page of BASIC RAM
screen	= $288		; start page of text matrix
repeat	= $28a		; key repeat ($80=repeat all keys)
mode	= $291		; upper/lower case change by shift+c= ($80=disabled)


warmstt	= $c7ae		; BASIC warm start
oreset	= $fd22		; original reset routine
creset	= $fd32		; continue the normal reset routine

;;; NMI handler
nmi
	sei
	ldx #$ff
	txs			; initialize the stack pointer
	jsr resetmem
	beq menu		; branch always
resetmem
	cld
	lda #$7f
	sta $913d		; disable VIA interrupts
	sta $913e		; acknowledge VIA interrupts
	lda #0
	sta $900e		; mute the audio
	sta $9002		; blank the screen (zero columns)
	.(
	ldx #13
	sta ultimem_ioram
loop	sta ultimem_cfg,x	; initialize the UltiMem registers
	dex
	bne loop
	ldx #ultimem_cfg_led
	stx ultimem_cfg
	ldx #$50
	stx ultimem_blk		; ROM at BLK5,BLK3
	.)
	.(
	ldx #$a
loop	sta $9132,x		; initialize the VIA registers
	dex
	bpl loop
	.)			; X=$ff
	stx $9123		; data direction register A (keyboard column)
	inx			; X=0
	.(
loop	sta 0,x			; fill $000..$3ff with $00
	sta !$100-2,x		; preserve the return address
	sta $200,x
	sta $300,x
	inx
	bne loop
	.)
	rts

;;; The entry point to the cartridge
reset
	;; clean up the VIA chips and the processor
	sei
	ldx #$ff
	txs			; initialize the stack pointer
	jsr resetmem
	;; check whether a key is pressed
	tay			; UltiMem registers enabled, LED off
	ldx $9120
	inx
	.(
	beq menu
	cpx #$ff
	beq go3k		; 3 kilobytes
	cpx #$80
	beq go8k		; 8+ kilobytes
	jmp boot0k
go3k	jmp boot3k
go8k	jmp boot8k
	;; no key pressed => go to the menu
	.)
menu	stx $9123		; select normal keyboard input
	dex
	stx $9122		; X=$ff

	;; initialize the screen output
	lda #cBg
	sta $900f		; black border and background
	sta $9000		; non-interlaced screen mode
	lda #$f2
	sta $9005		; screen memory at $1e00, lower case characters

	;; detect PAL/NTSC
	.(
waitnz	lda $9004		; wait for non-zero raster line
	beq waitnz
waitlast			; wait for last raster line
	lda $9004
	beq gotlast		; the raster counter wrapped around
	tay			; remember the raster line
	bne waitlast		; branch always

gotlast	cpy #(LINES_P - 1) / 2	; now y contains the maximum raster value
	beq tvpal
	cpy #(LINES_N - 1) / 2
	bne unknown
	jmp tvntsc
	;; unsupported video chip => display an error pattern
unknown	stx $900f
	sty $900f
	bne unknown
	.)

	;; video initialization
tvinit	sta $9003		; MENULINES lines of 8 pixels high characters
	stx $9001		; top screen line
	sty $9000		; left screen column
	lsr
	lsr
	sta pagesiz
	jsr $ff8a		; initialize the KERNAL jump vectors
	.(
	lda #<irq_p
	ldx #>irq_p
	ldy $9000
	cpy #SCREENLEFT_P
	beq setirq
	lda #<irq_n
	ldx #>irq_n
setirq	sta $314
	stx $315
	.)
	lda #>MENUSTART
	sta screen
	jsr $e536		; initialize the KERNAL screen and keyboard
	lda #$80
	sta repeat		; make all keys repeat
	sta mode		; disable upper/lower case changes (shift+c=)

	;; copy the auxiliary code
	.(
	ldx #block_temp_end-block_temp_
loop	lda block_temp-1,x
	sta block_temp_-1,x
	dex
	bne loop
	.)
	.(
	ldx #block_stack2_end-block_stack2_
loop	lda block_stack2-1,x
	sta block_stack2_-1,x
	dex
	bne loop
	.)
	.(
	ldx #3
loop	lda menuvec-1,x		; select the first entry of the root menu
	sta menupos-1,x
	dex
	bne loop
	.)
	jsr display		; display the menu
	.(
	lda #(FIRST_LINE / 2) - 1
vblank	cmp $9004
	bcc vblank		; wait for vertical blank area
	.)
	lda #$90
	sta $9002		; 16 characters per line, screen address $1e00
	lda #$40
	sta $912b		; enable Timer A free run on VIA 1
	rts

	;; PAL mode
tvpal	lda #MENULINES_P * 2
	ldx #SCREENTOP_P
	ldy #SCREENLEFT_P
	jsr tvinit

	;; synchronise with the screen
	.(
	ldx #RASTERLINE_P
init	cpx $9004
	beq init
	.)
	.(
coarse	cpx $9004
	bne coarse
	.)
	.(
	ldy #9
	bit $24
fine	lda $9003
	bit $24
	ldx #8
	nop
	nop
	nop
delay	dex			; wait x*5 cycles
	bne delay
#if (*/256) - (delay/256)
#echo "error: page boundary crossed, timing affected"
#endif
	cmp $9003
	beq * + 2
	dey
	bne fine
#if (*/256) - (fine/256)
#echo "error: page boundary crossed, timing affected"
#endif
	.)

	lda #<TIMER_VALUE_P
	ldx #>TIMER_VALUE_P
	ldy #3
bnetvst	bne tvstart

	;; NTSC mode
tvntsc	lda #MENULINES_N * 2
	ldx #SCREENTOP_N
	ldy #SCREENLEFT_N
	jsr tvinit

	;; synchronise with the screen
	.(
	ldx #RASTERLINE_N
init	cpx $9004
	beq init
	.)
	.(
coarse	cpx $9004
	bne coarse
	.)
	.(
	ldy #9
	bit $24
fine	ldx $9003
	txa
	bit $24
	bit $24
	ldx #8
delay	dex
	bne delay
#if (*/256) - (delay/256)
#echo "error: page boundary crossed, timing affected"
#endif
	cmp $9003
	beq * + 2
	dey
	bne fine
#if (*/256) - (fine/256)
#echo "error: page boundary crossed, timing affected"
#endif
	.)

	lda #<TIMER_VALUE_N
	ldx #>TIMER_VALUE_N
	ldy #LINECYCLES_N / 5 - 1

tvstart
#if (bnetvst/256) - (tvstart / 256)
#echo "error: page boundary crossed, timing affected"
#endif
	dey
	bne * - 1
#if (*/256) - ((*-3) / 256)
#echo "error: page boundary crossed, timing affected"
#endif
	sta $9126		; load the timer low byte latch
	stx $9125		; start Timer A

	lda #$c0
	cli
	sta $912e		; enable Timer A underflow interrupts
	bne getchar		; the first call to "display" is redundant

	;; main loop
update	jsr display
getchar	jsr $ffe4
	.(
	beq getchar
	ldx pagesiz
	ldy #0
	cmp #$13
	bne nohome
	;; home => move to the beginning of the list
home	jsr prev
	bcc home
	bcs update
nohome	cmp #$93
	bne noend
	;; clr => move to the end of the list
end	jsr next
	bcc end
	bcs update
noend	cmp #$11
	bne nodown
	;; down => move down by one entry
	jsr next
	bcc update
	bcs getchar
nodown	cmp #$1d
	bne nonext
	;; right => next page
nextp	jsr next
	bcs update
	dex
	bne nextp
	beq update		; branch always
nonext	cmp #$91
	bne noup
	;; up => move up by one entry
	jsr prev
bccupdate
	bcc update
	bcs getchar
noup	cmp #$9d
	bne noprev
	;; left => previous page
prevp	jsr prev
	bcs update
	dex
	bne prevp
bequpdate
	beq update
noprev	cmp #$0d
	beq go
	cmp #$8d
	bne nogo
	;; return => start the game
go	jmp launch

nogo	tax
	and #$3f		; mask the characters
	sta mbits
	cpx #$41
	bcc noch
	cpx #$5b
	bcc lch
	cpx #$c1
	bcc noch
	cpx #$db
	bcs noch
	;; upper-case character => move up until entry starts with char
uch	jsr prev
	bcs end
	and #$3f
	eor mbits
	bne uch
	beq bequpdate
	;; lower-case character => move down until entry starts with char
	;; if at end, move to beginning
lch	jsr next
	bcs home
	and #$3f
	eor mbits
	bne lch
	beq bequpdate
noch	ldy #0			; UltiMem registers enabled, LED off
	cpx #$3
	beq boot0k		; stop => reset to unexpanded VIC-20
	cpx #$33
	beq boot3k		; 3 => reset to 3k expanded VIC-20
	cpx #$38
	beq boot8k		; 8 => reset to 8k+ expanded VIC-20
	ldy #ultimem_cfg_dis	; UltiMem registers disabled, LED off
	cpx #$83
	beq boot0k		; shift+stop => reset to unexpanded, disable
	cpx #$23		; shift+3 => reset to 3k expanded, disable
	beq boot3k
	cpx #$28
	beq boot8k		; shift+8 => reset to 8k expanded, disable
	jmp getchar
	.)

	;; Boot into BASIC, with ultimem_cfg=Y
boot8k	ldx #$6a		; BLK5 ROM, BLK1..3 RAM
	stx ultimem_blk
	bne boot0k		; branch always
boot3k	ldx #2			; 3k RAM, nothing at I/O2, I/O3
	stx ultimem_ioram
boot0k	jsr cpreset
	.(
	sty reset_cfg
	lda ultimem_blk
	and #$3f
	sta reset_blk
	lda #>creset
	pha
	lda #<creset
	pha			; jump address
	lda #4
	pha			; Interrupt flag set, others clear
	lda #$20
	sta $c2			; end of RAM, normally at $2000
	lda #$1e		; A => screen at $1e00
	ldx #$10		; X => BASIC at $1000
	stx $97			; start of RAM, normally at $1000
	ldy ultimem_ioram	; cf. boot3k
	bne bexp3k
	ldy reset_blk		; cf. boot8k
	beq bexp0k
	;; expanded 8k+
bexp8k	txa			; A => screen at $1000
	ldx #$12		; X => BASIC at $1200
	ldy #$80		; Y => BASIC ends at $8000
	sty $c2			; end of RAM at $8000
	bne bptrset		; branch always
bexp3k	;; expanded 3k
	ldx #$04		; X => BASIC at $0400
	stx $97			; start of RAM at $0400
bexp0k	tay			; Y => BASIC ends at $1e00
bptrset	sta screen
	stx membot
	sty memtop
	.)
	jmp block_reset_

	;; display the menu, centered at the current entry (menupos)
display	ldy #0
	lda #>MENUSTART
	sty mout
	sta mout+1
	lda #>COLORSTART
	sty cout
	sta cout+1
	.(			; rewind by half a page
	ldx pagesiz
loop	jsr prev
	bcs done
	dex
	bne loop
done	.)
	txa			; X=number of blank lines to display at top
	pha
	.(
	beq done
loop	jsr blankln		; display blank lines at top if needed
	dex
	bne loop
done	.)
	pla
	tax
	jsr curr
	.(
loop	cpx pagesiz		; display menu entries at top
	clc
	beq done
	jsr println
	inx
	bne loop		; branch always
done	.)
	.(
	;; X=pagesiz
	jsr println		; display the current selection
	bcc loop
bloop	jsr blankln		; at the last menu entry,
	dex			; display X blank lines and return
	bne bloop
	rts
loop	jsr println		; display the entries following the selection
	bcs done
	dex
	bne loop
done	txa
done2	.)
	.(
	beq done
	pha
loop	jsr blankln		; display X blank lines at bottom
	dex
	bne loop
	pla
	tax
done	.)
	.(
loop	jsr prev		; rewind to the selected menu entry
	cpx pagesiz
	inx
	bcc loop
done	.)
	rts

	;; display a blank line, leave Y=0 and C=0, X unaffected, A trashed
blankln	ldy #15
	.(
loop	lda #$20		; space
	sta (mout),y
	lda #cFg
	sta (cout),y
	dey
	bpl loop
	.)
	iny
	clc			; advance to next line
	lda mout
	adc #MENUWIDTH
	sta mout
	sta cout
	.(
	bcc done
	inc mout+1
	inc cout+1
	clc
done	rts
	.)

;;; convert A from PETSCII to screen code using the following bijective mapping
;;; [map spare PETSCII codes to otherwise unused screen codes]
;;; $00-$1F	+128	$80	$80-$9F	ctrl
;;; $20-$3F		$00	$20-$3F	numbers and punctuation
;;; $40-$5F	-64	$C0	$00-$1F	characters
;;; $60-$7F	+64	$40	$A0-$BF	capital characters (unused)
;;; $80-$9F	+64	$40	$C0-$DF	shifted ctrl
;;; $A0-$BF	-64	$C0	$60-$7F	graphics
;;; $C0-$DF	-128	$80	$40-$5F	capital characters
;;; $E0-$FF		$00	$E0-$FF	graphics (unused)
pet2scr	cmp #$20
	.(
	bcs noctrl
invert	eor #$80		; $00..$1f -> $80..$9f (ctrl characters)
	rts
noctrl	cmp #$40
	bcc done		; $20..$3f -> $20..$3f (punctuation)
	cmp #$60
	bcc sub40		; $40..$5f -> $00..$1f (lower case)
	cmp #$a0
	bcs noalpha
	adc #$40		; $60..$9f -> $a0..$df
	rts
noalpha	cmp #$e0
	bcs done		; $e0..$ff -> $e0..$ff (graphics, unused)
	cmp #$c0
	bcs invert		; $c0..$df -> $40..$5f (upper case)
sub40	sbc #$40-1
done	rts
	.)

	;; display a menu entry, preserve X, assume Y=0, trash A, C=1 on EOF
println	ldy #0
	jsr pet2scr
	sta (mout),y		; the first character is in A
	.(
	lda typefl
	and typefl+1
	asl
	bcc notdir
	bpl notdir
	lda #cFgD		; color of subdirectories
	.byte $2c
notdir	lda #cFg		; color of other entries
	sta (cout),y		; set the color
	sta tmpos
loop	iny
	sec
	jsr getbyt
	jsr pet2scr
	sta (mout),y
	lda tmpos
	sta (cout),y
	cpy #MENUWIDTH-1
	bcc loop
	.)
	lda mout
	adc #NAMEBYTES-1		; C=1
	sta mout
	sta cout
	.(
	bcc done
	inc mout+1
	inc cout+1
done	ldy #0
	.)
	;; go to next menu entry, set C=1 if there is none
	;; preserve X and Y, A=first byte of encoded file name
next	jsr getbyt1
	jsr getbyt
	jsr getbyt
	jsr getbyt
	jmp prev2

	;; go to previous menu entry, set C=1 if there is none
	;; preserve X and Y, A=first byte of encoded file name
prev	jsr getbyt1
prev2	sta tmpos
	jsr getbyt
	sta tmpos+1
	jsr getbyt
	sta tmpos+2
	ora #$c0		; mask the status bits
	.(
	eor #$ff
	bne nonempty
	lda tmpos+1
	eor #$ff
	bne nonempty
	lda tmpos
	eor #$ff
	bne nonempty
	sec
	rts
nonempty
	.)
	lda tmpos
	sta menupos
	lda tmpos+1
	sta menupos+1
	lda tmpos+2
	sta menupos+2
	;; re-read the first character of current menu entry
curr	jsr getbyt1		; skip the prev and next links
	jsr getbyt
	jsr getbyt
	sta typefl		; store the MSB of prev link
	jsr getbyt
	jsr getbyt
	jsr getbyt
	sta typefl+1		; store the MSB of next link
	jsr getbyt		; read the first byte of the menu entry
	clc
	rts			; return the first byte of the menu entry

	;; convert the address and read the first character to A
getbyt1	lda menupos
	sta INPOS
	lda menupos+1
	sta ultimem_blk3
	and #$1f
	ora #$60
	sta INPOS+1
	lda menupos+2
	and #$3f		; FIXME remove this (use 24 bits!)
	lsr
	ror ultimem_blk3
	lsr
	ror ultimem_blk3
	lsr
	ror ultimem_blk3
	lsr
	ror ultimem_blk3
	lsr
	ror ultimem_blk3
	sta ultimem_blk3+1
	;; read a byte from the ROM via BLK3
getbyt
	jsr INPOS-1		; read the byte
	.(
	inc INPOS
	bne gotbyt
	inc INPOS+1
	bpl gotbyt
	pha
	lda #>$6000		; wrap around to the start of BLK3
	sta INPOS+1
	pla
	inc ultimem_blk3
	bne gotbyt
	inc ultimem_blk3+1
gotbyt
	.)
	rts

	;; raster interrupt handler
irq_p	lda $9124	; get the Timer A value
	.(
	sec		; (42 + 32 to 49 + 32 cycles delay at this stage)
	sbc #<TIMER_VALUE_P - 49 - 32 + VIA_RELOAD_TIME
	ldx #LINECYCLES_P * 8 / 5 ; delay to wait between changing colours
	cmp #8	; are we more than 7 cycles ahead of time?
	bcc save8
	pha	; yes, spend 8 extra cycles
	pla
	and #7	; and reset the high bit
save8	cmp #4
	bcc save4
	bit $24	; waste 4 cycles
	and #3
save4	cmp #2	; spend the rest of the cycles
	bcs *+2
	bcs *+2
	lsr
	bcs *+2
	.)
	;; now it has taken a fixed amount of cycles since the IRQ event
	lda #cBgS
	sta $900f
	lda #cBg
	dex
	bne *-1
	sta $900f
	jsr $ff9f	       ; read the keyboard
	pla
	tay
	pla
	tax
	pla
	rti
irq_n	lda $9124	; get the Timer A value
	.(
	sec		; (42 + 32 to 49 + 32 cycles delay at this stage)
	sbc #<TIMER_VALUE_N - 49 - 32 + VIA_RELOAD_TIME
	ldx #LINECYCLES_N * 8 / 5 ; delay to wait between changing colours
	cmp #8	; are we more than 7 cycles ahead of time?
	bcc save8
	pha	; yes, spend 8 extra cycles
	pla
	and #7	; and reset the high bit
save8	cmp #4
	bcc save4
	bit $24	; waste 4 cycles
	and #3
save4	cmp #2	; spend the rest of the cycles
	bcs *+2
	bcs *+2
	lsr
	bcs *+2
	.)
	;; now it has taken a fixed amount of cycles since the IRQ event
	lda #cBgS
	sta $900f
	lda #cBg
	dex
	bne *-1
	sta $900f
	jsr $ff9f	       ; read the keyboard
	pla
	tay
	pla
	tax
	pla
	rti

	;; routine to invalidate an a0CBM cartridge auto-start signature
	;; just before invoking decrunch
invalidate	= *-($a000-$6000)
	.(
	lda #$90
	sta ultimem_blk		; enable writeable RAM at BLK5, ROM at BLK3
	jsr $fd3f		; check if a signature is present
	bne done
	stx $a008		; invalidate the auto-start signature
done	sty ultimem_blk
	rts
	.)

	;; select a subdirectory
subdir	lda #DIRENTSIZE-1
	adc menupos		; C=1
	sta menupos
	lda menupos+1
	adc #0
	sta menupos+1
	lda menupos+2
	adc #0
	sta menupos+2
	jsr prev
	bcc nolaunch
	lda menupos		; invalid subdirectory pointer: restore it
	sbc #DIRENTSIZE		; C=1
	sta menupos
	lda menupos+1
	sbc #0
	sta menupos+1
	lda menupos+2
	sbc #0
	sta menupos+2
nolaunch
	pla			; discard the keypress ($0d or $8d)
	jmp update

	;; Uncrunch and launch the current program (at menupos)
launch	pha			; remember the keypress ($0d or $8d)
	jsr curr
	.(
	lda typefl
	asl
	bcc nosubdir
	bpl dolaunch		; 10=cartridge
				; 11=subdirectory if typefl+1=11
	bit typefl+1		; check the memory expansion bits
	bpl nolaunch
	bvs subdir		; 11=ok for subdirectory
	bvc nolaunch
nosubdir
	bpl nolaunch		; 00=reserved
				; 01=BASIC program
	bit typefl+1
	bmi dolaunch		; 10=24k, 11=no expansion
	bvc nolaunch		; 00=reserved
dolaunch
	.)
	sei
	asl
	lda typefl+1
	php			; C=1 if BASIC program, C=0 if cartridge
	pha
	;; Invalidate any a0CBM cartridge auto-start signature, so
	;; that we can load images that lack the signature and
	;; partially overwrite a previously loaded cartridge image.
	ldy #0
	sty ultimem_blk3
	sty ultimem_blk3+1
	ldy #50
	sty ultimem_blk		; ROM at BLK5 and BLK3
	jsr invalidate
	;; copy the decruncher code
	.(
	ldx #block_stack_end-block_stack_
loop	lda block_stack-1,x
	sta !block_stack_-1,x	; prohibit zero page addressing
	dex
	bne loop
	.)
	stx $9002		; blank the screen (zero columns)
	lda #$7f
	sta $913d		; disable VIA interrupts
	sta $913e		; acknowledge VIA interrupts

	;; Set the start address of the compressed data
	.(
	ldx #NAMEBYTES
loop	jsr getbyt		; skip the rest of the encoded file name
	dex
	bne loop
	.)

	sta OUTWARP		; end address high of first output block
	jsr getbyt
	sta esc			; starting escape

	jsr getbyt		; read startAddr
	sta OUTPOS
	jsr getbyt
	sta OUTPOS+1
	jsr getbyt		; read # of escape bits
	sta escB0+1
	sta escB1+1
	lda #8
	sec
	sbc escB1+1
	sta noesc+1		; 8-escBits

	jsr getbyt
	sta mg+1		; maxGamma + 1
	lda #9
	sec
	sbc mg+1		; 8 - maxGamma == (8 + 1) - (maxGamma + 1)
	sta longrle+1
	jsr getbyt
	sta mg1+1		; (1<<maxGamma)
	asl
	clc
	sbc #0
	sta mg21+1		; (2<<maxGamma) - 1
	jsr getbyt
	sta elzpb+1

	jsr getbyt		; rleUsed
	ldx #0
	tay
	.(
loop	beq done		; Y == 0 ?
	jsr getbyt
	sta table,x		; initialize the RLE table
	inx
	dey
	bne loop
done	.)
	lda #$80
	sta bitstr
	lda #$6a		; ror
	sta INPOSr
	pla			; memory expansion type (typefl+1)
	asl			; => 01=3k, 10=24k, 11=no expansion
	bcc use3k		; 00 or 01
	bpl use8k		; 10
use0k	lda #0;cfg0k
	.byte $2c
use3k	lda #0;cfg3k
	.byte $2c
use8k	lda #0;cfg8k
	sta INCFG1
;	ldx INBANK
	stx bankreg
	jsr decrunch
	lda OUTPOS
	sta bend		; copy the end address
	lda OUTPOS+1
	sta bend+1
	plp			; C=1 if BASIC program, C=0 if cartridge
	pla			; the keypress ($0d or $8d)
	and #$80
	beq use_no_dis
	;lda #cfg_dis|cfg_wp	; I/O disable and BLK5 write protect
use_no_dis
	ora INCFG1		; disable the I/O register on Shift+RETURN
	tay
	jsr cpreset		; copy the trampoline code for starting
	bcs ibasic		; C=1 if BASIC program, C=0 if cartridge
	;; auto-start cartridge => jump to the normal RESET routine
	lda #>oreset
	pha
	lda #<oreset
	pha			; start address
	lda #4			; Interrupt flag set, others clear
	bcc invoke		; branch always
	;; not an auto-start cartridge
ibasic	tya
	;and #$ff-cfg_dis-cfg_wp	; mask out the disable bits
	tay
	lda #$20
	sta $c2			; end of RAM, normally at $2000
	lda #$1e		; A => screen at $1e00
	ldx #$10		; X => BASIC at $1000
	stx $97			; start of RAM, normally at $1000
	;cpy #cfg0k		; cf. use0k
	;beq lexp0k
	;cpy #cfg3k		; cf. use3k
	;beq lexp3k
	;; expanded 8k+
	txa			; A => screen at $1000
	ldx #$12		; X => BASIC at $1200
	ldy #$80		; Y => BASIC ends at $8000
	sty $c2			; end of RAM at $8000
	bne lptrset		; branch always
lexp3k	;; expanded 3k
	ldx #$04		; X => BASIC at $0400
	stx $97			; start of RAM at $0400
lexp0k	tay			; Y => BASIC ends at $1e00
lptrset	sta screen
	stx membot
	sty memtop

	jsr $ff8a		; initialize the KERNAL jump vectors
	jsr $fdf9		; initialize the I/O chips
	jsr $e518		; initialize the screen
	jsr $e45b		; initialize jump vectors for BASIC
	jsr $e3a4		; initialize zero page for BASIC
	lda $2b
	ldy $2c
	jsr $c408		; check memory overlap
	jsr $c659		; CLR

	lda #>warmstt
	pha
	lda #<warmstt
	pha			; start address
	lda #0			; all flags clear
invoke	pha			; status register
	jmp block_reset_

	;; clear the low RAM, copy the RESET code, blank the screen
	;; restore normal key repeat and set the cassette buffer pointer
cpreset	sei
	tsx
	lda #0
	sta repeat		; restore normal key repeat
	.(
loop	dex
	sta $100,x		; clear the stack
	bne loop
	.)

	.(
loop	sta $200,x		; clear $200 to $27f
	inx
	bpl loop
	.)

	lda #<$33c		; initialize the cassette buffer pointer
	sta $b2
	lda #>$33c
	sta $b3

	ldx #block_reset_end-block_reset_
	.(
loop	lda block_reset-1,x
	sta !block_reset_-1,x	; prohibit zero page addressing
	dex
	bne loop
	.)
	stx $9002		; blank the screen (zero columns)
	rts

;	.dsb $c000-*, $ff
image_end
; Singly linked list of free memory, terminated by $ffffff
; (meaning that all memory following the $ffffff is free)
;freemem
;	.byte $ff, $ff, $ff	;all memory free
;rootmenu
	;; 3 bytes => directory entry type + link to previous entry
	;; b23..b22 => type of directory entry
	;;  00=reserved
	;;  01=BASIC program
	;;  10=cartridge
	;;  11=subdirectory (pointer to first entry follows)
	;; b21..b0 => start address of previous entry; 0x3fffff=none
;	.byte $ff, $ff, $ff	; no previous entry
	;; 3 bytes => memory expansion type + link to next entry
	;; b23..b22 => type of memory expansion (ignored for subdirectories)
	;;  00=reserved
	;;  01=3k
	;;  10=24k
	;;  11=no expansion (or the entry is a subdirectory)
	;;  There will be RAM at BLK5 in any case.
	;; b21..b0 => start address of next entry; or 0x3fffff=none
;	.byte $ff, $ff, $ff	; no next entry
	;; this is a subdirectory => include link to the subdirectory
;	.byte $ff, $ff, $ff	; subdirectory pointer (none)
