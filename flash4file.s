;;; @file flash4file.s
;;; Flash programming software for the Vic UltiMem
;;; @author Marko Mäkelä (marko.makela@iki.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2003,2010,2011,2012,2015 Marko Mäkelä (marko.makela@iki.fi)
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

flash	= $6000			; base address of the flash ROM (BLK3)
ultimem	= $9ff0			; Vic UltiMem control registers

MFR_ID	= $c2			; manufacturer ID
DEV_ID	= $c9			; device ID

f000	= flash			; flash address 0
f001	= flash + 1		; flash address 1
f555	= flash + $555		; flash address $555
f2aa	= flash + $2aa		; flash address $2aa

st	= $90			; I/O status word
curlf	= $b8			; current logical file
curfa	= $ba			; current first address (device number)
cursa	= $b9			; current secondary address
crsrchr	= $d1			; cursor position in screen memory
crsr_x	= $d3			; current cursor column
crsrcol	= $f3			; cursor position in color memory
txtcolor= 646			; current text color
pout	= $fb			; pointer to flash address (2 bytes)
size	= $fd			; size of block being programmed (16 bits)
tmp	= $ff			; auxiliary memory

;;; I/O routines
strout	= $cb1e			; output a NUL-terminated string
setmsg	= $ff90			; control KERNAL messages
setlfs	= $ffba			; set logical, first and second address
b_setnam= $e254			; parse and set file name in BASIC
;setnam	= $ffbd			; set file name
open	= $ffc0			; open a file
close	= $ffc3			; close a specified logical file
chkin	= $ffc6			; open channel for input
clrchn	= $ffcc			; clear input and output redirection
chrin	= $ffcf			; input character from channel
chrout	= $ffd2			; output a character

cTEXT	= 1			; white text color
cPROG	= 5			; green progress indicator color
cOK	= 0			; black screen signifies OK color
cBUSY	= 1			; white border signifies a wait
cERR	= 2			; red border signifies error
color	= $900f			; color register

;;; set the screen color to c
#define setcolor(c) lda #8+c:sta color
;;; print a NUL-terminated message
#define printmsg(x) lda#<x:ldy#>x:jsr strout
#define printmsg_rts(x) lda#<x:ldy#>x:jmp strout

	.word $1001
	*=$1001
prg	.word nextln
	.word 2015
	.byte $9e		; SYS
	.byte $30 + (start / 1000)
	.byte $30 + ((start - (start / 1000 * 1000)) / 100)
	.byte $30 + ((start - (start / 100 * 100)) / 10)
	.byte $30 + (start - (start / 10 * 10))
	.byte $22		; image file name in quotes
fn
	.byte "ULTIMEM.BIN"
	.byte $22
	.dsb fn-*+17, $3a
	.byte 0
nextln	.word 0
start	sei
	lda ultimem+2
	sta ultimem2_save
	lda ultimem+12
	sta ultimem12_save
	lda ultimem+13
	sta ultimem13_save
	setcolor(cOK)
	lda #cTEXT
	sta txtcolor
	lda #$10
	sta ultimem+2		; BLK3 flash ROM enable
	lda #0
	sta ultimem+12		; BLK3 bank select low
	sta ultimem+13		; BLK3 bank select high
	ldy #$f0
	sty flash		; poke x, $f0 (read array data)
	ldx #$90		; $90 (autoselect)
	jsr command
	lda f000		; read 0 (manufacturer id)
	cmp #MFR_ID
	beq mfr_ok

	sty flash		; poke x, $f0 (read array data)
	pha
	printmsg(mfr_mismatch)
	pla
	jsr hex2
	jmp error_println_exit

mfr_ok
	lda f001		; read 1 (device id)
	sty flash		; poke x, $f0 (read array data)
	cmp #DEV_ID
	beq dev_ok

	pha
	printmsg(dev_mismatch)
	pla
	jsr hex2
	jmp error_println_exit

dev_ok
	jsr b_setnam		; setnam
	ldx #1
	stx curlf
	dex
	stx cursa
	lda #$c0
	jsr setmsg
	jsr open
	bcc open_ok
	jmp file_read_error
open_ok	ldy st
	bne file_read_error
	ldx #1
	jsr chkin
	bcc next_op
	jmp error_println_exit
next_op	jsr read		; read the next op code
	jsr op_do		; process it
	bit st
	bvc next_op
	setcolor(cOK)
	printmsg(operation_complete)
	jmp exit

read	bit st
	bvs read_fail		; end of file
	jsr chrin
	ldy st
	bne read_fail
read_ok	rts
read_fail
	pha
	tya
	cmp #$40		; got last byte?
	pla
	beq read_ok
	pla
	pla
file_read_error
	printmsg(file_read)
error_println_exit
	lda #$0d		; output a carriage return (line break)
	jsr chrout
	setcolor(cERR)		; flag error status
exit	lda #1
	jsr close
	jsr clrchn
	lda #0			; exit
ultimem2_save = *-1
	sta ultimem+2
	lda #0
ultimem12_save = *-1
	sta ultimem+12
	lda #0
ultimem13_save = *-1
	sta ultimem+13
	cli
	rts

	;; execute the operation in .A
op_do	pha
	jsr hex2
	lda #$20
	jsr chrout
	pla
	cmp #(ops-optab)/2
	bcs op_unknown
	asl
	tax
	lda optab+1,x
	pha
	lda optab,x
	pha
	rts
op_unknown
	printmsg(unknown_op)
	pla			; discard the return address
	pla			; (return to caller's caller)
	jmp error_println_exit

	;; program data
program	jsr read		; parameter => size (16 bits)
	sta size
	jsr read
	sta size+1
	jsr read		; parameter => address (24 bits)
	sta pout
	jsr read
	sta pout+1
	jsr read
	sta tmp
	lda pout+1
	asl
	rol tmp
	asl
	rol tmp
	asl
	rol tmp
	sta ultimem+12
	lda tmp
	sta ultimem+13
	lda pout+1
	and #$1f
	ora #>flash
	sta pout+1
	setcolor(cBUSY)
	printmsg(programming)
	lda size+1
	jsr hex2
	lda size
	jsr hex2
	printmsg(programming_size)
	jsr printaddr
	printmsg(programming_size_end)
	inc size
	inc size+1
	ldx #0
prog	clc
	dec size		; all bytes programmed?
	bne progn
	dec size+1
	beq op_complete
progn	jsr read
	tay
	ldx #0
	eor (pout,x)
	beq progok		; already programmed (skip)
	ldx #$a0
	jsr command		; command (write byte)
	tya
	ldx #0
	sta (pout,x)		; We cannot use post-indexed mode here,
				; because it would read from the address first.
ppoll	lda f000		; poll the operation status
	eor f000
	and #$44		; bits 2 and 6 ($44) toggling?
	eor #$44
	beq ppoll		; yes, keep polling
	lda #$f0		; switch to reading array data
	sta flash		; poke x, $f0 (read array data)
	tya
	cmp (pout,x)		; verify the data
	bne verror
progok	inc pout		; increment the address, wrap if necessary
	bne prog
	inc pout+1
	bit pout+1
	bpl prog
	lda #>flash		; wrap the bank at $c000
	sta pout+1
	inc ultimem+12
	bne prog
	inc ultimem+13
	bne prog		; branch always
op_all_complete
	pla			; discard the return address
	pla			; (return to caller's caller)
	setcolor(cOK)
	printmsg(operation_complete)
	jmp exit
op_complete
	setcolor(cOK)
	printmsg_rts(operation_complete)

verror	pla			; discard the return address
	pla			; (return to caller's caller)
	printmsg(verify_failure)
	jsr printaddr
	printmsg(verify_failure_read)
	ldx #0
	lda (pout,x)
	jsr hex2
	jmp error_println_exit

erase	printmsg(erase_in_progress)
	ldx #$80		; command (erase)
	jsr command
	ldx #$10		; parameter (erase chip)
	jsr command
erasepoll
	setcolor(cBUSY)
	lda #cPROG
	ldy crsr_x
	sta (crsrcol),y		; set progress indicator color
epollx	ldx #progress_end - progress - 1
epoll	lda f000		; poll the device until operation complete
	eor f000
	and #$44		; bits 2 and 6 ($44) toggling?
	eor #$44
	bne erased		; no, completed
	lda progress,x
	sta (crsrchr),y
	dex
	bpl epoll
	bmi epollx
	ldy #$f0
	sty flash		; poke x, $f0 (read array data)
erased	ldx flash
	cpx #$ff
	beq op_complete
	pla			; discard the return address
	pla			; (return to caller's caller)
	txa
	pha
	printmsg(erase_failure)
	pla
	jsr hex2
	jmp error_println_exit

	;; erase a block
erase_blk
	printmsg(erase_sec_in_progress)
	jsr read
	pha
	jsr hex2
	lda #0
	sta tmp
	pla
	asl
	rol tmp
	asl
	rol tmp
	asl
	rol tmp
	sta ultimem+12
	lda tmp
	sta ultimem+13
	printmsg(erase_sec_in_progress_at)
	lda #0
	sta pout
	sta pout+1
	jsr printaddr
	printmsg(erase_in_progress2)
	ldx #$80		; command (erase)
	jsr command
	ldx #$30		; parameter (sector, specified with A23..A16)
	jsr command
	jmp erasepoll

	;; print the current flash ROM address
printaddr
	lda ultimem+12
	sta tmp
	lda ultimem+13		; read the most significant bits of the address
	asl tmp
	rol
	asl tmp
	rol
	asl tmp
	rol
	asl tmp
	rol
	asl tmp
	rol
	asl tmp
	rol
	jsr hex2
	lda pout+1
	and #$1f
	ora tmp
	jsr hex2
	lda pout
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

	;; issue a command in .X, trash .A
command	lda #$aa
	sta f555
	lda #$55
	sta f2aa
	stx f555		; poke $555, command
	rts
	;; progress indicator
progress
	.byte 109,112,110,125
progress_end

mfr_mismatch
	.byte "UNEXPECTED MANUFACTURER", 13, ", GOT ID ", 0
dev_mismatch
	.byte "UNEXPECTED DEVICE", 13, ", GOT ID ", 0
unknown_op
	.byte 28, "UNKNOWN OP", 5, 0
file_read
	.byte 28, "FILE READ FAILURE", 5, 0
erase_sec_in_progress
	.byte "ERASE SEC ", 0
erase_sec_in_progress_at
	.byte 13, "AT ", 0
erase_in_progress
	.byte "ERASE"
erase_in_progress2
	.byte ": ", 0
operation_complete
	.byte 30, "OK", 5, 13, 0
erase_failure
	.byte 28, "FAILED", 5, 13, "FIRST CELL IS ", 0
programming
	.byte "WRITE ", 0
programming_size
	.byte " BYTES", 13, "TO ", 0
programming_size_end
	.byte ": ", 0
verify_failure
	.byte 28, "FAILED", 5, 13, "AT ", 0
verify_failure_read
	.byte " READ: ", 0

	;; jump table of operation codes
optab	.word op_all_complete-1, erase-1, erase_blk-1, program-1
ops
