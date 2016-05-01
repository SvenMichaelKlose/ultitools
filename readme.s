;;; @file readme.s
;;; Instructions for the UltiMem menu.s
;;; @author Marko Mäkelä (marko.makela@iki.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2010,2015 Marko Mäkelä (marko.makela@iki.fi)
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

CR	= $0d
LIST	= $9b
RVS_ON	= $12
RVS_OFF	= $92
CLR	= $93
UP	= $91

#define RETURN	RVS_ON,$d2,$c5,$d4,$d5,$d2,$ce,RVS_OFF
#define SHIFT	RVS_ON,$d3,$c8,$c9,$c6,$d4,RVS_OFF
#define STOP	RVS_ON,$d3,$d4,$cf,$d0,RVS_OFF
#define CRSR	RVS_ON,$c3,$d2,$d3,$d2,RVS_OFF
#define HOME	RVS_ON,$c8,$cf,$cd,$c5,RVS_OFF
#define RESET	$d2,$c5,$d3,$c5,$d4

	.word $1001
	*=$1001
prg	.word nextln
	.word 2015
	.byte LIST,$3a,$22,CR

	.byte CLR,$0e," ** ", $d6, "IC-20 ", $d5, "LTI", $cd, "EM **",CR
	.byte "     INSTRUCTIONS",CR,CR
	.byte STOP,",",RVS_ON,"3",RVS_OFF,",",RVS_ON,"8",RVS_OFF," EXIT TO BASIC"
	.byte "+",SHIFT," HIDE THE EXP.",CR,CR
	.byte $ce, "AVIGATE THE MENU WITH"
	.byte CRSR,",",HOME,","
	.byte RVS_ON,"A",RVS_OFF,"-",RVS_ON,"Z",RVS_OFF,","
	.byte RVS_ON,$c1,RVS_OFF,"-",RVS_ON,$da,RVS_OFF, ", HIT"
	.byte RETURN," TO SELECT.",CR,CR
	.byte $d0, "RESS ",SHIFT,"+",RETURN, " FOR"
	.byte "MAXIMUM COMPATIBILITY.",CR,CR
	.byte $d0, "USH ",RESET," FOR MENU OR",CR,0
nextln	.word lastln
	.word 2015
	.byte $22,CR,UP,"POKE40946,64:SYS64802",$0d,$0d,$0d
	.byte "WWW.IKI.FI/MSMAKELA/8BIT/VFP/"
	.byte UP,UP,UP,UP,UP,UP,UP,0
lastln	.word 0
