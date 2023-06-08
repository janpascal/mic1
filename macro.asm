MACRO.ASM: sample macro file for MacAsm 2-pass Mac-1 assembler.

		OUTPUT	= 4094
		OUTSTAT = 4095

                STACKTOP = 4021
                SPACE	= 32
		K	= 4020		/ Define some symbols
		X	= 4000
		Y	= 3980
		A	= 4
		B	= 3
		P	= 1
		J	= 0
		V	= 5
		RES	= 4
		SOM	= 1
		I	= 0

		JUMP	HOOFD
PROD:		DESP	2
		LODL	A
		JNZE	ANIETZ
		LOCO	0
                JUMP    KLAAR
ANIETZ:		LODL    B
		JNZE	BNIETZ
		LOCO	0
		JUMP	KLAAR
BNIETZ:		LOCO	0
		STOL	P
		LOCO	1
		STOL	J
		LODL	A
		JNEG	L2
		JZER	L2
L1:		LODL	P
		ADDL	B
		STOL	P
		LOCO	1
		ADDL	J
		STOL	J
		SUBL	A
		JNEG	L1
		JZER	L1
L2:		LODL	P
KLAAR:		INSP	2
		RETN

INPROD:		DESP	2
		LOCO	0
		STOL	SOM
		LOCO	1
		STOL	I
L3:		LOCO	X-1
		ADDL	I
		PSHI
		LODL	V
		ADDL	I
		SUBD	C1
		PSHI
		CALL	PROD
		INSP	2
		ADDL	SOM
		STOL	SOM
		LOCO	1
		ADDL	I
		STOL	I
		SUBD	C20
		JNEG	L3
		JZER	L3
		LODL	SOM
		PUSH
		LODL	RES
		POPI
		INSP	2
		RETN

HOOFD:          LOCO	STACKTOP
		SWAP
		DESP	41

                LOCO	STARTTEXT
                PUSH
                CALL	OUTSTR
                INSP	1

		LOCO	1
		STOD	K
L4:		LODD	K
		PUSH
		LOCO	X-1
		ADDD	K
		POPI
		LOCO	2
		PUSH
		LODD	K
		PUSH
		CALL	PROD
		INSP	2
		ADDD	C1
		PUSH
		LOCO	Y-1
		ADDD	K
		POPI
		LOCO	1
		ADDD	K
		STOD	K
		SUBD	C20
		JNEG	L4
		JZER	L4
		LOCO	Y
		PUSH
		LOCO	K
		PUSH
		CALL	INPROD
		INSP	2

                LOCO	STRING
                PUSH
                CALL	OUTSTR
                INSP	1

		LODD	K
		PUSH
		CALL	OUTWORD
		INSP	1

                CALL	CRLF
		HALT

C1:		DW	1
C4:		DW	4
C20:		DW	20
C16:		DW	16
C5555h:		DW	21845
C65535:		DW	65535

STARTTEXT:	DW	"C"
		DW	"A"
		DW	"L"
		DW	"C"
		DW	"U"
		DW	"L"
		DW	"A"
		DW	"T"
		DW	"I"
		DW	"N"
		DW	"G"
		DW	"."
		DW	"."
		DW	"."
		DW	13
		DW	10
                DW	0

STRING:		DW	"A"
		DW	"N"
                DW	"S"
		DW	"W"
                DW	"E"
		DW	"R"
		DW	SPACE
                DW	"I"
		DW	"S"
                DW	SPACE
                DW	0

HEXTABLE:	DW	"0"
		DW	"1"
                DW	"2"
		DW	"3"
                DW	"4"
		DW	"5"
                DW	"6"
		DW	"7"
                DW	"8"
		DW	"9"
                DW	"A"
		DW	"B"
                DW	"C"
		DW	"D"
                DW	"E"
		DW	"F"

		CHAR	= 1		/ Character to write on stack

OUTCHAR:	LODL	CHAR		/ Take the character
		STOD	OUTPUT		/ write it to the output register
		LODD	C65535          / set output status bit
                STOD	OUTSTAT
CHARLOOP:	LODD	OUTSTAT		/ read status
		JNEG	CHARLOOP	/ and wait till flag clear
		RETN


                NUM	= 2		/ number to write on stack
                COUNT   = 0             / Number of bits done

OUTNUM:         DESP    1               / space for COUNT on stack
		LOCO	0
		STOL    COUNT
OUTLOOP:        LODL	NUM		/ take the number
		JNEG	DOONE
                LOCO	"0"
                JUMP	GOON
DOONE:		LOCO	"1"
GOON:		PUSH			/ Parameter for OUTCHAR
		CALL	OUTCHAR
                INSP	1		/ Remove parameter from stack
                LODL	NUM		/ Retrieve number
                LSHF			/ Shift left for next bit
                STOL	NUM		/ And store back
                LOCO	1    		/ Increase COUNT
                ADDL	COUNT
                STOL	COUNT
		SUBD	C16
		JNEG	OUTLOOP
		INSP	1
                RETN

                NYBBLE	= 1

OUTNYBBLE:	LOCO	HEXTABLE
		ADDL    NYBBLE
                PSHI
                CALL	OUTCHAR
                INSP	1
                RETN

                HEXNYB	= 3
                TEMP	= 1
                NYBCOUNT = 0

OUTNYB:         DESP	2
		LOCO	0
		STOL	TEMP
                STOL	NYBCOUNT
HEXLOOP:	LODL	TEMP
                LSHF
                STOL	TEMP

		LODL	HEXNYB
                JNEG	ITSAONE
                LOCO	0
                JUMP	HEXGOON
ITSAONE:	LOCO	1
HEXGOON:	ADDL	TEMP
		STOL	TEMP

                LODL	HEXNYB
                LSHF
                STOL	HEXNYB

                LOCO	1
                ADDL	NYBCOUNT
                STOL	NYBCOUNT
                SUBD	C4
                JNEG    HEXLOOP

                LODL	TEMP
                PUSH
                CALL	OUTNYBBLE
                INSP	1

                INSP	2
                RETN

                WORD	= 2
                WORDCOUNT = 0

OUTWORD:	DESP	1
		LOCO	0
                STOL	WORDCOUNT
WORDLOOP:	LODL	WORD
                PUSH
                CALL	OUTNYB
                POP
                STOL	WORD

                LOCO	1
                ADDL	WORDCOUNT
                STOL	WORDCOUNT
                SUBD	C4
                JNEG	WORDLOOP

                INSP	1
                RETN


CRLF:		LOCO	13
		PUSH
                CALL	OUTCHAR
                LOCO	10
                PUSH
                CALL	OUTCHAR
                INSP	2
                RETN

		STR	= 1

OUTSTR:		LODL	STR		/ AC -> STR
		PSHI			/ PUSH FIRST CHAR IN STR
                POP
                JZER	EINDE
                PUSH
                CALL	OUTCHAR
                INSP	1
                LOCO	1
                ADDL	STR
                STOL	STR
                JUMP	OUTSTR
EINDE:		RETN
