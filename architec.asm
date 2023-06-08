		NAME	Architecture
		LOCALS	@@

;*****************************************************************************
; Copyright (C) 1991,2011 Jan-Pascal van Best <janpascal@vanbest.org>
; 
; This file is part of MicroProgramming.
; 
; MicroProgramming is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; MicroProgramming is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with MicroProgramming.  If not, see <http://www.gnu.org/licenses/>.
;*****************************************************************************

		ASSUME	CS:CSeg, DS:DSeg, SS:_Stack

MicroMemSize	EQU	0100h
MainMemSize	EQU	1000h
MaxWordSize	EQU	32

TopOfStack	EQU	4021
InputRegister		EQU	4092
OutputRegister		EQU	4094
InputStatusRegister	EQU	4093
OutputStatusRegister	EQU	4095

StdIn		EQU	0000h
StdOut		EQU	0001h

;*****************************************************************************

DSeg		SEGMENT	PARA PUBLIC

MicroMem	EQU     0000h      			; DD	MicroMemSize DUP (0)
MainMem		EQU	(MicroMem + 4*MicroMemSize)	; DW	MainMemSize DUP (0)

RegPC		EQU	(MainMem + 2*MainMemSize)	; DW	0000h
RegAC		EQU	(RegPC + 2)			; DW	?
COMMENT |
RegSP		DW      InputRegister
RegIR		DW	?
RegTIR		DW	?
RegZero		DW	0
RegPlusOne	DW	1
RegMinusOne	DW	-1
RegAMASK	DW	0FFFh
RegSMASK	DW	00FFh
RegA		DW	?
RegB		DW	?
RegC		DW	?
RegD		DW	?
RegE		DW	?
RegF		DW	?
|
BusC		EQU	(RegPC + 2*16)			; DW	?
LatchA		EQU	(BusC + 2)			; DW	?
LatchB		EQU	(LatchA + 2)			; DW	?
AMUX		EQU	(LatchB + 2)			; DW	?
ALU		EQU	(AMUX + 2)			; DW	?
NFlag		EQU	(ALU + 2)			; DB	?
ZFlag		EQU	(NFlag + 1)			; DB	?
MAR		EQU	(ZFlag + 1)			; DW	?
MBR		EQU	(MAR + 2)			; DW	?

MMux		EQU	(MBR + 2)			; DB	?
MPC		EQU	(MMux + 1)			; DB	?
MIR		EQU	(MPC + 1)			; DD	?

ReadBusy	EQU	(MIR + 4)			; DB	?
WriteBusy	EQU	(ReadBusy + 1)			; DB	?

OutputChar	EQU	(WriteBusy + 1)

DSeg		ENDS

;*****************************************************************************

CSeg		SEGMENT	PARA PUBLIC

		PUBLIC	DoFirstCycle
		PUBLIC	DoSecondCycle
		PUBLIC	DoThirdCycle
		PUBLIC	DoFourthCycle
;                PUBLIC	InitData

;               EXTRN   OutputData: FAR

ALUJumpTable	DW	Offset ALUAdd
		DW	Offset ALUAnd
		DW	Offset ALUCopy
		DW	Offset ALUNot

ShiftJumpTable	DW	Offset ShiftNot
		DW	Offset ShiftRight
		DW	Offset ShiftLeft
		DW	Offset ShiftNot

MMuxJumpTable	DW	Offset MMuxNever
		DW	Offset MMuxOnN
		DW	Offset MMuxOnZ
		DW	Offset MMuxAlways

DataSegment	DW	?

;=============================================================================

PrintString	PROC	NEAR

		PUSH	DS
		PUSH	CS
		POP	DS

		CLD
@@Loop:
		LODSB
		TEST	AL, AL
		JZ	@@Exit
		PUSH	SI
		MOV	AH, 0Eh
		XOR	BX, BX
		INT	10h
		POP	SI
		JMP	@@Loop
@@Exit:
		POP	DS
		RET
PrintString	ENDP

;=============================================================================

HandleInput	PROC	NEAR

		MOV	SI, 2*InputStatusRegister
		TEST	WORD PTR DS:MainMem[SI], 8000h	; Input still pending ?
		JNZ	@@Exit

		MOV	AH, 06h
		MOV	DL, 0FFh
		JZ	@@Exit

		MOV	SI, 2*InputRegister
		XOR	AH, AH
		MOV	WORD PTR DS:MainMem[SI], AX

		MOV	SI, 2*InputStatusRegister
		OR	WORD PTR DS:MainMem[SI], 8000h

@@Exit:
		RET
HandleInput	ENDP

;=============================================================================

PrintHexDigit	PROC	NEAR

		CMP	DL, 0Ah
		JB	@@NoLetter
		ADD	DL, 'A'-'0'-0Ah
@@NoLetter:
		ADD	DL, '0'
		MOV	AH, 06h
		INT	21h			; Output to screen

		RET
PrintHexDigit	ENDP

;=============================================================================

PrintHexByte	PROC	NEAR

		PUSH	DX
		MOV	CL, 4
		SHR	DL, CL
		CALL	PrintHexDigit

		POP	DX
		AND	DL, 0Fh
		CALL	PrintHexDigit

		RET
PrintHexByte	ENDP

;=============================================================================

HandleOutput	PROC	NEAR

		MOV	SI, 2*OutputStatusRegister
		TEST	WORD PTR DS:MainMem[SI], 8000h	; Output pending ?
		JZ	@@Exit			; No.

		AND	WORD PTR DS:MainMem[SI], 7FFFh	; Clear flag

		MOV	SI, 2*OutputRegister
		MOV	AX, WORD PTR DS:MainMem[SI]	; Get Data
		MOV	BYTE PTR DS:OutputChar, AL		; Store for output

;                PUSH	DS
;                MOV	DS, CS:DataSegment
;                PUSH    AX
;
;                CALL    OutputData
;
;                POP	DS

;		PUSH	DX
;		MOV	DL, DH
;		CALL	PrintHexByte
;		POP	DX
;		CALL	PrintHexByte

@@Exit:
		RET
HandleOutput	ENDP


;=============================================================================

ALUAdd		PROC	NEAR

		ADD	AX, DX

		RET
ALUAdd		ENDP

;=============================================================================

ALUAnd		PROC	NEAR

		AND	AX, DX

		RET
ALUAnd		ENDP

;=============================================================================

ALUCopy		PROC	NEAR

		TEST	AX, AX

		RET
ALUCopy		ENDP

;=============================================================================

ALUNot		PROC	NEAR

		NOT	AX

		RET
ALUNot		ENDP

;=============================================================================

ShiftNot	PROC	NEAR

		RET
ShiftNot	ENDP

;=============================================================================

ShiftRight	PROC	NEAR

		SHR	AX, 1

		RET
ShiftRight	ENDP

;=============================================================================

ShiftLeft	PROC	NEAR

		SHL	AX, 1

		RET
ShiftLeft	ENDP

;=============================================================================

MMuxNever	PROC	NEAR

		RET
MMuxNever	ENDP

;=============================================================================

MMuxOnN  	PROC	NEAR

		CMP	BYTE PTR DS:NFlag, 1
		JNE	@@NoBranch
		MOV	AL, DL
@@NoBranch:
		RET
MMuxOnN		ENDP

;=============================================================================

MMuxOnZ  	PROC	NEAR

		CMP	BYTE PTR DS:ZFlag, 1
		JNE	@@NoBranch
		MOV	AL, DL
@@NoBranch:
		RET
MMuxOnZ		ENDP

;=============================================================================

MMuxAlways  	PROC	NEAR

		MOV	AL,DL

		RET
MMuxAlways	ENDP

;=============================================================================

DoFirstCycle	PROC	NEAR

		; Clockcycle 1 : Fetch micro-instruction

		PUSH	DS
		POP	ES
		CLD

		MOV	AL, BYTE PTR DS:MPC
		XOR	AH, AH
		SHL	AX, 1
		SHL	AX, 1
		MOV	SI, AX	; SI -> next instruction
		MOV	DI, MIR
		MOV	CX, 0002h
	REP	MOVSW			; Move next micro-instr. to MIR

		RET
DoFirstCycle	ENDP

;=============================================================================

DoSecondCycle	PROC	NEAR

		; Clockcycle 2 : Load latches A and B

		MOV	AL, BYTE PTR DS:MIR[1]	; BA-fields
		AND	AX, 000Fh		; Isolate A-field
		SHL	AL, 1			; Registers are words
		MOV	SI, AX
		MOV	AX, RegPC[SI]		; PC is first register
		MOV	WORD PTR DS:LatchA, AX

		MOV	AL, BYTE PTR DS:MIR[1]	; BA-fields
		AND	AX, 00F0h		; Isolate B-field
		SHR	AL, 1			; Divide by 16/2=8=2^3
		SHR	AL, 1
		SHR	AL, 1
		MOV	SI, AX
		MOV	AX, RegPC[SI]
		MOV	WORD PTR DS:LatchB, AX

		RET
DoSecondCycle	ENDP

;=============================================================================

DoThirdCycle	PROC	NEAR

		; Clockcycle 3 : calculate in ALU

		; 3a: Calculate AMUX

		MOV	AL, BYTE PTR DS:[MIR][3]   ; AMUX-field
		TEST	AL, AL
		JS	@@MBRToAMUX
		MOV	AX, WORD PTR DS:LatchA
		JMP	@@GoOnAMUX
@@MBRToAMUX:
		MOV	AX, WORD PTR DS:MBR
@@GoOnAMUX:
		MOV	WORD PTR DS:AMUX, AX

		; 3b: ALU-operation

		MOV	AX, WORD PTR DS:AMUX		; Load operands
		MOV	DX, WORD PTR DS:LatchB

		MOV	BL, BYTE PTR DS:[MIR][3]	; ALU-field
		AND	BX, 00011000b
		SHR	BX, 1
		SHR	BX, 1
		CALL	CS:ALUJumpTable[BX]
		MOV	WORD PTR DS:ALU, AX

		MOV	BYTE PTR DS:NFlag, 0
		JNS	@@NFlagZero
		MOV	BYTE PTR DS:NFlag, 1
@@NFlagZero:

		MOV	BYTE PTR DS:ZFlag, 0
		JNZ	@@ZFlagZero
		MOV	BYTE PTR DS:ZFlag, 1
@@ZFlagZero:

		; 3c: Copy LatchB to MAR if needed.

		TEST	BYTE PTR DS:MIR[2], 80h	; bit 7=MAR bit
		JZ	@@NoMAR
		MOV	AX, WORD PTR DS:LatchB
		AND	AX, 0FFFh
		MOV	WORD PTR DS:MAR, AX
@@NoMAR:

		; 3d: Shifter-operation

		MOV	AX, WORD PTR DS:ALU

		MOV	BL, BYTE PTR DS:[MIR][3]
		AND	BX, 00000110b        	; SHIFT-field
		CALL	CS:ShiftJumpTable[BX]
		MOV	WORD PTR DS:BusC, AX

		; 3e: Calculate MMux and MPC

		MOV	BL, BYTE PTR DS:[MIR][3]	; COND-field
		AND	BX, 01100000b
		MOV	CL, 4
		SHR	BX, CL

		MOV	AL, BYTE PTR DS:MPC
		INC	AL
		MOV	DL, BYTE PTR DS:MIR[0] ; ADDR field
		CALL	CS:MMuxJumpTable[BX]
		MOV	BYTE PTR DS:MMux, AL
		MOV	BYTE PTR DS:MPC, AL

		RET
DoThirdCycle	ENDP

;=============================================================================

DoFourthCycle	PROC	NEAR

		; Clockcycle 4: store bus C.

		MOV	AX, WORD PTR DS:BusC

		; 4a: store to MBR

		TEST	BYTE PTR DS:[MIR][3], 01	; MBR-bit
		JZ	@@NoStoreMBR
		MOV	WORD PTR DS:MBR, AX
@@NoStoreMBR:

		; 4b: store to register

		TEST	BYTE PTR DS:MIR[2], 10h	; ENC-bit
		JZ	@@NoStoreReg

		MOV	BL, BYTE PTR DS:MIR[2]	; C-field
		AND	BX, 000Fh
		SHL	BX, 1
		MOV	RegPC[BX], AX
@@NoStoreReg:

		; Memory management: Read

		TEST	BYTE PTR DS:MIR[2], 40h	; rd-bit
		JZ	@@NoRead

		CMP	BYTE PTR DS:ReadBusy, 01h
		JE	@@ReadContinues

		MOV	BYTE PTR DS:ReadBusy, 01h
		JMP	@@ReadExit
@@ReadContinues:
		MOV	BX, WORD PTR DS:MAR			; perform read
		SHL	BX, 1
		MOV	AX, WORD PTR DS:MainMem[BX]
		MOV	WORD PTR DS:MBR, AX
@@NoRead:
		MOV	BYTE PTR DS:ReadBusy, 00h
@@ReadExit:

		; Memory management: Write

		TEST	BYTE PTR DS:MIR[2], 20h	; wr-bit
		JZ	@@NoWrite

		CMP	BYTE PTR DS:WriteBusy, 01h
		JE	@@WriteContinues

		MOV	BYTE PTR DS:WriteBusy, 01h
		JMP	@@WriteExit
@@WriteContinues:
		MOV	BX, WORD PTR DS:MAR			; perform Write
		CMP	BX, OutputRegister
		JNE	@@NoOutput
		MOV	SI, 2*OutputStatusRegister
		OR	WORD PTR DS:MainMem[SI], 8000h
@@NoOutput:
		SHL	BX, 1
		MOV	AX, WORD PTR DS:MBR
		MOV	WORD PTR DS:MainMem[BX], AX
@@NoWrite:
		MOV	BYTE PTR DS:WriteBusy, 00h
@@WriteExit:

		; I/O management

		CALL	HandleInput
		CALL	HandleOutput

		RET
DoFourthCycle	ENDP


InitData	PROC	NEAR DSegment:Word

		PUSH	BP
                MOV	BP, SP

		MOV	AX, DSegment
                MOV	CS:DataSegment, AX

                POP	BP
                RET	0002h

InitData	ENDP


CSeg		ENDS

;*****************************************************************************

_Stack		SEGMENT PARA STACK

		DW	0200h DUP (?)

_Stack		ENDS

		END
