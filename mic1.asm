		NAME	Mic1
		LOCALS	@@

		ASSUME	CS:Code, DS:Data, SS:_Stack

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

Data		SEGMENT	PARA PUBLIC

MicroMem	DD	MicroMemSize DUP (0)
MainMem		DW	MainMemSize DUP (0)

Registers	LABEL	WORD
RegPC		DW	0000h
RegAC		DW	?
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

BusC		DW	?
LatchA		DW	?
LatchB		DW	?
AMUX		DW	?
ALU		DW	?
NFlag		DB	?
ZFlag		DB	?
MAR		DW	?
MBR		DW	?

MMux		DB	?
MPC		DB	?
MIR		DD	?

ReadBusy	DB	0
WriteBusy	DB	0

IOBuffer	DB	?

PSPSegment	DW	?
MicroFileName	DB	"MICRO.MIC", 0
		DB	40h DUP (0)
MacroFileName	DB	"MACRO.MAC", 0
		DB	40h DUP (0)
Handle		DW	?
BinaryBuffer	DB	MaxWordSize DUP (?)
NewLineBuffer	DB	?
WordSize	DW	?
WordBytes	DW	?

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

ErrorTxt	DB	13,10, "Error reading files.", 13,10, 0
REadyTxt	DB	13,10, "Mic-1 program terminated.", 13,10, 0
ReadingText	DB	13,10, "Reading file ", 0
ExecutingText	DB	13,10, "Executing", 13,10, 0

Data		ENDS

;*****************************************************************************

Code		SEGMENT	PARA PUBLIC

;=============================================================================

PrintString	PROC	NEAR

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
		RET
PrintString	ENDP

;=============================================================================

HandleInput	PROC	NEAR

		MOV	SI, 2*InputStatusRegister
		TEST	MainMem[SI], 8000h	; Input still pending ?
		JNZ	@@Exit

		MOV	AH, 06h
		MOV	DL, 0FFh
		JZ	@@Exit

		MOV	SI, 2*InputRegister
		XOR	AH, AH
		MOV	MainMem[SI], AX

		MOV	SI, 2*InputStatusRegister
		OR	MainMem[SI], 8000h

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
		TEST	MainMem[SI], 8000h	; Output pending ?
		JZ	@@Exit			; No.

		AND	MainMem[SI], 7FFFh	; Clear flag


		MOV	SI, 2*OutputRegister
		MOV	DX, MainMem[SI]		; Get data
		PUSH	DX
		MOV	DL, DH
		CALL	PrintHexByte
		POP	DX
		CALL	PrintHexByte

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

		CMP	NFlag, 1
		JNE	@@NoBranch
		MOV	AL, DL
@@NoBranch:
		RET
MMuxOnN		ENDP

;=============================================================================

MMuxOnZ  	PROC	NEAR

		CMP	ZFlag, 1
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

InterpretMicro	PROC	NEAR

@@MainLoop:
		CMP	MPC, MicroMemSize-1
		JNE	@@GoOn

		RET

		; Clockcycle 1 : Fetch micro-instruction
@@GoOn:
		PUSH	DS
		POP	ES
		CLD

		MOV	AL, MPC
		XOR	AH, AH
		SHL	AX, 1
		SHL	AX, 1
		MOV	SI, AX	; SI -> next instruction
		MOV	DI, Offset MIR
		MOV	CX, 0002h
	REP	MOVSW			; Move next micro-instr. to MIR

		; Clockcycle 2 : Load latches A and B

		MOV	AL, BYTE PTR MIR[1]	; BA-fields
		AND	AX, 000Fh		; Isolate A-field
		SHL	AL, 1			; Registers are words
		MOV	SI, AX
		MOV	AX, Registers[SI]
		MOV	LatchA, AX

		MOV	AL, BYTE PTR MIR[1]	; BA-fields
		AND	AX, 00F0h		; Isolate B-field
		SHR	AL, 1			; Divide by 16/2=8=2^3
		SHR	AL, 1
		SHR	AL, 1
		MOV	SI, AX
		MOV	AX, Registers[SI]
		MOV	LatchB, AX

		; Clockcycle 3 : calculate in ALU

		; 3a: Calculate AMUX

		MOV	AL, BYTE PTR [MIR][3]   ; AMUX-field
		TEST	AL, AL
		JS	@@MBRToAMUX
		MOV	AX, LatchA
		JMP	@@GoOnAMUX
@@MBRToAMUX:
		MOV	AX, MBR
@@GoOnAMUX:
		MOV	AMUX, AX

		; 3b: ALU-operation

		MOV	AX, AMUX		; Load operands
		MOV	DX, LatchB

		MOV	BL, BYTE PTR [MIR][3]	; ALU-field
		AND	BX, 00011000b
		SHR	BX, 1
		SHR	BX, 1
		CALL	ALUJumpTable[BX]
		MOV	ALU, AX

		MOV	NFlag, 0
		JNS	@@NFlagZero
		MOV	NFlag, 1
@@NFlagZero:

		MOV	ZFlag, 0
		JNZ	@@ZFlagZero
		MOV	ZFlag, 1
@@ZFlagZero:

		; 3c: Copy LatchB to MAR if needed.

		TEST	BYTE PTR MIR[2], 80h	; bit 7=MAR bit
		JZ	@@NoMAR
		MOV	AX, LatchB
		AND	AX, 0FFFh
		MOV	MAR, AX
@@NoMAR:

		; 3d: Shifter-operation

		MOV	AX, ALU

		MOV	BL, BYTE PTR [MIR][3]
		AND	BX, 00000110b        	; SHIFT-field
		CALL	ShiftJumpTable[BX]
		MOV	BusC, AX

		; 3e: Calculate MMux and MPC

		MOV	BL, BYTE PTR [MIR][3]	; COND-field
		AND	BX, 01100000b
		MOV	CL, 4
		SHR	BX, CL

		MOV	AL, MPC
		INC	AL
		MOV	DL, BYTE PTR MIR[0] ; ADDR field
		CALL	MMuxJumpTable[BX]
		MOV	MMux, AL
		MOV	MPC, AL

		; Clockcycle 4: store bus C.

		MOV	AX, BusC

		; 4a: store to MBR

		TEST	BYTE PTR [MIR][3], 01	; MBR-bit
		JZ	@@NoStoreMBR
		MOV	MBR, AX
@@NoStoreMBR:

		; 4b: store to register

		TEST	BYTE PTR MIR[2], 10h	; ENC-bit
		JZ	@@NoStoreReg

		MOV	BL, BYTE PTR MIR[2]	; C-field
		AND	BX, 000Fh
		SHL	BX, 1
		MOV	Registers[BX], AX
@@NoStoreReg:

		; Memory management: Read

		TEST	BYTE PTR MIR[2], 40h	; rd-bit
		JZ	@@NoRead

		CMP	ReadBusy, 01h
		JE	@@ReadContinues

		MOV	ReadBusy, 01h
		JMP	@@ReadExit
@@ReadContinues:
		MOV	BX, MAR			; perform read
		SHL	BX, 1
		MOV	AX, MainMem[BX]
		MOV	MBR, AX
@@NoRead:
		MOV	ReadBusy, 00h
@@ReadExit:

		; Memory management: Write

		TEST	BYTE PTR MIR[2], 20h	; wr-bit
		JZ	@@NoWrite

		CMP	WriteBusy, 01h
		JE	@@WriteContinues

		MOV	WriteBusy, 01h
		JMP	@@WriteExit
@@WriteContinues:
		MOV	BX, MAR			; perform Write
		CMP	BX, OutputRegister
		JNE	@@NoOutput
		MOV	SI, 2*OutputStatusRegister
		OR	MainMem[SI], 8000h
@@NoOutput:
		SHL	BX, 1
		MOV	AX, MBR
		MOV	MainMem[BX], AX
@@NoWrite:
		MOV	WriteBusy, 00h
@@WriteExit:

		; I/O management

		CALL	HandleInput
		CALL	HandleOutput

		JMP	@@MainLoop

InterpretMicro	ENDP

;=============================================================================

GetFileName	PROC	NEAR

		STOSB
@@Loop:
		JCXZ	@@Ready
		LODSB
		DEC	CX
		CMP	AL, ' '
		JBE	@@NameAborted
		STOSB
		JMP	@@Loop
@@NameAborted:
		INC	CX
		DEC	SI
@@Ready:
		XOR	AL, AL
		STOSB

		RET
GetFileName	ENDP

;=============================================================================

ParseCmdLine	PROC	NEAR

		PUSH	DS

		PUSH	DS
		POP	ES			; ES -> Data
		MOV	DS, ES:PSPSegment	; DS -> PSP

		MOV	CL, DS:[0080h]
		TEST	CL, CL
		JZ	@@Exit

		MOV	SI, 0081h
		XOR	CH, CH
		CLD
@@Loop1:
		LODSB
		CMP	AL, ' '
		JA	@@GetMicroName
		LOOP	@@Loop1
		JMP	@@Exit

@@GetMicroName:
		DEC	CX
		MOV	DI, Offset MicroFileName
		CALL	GetFileName

@@Loop2:
		JCXZ	@@Exit
		LODSB
		CMP	AL, ' '
		JA	@@GetMacroName
		LOOP	@@Loop2
		JMP	@@Exit

@@GetMacroName:
		DEC	CX
		MOV	DI, Offset MacroFileName
		CALL	GetFileName

@@Exit:
		POP	DS
		RET
ParseCmdLine	ENDP

;=============================================================================

SkipNewLine	PROC	NEAR

@@Loop:
		MOV	AH, 3Fh
		MOV	BX, Handle
		MOV	CX, 0001h
		MOV	DX, Offset NewLineBuffer
		INT	21h
		JC	@@Exit
		TEST	AX, AX
		JE	@@Exit

		CMP	NewLineBuffer, 0Ah	; Is it a Line Feed ?
		JNE	@@Loop
@@Exit:
		RET
SKipNewLine	ENDP

;=============================================================================

ReadOneFile	PROC	NEAR

		MOV	WordSize, CX
		MOV	AX, CX
		MOV	CL, 3
		SHR	AX, CL		; Divide word size to 8 to get # bytes.
		MOV	WordBytes, AX

		PUSH	DS
		POP	ES
		CLD

		; Open file  (DX 0->FileName)

		MOV	AX, 03D00h + 00100000b
		INT	21h
		JC	@@Exit

		MOV	Handle, AX

		; Skip first line

		CALL	SkipNewLine
		JC	@@ErrorExit

		; Read one word
@@FileLoop:
		MOV	AH, 3Fh
		MOV	BX, Handle
		MOV	CX, WordSize
		MOV	DX, Offset BinaryBuffer
		INT	21h
		JC	@@ErrorExit
		CMP	AX, CX
		JB	@@CloseFile	; End of File

		CALL	SkipNewLine
		JC	@@ErrorExit

		; Process word to buffer (ES:DI)

		MOV	CX, WordSize
		MOV	SI, Offset BinaryBuffer
@@WordLoop:
		PUSH	CX
		MOV	CX, WordBytes
		XOR	BX, BX
		LODSB
		SHR	AL, 1		; LSB to carry
@@BitLoop:
		RCL	BYTE PTR [DI+BX], 1	; Carry to next bytes
		INC	BX
		LOOP	@@BitLoop

		POP	CX
		LOOP	@@WordLoop

		ADD	DI, WordBytes

		JMP	@@FileLoop

		; Close file
@@CloseFile:
		MOV	AH, 03Eh
                MOV	BX, Handle
                INT     21h
@@Exit:
		RET
@@ErrorExit:
		MOV	AH, 03Eh
                MOV	BX, Handle
                INT     21h
		CLC
		JMP	@@Exit

ReadOneFile	ENDP

;=============================================================================

ReadFiles	PROC	NEAR

		MOV	SI, Offset ReadingText
		CALL	PrintString
		MOV	SI, Offset MicroFileName
		CALL	PrintString

		MOV	DX, Offset MicroFileName
		MOV	DI, Offset MicroMem
		MOV	CX, 32		; Word size
		CALL	ReadOneFile
		JC	@@Exit

		MOV	SI, Offset ReadingText
		CALL	PrintString
		MOV	SI, Offset MacroFileName
		CALL	PrintString

		MOV	DX, Offset MacroFileName
		MOV	DI, Offset MainMem
		MOV	CX, 16		; Word size
		CALL	ReadOneFile
@@Exit:
		RET
ReadFiles	ENDP

;=============================================================================

Install		PROC	FAR

		MOV	AX, Data
		MOV	DS, AX

		MOV	PSPSegment, ES

		CALL	ParseCmdLine
		CALL	ReadFiles
		JC	@@Error

		MOV	SI, Offset ExecutingText
		CALL	PrintString

		MOV	MPC, 00h
		MOV	RegPC, 0000h
		MOV	RegSP, TopOfStack

		CALL	InterpretMicro

		MOV	SI, Offset ReadyTxt
		CALL	PrintString

@@Exit:
		MOV	AX, 4C00h
		INT	21h

@@Error:
		MOV	SI, Offset ErrorTxt
		CALL	PrintString
		JMP	@@Exit

Install		ENDP


Code		ENDS

;*****************************************************************************

_Stack		SEGMENT PARA STACK

		DW	0200h DUP (?)

_Stack		ENDS

		END	Install
