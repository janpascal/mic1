{$A+,B-,D+,E+,F-,G-,I+,L+,N-,O-,R-,S+,V+,X+}
{$M 16384,0,655360}
{
    Copyright (C) 1991,2011 Jan-Pascal van Best <janpascal@vanbest.org>
    
    This file is part of MicroProgramming.

    MicroProgramming is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MicroProgramming is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MicroProgramming.  If not, see <http://www.gnu.org/licenses/>.
}

UNIT Architecture;

INTERFACE

USES
  Dos,
  Objects, Drivers, Views, Dialogs, MsgBox, App,
  Hex, MicroCmd, Disassembly, Values, Dump2;

CONST
  MicroMemSize = 256;
  MainMemSize  = 4096;
  NumRegs      = 16;
  TopOfStack   = 4021;

  InputRegister		= 4092;
  OutputRegister	= 4094;
  InputStatusRegister	= 4093;
  OutputStatusRegister	= 4095;

  NumberOfCLockCycles   = 4;

  MaxoutputLines      = 25;

  RegNames: ARRAY[0..NumRegs-1] OF STRING[5] =
    ('~P~C', '~A~C', '~S~P', '~I~R', '~T~IR', '0', '+1','-1',
    'AMask', 'SMask', 'A', 'B', 'C', 'D', 'E', 'F');
  RegNamesNoTilde: ARRAY[0..NumRegs-1] OF STRING[5] =
    ('PC', 'AC', 'SP', 'IR', 'TIR', '0', '1','-1',
    'AMask', 'SMask', 'A', 'B', 'C', 'D', 'E', 'F');

TYPE
  SetChar = SET OF Char;

  TMicroMem = ARRAY[0..MicroMemSize-1] OF LongInt;
  TMainMem  = ARRAY[0..MainMemSize-1] OF Word;

  PRegisterFile = ^TREgisterFile;
  TRegisterFile = RECORD
    PC, AC, SP, IR, TIR,
    Zero, PlusOne, MinusOne,
    AMask, SMask,
    A, B, C, D, E, F:  Word;
  END;

  PRegisterArray = ^TRegisterArray;
  TRegisterArray = ARRAY[0..15] OF Word;

  PInternals = ^TInternals;
  TInternals = RECORD
    BusC, LatchA, LatchB, AMUX, ALU: Word;
    NFlag, ZFlag:                    Boolean;
    MAR, MBR:                        Word;
    MMux, MPC:                       Byte;
    MIR:                             LongInt;
    ReadBusy, WriteBusy:             Boolean;
  END;

  PMicroData = ^TMicroData;
  TMicroData = RECORD
    MicroMem:  TMicroMem;
    MainMem:   TMainMem;
    Regs:      TRegisterFile;
    Internals: TInternals;
  END;

  PArchitecture = ^TArchitecture;
  TArchitecture = OBJECT(TObject)
    Data:        PMicroData;
    DataSpace:   Pointer;
    ClockCycle: Byte;
    CONSTRUCTOR Init;
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE DoFirstCycle;
    PROCEDURE DoSecondCycle;
    PROCEDURE DoThirdCycle;
    PROCEDURE DoFourthCycle;
    PROCEDURE HandleInput;
    PROCEDURE HandleOutput;
    PROCEDURE DoClockCycle(Cycle:Byte);
    PROCEDURE DoOneCLockCycle;
    PROCEDURE ProgramReset;
    PROCEDURE DoOneMicroInstruction;
    PROCEDURE DoOneMacroInstruction;
    PROCEDURE RunToMicro(Address:Word);
    PROCEDURE RunToMacro(Address:Word);
    PROCEDURE Run;
  END;

  PScreenWindow = ^TScreenWindow;
  TScreenWindow = OBJECT(TWindow)
    Control: PInputLine;
    CONSTRUCTOR Init(Bounds: TRect; AName: STRING; ANumber: Integer);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
  END;

  PInternalWindow = ^TInternalWindow;
  TInternalWindow = OBJECT(TValuesWindow)
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle:TTitleStr;
      ANumber:Integer; AData:PInternals);
  END;

  PRegisterWindow = ^TRegisterWindow;
  TRegisterWindow = OBJECT(TValuesWindow)
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle:TTitleStr;
      ANumber:Integer; AData:PRegisterFile);
  END;

  PClockPhaseWindow = ^TClockPhaseWindow;
  TClockPhaseWindow = OBJECT(TValuesWindow)
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle:TTitleStr;
      ANumber:Integer; MyArchitecture:PArchitecture);
  END;

  PMicroDumpWindow = ^TMicroDumpWindow;
  TMicroDumpWindow = OBJECT(TMemoryWindow)
    MyArchitecture: PArchitecture;
    CONSTRUCTOR Init(VAR Bounds:TRect; ANumber:Word; AMyArchitecture:PArchitecture);
  END;

  PMacroDumpWindow = ^TMacroDumpWindow;
  TMacroDumpWindow = OBJECT(TMemoryWindow)
    MyArchitecture: PArchitecture;
    CONSTRUCTOR Init(VAR Bounds:TRect; ANumber:Word; AMyArchitecture:PArchitecture);
  END;

  PMacroDisasmView = ^TMacroDisasmView;
  TMacroDisasmView = OBJECT(TDisasmView)
    CONSTRUCTOR Init(VAR Bounds: TRect; AData:Pointer );
    PROCEDURE DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word); VIRTUAL;
    FUNCTION Before(Address:Word): Word; VIRTUAL;
    FUNCTION After(Address:Word): Word; VIRTUAL;
  END;

  PMicroDisasmView = ^TMicroDisasmView;
  TMicroDisasmView = OBJECT(TDisasmView)
    CONSTRUCTOR Init(VAR Bounds: TRect; AData:Pointer );
    PROCEDURE DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word); VIRTUAL;
    FUNCTION Before(Address:Word): Word; VIRTUAL;
    FUNCTION After(Address:Word): Word; VIRTUAL;
  END;

  PMicroDisasmWindow = ^TMicroDisasmWindow;
  TMicroDisasmWindow = OBJECT(TDisasmWindow)
    MyArchitecture: PArchitecture;
    CONSTRUCTOR Init(VAR Bounds:TRect; ANumber:Word; AMyArchitecture:PArchitecture);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
  END;

  PMacroDisasmWindow = ^TMacroDisasmWindow;
  TMacroDisasmWindow = OBJECT(TDisasmWindow)
    MyArchitecture: PArchitecture;
    CONSTRUCTOR Init(VAR Bounds:TRect; ANumber:Word; AMyArchitecture:PArchitecture);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
  END;

IMPLEMENTATION


CONSTRUCTOR TInternalWindow.Init(VAR Bounds: TRect; ATitle:TTitleStr;
  ANumber:Integer; AData:PInternals);

BEGIN
  TValuesWindow.Init(Bounds, ATitle, True, False,
    NewWordItem('BusC', AData^.BusC, dfHexaDecimal, False,
    NewWordItem('LatchA', AData^.LatchA, dfHexaDecimal, False,
    NewWordItem('LatchB', AData^.LatchB, dfHexaDecimal, False,
    NewWordItem('AMUX', AData^.AMUX, dfHexaDecimal, False,
    NewWordItem('ALU', AData^.ALU, dfHexaDecimal, False,
    NewBooleanItem('NFlag', AData^.NFlag, dfTrueFalse, False,
    NewBooleanItem('ZFlag', AData^.ZFlag, dfTrueFalse, False,
    NewWordItem('MAR', AData^.MAR, dfHexaDecimal, False,
    NewWordItem('MBR', AData^.MBR, dfHexaDecimal, False,
    NewByteItem('MMux', AData^.MMux, dfHexaDecimal, False,
    NewByteItem('MPC', AData^.MPC, dfHexaDecimal, False,
    NewLongItem('MIR', AData^.MIR, dfHexaDecimal, False,
    NewBooleanItem('ReadBusy', AData^.ReadBusy, dfTrueFalse, False,
    NewBooleanItem('WriteBusy', AData^.WriteBusy, dfTrueFalse, False,
    NIL)))))))))))))));
END;

CONSTRUCTOR TRegisterWindow.Init(VAR Bounds: TRect; ATitle:TTitleStr;
  ANumber:Integer; AData:PRegisterFile);

BEGIN
  TValuesWindow.Init(Bounds, ATitle, True, False,
    NewWordItem('PC',    AData^.PC, dfDecimal, False,
    NewWordItem('AC',    AData^.AC, dfDecimal, False,
    NewWordItem('SP',    AData^.SP, dfDecimal, False,
    NewWordItem('IR',    AData^.IR, dfBinary, False,
    NewWordItem('TIR',   AData^.TIR, dfBinary, False,
    NewWordItem('Zero',  AData^.Zero, dfDecimal, False,
    NewWordItem('1',     AData^.PlusOne, dfDecimal, False,
    NewWordItem('-1',    AData^.MinusOne, dfDecimal, False,
    NewWordItem('AMASK', AData^.AMask, dfHexaDecimal, False,
    NewWordItem('SMASK', AData^.SMAsk, dfHexaDecimal, False,
    NewWordItem('A',     AData^.A, dfDecimal, False,
    NewWordItem('B',     AData^.B, dfDecimal, False,
    NewWordItem('C',     AData^.C, dfDecimal, False,
    NewWordItem('D',     AData^.D, dfDecimal, False,
    NewWordItem('E',     AData^.E, dfDecimal, False,
    NewWordItem('F',     AData^.F, dfDecimal, False,
    NIL)))))))))))))))));
END;

CONSTRUCTOR TClockPhaseWindow.Init(VAR Bounds: TRect; ATitle:TTitleStr;
      ANumber:Integer; MyArchitecture:PArchitecture);

BEGIN
  TValuesWindow.Init(Bounds, ATitle, True, False,
    NewWordItem('PC', MyArchitecture^.Data^.Regs.PC, dfDecimal, False,
    NewByteItem('MPC', MyArchitecture^.Data^.Internals.MPC, dfDecimal, False,
    NewByteItem('Clock phase', MyArchitecture^.ClockCycle, dfDecimal, False,
    NIL))));
END;

CONSTRUCTOR TScreenWindow.Init(Bounds: TRect; AName: STRING; ANumber: Integer);

VAR
  R: TRect;

BEGIN
  TWindow.Init(Bounds, AName, ANumber);
  Options := Options OR ofTileable;
  Options := Options AND ($FFFF-ofBuffered);
  GetExtent(R);
  R.Grow(-1, -1);

  Control := New(PInputLine, Init(R, 80 ));
  Insert(Control);
END;

PROCEDURE TScreenWindow.HandleEvent(VAR Event:TEvent);

VAR
  P: PString;
  BufPtr: ^TextBuf;
  Event2: TEvent;

BEGIN
  TWindow.HandleEvent(Event);
  IF (Event.What=evBroadCast) AND (Event.Command=cmProcessOutput) THEN
  BEGIN
    Write(TextBuf(Event.InfoPtr^), 1);
    Event2.What := evKeyDown;
    Event2.CharCode := Char(Event.InfoPtr^);
    Control^.HandleEvent(Event2);
  END;
END;

CONSTRUCTOR TMicroDumpWindow.Init(VAR Bounds:TRect; ANumber:Word;
  AMyArchitecture:PArchitecture);

BEGIN
  TMemoryWindow.Init(Bounds, 'Micro Memory', ANumber,
    Addr(AMyArchitecture^.Data^.MicroMem), MicroMemSize, 8, 32, dfDecimal, dfBinary);
  MyArchitecture := AMyArchitecture;
END;

CONSTRUCTOR TMacroDumpWindow.Init(VAR Bounds:TRect; ANumber:Word;
  AMyArchitecture:PArchitecture);

BEGIN
  TMemoryWindow.Init(Bounds, 'Macro Memory', ANumber,
    Addr(AMyArchitecture^.Data^.MainMem), MainMemSize, 12, 16, dfDecimal, dfHexaDecimal);
  MyArchitecture := AMyArchitecture;
END;

{===========================================================================}
{==                     TMacroDisAsmView                                  ==}
{===========================================================================}

CONSTRUCTOR TMacroDisasmView.Init(VAR Bounds: TRect; AData:Pointer );

BEGIN
  TDisasmView.Init(Bounds, AData, MainMemSize, 12, 16, dfHexadecimal);
END;

PROCEDURE TMacroDisasmView.DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word);

VAR
  OpCode: Word;

BEGIN
  NumWords := 1;

  OpCode := TWordArray(Data^)[Address];

  CASE OpCode SHR 12 OF
    $00: S := 'LODD';
    $01: S := 'STOD';
    $02: S := 'ADDD';
    $03: S := 'SUBD';
    $04: S := 'JPOS';
    $05: S := 'JZER';
    $06: S := 'JUMP';
    $07: S := 'LOCO';
    $08: S := 'LODL';
    $09: S := 'STOL';
    $0A: S := 'ADDL';
    $0B: S := 'SUBL';
    $0C: S := 'JNEG';
    $0D: S := 'JNZE';
    $0E: S := 'CALL';
    $0F: CASE (OpCode AND $0F00) SHR 8 OF
           $00: S := 'PSHI';
           $02: S := 'POPI';
           $04: S := 'PUSH';
           $06: S := 'POP';
           $08: S := 'RETN';
           $0A: S := 'SWAP';
           $0B: S := 'HALT';
           $0C: S := 'INSP';
           $0E: S := 'DESP';
           $0F: S := 'LSHF';
         ELSE
           S := '????';
         END;
  END;

  IF OpCode SHR 12<>$0F THEN
    S := S + ' ' + Copy(HexW(OpCode AND $0FFF), 2, 4)
  ELSE
    IF (OpCode AND $0F00) SHR 8 IN [$0C, $0E] THEN
      S := S + ' ' + HexB(OpCode AND $00FF);
END;

FUNCTION TMacroDisasmView.Before(Address:Word): Word;

BEGIN
  IF Address>0 THEN Before:=Address-1 ELSE Before:=0;
END;

FUNCTION TMacroDisasmView.After(Address:Word): Word;

BEGIN
  IF Address<Range-1 THEN After:=Address+1 ELSE After:=Range-1;
END;

{===========================================================================}
{==                     TMicroDisAsmView                                  ==}
{===========================================================================}

CONSTRUCTOR TMicroDisasmView.Init(VAR Bounds: TRect; AData:Pointer );

BEGIN
  TDisasmView.Init(Bounds, AData, MicroMemSize, 8, 32, dfDecimal);
END;

PROCEDURE TMicroDisasmView.DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word);

VAR
  OpCode: LongInt;
  AMUX, COND, ALU, SH, MBR, MAR, RD, WR, ENC, C, B, A, ADR: Byte;
  ADRStr, LatchA, LatchB, AMUXStr, ALUStr, SHStr: STRING;

BEGIN
  NumWords := 1;
  S := '';

  OpCode := TLongArray(Data^)[Address];

  ADR  := OpCode AND $000000FF;
  A    := (OpCode AND $00000F00) SHR 8;
  B    := (OpCode AND $0000F000) SHR 12;
  C    := (OpCode AND $000F0000) SHR 16;
  ENC  := (OpCode AND $00100000) SHR 20;
  WR   := (OpCode AND $00200000) SHR 21;
  RD   := (OpCode AND $00400000) SHR 22;
  MAR  := (OpCode AND $00800000) SHR 23;
  MBR  := (OpCode AND $01000000) SHR 24;
  SH   := (OpCode AND $06000000) SHR 25;
  ALU  := (OpCode AND $18000000) SHR 27;
  COND := (OpCode AND $60000000) SHR 29;
  AMUX := (OpCode AND $80000000) SHR 31;

  Str(ADR, ADRStr);

  LatchA := RegNamesNoTilde[A]; LatchB := RegNamesNoTilde[B];
  IF AMUX=0 THEN AMUXStr := LatchA ELSE AMUXStr := 'MBR';

  CASE ALU OF
    0: BEGIN
         IF LatchB='-1' THEN
           ALUStr := AMUXStr + '+(-1)'
         ELSE
           ALUStr := AMUXStr + '+' + LatchB;
       END;
    1: ALUStr := 'band(' + AMUXStr + ',' + LatchB + ')';
    2: ALUStr := AMUXStr;
    3: ALUStr := 'inv(' + AMUXStr + ')';
  END;

  CASE SH OF
    0: SHStr := ALUStr;
    1: SHStr := 'rshift(' + ALUStr + ')';
    2: SHStr := 'lshift(' + ALUStr + ')';
  END;

  IF MAR=1 THEN S := S + 'MAR:=' + LatchB + '; ';
  IF MBR=1 THEN S := S + 'MBR:=' + SHStr + '; ';
  IF ENC=1 THEN S := S + RegNamesNoTilde[C] + ':=' + SHStr + '; ';
  IF (MBR+ENC=0) AND ((COND=1) OR (COND=2)) THEN S := S + 'ALU:=' + ALUStr + '; ';
  IF RD=1 THEN S := S + 'rd; ';
  IF WR=1 THEN S := S + 'wr; ';
  CASE COND OF
    0: ;
    1: S := S + 'IF n THEN GOTO ' + ADRStr + '; ';
    2: S := S + 'IF z THEN GOTO ' + ADRStr + '; ';
    3: S := S + 'GOTO ' + ADRStr + '; ';
  END;
END;

FUNCTION TMicroDisasmView.Before(Address:Word): Word;

BEGIN
  IF Address>0 THEN Before:=Address-1 ELSE Before:=0;
END;

FUNCTION TMicroDisasmView.After(Address:Word): Word;

BEGIN
  IF Address<Range-1 THEN After:=Address+1 ELSE After:=Range-1;
END;

{===========================================================================}
{==                    TMicroDisAsmWindow                                 ==}
{===========================================================================}

CONSTRUCTOR TMicroDisasmWindow.Init(VAR Bounds:TRect; ANumber:Word;
  AMyArchitecture:PArchitecture);

VAR
  R: TRect;

BEGIN
  R := Bounds;
  R.B.X := R.B.X - R.A.X - 1; R.A.X := 1;
  R.B.Y := R.B.Y - R.A.Y - 1; R.A.Y := 1;
  TDisasmWindow.Init(Bounds, 'Micro Memory', ANumber,
    New(PMicroDisasmView, Init(R, Addr(AMyArchitecture^.Data^.MicroMem))));
  MyArchitecture := AMyArchitecture;
END;


PROCEDURE TMicroDisasmWindow.HandleEvent(VAR Event:TEvent);

BEGIN
  CASE Event.What OF
    evBroadCast: IF (Event.Command=cmNewAddress) AND
                    (Event.InfoPtr = MyArchitecture) THEN
      Control^.SetCurrentAddress(MyArchitecture^.Data^.Internals.MPC);
    evCommand: IF Event.Command=cmGotoCursor THEN
    BEGIN
      MyArchitecture^.RunToMicro(Control^.LocalAddress);
      Message(DeskTop, evBroadcast, cmNewAddress, MyArchitecture);
      DeskTop^.ReDraw;
      ClearEvent(Event);
    END;
  END;

  TDisasmWindow.HandleEvent(Event);
END;

{===========================================================================}
{==                    TMacroDisAsmWindow                                 ==}
{===========================================================================}

CONSTRUCTOR TMacroDisasmWindow.Init(VAR Bounds:TRect; ANumber:Word;
  AMyArchitecture:PArchitecture);

VAR
  R: TRect;

BEGIN
  R := Bounds;
  R.B.X := R.B.X - R.A.X - 1; R.A.X := 1;
  R.B.Y := R.B.Y - R.A.Y - 1; R.A.Y := 1;
  TDisasmWindow.Init(Bounds, 'Macro Memory', ANumber,
    New(PMacroDisasmView, Init(R, Addr(AMyArchitecture^.Data^.MainMem))));
  MyArchitecture := AMyArchitecture;
END;

PROCEDURE TMacroDisasmWindow.HandleEvent(VAR Event:TEvent);

BEGIN
  CASE Event.What OF
    evBroadCast: IF (Event.Command=cmNewAddress) AND
                    (Event.InfoPtr=MyArchitecture) THEN
      Control^.SetCurrentAddress(MyArchitecture^.Data^.Regs.PC);
    evCommand: IF Event.Command=cmGotoCursor THEN
      BEGIN
        MyArchitecture^.RunToMacro(Control^.LocalAddress);
        Message(DeskTop, evBroadcast, cmNewAddress, MyArchitecture);
        DeskTop^.ReDraw;
        ClearEvent(Event);
      END;
  END;

  TDisasmWindow.HandleEvent(Event);
END;

(* {$L Architecture.OBJ}

PROCEDURE DoFirstCycle; EXTERNAL;
PROCEDURE DoSecondCycle; EXTERNAL;
PROCEDURE DoThirdCycle; EXTERNAL;
PROCEDURE DoFourthCycle; EXTERNAL;

*)
CONSTRUCTOR TArchitecture.Init;

BEGIN
  TObject.Init;
  GetMem(DataSpace, SizeOf(TMicroData)+15);
  {IF PtrRec(DataSpace).Ofs=0 THEN}
    Data := DataSpace;
{  ELSE
    Data := PMicroData( Ptr(1+PtrRec(DataSpace).Seg, 0) );}
  FillChar(Data^, SizeOf(TMicroData), #0);
  ProgramReset;
END;

DESTRUCTOR TArchitecture.Done;

BEGIN
  FreeMem(DataSpace, 15+SizeOf(TMicroData));
  TObject.Done;
END;

(* First cycle: fetch micro instruction and move into MIR *)
PROCEDURE TArchitecture.DoFirstCycle;

BEGIN
  Data^.Internals.MIR := Data^.MicroMem[Data^.Internals.MPC];
END;

(* Load latches A and B *)
PROCEDURE TArchitecture.DoSecondCycle;

VAR
  MIR: LongInt;
  RegA: LongInt;
  RegB: LongInt;
  PRegs: PRegisterArray;

BEGIN
  PRegs := PRegisterArray(@Data^.Regs);
  MIR := Data^.Internals.MIR;

  RegA := (MIR AND $0F00) SHR 8;
  RegB := (MIR AND $F000) SHR 12;
  Data^.Internals.LatchA := PRegs^[RegA];
  Data^.Internals.LatchB := PRegs^[RegB];
END;

PROCEDURE TArchitecture.DoThirdCycle;

VAR
  MIR: LongInt;
  AMUX: Byte;
  ALU: Byte;
  Shift: Byte;
  Cond: Byte;
  NextMPC: Byte;
  Addr: Word;

BEGIN
  MIR := Data^.Internals.MIR;
  { 3a: Calculate AMUX }
  AMUX := (MIR AND $80000000) SHR 31;
  IF AMUX = 1 THEN
    Data^.Internals.AMUX := Data^.Internals.MBR
  ELSE
    Data^.Internals.AMUX := Data^.Internals.LatchA; 

  {3b: ALU-operation}
  ALU := (Data^.Internals.MIR AND $18000000) SHR 27;
  CASE ALU OF
    0: Data^.Internals.ALU := Data^.Internals.AMUX + Data^.Internals.LatchB;
    1: Data^.Internals.ALU := Data^.Internals.AMUX AND Data^.Internals.LatchB;
    2: Data^.Internals.ALU := Data^.Internals.AMUX;
    3: Data^.Internals.ALU := NOT Data^.Internals.AMUX;
  END;

  { Set flags }
  Data^.Internals.NFlag := (Data^.Internals.ALU AND $8000) > 0;
  Data^.Internals.ZFlag := Data^.Internals.ALU = 0;

  {3b: copy latchb to MAR if needed}
  IF (MIR AND $00800000 > 0) THEN
    Data^.Internals.MAR := Data^.Internals.LatchB AND $0FFF;

  {3d: Shifter-operations}
  Shift := (MIR AND $06000000) SHR 25;
  CASE Shift OF
    0: Data^.Internals.BusC := Data^.Internals.ALU;
    1: Data^.Internals.BusC := Data^.Internals.ALU SHR 1;
    2: Data^.Internals.BusC := Data^.Internals.ALU SHL 1;
  END;

  {3e: Calculate MMUX and MPC}
  Cond := (MIR AND $60000000) SHR 29;
  Addr := MIR AND $000000ff;
  NextMPC := Data^.Internals.MPC + 1;
  CASE Cond OF
    1: IF Data^.Internals.NFlag THEN
         NextMPC := Addr;
    2: IF Data^.Internals.ZFlag THEN
         NextMPC := Addr;
    3: NextMPC := Addr;
  END; 
  Data^.Internals.MMUX := NextMPC;
  Data^.Internals.MPC := NextMPC;
END;

{ Cycle 4: store bus C }
PROCEDURE TArchitecture.DoFourthCycle;

VAR
  MIR: LongInt;
  BusC: Word;
  PRegs: PRegisterArray;
  RegC: Byte;

BEGIN
  MIR := Data^.Internals.MIR;
  BusC := Data^.Internals.BusC;
  PRegs := PRegisterArray(@Data^.Regs);

  { 4a: store to MBR, test MBR bit }
  IF (MIR AND $01000000) SHR 24 > 0 THEN
     Data^.Internals.MBR := BusC;

  { 4b: store to register, test ENC bit }
  IF (MIR AND $00100000) SHR 20 > 0 THEN
  BEGIN
     {C-field}
     RegC := (MIR AND $000f0000) SHR 16;
     PRegs^[RegC] := BusC;
  END;

  { Memory management: read }
  IF (MIR AND $00400000) SHR 22 = 0 THEN
    Data^.Internals.ReadBusy := False
  ELSE
  BEGIN
    IF NOT Data^.Internals.ReadBusy THEN
       Data^.Internals.ReadBusy := True
    ELSE
    BEGIN
       Data^.Internals.MBR := Data^.MainMem[Data^.Internals.MAR];
       Data^.Internals.ReadBusy := False
    END;
  END;

  { Memory management: write }
  IF (MIR AND $00200000) SHR 21 = 0 THEN
    Data^.Internals.WriteBusy := False
  ELSE
  BEGIN
    IF NOT Data^.Internals.WriteBusy THEN
       Data^.Internals.WriteBusy := True
    ELSE
    BEGIN
       Data^.MainMem[Data^.Internals.MAR] := Data^.Internals.MBR;
       Data^.Internals.WriteBusy := False
    END;
  END;
 
  { I/O Management }
  HandleInput;
  HandleOutput;
END;

PROCEDURE TArchitecture.HandleInput;

BEGIN
  { TODO }
END;

PROCEDURE TArchitecture.HandleOutput;

BEGIN
  IF Data^.MainMem[OutputStatusRegister] AND $8000 > 0 THEN
  BEGIN
    Message(DeskTop, evBroadCast, cmProcessoutput, Addr(Data^.MainMem[OutputRegister]));  
    Data^.MainMem[OutputStatusRegister] := Data^.MainMem[OutputStatusRegister] AND $7FFF;
  END;
END;

PROCEDURE TArchitecture.DoClockCycle(Cycle:Byte);

BEGIN
  CASE Cycle OF
    0: DoFirstCycle;
    1: DoSecondCycle;
    2: DoThirdCycle;
    3: DoFourthCycle;
  END;
END;

PROCEDURE TArchitecture.DoOneClockCycle;

BEGIN
  DoClockCycle(ClockCycle);
  Inc(CLockCycle);
  IF CLockCycle>=NumberOfCLockCycles THEN
    ClockCycle := 0;
{  Message(DeskTop, evBroadcast, cmUpDate, NIL);
  Message(DeskTop, evBroadcast, cmNewAddress, @Self);
  DeskTop^.Redraw;}
END;

PROCEDURE TArchitecture.ProgramReset;

BEGIN
  FillChar(Data^.Regs, SizeOf(Data^.Regs), #0);
  FillChar(Data^.Internals, SizeOf(Data^.Internals), #0);
  ClockCycle := 0;
  WITH Data^.Regs DO
  BEGIN
    PC := 0;
    SP := TopOfStack;
    PlusOne  := 1;
    MinusOne := $FFFF;
    AMASK    := $0FFF;
    SMASK    := $00FF;
  END;
  WITH Data^.Internals DO
  BEGIN
    ReadBusy := False;
    WriteBusy := False;
  END;
END;


PROCEDURE TArchitecture.DoOneMicroInstruction;

BEGIN
  REPEAT
    DoOneClockCycle;
  UNTIL ClockCycle=0;
END;

PROCEDURE TArchitecture.RunToMicro(Address:Word);

BEGIN
  REPEAT
    DoOneMicroInstruction;
  UNTIL (Data^.Internals.MPC=Address) OR (Data^.Internals.MPC=MicroMemSize-1);
END;

PROCEDURE TArchitecture.DoOneMacroInstruction;

BEGIN
  REPEAT
    DoOneMicroInstruction;
  UNTIL (Data^.Internals.MPC=0) OR (Data^.Internals.MPC=MicroMemSize-1);
END;

PROCEDURE TArchitecture.RunToMacro(Address:Word);

BEGIN
  REPEAT
    DoOneMacroInstruction;
  UNTIL (Data^.Regs.PC=Address) OR (Data^.Internals.MPC=MicroMemSize-1);
END;

PROCEDURE TArchitecture.Run;

BEGIN
  REPEAT
    DoOneMacroInstruction;
  UNTIL Data^.Internals.MPC=MicroMemSize-1;
END;

END.
