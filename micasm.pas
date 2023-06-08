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

PROGRAM MicAsm;

USES
  Dos;

CONST
  AMUX = $80000000;
  COND = $20000000;
  ALU  = $08000000;
  SH   = $02000000;
  MBR  = $01000000;
  MAR  = $00800000;
  RD   = $00400000;
  WR   = $00200000;
  ENC  = $00100000;
  C    = $00010000;
  B    = $00001000;
  A    = $00000100;
  ADR  = $00000001;

  NoJump     = 0;
  JumpIfN    = 1;
  JumpIfZ    = 2;
  JumpAlways = 3;

  ALUPlus    = 0;
  ALUAnd     = 1;
  ALUCopyA   = 2;
  ALUNot     = 3;

  ShiftNot   = 0;
  ShiftRight = 1;
  ShiftLeft  = 2;

  RegPC       = 0;
  RegAC       = 1;
  RegSP       = 2;
  RegIR       = 3;
  RegTIR      = 4;
  RegZero     = 5;
  RegOne      = 6;
  RegMinusOne = 7;
  RegAMASK    = 8;
  RegSMASK    = 9;
  RegA        = 10;
  RegB        = 11;
  RegC        = 12;
  RegD        = 13;
  RegE        = 14;
  RegF        = 15;

  RegNames: ARRAY[0..16] OF STRING[5] =
    ( 'PC', 'AC', 'SP', 'TIR', 'IR', '0', '1', '-1',
      'AMASK', 'SMASK', 'A', 'B', 'C', 'D', 'E', 'F', 'MBR'  );
  RegCode: ARRAY[0..16] OF Byte =
    ( RegPC, RegAC, RegSP, RegTIR, RegIR, RegZero, RegOne, RegMinusOne,
      RegAMASK, RegSMASK, RegA, RegB, RegC, RegD, RegE, RegF, 16 );

PROCEDURE ToUpper(VAR s:STRING);

VAR
  i: Byte;

BEGIN
  FOR i:=1 TO Length(s) DO s[i] := UpCase(s[i]);
END;

PROCEDURE SearchRegisterForward(s: STRING; Start:Byte; VAR Number:Byte;
                                VAR Found:Boolean);

VAR
  j: Byte;

BEGIN
  j := 0;
  Found := False;
  WHILE NOT Found AND (j<=15) DO  { Try all registers }
  BEGIN
    Found := Copy(s, Start, Length(RegNAmes[j]))=RegNames[j];
    Inc(j);
  END;
  Number := RegCode[j-1];
END;


PROCEDURE TranslateLine(s:STRING; VAR Dest:LongInt; VAR Error:Boolean);

VAR
  i, j:    Byte;
  s1:      STRING;
  Number:  Byte;
  Found:   Boolean;
  Code:    Integer;
  Address: Byte;
  AMUXOn:  Boolean;

BEGIN
  Dest := $00000000;
  ToUpper(s);
  i := Pos('{',s);
  IF i<>0 THEN Delete(s, i, Length(s)+1-i);

  IF Pos('LSHIFT',s)<>0 THEN Dest := Dest OR (ShiftLeft*SH);
  IF Pos('RSHIFT',s)<>0 THEN Dest := Dest OR (ShiftRight*SH);
  IF Pos('RD',s)<>0 THEN Dest := Dest OR RD;
  IF Pos('WR',s)<>0 THEN Dest := Dest OR WR;

  i := Pos('GOTO', s);
  IF i<>0 THEN
  BEGIN
    Inc(i, 5);
    s1 := '';
    WHILE (i<=Length(s)) AND (s[i] IN ['0'..'9']) DO
    BEGIN
      s1 := s1 + s[i];
      Inc(i);
    END;
    Val(s1, Address, Code);
    IF Code<>0 THEN BEGIN Error:=True; Writeln('Error in GOTO address.') END;
    Dest := DEST OR (Address*ADR);
    IF Pos('IF N', s)<>0 THEN
      Dest := Dest OR (JumpIfN*COND)
    ELSE
      IF Pos('IF Z', s)<>0 THEN
        Dest := Dest OR (JumpIfZ*COND)
      ELSE
        Dest := Dest OR (JumpAlways*COND);
  END;

  AMUXOn := Pos('(MBR', s)<>0;
  IF AMUXOn THEN Dest := Dest OR AMUX;
  IF Pos('MBR:=', s)<>0 THEN Dest := Dest OR MBR;

  i := Pos('MAR:=', s);
  IF i<>0 THEN
  BEGIN
    Dest := Dest OR MAR;
    SearchRegisterForward(s, i+5, Number, Found);
    IF NOT Found THEN BEGIN Error:=TRue; Writeln('Error in source for MAR.') END;
    Dest := Dest OR ( LongInt(Number*B) AND $0000F000 );
    IF Found THEN Delete(s, i, 5+Length(RegNAmes[RegCode[Number]]));
  END;

  i := Pos(':=', s);
  IF i<>0 THEN
  BEGIN
    j := 0;
    Found := False;
    WHILE NOT Found AND (j<=15) DO  { Try all registers }
    BEGIN
      Found := Copy(s, i-Length(RegNames[j]), Length(RegNAmes[j]))=RegNames[j];
      Inc(j);
    END;
    j := RegCode[j-1];
    IF Found THEN Dest := Dest OR ENC OR (j*C);
  END;

  i := Pos('+(', s);
  IF i<>0 THEN BEGIN
    Dest := Dest OR (ALUPlus*ALU);
    SearchRegisterForward(s, i+2, Number, Found);
    IF NOT Found THEN
      IF NOT AMUXOn THEN BEGIN Error:=TRue; Writeln('Error in first argument of +.'); END
      ELSE
        Number := 16;
    IF Found THEN Dest := Dest OR (Number*A);
    SearchRegisterForward(s, i+2+Length(RegNames[RegCode[Number]])+1, Number, Found);
    IF NOT Found THEN BEGIN Error:=TRue; Writeln('Error in second argument of +.'); END;
    IF Found THEN Dest := Dest OR ( LongInt(Number*B) AND $0000F000 );
  END;

  i := Pos('BAND(', s);
  IF i<>0 THEN BEGIN
    Dest := Dest OR (ALUAnd*ALU);
    SearchRegisterForward(s, i+5, Number, Found);
    IF NOT Found THEN
      IF NOT AMUXOn THEN BEGIN Error:=TRue; Writeln('Error in first argument of BAND.'); END
      ELSE
        Number := 16;
    IF Found THEN Dest := Dest OR (Number*A);
    SearchRegisterForward(s, i+5+Length(RegNames[RegCode[Number]])+1, Number, Found);
    IF NOT Found THEN BEGIN Error:=TRue; Writeln('Error in second argument of BAND.'); END;
    IF Found THEN Dest := Dest OR ( LongInt(Number*B) AND $0000F000 );
  END;

  i := Pos('NOT(', s);
  IF i<>0 THEN BEGIN
    Dest := Dest OR (ALUNot*ALU);
    SearchRegisterForward(s, i+4, Number, Found);
    IF NOT Found AND NOT AMUXOn THEN BEGIN Error:=TRue; Writeln('Error in argument of NOT.'); END;
    IF Found THEN Dest := Dest OR (Number*A);
  END;

  i := Pos('COPY(', s);
  IF i<>0 THEN BEGIN
    Dest := Dest OR (ALUCopyA*ALU);
    SearchRegisterForward(s, i+5, Number, Found);
    IF NOT Found AND NOT AMUXOn THEN BEGIN Error:=TRue; Writeln('Error in argument of COPY.'); END;
    IF Found THEN Dest := Dest OR (Number*A);
  END;
END;

FUNCTION LongToBinary(l: LongInt): STRING;

VAR
  s: STRING;
  i: Byte;

BEGIN
  s:= '';
  FOR i:=0 TO 31 DO
  BEGIN
    IF (l AND $80000000)=0 THEN
      s := s+'0'
    ELSE
      s := s+'1';
    l := l SHL 1;
  END;
  LongToBinary := s;
END;

PROCEDURE Translate(VAR Infile, Outfile:TEXT; VAR Error:Boolean);

VAR
  Code: LongInt;
  s:       STRING;

BEGIN
  {$I-}
  Writeln;
  Readln(InFile, s);
  Writeln(s);
  Writeln(Outfile, s);

  WHILE NOT Error AND NOT SeekEof(InFile) DO
  BEGIN
    Readln(InFile, s);
    IF IOResult<>0 THEN Error:=True;
    Writeln(s);
    TranslateLine(s, Code, Error);
    Writeln(OutFile, LongToBinary(Code), '  ', s);
    IF IOResult<>0 THEN Error:=True;
  END;
  {$I+}
END;


PROCEDURE GetFileNames(VAR InNAme, OutName:STRING; VAR Error:Boolean);

VAR
  Dir:  DirStr;
  Name: NameStr;
  Ext:  ExtStr;

BEGIN
  CASE ParamCount OF
    1: BEGIN
         InName := ParamStr(1);
         FSplit(InName, Dir, Name, Ext);
         OutName := Dir + Name + '.MIC';
       END;
    2: BEGIN
         InName := ParamStr(1);
         OutName := ParamStr(2);
       END;
    ELSE
       Error := True;
  END;
END;

PROCEDURE OpenFiles(InName, OutName:STRING; VAR InFile,OutFile:TEXT;
                     VAR Error:Boolean);

BEGIN
  {$I-}
  Assign(InFile, InName);
  Reset(InFile);
  Error := IOResult<>0;

  IF NOT Error THEN
  BEGIN
    Assign(OutFile, OutName);
    Rewrite(OutFile);
    Error := IOResult<>0;
  END;
  {$I+}
END;

PROCEDURE CloseFiles(VAR InFile,OutFile:TEXT; VAR Error:Boolean);

VAR
  IOCode: Integer;

BEGIN
  {$I-}
  Close(InFile);
  Error := IOResult<>0;

  IF NOT Error THEN Close(OutFile);

  Error := IOResult<>0;
  {$I+}
END;

VAR
  Error:           Boolean;
  InName, OutName: STRING;
  InFile, OutFile: TEXT;

BEGIN
  Writeln('MicAsm, Copyright (C) 1991 by J.P. van Best, Oegstgeest, The Netherlands.');
  Error := False;

  GetFileNames(InName, OutName, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Usage: MICASM Infile [OutFile]');
    Writeln('Converts ASCII file Infile with MAT-2 code to binary MIC-1 file.');
    Writeln('Default output extension ''.MIC''.');
    Halt(1);
  END;

  OpenFiles(InName, OutName, InFile, OutFile, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Error opening ', InName,' for input and ', OutName, ' for output.');
    Halt(1);
  END;

  Writeln('Converting ', InName,' to ', OutName, '.');

  Translate(Infile, Outfile, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Error in translation.');
  END;

  CloseFiles(InFile, OutFile, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Error in closing ', InName,' and ', OutName, '.');
    Halt(1);
  END;

END.


