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

PROGRAM MacAsm;

USES
  Dos;

CONST
  MaxSymbolSize = 32;
  Numops        = 26;

  OpString: ARRAY[0..NumOps-1] OF STRING[4] =
    ('LODD', 'STOD', 'ADDD', 'SUBD', 'JPOS', 'JZER', 'JUMP', 'LOCO',
     'LODL', 'STOL', 'ADDL', 'SUBL', 'JNEG', 'JNZE', 'CALL', 'PSHI',
     'POPI', 'PUSH', 'POP',  'RETN', 'SWAP', 'HALT', 'INSP', 'DESP',
     'LSHF', 'DW');

  OpCode: ARRAY[0..NumOps-1] OF Word =
    ($0000, $1000, $2000, $3000, $4000, $5000, $6000, $7000,
     $8000, $9000, $A000, $B000, $C000, $D000, $E000, $F000,
     $F200, $F400, $F600, $F800, $FA00, $FB00, $FC00, $FE00,
     $FF00, $0000);

  NeedOperand: ARRAY[0..NumOps-1] OF Boolean =
    ( True,  True,  True,  True,  True,  True,  True,  True,
      True,  True,  True,  True,  True,  True,  True, False,
      False, False, False, False, False, False, True,  True,
      False, True );

TYPE
  PSymbolTree = ^TSymbolTree;
  TSymbolTree = RECORD
    Left, Right: PSymbolTree;
    Symbol: STRING[MaxSymbolSize];
    Value:  Integer;
  END;

PROCEDURE ToUpper(VAR s:STRING);

VAR
  i: Byte;

BEGIN
  FOR i:=1 TO Length(s) DO s[i] := UpCase(s[i]);
END;

PROCEDURE AddSymbol(VAR Tree: PSymbolTree; Symbol:STRING; Value:Integer);

VAR
  p, Parent: PSymbolTree;

BEGIN
  IF Tree=NIL THEN
  BEGIN
    New(Tree);
    p := Tree;
  END
  ELSE BEGIN
    p := Tree;
    WHILE p<>NIL DO BEGIN
      Parent := p;
      IF Symbol<Parent^.Symbol THEN p:=Parent^.Left ELSE p:=Parent^.Right;
    END;
    New(p);
    IF Symbol<Parent^.Symbol THEN Parent^.Left:=p ELSE Parent^.Right:=p;
  END;

  p^.Symbol := Symbol;
  p^.Value  := Value;
  p^.Left   := NIL;
  p^.Right  := NIL;
END;

PROCEDURE GetSymbol(Tree:PSymbolTree; Symbol:STRING; VAR Value:Integer; VAR Found:Boolean);

VAR
  p: PSymbolTree;

BEGIN
  p := Tree;
  Found := False;
  WHILE NOT Found AND NOT (p=NIL) DO
  BEGIN
    Found := p^.Symbol=Symbol;
    IF NOT Found THEN
      IF Symbol<p^.Symbol THEN p:=p^.Left ELSE p:=p^.Right;
  END;
  IF Found THEN Value:=p^.Value;
END;

PROCEDURE DeleteTree(VAR T:PSymbolTree);

BEGIN
  IF T<>NIL THEN
  BEGIN
    DeleteTree(T^.Left);
    DeleteTree(T^.Right);
    Dispose(T);
  END;
END;

PROCEDURE SHowTree(T: PSymbolTree);

BEGIN
  IF T<>NIL THEN
  BEGIN
    ShowTree(T^.Left);
    Writeln(T^.Symbol:10, '= ', T^.Value);
    ShowTree(T^.Right);
  END;
END;

PROCEDURE PrepareLine(VAR s:STRING);

VAR
  i: Byte;

BEGIN
  ToUpper(s);
  i := Pos('/',s);
  IF i<>0 THEN Delete(s, i, Length(s)+1-i);
END;

PROCEDURE KillSpaces(VAR s:STRING);

BEGIN
  WHILE Pos(' ', s)<>0 DO Delete(s, Pos(' ',s), 1);
  WHILE Pos(#9, s)<>0 DO Delete(s, Pos(#9,s), 1);   { and tabs }
END;

FUNCTION GetOpCode(s:STRING):Byte;

VAR
  Found: Boolean;
  i:     Byte;

BEGIN
  Found := False;
  i := 0;
  WHILE NOT Found AND (i<NumOps) DO
  BEGIN
    Found := Pos(OpString[i], s)<>0;
    IF NOT Found THEN Inc(i);
  END;

  IF Found THEN GetOpCode := i ELSE GetOpCode := NumOps;
END;

FUNCTION GetOperand(s:STRING; T:PSymbolTree): Word;

VAR
  Value: Integer;
  Found: Boolean;
  Code:  Integer;

BEGIN
  Val(s, Value, Code);
  Found := Code=0;
  IF NOT Found THEN
    GetSymbol(T, s, Value, Found);
  IF NOT FOund THEN
  BEGIN
    Found := s[1] IN ['"', ''''];
    IF Found THEN Value := Ord(s[2]);
  END;
  IF NOT Found THEN
    Writeln('  ___ Symbol not found. ___ ');
  GetOperand := Value;
END;

PROCEDURE TranslateLine(s:STRING; T: PSymbolTree; VAR Dest:Word;
                        VAR IsCode: Boolean; VAR Error:Boolean);

VAR
  OpNumber: Byte;
  Operand:  Word;
  Place:    Byte;

BEGIN
  PrepareLine(s);
  OpNumber := GetOPCode(s);
  IsCode := OpNumber<NumOps;
  IF IsCode THEN
  BEGIN
    Dest := OpCode[OpNumber];
    Place := Pos(OpString[OpNumber], s);
    Delete(s, 1, Place+Length(OpString[OpNumber]));
    KillSpaces(s);
    IF NOT NeedOperand[OpNumber] THEN
    BEGIN IF s<>'' THEN Writeln('Inexpected operand.') END
    ELSE
    BEGIN
      Place := Pos('+', s);
      IF Place<>0 THEN
        Operand := GetOperand(Copy(s, 1, Place-1), T) +
                     GetOperand(Copy(s, Place+1, Length(s)-Place), T)
      ELSE
      BEGIN
        Place := Pos('-', s);
        IF Place<>0 THEN
          Operand := GetOperand(Copy(s, 1, Place-1), T) -
                       GetOperand(Copy(s, Place+1, Length(s)-Place), T)
        ELSE
        BEGIN
          Place := Pos('*', s);
          IF Place<>0 THEN
            Operand := GetOperand(Copy(s, 1, Place-1), T) *
                         GetOperand(Copy(s, Place+1, Length(s)-Place), T)
          ELSE
          BEGIN
            Place := Pos('\', s);
            IF Place<>0 THEN
              Operand := GetOperand(Copy(s, 1, Place-1), T) DIV
                           GetOperand(Copy(s, Place+1, Length(s)-Place), T)
            ELSE
              Operand := GetOperand(s, T);
          END;
        END;
      END;
      Dest := Dest OR Operand;
    END;
  END;
END;

FUNCTION WordToBinary(w: Word): STRING;

VAR
  s: STRING;
  i: Byte;

BEGIN
  s:= '';
  FOR i:=0 TO 15 DO
  BEGIN
    IF (w AND $8000)=0 THEN
      s := s+'0'
    ELSE
      s := s+'1';
    w := w SHL 1;
  END;
  WordToBinary := s;
END;



PROCEDURE PassOne(VAR InFile:TEXT; VAR T:PSymbolTree; VAR Error:Boolean);

VAR
  Address: Word;
  s, s1, Symbol: STRING;
  Value, Code: Integer;
  i:           Byte;

BEGIN
  {$I-}
  T := NIL;
  Address := 0;
  Readln(InFile, s);
  WHILE NOT Error AND NOT SeekEof(InFile) DO
  BEGIN
    Readln(InFile, s);
    If IOResult<>0 THEN Error:=True;
    PrepareLine(s);
    i := Pos(':', s);    { Label ? }
    IF i<>0 THEN
      AddSymbol(T, Copy(s,1,i-1), Address);
    i := Pos('=', s);    { Equate ? }
    IF i<>0 THEN
    BEGIN
      Symbol := Copy(s, 1, i-1);
      KillSpaces(Symbol);
      s1 := Copy(s, i+1, Length(s)-i);
      KillSpaces(s1);
      Val(s1, Value, Code);
      IF Code<>0 THEN Error:=True;
      AddSymbol(T, Symbol, Value);
    END;
    IF GetOpCode(s)<>NumOps THEN Inc(Address);
  END;
  {$I+}
END;


PROCEDURE Translate(VAR Infile, Outfile:TEXT; T:PSymbolTree; VAR Error:Boolean);

VAR
  Code: Word;
  s:    STRING;
  CodeString: STRING;
  Address:    Word;
  IsCode: Boolean;

BEGIN
  {$I-}
  Writeln;
  Readln(InFile, s);
  Writeln(s);
  Writeln(Outfile, s);

  Address := 0;
  WHILE NOT Error AND NOT Eof(InFile) DO
  BEGIN
    WHILE Eoln(InFile) AND NOT Eof(InFile) DO REadln(InFile);
    IF NOT Eof(InFile) THEN
    BEGIN
      Readln(InFile, s);
      IF IOResult<>0 THEN Error:=True;
      TranslateLine(s, T, Code, IsCode, Error);
      IF IsCode THEN
      BEGIN
	CodeString := WordToBinary(Code);
	Writeln(OutFile, CodeString, Address:6, ': ', s);
        Inc(Address);
      END
      ELSE
	CodeString := '                ';

      Writeln(CodeString, Address:6, ': ', s);

      IF IOResult<>0 THEN Error:=True;
    END;
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
         OutName := Dir + Name + '.MAC';
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
  SymbolTree:      PSymbolTree;

BEGIN
  Writeln('MacAsm, Copyright (C) 1991 by J.P. van Best, Oegstgeest, The Netherlands.');
  Error := False;

  GetFileNames(InName, OutName, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Usage: MACASM Infile [OutFile]');
    Writeln('Converts ASCII file Infile with MAC-1 assembler code to binary MAC-1 file.');
    Writeln('Default output extension ''.MAC''.');
    Halt(1);
  END;

  OpenFiles(InName, OutName, InFile, OutFile, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Error opening ', InName,' for input and ', OutName, ' for output.');
    Halt(1);
  END;

  Writeln('Pass one (make symbol table).');
  PassOne(InFile, SymbolTree, Error);
  IF Error THEN
  BEGIN
    Writeln;
    Writeln('Error in creating symbol table');
  END;

  ShowTree(SymbolTree);

  Writeln('Converting ', InName,' to ', OutName, '.');

  Reset(InFile);
  Translate(Infile, Outfile, SymbolTree, Error);
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


