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

UNIT Hex;

INTERFACE

TYPE
  PLongArray = ^TLongArray;
  TLongArray = ARRAY[0..16380] OF LongInt;
  PBoolean = ^Boolean;
  PByte    = ^Byte;
  PWord    = ^Word;
  PLong    = ^LongInt;

FUNCTION HexStringToByte(S:STRING): Byte;
FUNCTION HexB(B:Byte): STRING;
FUNCTION HexW(W:Word): STRING;
FUNCTION HexL(L:Longint): STRING;
FUNCTION BinB(B:Byte): STRING;
FUNCTION BinW(W:Word): STRING;
FUNCTION BinL(L:LongInt): STRING;
FUNCTION HexStringToLongInt(s:STRING): LongInt;
FUNCTION BinStringToLongInt(s:STRING): LongInt;
FUNCTION TestHexString(S: STRING): Boolean;
FUNCTION TestBinString(S: STRING): Boolean;
{FUNCTION StringToLongInt(s:STRING): LongInt;}

IMPLEMENTATION

FUNCTION TestHexString(S: STRING): Boolean;

VAR
  i: Byte;
  Ok: Boolean;

BEGIN
  Ok := True;
  FOR i:=1 TO Length(S) DO
    IF NOT (UpCase(S[i]) IN ['0'..'9','A'..'F']) THEN Ok:=False;
  TestHexString := Ok;
END;
 
FUNCTION TestBinString(S: STRING): Boolean;

VAR
  i: Byte;
  Ok: Boolean;

BEGIN
  Ok := True;
  FOR i:=1 TO Length(S) DO
    IF NOT (S[i] IN ['0','1']) THEN Ok:=False;
  TestBinString := Ok;
END;
 
FUNCTION HexCharToByte(C:Char): Byte;

BEGIN
  HexCharToByte := (Ord(C) AND $0F) + 9*Ord(C>'9');
END;

FUNCTION HexStringToByte(S:STRING): Byte;

BEGIN
  HexStringToByte := 16*HexCharToByte(S[1])+HexCharToByte(S[2]);
END;

FUNCTION HexB(B:Byte): STRING;

CONST
  HexChars : ARRAY[$00..$0F] OF Char =
    '0123456789ABCDEF';

BEGIN
  HexB := HexChars[B SHR 4] + HexChars[B AND $0F];
END;

FUNCTION HexW(W:Word): STRING;

BEGIN
  HexW := HexB(W SHR 8) + HexB(W AND $00FF);
END;

FUNCTION HexL(L:LongInt): STRING;

BEGIN
  HexL := HexW(L SHR 16) + HexW(L AND $0000FFFF);
END;

FUNCTION BinB(B:Byte): STRING;

VAR
  i: Byte;
  S: STRING;

BEGIN
  S := '';
  FOR i:=0 TO 7 DO
  BEGIN
    IF (B AND 1) = 1 THEN S := '1' + S ELSE S := '0' + S;
    B := B SHR 1;
  END;
  BinB := S;
END;

FUNCTION BinW(W:Word): STRING;

BEGIN
  BinW := BinB(W SHR 8) + BinB(W AND $00FF);
END;

FUNCTION BinL(L:LongInt): STRING;

BEGIN
  BinL := BinW(L SHR 16) + BinW(L AND $0000FFFF);
END;

FUNCTION HexStringToLongInt(s:STRING): LongInt;

VAR
  Value: LongInt;

BEGIN
  Value := 0;
  WHILE s<>'' DO
  BEGIN
    Value := Value SHL 4;
    Value := Value + HexCharToByte(s[1]);
    Delete(s,1,1);
  END;
  HexStringToLongInt := Value;
END;

FUNCTION BinStringToLongInt(s:STRING): LongInt;

VAR
  Value: LongInt;

BEGIN
  Value := 0;
  WHILE s<>'' DO
  BEGIN
    Value := Value SHL 1;
    IF s[1]='1' THEN Inc(Value);
    Delete(s,1,1);
  END;
  BinStringToLongInt := Value;
END;

{
FUNCTION StringToLongInt(s:STRING): LongInt;

BEGIN
  IF (Copy(s,1,2)='0x') OR (Copy(s,1,2)='0X') OR
     (Copy(s,Length(s),1) IN ['h','H']) OR
     (s[1]='$') THEN
    StringToLongInt := HexStringToLongInt(
  ELSE
    IF (Copy(s,1,2)='0b') OR (Copy(s,1,2)='0B') OR
     (Copy(s,Length(s),1) IN ['b','B']) OR
     (s[1]='%') THEN
    StringToLongInt := HexStringToLongInt
  ELSE
}




END.
