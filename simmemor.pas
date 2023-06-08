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

UNIT SimMemory;

INTERFACE

USES
   Objects, Hex;

TYPE
   PMEmory = ^TMemory;
   TMemory = OBJECT(TObject)
      Data: Pointer;
      NumDataBits: Word;
      NumAddressBits: Word;
      Size: Word;
      BytesPerWord: Word;
      Owner: Boolean;
      CONSTRUCTOR Init(VAR AData:Pointer; ANumDataBits:Word;
         ANumAddressBits:Word; AOwner,AMakeMemory:Boolean);
      DESTRUCTOR Done; VIRTUAL;
      PROCEDURE Poke(Address:LongInt; Value:LongInt);
      FUNCTION Peek(Address:LongInt): LongInt;
   END;

IMPLEMENTATION

{===========================================================================}
{=======                        TMemory                               ======}
{===========================================================================}

CONSTRUCTOR TMemory.Init(VAR AData:Pointer; ANumDataBits:Word;
   ANumAddressBits:Word; AOwner,AMakeMemory:Boolean);

BEGIN
   TObject.Init;
   NumDataBits := ANumDataBits;
   NumAddressBits := ANumAddressBits;
   Size := 1 SHL NumAddressBits;
   BytesPerWord := (NumDataBits + 7) DIV 8;
   Owner := AOwner;
   IF AMakeMemory THEN GetMem(AData, Size * BytesPerWord);
   Data := AData;
END;

DESTRUCTOR TMemory.Done;

BEGIN
   IF Owner THEN FreeMem(Data, Size * BytesPerWord);
   TObject.Done;
END;

PROCEDURE TMemory.Poke(Address:LongInt; Value:LongInt);

BEGIN
   Move(PByteArray(Data)^[Address * BytesPerWord], Value, BytesPerWord);
END;

FUNCTION TMemory.Peek(Address:LongInt): LongInt;

VAR
  Value: LongInt;

BEGIN
   Move(PByteArray(Data)^[Address * BytesPerWord], Value, 4);
   Peek := Value AND ($FFFFFFFF SHR (8 * BytesPerWord));
END;

END.
