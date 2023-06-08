{$A+,B-,D+,E-,F-,G-,I+,L+,N-,O-,R-,S+,V+,X+}
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

UNIT WinNumbers;

INTERFACE

USES
  Objects, Drivers, Views, Menus, Dialogs, App,
  Hex;

CONST
  cmClosingNumber = 1999;

TYPE
  PWordCollection = ^TWordCollection;
  TWordCollection = OBJECT(TSortedCollection)
    PROCEDURE FreeItem(Item: Pointer); VIRTUAL;
    FUNCTION Compare(Key1, Key2: Pointer): LongInt; VIRTUAL;
  END;

  PWindowNumbers = ^TWindowNumbers;
  TWindowNumbers = OBJECT(TObject)
    Numbers: PWordCollection;
    CONSTRUCTOR Init;
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE AddNumber(ANumber:Word);
    PROCEDURE RemoveNumber(ANumber:Word);
    FUNCTION GetFreeNumber: Word;
    FUNCTION NextNumber:Word;
  END;

  PNumberWindow = ^TNumberWindow;
  TNumberWindow = OBJECT(TWindow)
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle: TTitleStr; UseNumber:Boolean);
    PROCEDURE Close; VIRTUAL;
  END;

  PNumberDialog = ^TNumberDialog;
  TNumberDialog = OBJECT(TDialog)
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle: TTitleStr; UseNumber:Boolean);
    PROCEDURE Close; VIRTUAL;
  END;

VAR
  WindowNumbers: PWindowNumbers;

IMPLEMENTATION

{===========================================================================}
{==                        TWordCollection                                ==}
{===========================================================================}

FUNCTION TWordCollection.Compare(Key1, Key2: Pointer): LongInt;

BEGIN
  IF PWord(Key1)^ < PWord(Key2)^ THEN Compare := -1 ELSE
  IF PWord(Key1)^ = PWord(Key2)^ THEN Compare :=  0 ELSE Compare := 1;
END;

PROCEDURE TWordCollection.FreeItem(Item: Pointer);

BEGIN
  Dispose(PWord(Item));
END;

{===========================================================================}
{==                         TWindowNumbers                                ==}
{===========================================================================}

CONSTRUCTOR TWindowNumbers.Init;

BEGIN
  TObject.Init;
  Numbers := New(PWordCollection, Init(5,2));
END;

DESTRUCTOR TWindowNumbers.Done;

BEGIN
  Dispose(Numbers, Done);
  TObject.Done;
END;

PROCEDURE TWindowNumbers.AddNumber(ANumber:Word);

VAR
  Number: PWord;

BEGIN
  New(Number);
  Number^ := ANumber;
  Numbers^.Insert(Number);
END;

PROCEDURE TWindowNumbers.RemoveNumber(ANumber:Word);

BEGIN
  Numbers^.AtFree(Numbers^.IndexOf(@ANumber));
END;

FUNCTION TWindowNumbers.GetFreeNumber: Word;

VAR
  i: Word;
  Index: LongInt;

BEGIN
  i := 1;
  WHILE Numbers^.Search(@i, Index) AND (i<=9) DO Inc(i);
  IF i>9 THEN i:=0;
  GetFreeNumber := i;
END;

FUNCTION TWindowNumbers.NextNumber:Word;

VAR
  Number: Word;

BEGIN
  Number := GetFreeNumber;
  AddNumber(Number);
  NextNumber := Number;
END;

{===========================================================================}
{==                          TNumberDialog                                ==}
{===========================================================================}

CONSTRUCTOR TNumberDialog.Init(VAR Bounds: TRect; ATitle: TTitleStr; UseNumber:Boolean);

BEGIN
  TDialog.Init(Bounds, ATitle);
  IF UseNumber THEN Number := WindowNumbers^.NextNumber ELSE Number:=wnNoNumber;
END;

PROCEDURE TNumberDialog.Close;

BEGIN
  IF (Number<>wnNoNumber) AND Valid(cmClose) THEN
    WindowNumbers^.RemoveNumber(Number);
  TDialog.Close;
END;

{===========================================================================}
{==                          TNumberWindow                                ==}
{===========================================================================}

CONSTRUCTOR TNumberWindow.Init(VAR Bounds: TRect; ATitle: TTitleStr; UseNumber:Boolean);

BEGIN
  TWindow.Init(Bounds, ATitle, wnNoNumber);
  IF UseNumber THEN Number := WindowNumbers^.NextNumber ELSE Number:=wnNoNumber;
END;

PROCEDURE TNumberWindow.Close;

BEGIN
  IF (Number<>wnNoNumber) AND Valid(cmClose) THEN
    WindowNumbers^.RemoveNumber(Number);
  TWindow.Close;
END;

BEGIN
  WindowNumbers := New(PWindowNumbers, Init);
END.
