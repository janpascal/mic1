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

UNIT Dump2;

{$X+}

INTERFACE

USES
  Objects, Drivers, Views, Dialogs, Menus, App,
  Hex, MicroCmd, Values;

TYPE
  PMemoryViewer = ^TMemoryViewer;
  TMemoryViewer = OBJECT(TListViewer)
    Data:     Pointer;
    AddressSize: Byte;
    WordSize: Byte;
    AddressFormat: Byte;
    DataFormat:    Byte;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar:PScrollBar;
                  AData:Pointer; ARange: Word; AAddressSize, AWordSize:Byte;
                  AAddressFormat, ADataFormat: Byte);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    FUNCTION GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING; VIRTUAL;
    PROCEDURE SelectItem(Item: sw_Integer); VIRTUAL;
{    PROCEDURE GetAutoSize(VAR NewSize:TPoint);}
    PROCEDURE GetPrefBounds(VAR Bounds:TRect);
{    PROCEDURE CalcBounds(VAR Bounds: TRect; Delta: TPoint); VIRTUAL;}
  END;

  PMemoryWindow = ^TMemoryWindow;
  TMemoryWindow = OBJECT(TWindow)
    Control: PMemoryViewer;
    CONSTRUCTOR Init(Bounds:TRect; ATitle:STRING; ANumber:Integer;
                  AData:Pointer; ANumWords: Word; AAddressSize, AWordSize:Byte;
                  AAddressFormat, ADataFormat: Byte);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    FUNCTION GetPalette: PPalette; virtual;
  END;

IMPLEMENTATION

{ TMemoryViewer }

CONSTRUCTOR TMemoryViewer.Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar:PScrollBar;
                            AData:Pointer; ARange: Word;
			    AAddressSize, AWordSize:Byte;
                            AAddressFormat, ADataFormat: Byte);

BEGIN
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  SetState(sfCursorVis, True);
  GrowMode := gfGrowAll + gfGrowRel;
  Data := AData;
  SetRange(ARange);
  AddressSize := AAddressSize;
  WordSize := AWordSize;
  AddressFormat := AAddressFormat;
  DataFormat := ADataFormat;
END;

FUNCTION TMemoryViewer.GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING;

VAR
  S, S0: STRING;

BEGIN
  CASE AddressFormat OF
    dfDecimal: BEGIN
      Str(Item:5, S);
    END;
    dfHexaDecimal: BEGIN
      S := HexL(Item);
      Delete(S, 1, Length(S)-AddressSize DIV 4);
    END;
    dfBinary: BEGIN
      S := BinL(Item);
      Delete(S, 1, Length(S)-AddressSize);
    END;
  END;

  S := ' ' + S + ': ';
  CASE DataFormat OF
    dfDecimal: CASE WordSize OF
      8: Str(TByteArray(Data^)[Item]:3, S0);
     16: Str(TWordArray(Data^)[Item]:6, S0);
     32: Str(TLongArray(Data^)[Item]:11, S0);
    END;
    dfHexaDecimal: CASE WordSize OF
      8: S0 := HexB(TByteArray(Data^)[Item]);
     16: S0 := HexW(TWordArray(Data^)[Item]);
     32: S0 := HexL(TLongArray(Data^)[Item]);
    END;
    dfBinary: CASE WordSize OF
      8: S0 := BinB(TByteArray(Data^)[Item]);
     16: S0 := BinW(TWordArray(Data^)[Item]);
     32: S0 := BinL(TLongArray(Data^)[Item]);
    END;
  END;
  GetText := Copy(S + S0, 1, MaxLen);
END;

PROCEDURE TMemoryViewer.HandleEvent(VAR Event:TEvent);

PROCEDURE LocalMenu;

VAR
  P: TPoint;
  R: TRect;
  M: PMenuBox;
  Command: Word;

BEGIN
  P.X := 1; P.Y := Focused-TopItem;
  MakeGlobal(P, R.A);
  R.B.X := R.A.X + 30; R.B.Y := R.A.Y + 10;

  IF R.B.X > DeskTop^.Size.X THEN R.Move(DeskTop^.Size.X - R.B.X, 0);
  IF R.B.Y > DeskTop^.Size.Y THEN R.Move(0, DeskTop^.Size.Y - R.B.Y);

  M := New(PMenuBox, Init(R, NewMenu(
    NewItem('~A~ddress format', '', kbNoKey, cmChangeAddressFormat, hcNoContext,
    NewItem('~D~ata format', '', kbNoKey, cmChangeDataFormat, hcNoContext,
    NIL))),
  NIL));
  Command := DeskTop^.ExecView(M);
  Dispose(M, Done);
  CASE Command OF
    cmChangeAddressFormat: AddressFormat := ChangeFormat('Address format', AddressFormat);
    cmChangeDataFormat: DataFormat := ChangeFormat('Data format', DataFormat);
  END;
  Message(Owner, evCommand, cmSetSize, @Self)
END;

VAR
  NewAddress: Word;
   
BEGIN
  TListViewer.HandleEvent(Event);
  IF (Event.What=evCommand) THEN
  BEGIN
    CASE Event.Command OF
      cmLocal: BEGIN
        LocalMenu;
        ClearEvent(Event);
      END;
      cmGoto: BEGIN
        NewAddress := GetNewAddress('Address', 'Goto', Focused,
          AddressFormat, Range);
        FocusItem(NewAddress);
      END;
    END;
  END;
END;

PROCEDURE TMemoryViewer.SelectItem(Item: sw_Integer);

VAR
  Dialog:    PDialog;
  R:         TRect;
  Value:     PValue;
  ValueItem: PValueItem;
  Command:   Word;

BEGIN
  TListViewer.SelectItem(Item);

  CASE WordSize OF
    8: Value := New(PByteValue, Init(@PByteArray(Data)^[Item], DataFormat, False));
    16:Value := New(PWordValue, Init(@PWordArray(Data)^[Item], DataFormat, False));
    32:Value := New(PLongValue, Init(@PLongArray(Data)^[Item], DataFormat, False));
  END;

  ValueItem := New(PValueItem, Init('Data', Value, NIL));

  R.Assign(0, 0, 55, 10);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  Dialog := New(PValueEditDialog, Init(R, '', ValueItem, False));

  Dialog^.SetData(Value^.Data^);

  REPEAT
    Command := DeskTop^.ExecView(Dialog);
    IF Command=cmSetFormat THEN
    BEGIN
      Message(Owner, evCommand, cmSetSize, @Self);
    END;
  UNTIL Command<>cmSetFormat;

  IF Command<>cmCancel THEN
    Dialog^.GetData(ValueItem^.Value^.Data^);

  Dispose(Dialog, Done);
  DataFormat := Value^.DataFormat;
  Dispose(ValueItem, Done);

  DeskTop^.ReDraw;
END;

PROCEDURE TMemoryViewer.GetPrefBounds(VAR Bounds:TRect);

VAR
  NewSize: TPoint;

BEGIN
  NewSize.X := 1+2+1+1;
  NewSize.Y := Size.Y;
  CASE AddressFormat OF
    dfDecimal:     Inc(NewSize.X, 5);
    dfHexaDecimal: Inc(NewSize.X, AddressSize DIV 4);
    dfBinary: 	   Inc(NewSize.X, AddressSize DIV 1);
  END;

  CASE DataFormat OF
    dfDecimal: CASE WordSize OF
      8: Inc(NewSize.X, 3);
     16: Inc(NewSize.X, 6);
     32: Inc(NewSize.X, 11);
    END;
    dfHexaDecimal: CASE WordSize OF
      8: Inc(NewSize.X, 2);
     16: Inc(NewSize.X, 4);
     32: Inc(NewSize.X, 8);
    END;
    dfBinary: CASE WordSize OF
      8: Inc(NewSize.X, 8);
     16: Inc(NewSize.X, 16);
     32: Inc(NewSize.X, 32);
    END;
  END;

  Bounds.A.X := 1; Bounds.A.Y := 1;
  Bounds.B.X := NewSize.X+1; Bounds.B.Y := NewSize.Y+1;
END;

{ TMemoryWindow }

CONSTRUCTOR TMemoryWindow.Init(Bounds:TRect; ATitle:STRING; ANumber:Integer;
  AData:Pointer; ANumWords:Word; AAddressSize, AWordSize:Byte;
  AAddressFormat, ADataFormat: Byte );

VAR
  R: TRect;
  NewSize: TPoint;

BEGIN
  TWindow.Init(Bounds, ATitle, ANumber);
  Options := Options AND ($FFFF-ofBuffered);
  GrowMode := 0;
  SetState(sfCursorVis, True);
  GetExtent(R);
  R.GRow(-1, -1);
  Control := New(PMemoryViewer, Init(R,
    NIL,
    StandardScrollBar(sbVertical + sbHandleKeyboard),
    AData, ANumWords, AAddressSize, AWordSize, AAddressFormat, ADataFormat));
  Insert(Control);
  Control^.GetPrefBounds(R);
  GrowTo(R.B.X+1, R.B.Y+1);
  Control^.Locate(R);
END;

PROCEDURE TMemoryWindow.HandleEvent(VAR Event:TEvent);

VAR
  R: TRect;

BEGIN
  IF (Event.What=evCommand) AND (Event.Command=cmSetSize) THEN
  BEGIN
    Control^.GetPrefBounds(R);
    GrowTo(R.B.X+1, R.B.Y+1);
    ClearEvent(Event)
  END
  ELSE
    TWindow.HandleEvent(Event);
END;

FUNCTION TMemoryWindow.GetPalette: PPalette;

CONST
  NewPalette: TPalette = CDialog;

BEGIN
  GetPalette := @NewPalette;
END;

END.






