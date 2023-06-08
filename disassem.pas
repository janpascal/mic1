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

UNIT Disassembly;

{$X+}

INTERFACE

USES
  Objects, Drivers, Views, Dialogs, Menus, App,
  Hex, MicroCmd, Values;

TYPE
  PDisasmView = ^TDisASmView;
  TDisAsmView = OBJECT(TView)
    Data: Pointer;
    AddressSize: Byte;  { in bits }
    WordSize: Byte;
    AddressFormat: Byte;
    LocalAddress: Word;
    CurrentAddress: Word;
    TopAddress: Word;
    BottomAddress: Word;
    Range: Word;
    CONSTRUCTOR Init(VAR Bounds: TRect; AData:Pointer; ARange: Word;
                  AAddressSize, AWordSize:Byte; AAddressFormat:Byte);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    PROCEDURE DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word); VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
    PROCEDURE SetCurrentAddress(NewAddress:Word);
    PROCEDURE SetLocalAddress(NewAddress:Word);
    FUNCTION Before(Address:Word): Word; VIRTUAL;
    FUNCTION After(Address:Word): Word; VIRTUAL;
  END;

  PDisasmWindow = ^TDisasmWindow;
  TDisasmWindow = OBJECT(TWindow)
    Control: PDisasmView;
    CONSTRUCTOR Init(VAR Bounds:TRect; ATitle:STRING; ANumber:Integer; AControl:PDisasmView);
    FUNCTION GetPalette: PPalette; virtual;
  END;

IMPLEMENTATION

{===========================================================================}
{==                       TDisAsmView                                     ==}
{===========================================================================}

CONSTRUCTOR TDisasmView.Init(VAR Bounds: TRect; AData:Pointer; ARange: Word;
              AAddressSize, AWordSize:Byte; AAddressFormat:Byte);

BEGIN
  TView.Init(Bounds);

  GrowMode := gfGrowAll + gfGrowRel;
  SetState(sfCursorVis, True);
  Options := ofSelectable;

  Data := AData;
  Range := ARange;
  AddressSize := AAddressSize;
  WordSize := AWordSize;
  AddressFormat := AAddressFormat;
  CurrentAddress := 0;
  LocalAddress := 0;
  TopAddress := 0;
END;

PROCEDURE TDisasmView.HandleEvent(VAR Event:TEvent);

VAR
  i: Word;
  MouseWhere: TPoint;

BEGIN
  TView.HandleEvent(Event);
  CASE Event.What OF
    evKeyDown: BEGIN
      CASE Event.KeyCode OF
        kbUp: BEGIN
          LocalAddress := Before(LocalAddress);
          IF LocalAddress<TopAddress THEN TopAddress := LocalAddress;
          DrawView;
        END;
        kbPgUp: BEGIN
          FOR i:=1 TO Size.Y-1 DO LocalAddress := Before(LocalAddress);
          FOR i:=1 TO Size.Y-1 DO TopAddress := Before(TopAddress);
          DrawView;
        END;
        kbCtrlPgUp: BEGIN
          LocalAddress := 0;
          TopAddress := 0;
          DrawView;
        END;
        kbDown: BEGIN
          LocalAddress := After(LocalAddress);
          IF LocalAddress>BottomAddress THEN TopAddress := After(TopAddress);
          DrawView;
        END;
        kbPgDn: BEGIN
          FOR i:=1 TO Size.Y-1 DO LocalAddress := After(LocalAddress);
          FOR i:=1 TO Size.Y-1 DO TopAddress := After(TopAddress);
          IF TopAddress>Range-Size.Y THEN TopAddress := Range-Size.Y;
          DrawView;
        END;
        kbCtrlPgDn: BEGIN
          LocalAddress := Range-Size.Y;
          TopAddress := Range-Size.Y;
          DrawView;
        END;
      END;
    END;
    evCommand: BEGIN
      IF Event.Command=cmGoto THEN
      BEGIN
        LocalAddress := GetNewAddress('Address', 'Goto', LocalAddress,
          AddressFormat, Range);
        IF (LocalAddress<TopAddress) OR (LocalAddress>BottomAddress) THEN
        BEGIN
          TopAddress := Before(LocalAddress);
          IF TopAddress>Range-Size.Y THEN TopAddress := Range-Size.Y;
        END;
        DrawView;
      END;
    END;
    evMouseDown: BEGIN
      MakeLocal(Event.Where, MouseWhere);
      LocalAddress := TopAddress + MouseWhere.Y;
      DrawView;
    END;
  END;
END;

PROCEDURE TDisasmView.DisAssemble(Address: Word; VAR S:STRING; VAR NumWords:Word);

BEGIN
  Abstract;
END;

PROCEDURE TDisasmView.Draw;

VAR
  B: TDrawBuffer;
  C, NormalColor, CurrentColor, LocalColor: Byte;
  i,j: Integer;
  S, S0: STRING;
  Code: STRING;
  NumWords: Word;
  Address: Word;

BEGIN
  NormalColor := GetColor(6);
  LocalColor := GetColor(7);
  CurrentColor := GetColor(4);

  Address := TopAddress;
  BottomAddress := Range;
  FOR i:=0 TO Size.Y-1 DO
  BEGIN
    MoveChar(B, ' ', NormalColor, Size.X);
    IF Address < Range THEN
    BEGIN
      S := LongToString(Address, AddressSize, AddressFormat);
      S := ' ' + S + ': ';

      DisAssemble(Address, Code, NumWords);

      FOR j:=0 TO NumWords-1 DO
      BEGIN
        CASE WordSize OF
           8: S0 := HexB(TByteArray(Data^)[Address]);
          16: S0 := HexW(TWordArray(Data^)[Address]);
          32: S0 := HexL(TLongArray(Data^)[Address]);
        END;
        S := S + S0 + ' ';
      END;

      S := Copy(S + ' ' + Code + ' ', 1, Size.X);
      IF Address=CurrentAddress THEN
      BEGIN
        MoveChar(B, ' ', CurrentColor, Size.X);
        MoveStr(B, S, CurrentColor)
      END
      ELSE
        IF Address=LocalAddress THEN
        BEGIN
          MoveChar(B, ' ', LocalColor, Size.X);
          MoveStr(B, S, LocalColor)
        END
        ELSE
          MoveStr(B, S, NormalColor);

      IF (i=Size.Y-1) THEN BottomAddress := Address;

      Inc(Address, NumWords);
    END;
    WriteLine(0, i, Size.X, 1, B);
  END;
END;

FUNCTION TDisasmView.Before(Address:Word): Word;

VAR
  NewAddress: Word;
  NumWords: Word;
  S: STRING;

BEGIN
  NumWords := 1;
  NewAddress := Address;
  WHILE (NewAddress>0) AND (NewAddress+NumWords<>Address) DO
  BEGIN
    Dec(NewAddress);
    Disassemble(NewAddress, S, NumWords);
  END;

  Before := NewAddress;
END;

FUNCTION TDisasmView.After(Address:Word): Word;

VAR
  NumWords: Word;
  S: STRING;

BEGIN
  Disassemble(Address, S, NumWords);
  After := Address + NumWords;
END;

PROCEDURE TDisasmView.SetCurrentAddress(NewAddress:Word);

BEGIN
  IF (NewAddress=Before(TopAddress)) THEN
    TopAddress := Before(TopAddress)
  ELSE
    IF NewAddress=After(BottomAddress) THEN
      TopAddress := After(TopAddress)
    ELSE
      IF (NewAddress<TopAddress) OR (NewAddress>BottomAddress) THEN
        TopAddress := Before(NewAddress);

  IF TopAddress>Range-Size.Y THEN TopAddress := Range-Size.Y;
  CurrentAddress := NewAddress;
  LocalAddress := NewAddress;
END;

PROCEDURE TDisasmView.SetLocalAddress(NewAddress:Word);

BEGIN
  IF (NewAddress=Before(TopAddress)) THEN
    TopAddress := Before(TopAddress)
  ELSE
    IF NewAddress=After(BottomAddress) THEN
      TopAddress := After(TopAddress)
    ELSE
      IF (NewAddress<TopAddress) OR (NewAddress>BottomAddress) THEN
        TopAddress := Before(NewAddress);

  IF TopAddress>Range-Size.Y THEN TopAddress := Range-Size.Y;
  LocalAddress := NewAddress;
END;

CONSTRUCTOR TDisasmWindow.Init(VAR Bounds:TRect; ATitle:STRING; ANumber:Integer; AControl: PDisasmView);

VAR
  R: TRect;
  NewSize: TPoint;

BEGIN
  TWindow.Init(Bounds, ATitle, ANumber);
  Options := Options AND ($FFFF-ofBuffered);
  GrowMode := 0;
  SetState(sfCursorVis, True);

  Control := AControl;
  Insert(Control);
END;

FUNCTION TDisasmWindow.GetPalette: PPalette;

CONST
  NewPalette: TPalette = CBlueWindow;

BEGIN
  GetPalette := @NewPalette;
END;

END.
