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

UNIT Dump;

{$X+}

INTERFACE

USES
  Objects, Drivers, Views, Dialogs, Menus, App,
  Hex;

CONST
  cmChangeAddressFormat = 2001;
  cmChangeDataFormat    = 2002;
  cmAutoSize            = 2003;
  cmGoto                = 2004;

TYPE
  PMemoryViewer = ^TMemoryViewer;
  TMemoryViewer = OBJECT(TScroller)
    Data:     Pointer;
    WordSize: Byte;
    NumWords: Word;
    AddressFormat: Byte;
    DataFormat:    Byte;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar:PScrollBar;
                  AData:Pointer; ANumWords: Word; AWordSize:Byte;
                  AAddressFormat, ADataFormat: Byte);
    PROCEDURE Draw; VIRTUAL;
{    PROCEDURE DrawView; VIRTUAL; }
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
{    PROCEDURE SetState(AState:Word; Enable:Boolean); VIRTUAL; }
    PROCEDURE GetAutoSize(VAR NewSize:TPoint);
  END;

  PMemoryWindow = ^TMemoryWindow;
  TMemoryWindow = OBJECT(TWindow)
    Control: PMemoryViewer;
    CONSTRUCTOR Init(Bounds:TRect; ATitle:STRING; ANumber:Integer;
                  AData:Pointer; ANumWords: Word; AWordSize:Byte;
                  AAddressFormat, ADataFormat: Byte);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
{
    PROCEDURE Redraw; VIRTUAL;
    PROCEDURE Draw; VIRTUAL;
}  END;
IMPLEMENTATION

FUNCTION ChangeFormat(ATitle:STRING; OldFormat: Word): Byte;

VAR
  Dialog: PDialog;
  R:      TRect;
  Control: PView;
  NewFormat: Byte;
  Command : Word;

BEGIN
  R.Assign(0, 0, 35, 10);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  Dialog := New(PDialog, Init(R, ATitle));
  WITH Dialog^ DO
  BEGIN
    R.Assign(2, 3, Size.X-2, 6);
    Control := New(PRadioButtons, Init(R,
      NewSItem('~D~ecimal',
      NewSItem('~H~exadecimal',
      NewSItem('~B~inary',
      NIL)))));
    Insert(Control);
    Control^.SetData(OldFormat);

    R.Assign(2, 2, Size.X-2, 3);
    Insert(New(PLabel, Init(R, '~F~ormat', Control)));

    R.Assign(5, Size.Y-3, 15, Size.Y-1);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
    R.Assign(20, Size.Y-3, 30, Size.Y-1);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

    SelectNext(False);
  END;

  Command := DeskTop^.ExecView(Dialog);
  IF Command<>cmQuit THEN
  BEGIN
    Dialog^.GetData(NewFormat);
    ChangeFormat := NewFormat;
  END;

  Dispose(Dialog, Done);
END;


{ TMemoryViewer }

CONSTRUCTOR TMemoryViewer.Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar:PScrollBar;
                            AData:Pointer; ANumWords: Word; AWordSize:Byte;
                            AAddressFormat, ADataFormat: Byte);

BEGIN
  TScroller.Init(Bounds, AHScrollBar, AVScrollBar);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options := Options OR (ofSelectable + ofFirstClick);
  SetState(sfCursorVis, True);
  Data := AData;
  NumWords := ANumWords;
  WordSize := AWordSize;
  AddressFormat := AAddressFormat;
  DataFormat := ADataFormat;

  SetLimit(Size.X, NumWords);
END;

PROCEDURE TMemoryViewer.Draw;

VAR
  B: TDrawBuffer;
  C: Byte;
  i: Integer;
  S, S0: STRING;

BEGIN
  C := GetColor(1);
  FOR i:=0 TO Size.Y-1 DO
  BEGIN
    MoveChar(B, ' ', C, Size.X);
    IF Delta.Y + i <= NumWords THEN
    BEGIN
      CASE AddressFormat OF
        nbDecimal:     Str(Delta.Y + i:5, S);
        nbHexaDecimal: S := HexW(Delta.Y + i);
        nbBinary:      S := BinW(Delta.Y + i);
      END;
      S := ' ' + S + ': ';
      CASE DataFormat OF
        nbDecimal: CASE WordSize OF
          1: Str(TByteArray(Data^)[Delta.Y + i]:3, S0);
          2: Str(TWordArray(Data^)[Delta.Y + i]:6, S0);
          4: Str(TLongArray(Data^)[Delta.Y + i]:11, S0);
        END;
        nbHexaDecimal: CASE WordSize OF
          1: S0 := HexB(TByteArray(Data^)[Delta.Y + i]);
          2: S0 := HexW(TWordArray(Data^)[Delta.Y + i]);
          4: S0 := HexL(TLongArray(Data^)[Delta.Y + i]);
        END;
        nbBinary: CASE WordSize OF
          1: S0 := BinB(TByteArray(Data^)[Delta.Y + i]);
          2: S0 := BinW(TWordArray(Data^)[Delta.Y + i]);
          4: S0 := BinL(TLongArray(Data^)[Delta.Y + i]);
        END;
      END;
      MoveStr(B, Copy(S + S0, 1, Size.X), C);
    END;
    WriteLine(0, i, Size.X, 1, B);
  END;
END;

PROCEDURE TMemoryViewer.HandleEvent(VAR Event:TEvent);

PROCEDURE LocalMenu;

VAR
  R: TRect;
  M: PMenuBox;
  Command: Word;

BEGIN
  R.Assign(0, 0, 30, 10);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  M := New(PMenuBox, Init(R, NewMenu(
    NewItem('~A~ddress format', '', kbNoKey, cmChangeAddressFormat, hcNoContext,
    NewItem('~D~ata format', '', kbNoKey, cmChangeDataFormat, hcNoContext,
    NewItem('~G~oto', '', kbNoKey, cmGoto, hcNoContext,
    NIL)))),
  NIL));
  Command := DeskTop^.ExecView(M);
  Dispose(M, Done);
  CASE Command OF
    cmChangeAddressFormat: AddressFormat := ChangeFormat('Address format', AddressFormat);
    cmChangeDataFormat: DataFormat := ChangeFormat('Data format', DataFormat);
  END;
  Message(Owner, evCommand, cmAutoSize, NIL)
END;
   
BEGIN

  TScroller.HandleEvent(Event);
  IF Event.What=evKeyDown THEN
    CASE Event.KeyCode OF
      kbAltF10: BEGIN
                  LocalMenu;
                  ClearEvent(Event);
                END;
    END;
END;
{
PROCEDURE TMemoryViewer.SetState(AState:Word; Enable:Boolean);

BEGIN
  TScroller.SetState(AState, Enable);
  IF Enable AND (AState AND sfExposed<>0) THEN
    SetLimit(Limit.X, Limit.Y);
END;
}
{
PROCEDURE TMemoryViewer.DrawView;

BEGIN
  TScroller.DrawView;
END;
}
PROCEDURE TMemoryViewer.GetAutoSize(VAR NewSize:TPoint);

BEGIN
  NewSize.X := 1+2+1;
  NewSize.Y := Size.Y;
  CASE AddressFormat OF
    nbDecimal:     Inc(NewSize.X, 5);
    nbHexaDecimal: Inc(NewSize.X, 4);
    nbBinary: 	   Inc(NewSize.X, 16);
  END;

  CASE DataFormat OF
    nbDecimal: CASE WordSize OF
      1: Inc(NewSize.X, 3);
      2: Inc(NewSize.X, 6);
      4: Inc(NewSize.X, 11);
    END;
    nbHexaDecimal: CASE WordSize OF
      1: Inc(NewSize.X, 2);
      2: Inc(NewSize.X, 4);
      4: Inc(NewSize.X, 8);
    END;
    nbBinary: CASE WordSize OF
      1: Inc(NewSize.X, 8);
      2: Inc(NewSize.X, 16);
      4: Inc(NewSize.X, 32);
    END;
  END;
END;

{ TMemoryWindow }

CONSTRUCTOR TMemoryWindow.Init(Bounds:TRect; ATitle:STRING; ANumber:Integer;
  AData:Pointer; ANumWords:Word; AWordSize:Byte;
  AAddressFormat, ADataFormat: Byte );

VAR
  R: TRect;
  NewSize: TPoint;

BEGIN
  TWindow.Init(Bounds, ATitle, ANumber);
  Options := Options AND ($FFFF-ofBuffered);
  GetExtent(R);
  R.GRow(-1, -1);
  Control := New(PMemoryViewer, Init(R,
    StandardScrollBar(sbHorizontal + sbHandleKeyboard),
    StandardScrollBar(sbVertical + sbHandleKeyboard),
    AData, ANumWords, AWordSize, AAddressFormat, ADataFormat));
  Insert(Control);
  Control^.GetAutoSize(NewSize);
  GrowTo(NewSize.X+2, NewSize.Y+2);
  Control^.GrowTo(NewSize.X, NewSize.Y);
END;


PROCEDURE TMemoryWindow.HandleEvent(VAR Event:TEvent);

VAR
  NewSize: TPoint;

BEGIN
  TWindow.HandleEvent(Event);

  IF (Event.What=evCommand) AND (Event.Command=cmAutoSize) THEN
  BEGIN
    Control^.GetAutoSize(NewSize);
    GrowTo(NewSize.X+2, NewSize.Y+2);
    Control^.GrowTo(NewSize.X, NewSize.Y);
  END;
END;
{
PROCEDURE TMemoryWindow.Redraw;

BEGIN
  TWindow.ReDraw;
END;

PROCEDURE TMemoryWindow.Draw;

BEGIN
  TWindow.Draw;
END;

}



END.






