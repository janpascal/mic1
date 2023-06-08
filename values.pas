{$A+,B-,D+,E+,F-,G-,I+,L+,N-,O-,R-,S+,V+,X+}
{$M 1638400,0,6553600}
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

UNIT Values;

INTERFACE

USES
  Dos,
  Objects, Drivers, Views, Dialogs, MsgBox, App,
  Hex, WinNumbers, DescList, MicroCmd;

TYPE
  PStringPtrArray = ^TStringPtrArray;
  TStringPtrArray = ARRAY[0..16380] OF PString;

CONST
  Spaces = '                                ';

  dfTrueFalse = 0;
  dfYesNo     = 1;

  dfDecimal     = 0;
  dfBinary      = 1;
  dfHexadecimal = 2;

TYPE
  PValue = ^TValue;
  TValue = OBJECT(TObject)
    Data: Pointer;
    DataFormat: Word;
    ValueOwner: Boolean;
    CONSTRUCTOR Init(AData:Pointer; ADataFormat: Word; AValueOwner: Boolean);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION DataSize: Word; VIRTUAL;
    FUNCTION StringToData(S:STRING): Boolean; VIRTUAL;
    FUNCTION DataToString: STRING; VIRTUAL;
    FUNCTION EditView(VAR Bounds: TRect): PView; VIRTUAL;
    FUNCTION GetDataFormatNames: PSItem; VIRTUAL;
  END;

  PStandardEditView = ^TStandardEditView;
  TStandardEditView = OBJECT(TInputLine)
    Value: PValue;
    CONSTRUCTOR Init(VAR Bounds:TRect; AValue:PValue);
    FUNCTION DataSize: DWord; VIRTUAL;
    PROCEDURE GetData(var Rec); VIRTUAL;
    PROCEDURE SetData(var Rec); VIRTUAL;
    FUNCTION Valid(Command:Word): Boolean; VIRTUAL;
   END;

  PStringValue = ^TStringValue;
  TStringValue = OBJECT(TValue)
    MaxLen: Byte;
    CONSTRUCTOR Init(AData:PString; AMaxLen:Word; AValueOwner:Boolean);
    FUNCTION DataSize: Word; VIRTUAL;
    FUNCTION StringToData(S:STRING): Boolean; VIRTUAL;
    FUNCTION DataToString: STRING; VIRTUAL;
  END;

  PBooleanValue = ^TBooleanValue;
  TBooleanValue = OBJECT(TValue)
    CONSTRUCTOR Init(AData:PBoolean; ADataFormat: Word; AValueOwner: Boolean);
    FUNCTION DataSize: Word; VIRTUAL;
    FUNCTION StringToData(S:STRING): Boolean; VIRTUAL;
    FUNCTION DataToString: STRING; VIRTUAL;
    FUNCTION EditView(VAR Bounds: TRect): PView; VIRTUAL;
    FUNCTION GetDataFormatNames: PSItem; VIRTUAL;
  END;

  PBooleanEditView = ^TBooleanEditView;
  TBooleanEditView = OBJECT(TRadioButtons)
    MyValue: PBooleanValue;
    CONSTRUCTOR Init(VAR Bounds:TRect; AValue:PBooleanValue);
    PROCEDURE GetData(var Rec); VIRTUAL;
    PROCEDURE SetData(var Rec); VIRTUAL;
   END;

  POrdinalValue = ^TOrdinalValue;
  TOrdinalValue = OBJECT(TValue)
    NumBits: Byte;
    MinValue, MaxValue: LongInt;
    CONSTRUCTOR Init(AData:Pointer; ANumBits:Byte;
      AMinValue, AMaxValue: LongInt; ADataFormat: Word; AValueOwner: Boolean);
    FUNCTION StringToData(S:STRING): Boolean; VIRTUAL;
    FUNCTION DataToString: STRING; VIRTUAL;
    FUNCTION DataSize: Word; VIRTUAL;
    FUNCTION GetDataFormatNames: PSItem; VIRTUAL;
  END;

  PByteValue = ^TByteValue;
  TByteValue = OBJECT(TOrdinalValue)
    CONSTRUCTOR Init(AData:PByte; ADataFormat: Word; AValueOwner: Boolean);
  END;

  PWordValue = ^TWordValue;
  TWordValue = OBJECT(TOrdinalValue)
    CONSTRUCTOR Init(AData:PWord; ADataFormat: Word; AValueOwner: Boolean);
  END;

  PLongValue = ^TLongValue;
  TLongValue = OBJECT(TOrdinalValue)
    CONSTRUCTOR Init(AData:PLong; ADataFormat: Word; AValueOwner: Boolean);
  END;

  PValueItem = ^TValueItem;
  TValueItem = OBJECT(TObject)
    Name: PString;
    Value: PValue;
    Next: PValueItem;
    CONSTRUCTOR Init(AName:STRING; AValue:PValue; ANext:PValueItem);
    DESTRUCTOR Done; VIRTUAL;
  END;
(*
  PValueDescription = ^TValueDescription;
  TValueDescription = OBJECT(TDescription)
    ValueItem: PValueItem;
    CONSTRUCTOR Init(AValueItem:PValueItem; ANext: PDescription);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION GetText(Line: Integer; MaxLen: Integer): STRING; VIRTUAL;
    PROCEDURE SelectLine(Line: Integer; KeyCode: Word); VIRTUAL;
    PROCEDURE Update; VIRTUAL;
  END;
*)
  PValueEditDialog = ^TValueEditDialog;
  TValueEditDialog = OBJECT(TDialog)
    ValueItem: PValueItem;
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle: TTitleStr; AValueItem: PValueItem; AllowChangeFormat:Boolean);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
  END;

  PValues = ^TValues;
  TValues = OBJECT(TListViewer)
    Data: PValueItem;
    MaxNameLength: Word;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      AData: PValueItem);
    DESTRUCTOR Done; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    FUNCTION GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING; VIRTUAL;
    PROCEDURE SelectItem(Item: LongInt); VIRTUAL;
    PROCEDURE GetPrefBounds(VAR Bounds:TRect); VIRTUAL;
  END;

  PValuesWindow = ^TValuesWindow;
  TValuesWindow = OBJECT(TNumberDialog)
    Buttons: Boolean;
    CONSTRUCTOR Init(VAR Bounds: TRect; ATitle:TTitleStr; UseNumber:Boolean;
      AButtons:Boolean; ValueItem:PValueItem);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    FUNCTION GetPalette: PPalette; VIRTUAL;
    PROCEDURE SizeLimits(VAR Min, Max: TPoint); VIRTUAL;
  END;


FUNCTION NewBooleanItem(Name:STRING; VAR Data:Boolean; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;
FUNCTION NewStringItem(Name:STRING; VAR Data:STRING; MaxLen:Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;
FUNCTION NewByteItem(Name:STRING; VAR Data:Byte; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;
FUNCTION NewWordItem(Name:STRING; VAR Data:Word; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;
FUNCTION NewLongItem(Name:STRING; VAR Data:LongInt; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;

FUNCTION LongToString(Value:LongInt; NumBits:Byte; DataFormat:Byte): STRING;
FUNCTION ChangeFormat(ATitle:STRING; OldFormat: Word): Byte;
FUNCTION GetNewAddress(Name,Title:STRING; OldAddress:Word; DataFormat:Word;
  Range:Word): Word;

IMPLEMENTATION

PROCEDURE UpperCase(VAR S:STRING);

VAR
  i: Byte;

BEGIN
  FOR i:=1 TO Length(S) DO S[i] := UpCase(S[i]);
END;

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
      NewSItem('~B~inary',
      NewSItem('~H~exadecimal',
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
  IF Command=cmQuit THEN
    ChangeFormat := OldFormat
  ELSE
  BEGIN
    Dialog^.GetData(NewFormat);
    ChangeFormat := NewFormat;
  END;

  Dispose(Dialog, Done);
END;

FUNCTION GetNewAddress(Name,Title:STRING; OldAddress:Word; DataFormat:Word;
  Range:Word): Word;

VAR
  Dialog: PDialog;
  R:      TRect;
  Command : Word;
  ValueItem: PValueItem;
  NewAddress: Word;

BEGIN
  ValueItem := NewWordItem(Name, OldAddress, DataFormat, False, NIL);
  PWordValue(ValueItem^.Value)^.MaxValue := Range-1;

  R.Assign(0, 0, 35, 10);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  Dialog := New(PValueEditDialog, Init(R, Title, ValueItem, False));

  Command := DeskTop^.ExecView(Dialog);
  IF Command=cmQuit THEN
    GetNewAddress := OldAddress
  ELSE
  BEGIN
    Dialog^.GetData(NewAddress);
    GetNewAddress := NewAddress;
  END;

  Dispose(Dialog, Done);
END;

{===========================================================================}
{==                    TStandardEditView                                  ==}
{===========================================================================}

CONSTRUCTOR TStandardEditView.Init(VAR Bounds:TRect; AValue:PValue);

BEGIN
  IF Bounds.B.Y-Bounds.A.Y>=1 THEN Bounds.B.Y := Bounds.A.Y + 1;
  TInputLine.Init(Bounds, 255);
  Value := AValue;
END;

FUNCTION TStandardEditView.DataSize: DWord;

BEGIN
  DataSize := Value^.DataSize;
END;

PROCEDURE TStandardEditView.GetData(var Rec);

BEGIN
  Move(Value^.Data^, Rec, DataSize);
END;

PROCEDURE TStandardEditView.SetData(var Rec);

BEGIN
  Move(Rec, Value^.Data^, DataSize);
  STRING(Data^) := Value^.DataToString;
  SelectAll(True);
END;

FUNCTION TStandardEditView.Valid(Command:Word): Boolean;

VAR
  S: STRING;
  Code: Integer;
  AnInt: LongInt;
  Ok: Boolean;

BEGIN
  IF Command=cmValid THEN
    Valid := TInputLine.Valid(Command)
  ELSE
    IF Command=cmOK THEN
    BEGIN
      Ok := Value^.StringToData(STRING(Data^));
      IF NOT Ok THEN
        MessageBox(^C'Invalid input', NIL, mfError OR mfOKButton);
      Valid := Ok;
    END
    ELSE
      Valid := TInputLine.Valid(Command);
END;


{===========================================================================}
{==                     TBooleanEditView                                  ==}
{===========================================================================}


CONSTRUCTOR TBooleanEditView.Init(VAR Bounds:TRect; AValue:PBooleanValue);

VAR
  Number: Word;

BEGIN
  IF Bounds.B.Y-Bounds.A.Y>=2 THEN Bounds.B.Y := Bounds.A.Y + 2;
  MyValue := AValue;
  CASE AValue^.DataFormat OF
    dfTrueFalse: TRadioButtons.Init(Bounds,
      NewSItem('True',
      NewSItem('False',
      NIL)));
    dfYesNo:     TRadioButtons.Init(Bounds,
      NewSItem('Yes',
      NewSItem('No',
      NIL)));
  END;
END;

PROCEDURE TBooleanEditView.GetData(var Rec);

VAR
  w: Word;

BEGIN
  TRadioButtons.GetData(w);

  Boolean(Rec) := w=0;
END;

PROCEDURE TBooleanEditView.SetData(var Rec);

VAR
  Number: Word;

BEGIN
  IF Boolean(Rec) THEN Number:=0 ELSE Number:=1;
  TRadioButtons.SetData(Number)
END;

{===========================================================================}
{==                        TValue                                         ==}
{===========================================================================}


CONSTRUCTOR TValue.Init(AData:Pointer; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TObject.Init;
  Data := AData;
  DataFormat := ADataFormat;
  ValueOwner := AValueOwner;
END;

DESTRUCTOR TValue.Done;

BEGIN
  TObject.Done;
  IF ValueOwner THEN FreeMem(Data, DataSize);
END;

FUNCTION TValue.StringToData(S:STRING): Boolean;

BEGIN
  Abstract;
END;

FUNCTION TValue.DataToString: STRING;

BEGIN
  Abstract;
END;

FUNCTION TValue.DataSize: Word;

BEGIN
  Abstract;
END;

FUNCTION TValue.EditView(VAR Bounds: TRect): PView;

BEGIN
  EditView := New(PStandardEditView, Init(Bounds, @Self));
END;

FUNCTION TValue.GetDataFormatNames: PSItem;

BEGIN
  GetDataFormatNames := NIL;
END;

{===========================================================================}
{==                         TStringValue                                  ==}
{===========================================================================}

CONSTRUCTOR TStringValue.Init(AData:PString; AMaxLen:Word; AValueOwner:Boolean);

BEGIN
  TValue.Init(AData, 0, AValueOwner);
  MaxLen := AMaxLen;
END;

FUNCTION TStringValue.DataSize: Word;

BEGIN
  DataSize := 1 + MaxLen;
END;

FUNCTION TStringValue.StringToData(S:STRING): Boolean;

BEGIN
  IF Length(S)<=MaxLen THEN
  BEGIN
    PString(Data)^ := S;
    StringToData := True
  END
  ELSE
    StringToData := False;
END;

FUNCTION TStringValue.DataToString: STRING;

BEGIN
  DataToString := PString(Data)^;
END;

{===========================================================================}
{==                        TBooleanValue                                  ==}
{===========================================================================}


CONSTRUCTOR TBooleanValue.Init(AData:PBoolean; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TValue.Init(AData, ADataFormat, AValueOwner);
END;

FUNCTION TBooleanValue.DataSize: Word;

BEGIN
  DataSize := SizeOf(Boolean);
END;

FUNCTION TBooleanValue.StringToData(S:STRING): Boolean;

BEGIN
  Abstract;
END;

FUNCTION TBooleanValue.DataToString: STRING;

VAR
  s: STRING;

BEGIN
  CASE DataFormat OF
    dfTrueFalse: IF Boolean(Data^) THEN s:='True' ELSE s:='False';
    dfYesNo:     IF Boolean(Data^) THEN s:='Yes' ELSE s:='No';
  END;
  DataToString := s;
END;

FUNCTION TBooleanValue.EditView(VAR Bounds: TRect): PView;

BEGIN
  EditView := New(PBooleanEditView, Init(Bounds, @Self));
END;

FUNCTION TBooleanValue.GetDataFormatNames: PSItem;

BEGIN
  GetDataFormatNames :=
    NewSItem('True/False',
    NewSItem('Yes/No', NIL));
END;


{===========================================================================}
{==                        TOrdinalValue                                  ==}
{===========================================================================}

CONSTRUCTOR TOrdinalValue.Init(AData:Pointer; ANumBits:Byte;
  AMinValue, AMaxValue: LongInt; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TValue.Init(AData, ADataFormat, AValueOwner);
  NumBits := ANumBits;
  MinValue := AMinValue;
  MaxValue := AMaxValue;
END;

FUNCTION TOrdinalValue.DataSize: Word;

BEGIN
  DataSize := (NumBits+7) DIV 8;
END;

FUNCTION TOrdinalValue.StringToData(S:STRING): Boolean;

VAR
  MyValue: LongInt;
  MyDataFormat: Word;
  Ok: Boolean;
  Code: Integer;

BEGIN
  UpperCase(S);
  MyDataFormat := DataFormat;

  IF S[1]='$' THEN
  BEGIN
    MyDataFormat := dfHexaDecimal;
    Delete(S, 1, 1)
  END
  ELSE
  IF S[1]='%' THEN
  BEGIN
    MyDataFormat := dfBinary;
    Delete(S, 1, 1)
  END
  ELSE
  IF (Copy(S, 1, 2)='0X') THEN
  BEGIN
    MyDataFormat := dfHexaDecimal;
    Delete(S, 1, 2)
  END
  ELSE
  IF S[Length(S)]='H' THEN
  BEGIN
    MyDataFormat := dfHexaDecimal;
    Delete(S, Length(S), 1)
  END
  ELSE
  IF (Copy(S, 1, 2)='0B') THEN
  BEGIN
    MyDataFormat := dfBinary;
    Delete(S, 1, 2)
  END
  ELSE
  IF S[Length(S)]='B' THEN
  BEGIN
    MyDataFormat := dfBinary;
    Delete(S, Length(S), 1)
  END
  ELSE
  IF S[Length(S)]='D' THEN
  BEGIN
    MyDataFormat := dfDecimal;
    Delete(S, Length(S), 1)
  END
  ELSE
  IF (Copy(S, 1, 2)='0D') THEN
  BEGIN
    MyDataFormat := dfDecimal;
    Delete(S, 1, 2)
  END;

  CASE MyDataFormat OF
    dfDecimal: BEGIN
      Val(S, MyValue, Code);
      Ok := Code=0;
    END;
    dfHexaDecimal: BEGIN
      Ok := TestHexString(S);
      IF Ok THEN MyValue := HexStringToLongInt(S);
    END;
    dfBinary: BEGIN
      Ok := TestBinString(S);
      IF Ok THEN MyValue := BinStringToLongInt(S);
    END;
  END;

  OK := OK AND (MyValue>=MinValue) AND (MyValue<=MaxValue);
  StringToData := Ok;
  IF Ok THEN Move(MyValue, Data^, (NumBits+7) DIV 8);
END;


FUNCTION TOrdinalValue.DataToString: STRING;

VAR
  MyValue : LongInt;
  s: STRING;

BEGIN
  MyValue := 0;
  Move(Data^, MyValue, DataSize);
  CASE DataFormat OF
    dfDecimal:
    BEGIN
      Str(MyValue, s);
      s := s + 'd';
    END;
    dfHexaDecimal: BEGIN
      s := HexL(MyValue);
      Delete(s, 1, Length(s) - (NumBits+3) DIV 4);
      s := s + 'h';
    END;
    dfBinary: BEGIN
      s := BinL(MyValue);
      Delete(s, 1, Length(s) - NumBits);
      s := s + 'b';
    END;
  END;
  DataToString := s;
END;

FUNCTION TOrdinalValue.GetDataFormatNames: PSItem;

BEGIN
  GetDataFormatNames :=
    NewSItem('Decimal',
    NewSItem('Binary',
    NewSItem('Hexadecimal', NIL)));
END;

{===========================================================================}
{==                          TxxxxValue                                   ==}
{===========================================================================}


CONSTRUCTOR TByteValue.Init(AData:PByte; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TOrdinalValue.Init(AData, 8, 0, 255, ADataFormat, AValueOwner);
END;

CONSTRUCTOR TWordValue.Init(AData:PWord; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TOrdinalValue.Init(AData, 16, 0, 65535, ADataFormat, AValueOwner);
END;

CONSTRUCTOR TLongValue.Init(AData:PLong; ADataFormat: Word; AValueOwner: Boolean);

BEGIN
  TOrdinalValue.Init(AData, 32, -MaxLongInt, MaxLongInt, ADataFormat, AValueOwner);
END;

{===========================================================================}
{==                          TValueItem                                   ==}
{===========================================================================}


CONSTRUCTOR TValueItem.Init(AName:STRING; AValue:PValue; ANext:PValueItem);

BEGIN
  Name := NewStr(AName);
  Value := AValue;
  Next := ANext;
END;

DESTRUCTOR TValueItem.Done;

BEGIN
  Dispose(Value, Done);
  DisposeStr(Name);
  IF Next<>NIL THEN Dispose(Next, Done);
  TObject.Done;
END;

{===========================================================================}
{==                          TValues                                      ==}
{===========================================================================}

CONSTRUCTOR TValues.Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
  AData:PValueItem);

VAR
  p : PValueItem;
  i : Integer;
  NewSize: TPoint;

BEGIN
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  SetState(sfCursorVis, True);

  Data := AData;

  p := Data;
  MaxNameLength := 0;
  i := 0;
  WHILE p<>NIL DO
  BEGIN
    Inc(i);
    IF Length(p^.Name^)>MaxNameLength THEN MaxNameLength := Length(p^.Name^);
    p := p^.Next;
  END;

  SetRange(i);
  GrowMode := gfGrowAll + gfGrowRel;
END;

DESTRUCTOR TValues.Done;
{
VAR
  P, Temp: PValueItem;
}
BEGIN
  Dispose(Data, Done);
{    Dispose(P, Done);
    P := Temp^.Next;
  END;}
  TListViewer.Done;
END;

PROCEDURE TValues.HandleEvent(VAR Event:TEvent);

VAR
  NewBounds: TRect;

BEGIN
  TListViewer.HandleEvent(Event);
  IF (Event.What=evCommand) AND (Event.Command=cmSetSize) THEN
  BEGIN
    GetPrefBounds(NewBounds);
    Locate(NewBounds);
    ClearEvent(Event);
  END;
END;

FUNCTION TValues.GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING;

VAR
  i: Integer;
  S: STRING;
  ValueItem: PValueItem;

BEGIN
  ValueItem := Data;
  FOR i:=1 TO Item DO
    ValueItem := ValueItem^.Next;

  S := Copy(ValueItem^.Name^+Spaces, 1, MaxNameLength);

  GetText := Copy(S + ' : ' + ValueItem^.Value^.DataToString, 1, MaxLen);
END;

PROCEDURE TValues.SelectItem(Item: LongInt);

VAR
  Dialog:    PDialog;
  R:         TRect;
  S:         STRING;
  ValueItem: PValueItem;
  i:         Word;
  Ready:     Boolean;
  Control:   PView;
  Command:   Word;

BEGIN
  TListViewer.SelectItem(Item);

  ValueItem := Data;
  FOR i:=1 TO Item DO ValueItem := ValueItem^.Next;

  R.Assign(0, 0, 55, 10);
  R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
  Dialog := New(PValueEditDialog, Init(R, '', ValueItem, True));

  Dialog^.SetData(ValueItem^.Value^.Data^);

  REPEAT
    Command := DeskTop^.ExecView(Dialog);
    IF Command=cmSetFormat THEN
    BEGIN
      Message(Owner, evCommand, cmSetSize, @Self);
    END;
  UNTIL Command<>cmSetFormat;

  IF Command<>cmCancel THEN
  BEGIN
    Dialog^.GetData(ValueItem^.Value^.Data^);
    Message(Owner, evCommand, cmSetSize, @Self);
  END;

  Dispose(Dialog, Done);

  DeskTop^.ReDraw;
END;

PROCEDURE TValues.GetPrefBounds(VAR Bounds:TRect);

VAR
  ValueItem: PValueItem;
  LineLen: Word;
  MaxLineLen: Word;

BEGIN
  ValueItem := Data;
  MaxLineLen := 0;
  WHILE ValueItem<>NIL DO
  BEGIN
    LineLen := Length(ValueItem^.Value^.DataToString);
    IF LineLen > MaxLineLen THEN MaxLineLen := LineLen;
    ValueItem := ValueItem^.Next;
  END;

  Bounds.A.X := 1;
  Bounds.A.Y := 1;
  Bounds.B.X := 1 + MaxNameLength + 3 + MaxLineLen + 2;
  Bounds.B.Y := Range + 1;
END;

{===========================================================================}
{==                       TValueEditDialog                                ==}
{===========================================================================}

CONSTRUCTOR TValueEditDialog.Init(VAR Bounds: TRect; ATitle: TTitleStr;
  AValueItem: PValueItem; AllowChangeFormat:Boolean);

VAR
  S: STRING;
  R: TRect;
  Control: PView;

BEGIN
  TDialog.Init(Bounds, ATitle);

  ValueItem := AValueItem;

  S := ValueItem^.Name^ + ' :';
  R.Assign(2+Length(S), 2, Size.X-2, Size.Y-3);
  Control := ValueItem^.Value^.EditView(R);
  Insert(Control);

  R.Assign(2, 2, 2+Length(S), 3);
  Insert(New(PLabel, Init(R, S, Control)));

  R.Assign(5, Size.Y-3, 15, Size.Y-1);
  Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
  R.Assign(20, Size.Y-3, 30, Size.Y-1);
  IF AllowChangeFormat AND (ValueItem^.Value^.GetDataFormatNAmes<>NIL) THEN
  BEGIN
    Insert(New(PButton, Init(R, '~F~ormat', cmSetFormat, bfNormal)));
    R.Assign(35, Size.Y-3, 45, Size.Y-1);
  END;
  Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

  SelectNext(False);
END;

PROCEDURE TValueEditDialog.HandleEvent(VAR Event:TEvent);

VAR
  Dialog: PDialog;
  R:      TRect;
  Control: PView;
  NewFormat: Byte;
  Command : Word;

BEGIN
  TDialog.HandleEvent(Event);
  IF (Event.What=evCommand) AND (Event.Command=cmSetFormat) THEN
  BEGIN
    R.Assign(0, 0, 35, 10);
    R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
    Dialog := New(PDialog, Init(R, 'New data format'));
    WITH Dialog^ DO
    BEGIN
      R.Assign(2, 3, Size.X-2, 6);
      Control := New(PRadioButtons, Init(R, ValueItem^.Value^.GetDataFormatNames));
      Insert(Control);

      R.Assign(2, 2, Size.X-2, 3);
      Insert(New(PLabel, Init(R, '~F~ormat', Control)));

      R.Assign(5, Size.Y-3, 15, Size.Y-1);
      Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
      R.Assign(20, Size.Y-3, 30, Size.Y-1);
      Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));

      SelectNext(False);
    END;

    Dialog^.SetData(ValueItem^.Value^.DataFormat);

    Command := DeskTop^.ExecView(Dialog);
    IF Command<>cmCancel THEN
      Dialog^.GetData(ValueItem^.Value^.DataFormat);

    Dispose(Dialog, Done);
    ClearEvent(Event);
    EndModal(cmSetFormat);
  END;
END;

{===========================================================================}
{==                       TValuesWindow                                   ==}
{===========================================================================}

CONSTRUCTOR TValuesWindow.Init(VAR Bounds: TRect; ATitle:TTitleStr;
  UseNumber:Boolean; AButtons:Boolean; ValueItem:PValueItem);

VAR
  R: TRect;
  Button: PButton;
  Control: PValues;
  NewSize: TPoint;

BEGIN
  TNumberDialog.Init(Bounds, ATitle, UseNumber);
  Options := Options AND ($FFFF-ofBuffered);
  Flags := Flags OR wfGrow;
  Buttons := AButtons;
  SetState(sfCursorVis, True);

  IF Buttons THEN
  BEGIN
    GetExtent(R);
    R.A.X := R.B.X - 25;
    R.A.Y := R.B.Y - 3;
    R.B.X := R.A.X + 10;
    R.B.Y := R.A.Y + 2;
    Button := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
    Button^.GrowMode := gfGrowAll; {LoY + gfGrowLoX;}
    Insert(Button);

    GetExtent(R);
    R.A.X := R.B.X - 12;
    R.A.Y := R.B.Y - 3;
    R.B.X := R.A.X + 10;
    R.B.Y := R.A.Y + 2;
    Button := New(PButton, Init(R, 'O~K~', cmOk, bfDefault));
    Button^.GrowMode := gfGrowAll; {LoY + gfGrowLoX;}
    Insert(Button);
  END;

  GetExtent(R);
  IF Buttons THEN R.GRow(-1, -4) ELSE R.Grow(-1,-1);
  Control := New(PValues, Init(R,
    NIL, StandardScrollBar(sbVertical + sbHandleKeyboard),
    ValueItem));
  Control^.GrowMode := 0; {gfGrowHiX + gfGrowHiY;}
  Insert(Control);

  Control^.GetPrefBounds(R);
  IF Buttons THEN GrowTo(R.B.X+1, R.B.Y+4) ELSE GrowTo(R.B.X+1, R.B.Y+1);
  Control^.Locate(R);
END;

FUNCTION TValuesWindow.GetPalette: PPalette;

CONST
  NewPalette: TPalette = CDialog;

BEGIN
  GetPAlette := @NewPalette;
END;

PROCEDURE TValuesWindow.HandleEvent(VAR Event:TEvent);

VAR
  NewSize: TPoint;
  R: TRect;

BEGIN
  IF (Event.What=evCommand) THEN
  CASE Event.Command OF
    cmSetSize: BEGIN
      PValues(Event.InfoPtr)^.GetPrefBounds(R);
      IF Buttons THEN GrowTo(R.B.X+1, R.B.Y+4) ELSE GrowTo(R.B.X+1, R.B.Y+1);
      PValues(Event.InfoPtr)^.Locate(R);
      ClearEvent(Event);
    END;
    cmOk: EndModal(cmOk);
    ELSE
      TNumberDialog.HandleEvent(Event);
  END
  ELSE
    TNumberDialog.HandleEvent(Event);
END;

PROCEDURE TValuesWindow.SizeLimits(VAR Min, Max: TPoint);

BEGIN
  TNumberDialog.SizeLimits(Min, Max);
  Min.X := 5 + Length(Title^);
  IF Min.X < 28 THEN Min.X := 28;
  Min.Y := 3;
END;

{***************************************************************************}
{***                             TValueDescription                       ***}
{***************************************************************************}
(*
CONSTRUCTOR TValueDescription.Init(AValueItem:PValueItem; ANext: PDescription);

BEGIN
  TDescription.Init(ANext);
  NumLines := 1;
  ValueItem := AValueItem;
END;

DESTRUCTOR TValueDescription.Done;

BEGIN
  Dispose(ValueItem, Done);
  TDescription.Done;
END;

FUNCTION TValueDescription.GetText(Line: Integer; MaxLen: Integer): STRING;

BEGIN
  IF Line=0 THEN GetText := Copy(ValueItem^.Name^ + ' ' +
    ValueItem^.Value^.DataToString, 1, MaxLen)
  ELSE GetText := 'Line <> 0';
END;

PROCEDURE TValueDescription.SelectLine(Line: Integer; KeyCode: Word);

VAR
  Dialog:    PDialog;
  R:         TRect;
  S:         STRING;
{  ValueItem: PValueItem;}
  i:         Word;
  Ready:     Boolean;
  Control:   PView;
  Command:   Word;

BEGIN
  IF KeyCode=kbEnter THEN
  BEGIN
    R.Assign(0, 0, 55, 10);
    R.Move((Desktop^.Size.X - R.B.X) div 2, (Desktop^.Size.Y - R.B.Y) div 2);
    Dialog := New(PValueEditDialog, Init(R, '', ValueItem, True));

    Dialog^.SetData(ValueItem^.Value^.Data^);

    REPEAT
      Command := DeskTop^.ExecView(Dialog);
      IF Command=cmSetFormat THEN
      BEGIN
        Message(Owner, evCommand, cmSetSize, @Self);
      END;
    UNTIL Command<>cmSetFormat;

    IF Command<>cmCancel THEN
    BEGIN
      Dialog^.GetData(ValueItem^.Value^.Data^);
      Message(Owner, evCommand, cmSetSize, @Self);
    END;

    Dispose(Dialog, Done);

    DeskTop^.ReDraw;
  END;
END;

PROCEDURE TValueDescription.Update; VIRTUAL;
*)
{===========================================================================}
{==                          Misc.                                        ==}
{===========================================================================}


FUNCTION NewBooleanItem(Name:STRING; VAR Data:Boolean; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;

BEGIN
  NewBooleanItem := New(PValueItem, Init(Name,
    New(PBooleanValue, Init(@Data, DataFormat, ValueOwner)),
    Next));
END;

FUNCTION NewStringItem(Name:STRING; VAR Data:STRING; MaxLen:Word;
  ValueOwner:Boolean; Next:PValueItem): PValueItem;

BEGIN
  NewStringItem := New(PValueItem, Init(Name,
    New(PStringValue, Init(@Data, MaxLen, ValueOwner)),
    Next));
END;

FUNCTION NewByteItem(Name:STRING; VAR Data:Byte; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;

BEGIN
  NewByteItem := New(PValueItem, Init(Name,
    New(PByteValue, Init(@Data, DataFormat, ValueOwner)),
    Next));
END;

FUNCTION NewWordItem(Name:STRING; VAR Data:Word; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;

BEGIN
  NewWordItem := New(PValueItem, Init(Name,
    New(PWordValue, Init(@Data, DataFormat, ValueOwner)),
    Next));
END;

FUNCTION NewLongItem(Name:STRING; VAR Data:LongInt; DataFormat: Word;
  ValueOwner: Boolean; Next:PValueItem): PValueItem;

BEGIN
  NewLongItem := New(PValueItem, Init(Name,
    New(PLongValue, Init(@Data, DataFormat, ValueOwner)),
    Next));
END;

FUNCTION LongToString(Value:LongInt; NumBits:Byte; DataFormat:Byte): STRING;

VAR
  s: STRING;

CONST
  DecLength: ARRAY[0..32] OF Byte =
    (1,1,1,1, 2,2,2, 3,3,3, 4,4,4,4, 5,5,5, 6,6,6, 7,7,7,7, 8,8,8, 9,9,9, 10,10,10);

BEGIN
  CASE DataFormat OF
    dfDecimal: BEGIN
      Str(Value:12, s);
      Delete(s, 1, Length(s) - DecLength[NumBits] - 1);
    END;
    dfHexaDecimal: BEGIN
      s := HexL(Value);
      Delete(s, 1, Length(s) - (NumBits+3) DIV 4);
    END;
    dfBinary: BEGIN
      s := BinL(Value);
      Delete(s, 1, Length(s) - NumBits);
    END;
  END;
  LongToString := S;
END;

END.


