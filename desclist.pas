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

UNIT DescList;

INTERFACE

USES
  Objects, Drivers, Views, Dialogs;

TYPE
  PDescription = ^TDescription;
  TDescription = OBJECT(TObject)
    NumLines: Word;
    Next: PDescription;
    CONSTRUCTOR Init(ANext: PDescription);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION GetText(Line: Integer; MaxLen: Integer): STRING; VIRTUAL;
    FUNCTION GetNumLines: Word;
    PROCEDURE SelectLine(Line: Integer; KeyCode: Word); VIRTUAL;
    PROCEDURE Update; VIRTUAL;
  END;

  PDescriptionViewer = ^TDescriptionViewer;
  TDescriptionViewer = OBJECT(TListViewer)
    Description: PDescription;
    CONSTRUCTOR Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      ADescription: PDescription);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING; VIRTUAL;
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    PROCEDURE SelectItem(Item: sw_Integer); VIRTUAL;
  END;

  PChildItem = ^TChildItem;
  TChildItem = RECORD
    Child: PDescription;
    StartLine: Word;
    EndLine: Word;
  END;

  PChildItemArray = ^TChildItemArray;
  TChildItemArray = ARRAY[0..8190] OF TChildItem;

  PParentDescription = ^TParentDescription;
  TParentDescription = OBJECT(TDescription)
    Children: PDescription;
    NumChildren: Word;
    ChildItems: PChildItemArray;
    CONSTRUCTOR Init(AChildren:PDescription; ANext:PDescription);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION GetText(Line: Integer; MaxLen: Integer): STRING; VIRTUAL;
    PROCEDURE SelectLine(Line: Integer; KeyCode: Word); VIRTUAL;
    PROCEDURE Update; VIRTUAL;
  END;

IMPLEMENTATION

{****************************************************************************}
{***                          TDescriptionViewer                          ***}
{****************************************************************************}

CONSTRUCTOR TDescriptionViewer.Init(VAR Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
  ADescription: PDescription);

BEGIN
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  Description := ADescription;
END;

DESTRUCTOR TDescriptionViewer.Done;

BEGIN
  Dispose(Description, Done);
  TListViewer.Done;
END;

FUNCTION TDescriptionViewer.GetText(Item: sw_Integer; MaxLen: sw_Integer): STRING;

BEGIN
  GetText := Description^.GetText(Item, MaxLen);
END;

PROCEDURE TDescriptionViewer.HandleEvent(VAR Event:TEvent);

VAR
  KeyCode: Word;

BEGIN
  IF (Event.What=evKeyDown) THEN
  BEGIN
    KeyCode := Event.KeyCode;
    IF (KeyCode=kbEnter) OR (KeyCode=kbIns) OR
       (KeyCode=kbDel) OR (KeyCode=kbGrayMinus) OR (KeyCode=kbGrayPlus) THEN
    BEGIN
      Description^.SelectLine(Focused, KeyCode);
      ClearEvent(Event);
    END;
  END;

  TListViewer.HandleEvent(Event);
END;

PROCEDURE TDescriptionViewer.SelectItem(Item: sw_Integer);

BEGIN
  Description^.SelectLine(Item, kbEnter);
END;

{***************************************************************************}
{***                            TDescription                             ***}
{***************************************************************************}


CONSTRUCTOR TDescription.Init(ANext:PDescription);

BEGIN
  TObject.Init;
  Next := ANext;
  NumLines := 0;
END;

DESTRUCTOR TDescription.Done;

BEGIN
  IF Next<>NIL THEN Dispose(Next, Done);
  TObject.Done;
END;

FUNCTION TDescription.GetText(Line: Integer; MaxLen: Integer): STRING;

BEGIN
  GetText := '.';
END;

FUNCTION TDescription.GetNumLines: Word;

BEGIN
  GetNumLines := NumLines;
END;

PROCEDURE TDescription.SelectLine(Line: Integer; KeyCode: Word);

BEGIN
END;

PROCEDURE TDescription.Update;

BEGIN
END;

{***************************************************************************}
{***                         TParentDescription                          ***}
{***************************************************************************}

CONSTRUCTOR TParentDescription.Init(AChildren:PDescription; ANext:PDescription);

BEGIN
  TDescription.Init(ANext);
  Children := AChildren;
  UpDate;
END;

DESTRUCTOR TParentDescription.Done;

BEGIN
  IF Children<>NIL THEN Dispose(Children, Done);
  FreeMem(ChildItems, SizeOf(TChildItem) * NumChildren);
  TDescription.Done;
END;

FUNCTION TParentDescription.GetText(Line: Integer; MaxLen: Integer): STRING;

VAR
  i: Word;

BEGIN
  i := 0;
  WHILE (i<NumChildren) AND (Line>ChildItems^[i].EndLine) DO Inc(i);
  IF i<NumChildren THEN
    GetText := ChildItems^[i].Child^.GetText(Line-ChildItems^[i].StartLine, MaxLen)
  ELSE
    GetText := '...';
END;

PROCEDURE TParentDescription.SelectLine(Line: Integer; KeyCode: Word);

VAR
  i: Word;

BEGIN
  i := 0;
  WHILE (i<NumChildren) AND (Line>ChildItems^[i].EndLine) DO Inc(i);
  IF i<NumChildren THEN ChildItems^[i].Child^.SelectLine(Line-ChildItems^[i].StartLine, KeyCode);
END;

PROCEDURE TParentDescription.Update;

VAR
  Child: PDescription;
  i, StartLine: Word;

BEGIN
  NumLines := 0;
  NumChildren := 0;
  Child := Children;
  WHILE Child<>NIL DO
  BEGIN
    Inc(NumLines, Child^.GetNumLines);
    Inc(NumChildren);
    Child := Child^.Next;
  END;

  GetMem(ChildItems, SizeOf(TChildItem) * NumChildren);

  StartLine := 0;
  i := 0;
  Child := Children;
  WHILE Child<>NIL DO
  BEGIN
    ChildItems^[i].Child := Child;
    ChildItems^[i].StartLine := StartLine;
    ChildItems^[i].EndLine := StartLine + Child^.GetNumLines - 1;
    Inc(StartLine, Child^.GetNumLines);
    Inc(i);
    Child := Child^.Next;
  END;
END;

END.
