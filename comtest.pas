{$A+,B-,D+,E-,F-,G-,I+,L+,N-,O-,R-,S+,V-,X+}
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

PROGRAM ComTest;

USES
  Dos,
  Objects, Drivers, Memory, Views, Menus, Dialogs, StdDlg, MsgBox, App, HelpFile,
  Hex, Values, SimMemory, Components;

CONST
  ApplicName = 'Component Test';

TYPE
  PChooseCollectionDialog = ^TChooseCollectionDialog;
  TChooseCollectionDialog = OBJECT(TDialog)
    MyNumber: Word;
    CONSTRUCTOR Init(AName:STRING; Collection:PStringCollection);
    PROCEDURE HandleEvent(VAR Event:TEvent); VIRTUAL;
    FUNCTION GetNumber: Word;
  END;

  PComponentTest = ^TComponentTest;
  TComponentTest = OBJECT(TApplication)
    ComponentList: PComponent;
    CONSTRUCTOR Init;
    DESTRUCTOR  Done; VIRTUAL;
    FUNCTION    GetPalette: PPalette; VIRTUAL;
    PROCEDURE   HandleEvent( VAR Event : TEvent ); VIRTUAL;
    PROCEDURE   InitMenuBar; VIRTUAL;
    PROCEDURE   InitStatusLine; VIRTUAL;
    PROCEDURE   AddComponentToList(Component:PComponent);
    FUNCTION    GetComponentNames: PStringCollection;
    FUNCTION    GetComponent(Name:STRING):PComponent;
  END;

CONST
  cmSystem     = 1001;
  cmChDir      = 1002;
  cmDosShell   = 1003;
  cmScreenSize = 1004;
  cmNewComponent = 1005;
  cmViewComponents = 1006;


CONSTRUCTOR TChooseCollectionDialog.Init(AName:STRING; Collection:PStringCollection);

CONST
  MaxLen: Byte = 0;

{$F+}
PROCEDURE Max(S:PString);

BEGIN
  IF Length(S^) + 5 > MaxLen THEN MaxLen := 5 + Length(S^);
END;
{$F-}

VAR
  R: TRect;
  Control: PListBox;

BEGIN
  MaxLen := 5 + Length(ANAme);
  Collection^.ForEach(@Max);
  R.Assign(0,0,MaxLen,Collection^.Count+2);
  R.Move((Desktop^.Size.X - R.B.X) DIV 2, (Desktop^.Size.Y - R.B.Y) DIV 2);
  TDialog.Init(R, AName);
  GetExtent(R);
  R.Grow(-1,-1);
  Control := New(PListBox, Init(R, 1,
    StandardScrollBar(sbVertical + sbHandleKeyboard)));
  Control^.NewList(Collection);
  Insert(Control);
END;

PROCEDURE TChooseCollectionDialog.HandleEvent(VAR Event:TEvent);

BEGIN
  TDialog.HandleEvent(Event);
  IF (Event.What=evBroadCast) AND (Event.Command=cmListItemSelected) THEN
  BEGIN
    MyNumber := PListBox(Event.InfoPtr)^.Focused;
    EndModal(cmOk);
    ClearEvent(Event);
  END;
END;

FUNCTION TChooseCollectionDialog.GetNumber: Word;

BEGIN
  GetNumber := MyNumber;
END;
{
FUNCTION SelectFromCollection(ANAme:STRING; Collection:PStringCollection): Integer;

VAR
  Dialog: PDialog;

BEGIN
  Dialog := New(PChooseCollectionDialog, Init(AName, Collection));
  CommandSelectFromCollection := DeskTop^.ExecView(Dialog);
  Dispose(Dialog, Done);
END;
}
CONSTRUCTOR TComponentTest.Init;

BEGIN
  TApplication.Init;
  ComponentList := NIL;
  ShowMouse;
END;

DESTRUCTOR TComponentTest.Done;

BEGIN
  IF ComponentList<>NIL THEN Dispose(ComponentList, Done);
  TApplication.Done;
END;

FUNCTION TComponentTest.GetPalette: PPalette;
CONST
  CNewColor = CColor + CHelpColor;
  CNewBlackWhite = CBlackWhite + CHelpBlackWhite;
  CNewMonochrome = CMonochrome + CHelpMonochrome;
  P: array[apColor..apMonochrome] of string[Length(CNewColor)] =
    (CNewColor, CNewBlackWhite, CNewMonochrome);
BEGIN
  GetPalette := @P[AppPalette];
END;

PROCEDURE TComponentTest.HandleEvent(VAR Event: TEvent);

PROCEDURE About;

VAR
  Dialog: PDialog;
  Control: PView;
  R: TRect;

BEGIN
  R.Assign(0, 0, 65, 20);
  R.Move((Desktop^.Size.X - R.B.X) DIV 2, (Desktop^.Size.Y - R.B.Y) DIV 2);
  Dialog := New(PDialog, Init(R, ApplicName));
  WITH Dialog^ DO
  BEGIN
    R.Assign(3, 3, Size.X - 3, 6);
    Control := New(PStaticText, Init(R,
      ^C'Component Test tests my Components unit.'));
    Insert(Control);

    R.Assign(3, 10, Size.X - 3, 13);
    Control := New(PStaticText, Init(R,
      ^C'Jan-Pascal van Best' +
      ^M^C'Klikspaanweg 58-5' +
      ^M^C'2324 LZ  Leiden'));
    Insert(Control);

    R.Assign(3, 13, Size.X-3, 17);
    Control := New(PStaticText, Init(R,
      ^C'The Netherlands' +
      ^M^C'Tel. (0)71-177669'));
    Insert(Control);

    R.Assign(0, 0, 10, 2);
    R.Move(Size.X DIV 2-5, Size.Y-3);
    Control := New(PButton, Init(R, 'O~K~', cmOk, bfNormal));
    Insert(Control);
    SelectNext(False);
  END;
  DeskTop^.ExecView(Dialog);
  Dispose(Dialog, Done);
END;

procedure ChangeDir;
var
  D: PChDirDialog;
begin
  D := New(PChDirDialog, Init(cdNormal + cdHelpButton, 101));
  D^.HelpCtx := hcNoContext;
  if ValidView(D) <> nil then
  begin
    DeskTop^.ExecView(D);
    Dispose(D, Done);
  end;
end;

procedure DosShell;
begin
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneMemory;
  SetMemTop(HeapPtr);
  PrintStr('Type EXIT to return...');
  SwapVectors;
  Exec(GetEnv('COMSPEC'), '');
  SwapVectors;
  SetMemTop(HeapEnd);
  InitMemory;
  InitVideo;
  InitEvents;
  InitSysError;
  Redraw;
end;

PROCEDURE ScreenSize;

VAR
  NewMode: Word;

BEGIN
  IF HiResScreen THEN
  BEGIN
    NewMode := Screenmode XOR smFont8x8;
    IF NewMode AND smFont8x8 <> 0 THEN
      ShadowSize.X := 0{1}
    ELSE
      ShadowSize.X := 0{2};
    SetScreenMode(NewMode);
  END;
END;

PROCEDURE MakeComponent;

VAR
  NAmes: PStringCollection;
  Number: Integer;
  ComponentName: STRING;
  ComponentType: PComponentType;
  Parameters: PValueItem;
  ChooseComponentDialog: PChooseCollectionDialog;
  ParametersDialog: PValuesWindow;
  R: TRect;
  Command: Word;
  Component: PComponent;

BEGIN
  Names := GetComponentTypeNAmes;
  ChooseComponentDialog := New(PChooseCollectionDialog, Init('Components', Names));
  Command := DeskTop^.ExecView(ChooseComponentDialog);
  IF Command<>cmCancel THEN
  BEGIN
    ComponentName := PString(Names^.At(ChooseComponentDialog^.GetNumber))^;
    ComponentType := GetComponentType(ComponentName);
    Parameters := GetParameters(ComponentType);
    R.Assign(15,8,50,20);
    ParametersDialog := New(PValuesWindow, Init(R, 'Parameters', False, True, Parameters));
    Command := DeskTop^.ExecView(ParametersDialog);
    IF Command<>cmCancel THEN
    BEGIN
      Component := NewComponent(ComponentType, Parameters);
      AddComponentToList(Component);
    END;
    Dispose(ParametersDialog, Done); { Also disposes of parameters }
  END;
  Dispose(ChooseComponentDialog, Done);
  Dispose(Names, Done);
END;

PROCEDURE ViewComponents;

VAR
  Names: PStringCollection;
  ChooseComponentDialog: PChooseCollectionDialog;
  Command: Word;
  ComponentName: STRING;
  Component: PComponent;
  ComponentWindow: PWindow;

BEGIN
  Names := GetComponentNames;
  ChooseComponentDialog := New(PChooseCollectionDialog, Init('Components', Names));
  Command := DeskTop^.ExecView(ChooseComponentDialog);
  IF Command<>cmCancel THEN
  BEGIN
    ComponentName := PString(Names^.At(ChooseComponentDialog^.GetNumber))^;
    Component := GetComponent(ComponentName);
    ComponentWindow := Component^.GetComponentWindow(True);
    IF ValidView(ComponentWindow)<>NIL THEN
      DeskTop^.Insert(ComponentWindow);
  END;
  Dispose(ChooseComponentDialog, Done);
  Dispose(Names, Done);
END;

BEGIN
  TApplication.HandleEvent( Event );
  CASE Event.What OF
    evCommand:
      CASE Event.Command OF
        cmSystem:           About;
        cmChDir:            ChangeDir;
        cmDosShell:         DosShell;
        cmScreenSize:	    ScreenSize;
        cmNewComponent:     MakeComponent;
        cmViewComponents:   ViewComponents;
      ELSE
        Exit;
      END;
  ELSE
    Exit;
  END;
  ClearEvent( Event );
END;

PROCEDURE TComponentTest.InitMenuBar;

VAR
  R: TRect;

BEGIN
  GetExtent( R );
  R.B.Y := R.A.Y + 1;
  MenuBar := New( PMenuBar, Init( R, NewMenu(
    NewItem('~'#240'~', '', kbAltSpace, cmSystem, hcNoContext,
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('~C~hange dir...', '', kbNoKey, cmChDir, hcNoContext,
      NewItem('~D~OS Shell', '', kbNoKey, cmDosShell, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      NIL)))),
    NewSubmenu('~C~omponents', hcNoContext, NewMenu(
      NewItem('~N~ew', '', kbNoKey, cmNewComponent, hcNoContext,
      NewItem('~V~iew', '', kbNoKey, cmViewComponents, hcNoContext,
      NIL))),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      NewItem('~R~esize/move','Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
      NewItem('~N~ext', 'F6', kbF6, cmNext, hcNoContext,
      NewItem('~P~revious', 'Shift-F6', kbShiftF6, cmPrev, hcNoContext,
      NewItem('~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext, nil)))))),
    NewSubMenu('~O~ptions', hcNoContext, NewMenu(
      NewItem('~S~creen size', '', kbNoKey, cmScreenSize, hcNoContext,
      NIL)),
    NIL))))))));
END;

PROCEDURE TComponentTest.InitStatusLine;

VAR
  R : TRect;

BEGIN
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New( StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      NewStatusKey('~Alt-F3~ Close', kbAltF3, cmClose,
      NewStatusKey('~F5~ Zoom', kbF5, cmZoom,
      NewStatusKey('', kbF10,  cmMenu,
      NewStatusKey('', kbCtrlF5, cmResize, NIL))))),
    NIL)));
END;

PROCEDURE TComponentTest.AddComponentToList(Component:PComponent);

BEGIN
  PComponent(Component^.Next) := ComponentList;
  ComponentList := Component;
END;

FUNCTION TComponentTest.GetComponentNames: PStringCollection;

VAR
  Component: PComponent;
  Names: PStringCollection;

BEGIN
  Names := New(PStringCollection, Init(5,5));

  Component := ComponentList;
  WHILE Component<>NIL DO
  BEGIN
    Names^.Insert(NewStr(Component^.Name^));
    Component := PComponent(Component^.Next);
  END;
  GetComponentNames := Names;
END;

FUNCTION TComponentTest.GetComponent(Name:STRING):PComponent;

VAR
  Component: PComponent;

BEGIN
  Component := ComponentList;
  WHILE (Component<>NIL) AND (Component^.Name^<>Name) DO
    Component := PComponent(Component^.Next);
  GetComponent := Component;
END;


VAR
  ComponentTest : TComponentTest;

BEGIN
  ComponentTest.Init;
  ComponentTest.Run;
  ComponentTest.Done;
END.


