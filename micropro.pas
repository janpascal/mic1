{$A+,B-,D+,E+,F-,G-,I+,L+,N-,O-,R-,S+,V+,X+}
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

PROGRAM MicroProgramming;

USES
  Crt, Dos,
  Video,
  Objects, Drivers, Views, Menus, Dialogs, StdDlg, MsgBox, App, FvConsts,
  Hex, MicroCmd, Values, Dump2, Architecture;

CONST
  ApplicName = 'MicroProgramming';

  hiReadMicro           = 100;
  hiReadMacro           = 101;

TYPE
  PEmulator = ^TEmulator;
  TEmulator = OBJECT (TApplication)
    Arch:       PArchitecture;
    NextNumber: Byte;
    CONSTRUCTOR Init;
(*    FUNCTION    GetPalette: PPalette; VIRTUAL;*)
    PROCEDURE   HandleEvent( VAR Event : TEvent ); VIRTUAL;
    PROCEDURE   InitMenuBar; VIRTUAL;
    PROCEDURE   InitStatusLine; VIRTUAL;
    PROCEDURE   ReadMicroMem(FileName:PathStr);
    PROCEDURE   ReadMacroMem(FileName:PathStr);
    PROCEDURE	DoUpdate;
    PROCEDURE   StartUp;
  END;



CONSTRUCTOR TEmulator.Init;

BEGIN
  TApplication.Init;
  Arch := New(PArchitecture, Init);
  NextNumber := 1;
  ShowMouse;
  ShadowSize.X := 0;
END;

PROCEDURE TEmulator.ReadMicroMem(FileName:PathStr);

VAR
  f: TEXT;
  s: STRING;
  Value: LongInt;
  i:     Byte;
  Address: Word;


BEGIN
  Assign(f, FileName);
  {$I-}
  Reset(f);
  IF IOResult<>0 THEN
    MessageBox('Can''t access '+FileName+'.', NIL, mfError + mfCancelButton)
  ELSE
  BEGIN
    Readln(f);   { Skip first comment line }
    Address := 0;
    WHILE NOT Eof(f) DO
    BEGIN
      Readln(f, s);
      Value := 0;
      FOR i:=1 TO 32 DO
      BEGIN
        Value := Value SHL 1;
        IF s[i]='1' THEN Inc(Value);
      END;
      Arch^.Data^.MicroMem[Address] := Value;
      Inc(Address);
    END;
    IF IOResult<>0 THEN
      MessageBox('Error reading '+FileName+'.', NIL, mfError + mfCancelButton);
  END;
  {$I+}
END;

PROCEDURE TEmulator.ReadMacroMem(FileName:PathStr);

VAR
  f: TEXT;
  s: STRING;
  Value: Word;
  i:     Byte;
  Address: Word;

BEGIN
  Assign(f, FileName);
  {$I-}
  Reset(f);
  IF IOResult<>0 THEN
    MessageBox('Can''t access '+FileName+'.', NIL, mfError + mfCancelButton)
  ELSE
  BEGIN
    Readln(f);   { Skip first comment line }
    Address := 0;
    WHILE NOT Eof(f) DO
    BEGIN
      Readln(f, s);
      Value := 0;
      FOR i:=1 TO 16 DO
      BEGIN
        Value := Value SHL 1;
        IF s[i]='1' THEN Inc(Value);
      END;
      Arch^.Data^.MainMem[Address] := Value;
      Inc(Address);
    END;
    IF IOResult<>0 THEN
      MessageBox('Error reading '+FileName+'.', NIL, mfError + mfCancelButton);
  END;
  {$I+}
END;


(*
FUNCTION TEmulator.GetPalette: PPalette;
CONST
  CNewColor = CColor + CHelpColor;
  CNewBlackWhite = CBlackWhite + CHelpBlackWhite;
  CNewMonochrome = CMonochrome + CHelpMonochrome;
  P: array[apColor..apMonochrome] of string[Length(CNewColor)] =
    (CNewColor, CNewBlackWhite, CNewMonochrome);
BEGIN
  GetPalette := @P[AppPalette];
END;
  *)
PROCEDURE TEmulator.DoUpdate;

BEGIN
  Message(@Self, evBroadcast, cmUpDate, NIL);
  Message(@Self, evBroadcast, cmNewAddress, Arch);
  DeskTop^.Redraw;
END;

PROCEDURE TEmulator.HandleEvent( VAR Event : TEvent );

PROCEDURE About;

VAR
  Dialog: PDialog;
  Control: PView;
  R: TRect;
BEGIN
  R.Assign(0, 0, 90, 24);
  R.Move((Desktop^.Size.X - R.B.X) DIV 2, (Desktop^.Size.Y - R.B.Y) DIV 2);
  {R.Assign(5, 5, Desktop^.Size.X-5, Desktop^.Size.Y-5);}
  Dialog := New(PDialog, Init(R, ApplicName));
  WITH Dialog^ DO
  BEGIN
    R.Assign(3, 2, Size.X - 3, 5);
    Control := New(PStaticText, Init(R,
      ' MicroProgramming is a program that simulates the microprogramming level ' +
      'of the MIC-1 example in Tanenbaum''s book ''Structured computer ' +
      'architecture''.'));
    Insert(Control);

    R.Assign(3, 5, Size.X - 3, 6);
    Control := New(PStaticText, Init(R,
      ' Copyright (C) 1991,2011 Jan-Pascal van Best <janpascal@vanbest.org>' ));
    Insert(Control);

    R.Assign(3, 7, Size.X - 3, 10);
    Control := New(PStaticText, Init(R,
    ' This program is free software: you can redistribute it and/or modify ' +
    'it under the terms of the GNU General Public License as published by ' +
    'the Free Software Foundation, either version 3 of the License, or ' +
    '(at your option) any later version.'));
    Insert(Control);

    R.Assign(3, 11, Size.X - 3, 14);
    Control := New(PStaticText, Init(R,
    ' This program is distributed in the hope that it will be useful, ' +
    'but WITHOUT ANY WARRANTY; without even the implied warranty of ' +
    'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the ' +
    'GNU General Public License for more details.'));
    Insert(Control);

    R.Assign(3, 15, Size.X - 3, 17);
    Control := New(PStaticText, Init(R,
    ' You should have received a copy of the GNU General Public License ' +
    'along with this program.  If not, see <http://www.gnu.org/licenses/>. ' ));
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
{
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
}
PROCEDURE ReadMacro;

VAR
  D: PFileDialog;
  FileName: PathStr;

BEGIN
  D := New(PFileDialog, Init('*.MAC', 'Read Macromemory',
    '~F~iles', fdOkButton + fdHelpButton, hiReadMacro));
  D^.HelpCtx := hcNoContext;
  IF ValidView(D) <> NIL THEN
  BEGIN
    IF Desktop^.ExecView(D) <> cmCancel THEN
    BEGIN
      D^.GetFileName(FileName);
      REadMacroMem(FileName);
      DoUpdate;
    END;
    Dispose(D, Done);
  END;
END;

PROCEDURE ReadMicro;

VAR
  D: PFileDialog;
  FileName: PathStr;

BEGIN
  D := New(PFileDialog, Init('*.MIC', 'Read Micromemory',
    '~F~iles', fdOkButton + fdHelpButton, hiReadMicro));
  D^.HelpCtx := hcNoContext;
  IF ValidView(D) <> NIL THEN
  BEGIN
    IF Desktop^.ExecView(D) <> cmCancel THEN
    BEGIN
      D^.GetFileName(FileName);
      REadMicroMem(FileName);
      DoUpdate;
    END;
    Dispose(D, Done);
  END;
END;


PROCEDURE ViewRegisters;

VAR
  W: PRegisterWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,17,18);
  R.Move(0,17);
  W := New(PRegisterWindow, Init(R, 'Regs', NextNumber,
    Addr(Arch^.Data^.Regs)));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
END;

PROCEDURE ViewInternals;

VAR
  W: PInternalWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,49,21);
  R.Move(30,17);
  W := New(PInternalWindow, Init(R, 'Internals', NextNumber, Addr(Arch^.Data^.Internals)));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
END;

PROCEDURE ClockPhase;

VAR
  W: PClockPhaseWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,49,21);
  R.Move(50,19);
  W := New(PClockPhaseWindow, Init(R, 'Timing', NextNumber, Arch));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
END;

PROCEDURE ClockCycle;

BEGIN
  Arch^.DoOneCLockCycle;
  DoUpdate;
END;

PROCEDURE MicroInstruction;

BEGIN
  Arch^.DoOneMicroInstruction;
  DoUpdate;
END;

PROCEDURE MacroInstruction;

BEGIN
  Arch^.DoOneMacroInstruction;
  DoUpdate;
END;

PROCEDURE ProgramReset;

BEGIN
  Arch^.ProgramReset;
  DoUpdate;
END;

PROCEDURE Runto90;

BEGIN
  Arch^.RunToMacro(90);
  DoUpdate;
  Sound(440);
  Delay(500);
  NoSound;
END;

PROCEDURE DumpMicro;

VAR
  W: PWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,43,10);
  R.Move(15,32);
  W := New(PMicroDumpWindow, Init(R, NextNumber, Arch));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
END;

PROCEDURE DumpMacro;

VAR
  W: PWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,17,18);
  W := New(PMacroDumpWindow, Init(R, NextNumber, Arch));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    W^.GetExtent(R);
    W^.MoveTo(DeskTop^.Size.X-R.B.X, DeskTop^.Size.Y-R.B.Y-5);
    Inc(NextNumber);
  END;
END;

PROCEDURE DisasmMicro;

VAR
  W: PWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,57,18);
  W := New(PMicroDisasmWindow, Init(R, NextNumber, Arch));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
  DoUpdate;
END;

PROCEDURE DisasmMacro;

VAR
  W: PWindow;
  R: TRect;

BEGIN
  R.Assign(0,0,23,18);
  R.Move(DeskTop^.Size.X-24,0);
  W := New(PMacroDisasmWindow, Init(R, NextNumber, Arch));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
  DoUpdate;
END;

PROCEDURE ViewOutput;

VAR
  W: PWindow;
  R: TRect;

BEGIN
  DeskTop^.GetExtent(R);
  R.A.Y := R.B.Y - 3;
  W := New(PScreenWindow, Init(R, 'Output', NextNumber));
  IF ValidView(W) <> NIL THEN
  BEGIN
    DeskTop^.Insert(W);
    Inc(NextNumber);
  END;
END;

PROCEDURE ScreenSize;

VAR
  NewMode: Word;

BEGIN
  IF HiResScreen THEN
  BEGIN
    (*
    NewMode := Screenmode XOR smFont8x8;
    IF NewMode AND Font8x8 <> 0 THEN
      ShadowSize.X := 0{1}
    ELSE
      ShadowSize.X := 0{2};
    SetScreenMode(NewMode);
    *)
  END;
END;

PROCEDURE RunProgram;

BEGIN
  Arch^.Run;
  DoUpdate;
END;

BEGIN
  TApplication.HandleEvent( Event );
  CASE Event.What OF
    evCommand:
      CASE Event.Command OF
        cmSystem:           About;
        cmReadMicro:        ReadMicro;
        cmReadMacro:        ReadMacro;
        cmViewRegisters:    ViewRegisters;
        cmViewInternals:    ViewInternals;
        cmChDir:            ChangeDir;
        cmDosShell:         {DosShell};
        cmCLockCycle:       ClockCycle;
        cmMicroInstruction: MicroInstruction;
        cmMacroInstruction: MacroInstruction;
        cmProgramReset:     ProgramReset;
        cmRunTo90:          RunTo90;
        cmViewOutput:       ViewOutput;
        cmDumpMicro:        DumpMicro;
        cmDumpMacro:        DumpMacro;
        cmScreenSize:	    ScreenSize;
        cmCLockPhase:       ClockPhase;
        cmCodeMacro:	    DisasmMacro;
        cmCodeMicro:        DisasmMicro;
        cmRun:              RunProgram;
      ELSE
        Exit;
      END;
  ELSE
    Exit;
  END;
  ClearEvent( Event );
END;

PROCEDURE TEmulator.InitMenuBar;

VAR
  R: TRect;

BEGIN
  GetExtent( R );
  R.B.Y := R.A.Y + 1;
  MenuBar := New( PMenuBar, Init( R, NewMenu(
    NewItem('~'#240'~', '', kbAltSpace, cmSystem, hcNoContext,
    NewSubMenu('~F~ile', hcNoContext, NewMenu(
      NewItem('Read m~i~cromemory...', '', kbNoKey, cmReadMicro, hcNoContext,
      NewItem('Read m~a~cromemory...', '', kbNoKey, cmReadMacro, hcNoContext,
      NewLine(
      NewItem('~C~hange dir...', '', kbNoKey, cmChDir, hcNoContext,
      NewItem('~D~OS Shell', '', kbNoKey, cmDosShell, hcNoContext,
      NewItem('E~x~it', 'Alt-X', kbAltX, cmQuit, hcNoContext,
      NIL))))))),
    NewSubMenu('~V~iew', hcNoContext, NewMenu(
      NewItem('Disasm M~i~cromemory', '', kbNoKey, cmCodeMicro, hcNoContext,
      NewItem('Disasm M~a~cromemory', '', kbNoKey, cmCodeMacro, hcNoContext,
      NewItem('Dump Micromemory', '', kbNoKey, cmDumpMicro, hcNoContext,
      NewItem('Dump Macromemory', '', kbNoKey, cmDumpMacro, hcNoContext,
      NewItem('~T~iming', '', kbNoKey, cmClockPhase, hcNoContext,
      NewItem('~R~egisters', '', kbNoKey, cmViewRegisters, hcNoContext,
      NewItem('~I~nternals', '', kbNoKey, cmViewInternals, hcNoContext,
      NewItem('~O~utput', '', kbNoKey, cmViewOutput, hcNoContext,
      NIL))))))))),
    NewSubMenu('~R~un', hcNoContext, NewMenu(
      NewItem('~C~lockcycle', 'Alt-F7', kbAltF7, cmClockCycle, hcNoContext,
      NewItem('M~i~cro-instruction', 'F7', kbF7, cmMicroInstruction, hcNoContext,
      NewItem('M~a~cro-instruction', 'F8', kbF8, cmMacroInstruction, hcNoContext,
      NewItem('~R~un', 'Ctrl-F9', kbCtrlF9, cmRun, hcNoContext,
      NewItem('~G~o to cursor', 'F4', kbF4, cmGotoCursor, hcNoContext,
      NewItem('Program reset', 'Ctrl-F2', kbCtrlF2, cmProgramReset, hcNoContext,
      NIL))))))),
    NewSubMenu('~W~indows', hcNoContext, NewMenu(
      NewItem('~R~esize/move','Ctrl-F5', kbCtrlF5, cmResize, hcNoContext,
      NewItem('~Z~oom', 'F5', kbF5, cmZoom, hcNoContext,
      NewItem('~N~ext', 'F6', kbF6, cmNext, hcNoContext,
      NewItem('~P~revious', 'Shift-F6', kbShiftF6, cmPrev, hcNoContext,
      NewItem('~C~lose', 'Alt-F3', kbAltF3, cmClose, hcNoContext, nil)))))),
    NewSubMenu('~O~ptions', hcNoContext, NewMenu(
      NewItem('~S~creen size', '', kbNoKey, cmScreenSize, hcNoContext,
      NIL)),
    NIL)))))))));
END;

PROCEDURE TEmulator.InitStatusLine;

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
      NewStatusKey('~Alt-F10~ Local', kbAltF10, cmLocal,
      NewStatusKey('~Alt-G~ Goto', kbAltG, cmGoto,
      NewStatusKey('', kbF10,  cmMenu,
      NewStatusKey('', kbCtrlF5, cmResize, NIL))))))),
    NIL)));
END;

PROCEDURE TEmulator.StartUp;

VAR
  Event: TEvent;

BEGIN
  ReadMicroMem('MICRO.MIC');
  ReadMacroMem('MACRO.MAC');

  Event.What := evCommand; Event.Command := cmScreenSize;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmCodeMicro;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmCodeMacro;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmViewRegisters;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmViewInternals;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmViewOutput;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmClockPhase;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmDumpMicro;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmDumpMacro;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmSystem;
  HandleEvent(Event);

  Event.What := evCommand; Event.Command := cmNext;
  HandleEvent(Event);
END;

VAR
  Emulator : TEmulator;

BEGIN
  WriteLN('Hello, World!');
  Emulator.Init;
  Emulator.StartUp;
  Emulator.Run;
  Emulator.Done;
END.

