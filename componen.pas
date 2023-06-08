{$A+,B-,D+,E-,F-,I+,L+,N-,O-,R+,S+,V-}
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

UNIT Components;

INTERFACE

USES
  Objects, Drivers, Views, Menus, Dialogs, App,
  Hex, WinNumbers, Values, SimMemory;

CONST
  cpOk                = 0;
  cpError             = 1;
  cpIllegalConnection = 2;

  csStandard = $0000;
  csStable   = $0001;
  csTriState = $0002;

  cfStandard          = $0000;
  cfInput             = $0001;
  cfOutput            = $0002;
  cfStandardConnector = $0004;
  cfHasStandardInput  = $0001;
  cfHasStandardOutput = $0002;

  shNoShift    = 0;
  shShiftRight = 1;
  shShiftLeft  = 2;

  aluAdd  = 0;
  aluAnd  = 1;
  aluCopy = 2;
  aluNot  = 3;

  mslNoJump     = 0;
  mslJumpN      = 1;
  mslJumpZ      = 2;
  mslJumpAlways = 3;

  MaxNumSplitterOutputs = 32;

TYPE
  TComponentName = STRING[20];

  PComponent = ^TComponent;

  PBaseComponent = ^TBaseComponent;
  TBaseComponent = OBJECT(TObject)
    Name: PString;
    Flags: Word;
    State: Word;
    Next: PBaseComponent;
    CONSTRUCTOR Init(AName:STRING; ANext:PBaseComponent);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION WellConnected: Boolean; VIRTUAL;
    FUNCTION Recalculate: Boolean; VIRTUAL;
    FUNCTION RecalculateAll: Boolean;
    PROCEDURE SetState(AState:Word; Enable:Boolean); VIRTUAL;
    FUNCTION GetState(AState:Word): Boolean;
    PROCEDURE SetFlags(AFlags:Word; Enable:Boolean);
    FUNCTION GetFlags(AFlags:Word): Boolean;
    PROCEDURE Reset; VIRTUAL;
    PROCEDURE Show; VIRTUAL;
{    PROCEDURE Store(VAR S:TStream); VIRTUAL;}
  END;

  PConnector = ^TConnector;
  TConnector = OBJECT(TBaseComponent)
    Value: LongInt;
    NumBits: Word;
    Owner: PComponent;
    ConnectedTo: PConnector;
    CONSTRUCTOR Init(AName: STRING; ANumBits:Word; AOwner: PComponent;
      AFlags:Word; ANext:PConnector);
    FUNCTION Connect(AConnector: PConnector): Word;
    FUNCTION DisConnect: Word;
    FUNCTION WellConnected: Boolean; VIRTUAL;
    FUNCTION Recalculate: Boolean; VIRTUAL;
    PROCEDURE SetState(AState:Word; Enable:Boolean); VIRTUAL;
    PROCEDURE Show; VIRTUAL;
    FUNCTION GetDescription:STRING;
  END;

  TComponent = OBJECT(TBaseComponent)
    TypeName: PString;
    Connectors: PConnector;
    CONSTRUCTOR Init(AName:STRING; ATypeName:STRING; ANext:PComponent);
    DESTRUCTOR Done; VIRTUAL;
    FUNCTION AddConnector(AConnector:PConnector): Word; VIRTUAL;
    FUNCTION RemoveConnector(AConnector:PConnector): Word;
    FUNCTION ConnectConnector(MyConnector:PConnector; OtherConnector:PConnector): Word;
    FUNCTION GetConnector(AName:STRING):PConnector;
    FUNCTION StandardConnector(AName:STRING; AFlags:Word):PConnector; VIRTUAL;
    FUNCTION WellConnected: Boolean; VIRTUAL;
    FUNCTION Recalculate: Boolean; VIRTUAL;
    PROCEDURE CalculateOutputs; VIRTUAL;
    PROCEDURE Reset; VIRTUAL;
    PROCEDURE Show; VIRTUAL;
    FUNCTION GetComponentWindow(UseNumber:Boolean): PWindow;
  END;

  PValueComponent = ^TValueComponent;
  TValueComponent = OBJECT(TComponent)
    NumBits: Word;
    Value: LongInt;
    CONSTRUCTOR Init(AName:STRING; ATypeName:STRING; ANumBits:Word; ANext:PComponent);
    FUNCTION AddConnector(AConnector:PConnector): Word; VIRTUAL;
    FUNCTION StandardConnector(AName:STRING; AFlags:Word):PConnector; VIRTUAL;
    PROCEDURE CalculateOutputs; VIRTUAL;
    PROCEDURE Show; VIRTUAL;
  END;

  PTriStateBuffer = ^TTriStateBuffer;
  TTriStateBuffer = OBJECT(TValueComponent)
    Enable: PConnector;
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANExt:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PConstant = ^TConstant;
  TConstant = OBJECT(TValueComponent)
    CONSTRUCTOR Init(AName:STRING; ANumBits:Word; AValue: LongInt;
      ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE Reset; VIRTUAL;
  END;

  PRegister = ^TRegister;
  TRegister = OBJECT(TValueComponent)
    InputEnable: PConnector;
    CONSTRUCTOR Init(AName:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    FUNCTION Recalculate: Boolean; VIRTUAL;
  END;

  PBus = ^TBus;
  TBus = OBJECT(TValueComponent)
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PAndPort = ^TAndPort;
  TAndPort = OBJECT(TValueComponent)
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANExt:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  POrPort = ^TOrPort;
  TOrPort = OBJECT(TValueComponent)
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANExt:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PXORPort = ^TXORPort;
  TXORPort = OBJECT(TValueComponent)
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANExt:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PMultiplexer = ^TMultiplexer;
  TMultiplexer = OBJECT(TValueComponent)
    NumInputs: Word;
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    FUNCTION StandardConnector(AName:STRING; AFlags:Word):PConnector; VIRTUAL;
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PShifter = ^TShifter;
  TShifter = OBJECT(TValueComponent)
    Direction: PConnector;
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PALU = ^TALU;
  TALU = OBJECT(TValueComponent)
    SecondInput, Operation: PConnector;
    NFlag, ZFLag: PConnector;
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PMicroSeqLogic = ^TMicroSeqLogic;
  TMicroSeqLogic = OBJECT(TValueComponent)
    NFlag, ZFLag: PConnector;
    CONSTRUCTOR Init(ANAme:STRING; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PROM = ^TROM;
  TROM = OBJECT(TValueComponent)
    Memory: PMemory;
    CONSTRUCTOR Init(ANAme:STRING; AMemory:PMemory; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PRAM = ^TRAM;
  TRAM = OBJECT(TValueComponent)
    Memory: PMemory;
    Address: PConnector;
    Data: PConnector;
    WriteEnable: PConnector;
    ReadEnable: PConnector;
    CONSTRUCTOR Init(ANAme:STRING; AMemory:PMemory; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    FUNCTION Recalculate: Boolean; VIRTUAL;
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PIncreaser = ^TIncreaser;
  TIncreaser = OBJECT(TValueComponent)
    CONSTRUCTOR Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PDecoder = ^TDecoder;
  TDecoder = OBJECT(TComponent)
    NumInBits: Byte;
    NumOutBits: Byte;
    Input: PConnector;
    Output: PConnector;
    CONSTRUCTOR Init(AName:STRING; ANumBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    PROCEDURE CalculateOutputs; VIRTUAL;
  END;

  PEnableDecoder = ^TEnableDecoder;
  TEnableDecoder = OBJECT(TDecoder)
    Enable: PConnector;
    CONSTRUCTOR Init(AName:STRING; ANumInBits:Word; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    FUNCTION Recalculate: Boolean; VIRTUAL;
  END;

  PBitSplitter = ^TBitSplitter;
  TBitSplitter = OBJECT(TComponent)
    NumBits: Word;
    Value: LongInt;
    Input: PConnector;
    CONSTRUCTOR Init(AName:STRING; ANumBits:Word; NumOutputs:Word;
       OutputBits:Pointer; ANext:PComponent);
    CONSTRUCTOR ParamInit(Parameters:PValueItem);
    FUNCTION WellConnected: Boolean; VIRTUAL;
    PROCEDURE CalculateOutputs; VIRTUAL;
    PROCEDURE Show; VIRTUAL;
  END;

  TGetParameters = FUNCTION:PValueItem;
  TNewComponent = PROCEDURE(Parameters:PValueItem);

  PComponentType = ^TComponentType;
  TComponentType = RECORD
     Number: Word;
     Name: STRING[30];
     VMTLink: Word;
     GetParameters: Pointer;
     ParamInit: Pointer;
     Next: PComponentType;
  END;

PROCEDURE ConnectTo(OutputComp:PComponent; OutputConn:STRING;
                    InputComp:PComponent; InputConn:STRING);
PROCEDURE RegisterComponent(VAR Rec:TComponentType);
FUNCTION  GetComponentTypeNames:PStringCollection;
FUNCTION  GetComponentType(Name:STRING):PComponentType;
FUNCTION  GetParameters(ComponentType:PComponentType): PValueItem;
FUNCTION  NewComponent(ComponentType:PComponentType; Parameters:PValueItem):PComponent;

IMPLEMENTATION

VAR
  ComponentList: PComponentType;

FUNCTION GetComponentPointer(ComponentType:Word): PComponentType;

VAR
  P: PComponentType;

BEGIN
  P := ComponentList;
  WHILE (P<>NIL) AND (P^.Number<>ComponentType) DO P:=P^.Next;
  GetComponentPointer := P;
END;

FUNCTION GetParameters(ComponentType:PComponentType): PValueItem;

BEGIN
  GetParameters := TGetParameters(ComponentType^.GetParameters);
END;

FUNCTION NewComponent(ComponentType:PComponentType; Parameters:PValueItem):PComponent;

VAR
  Component: PComponent;
  ParamInit: PROCEDURE;
  VMTLink: Word;

BEGIN
  Move(ComponentType^.ParamInit, ParamInit, 4);
  VMTLink := ComponentType^.VMTLink;
  ASM
	LES	DI, Parameters		{ Parameters for ParamInit on stack }
	PUSH	ES
	PUSH	DI
	MOV	AX, VMTLink
	PUSH	AX
	XOR	AX, AX			{ NIL pointer for @Self }
	PUSH	AX
	PUSH	AX
	CALL	[ParamInit]
	MOV	WORD PTR [Component+00], AX	{ Returns @Self }
	MOV	WORD PTR [Component+02], DX
  END;
  NewComponent := Component;
END;

{===========================================================================}
{=======                    TBaseComponent                            ======}
{===========================================================================}

CONSTRUCTOR TBaseComponent.Init(AName:STRING; ANext:PBaseComponent);

BEGIN
  Name := NewStr(ANAme);
  Flags := cfStandard;
  State := csStandard;
  Next := ANext;
END;

DESTRUCTOR TBaseComponent.Done;

BEGIN
  DisposeStr(Name);
  IF Next<>NIL THEN Dispose(Next, Done);
END;

FUNCTION TBaseComponent.WellConnected: Boolean;

BEGIN
  IF Next<>NIL THEN
    WellConnected := Next^.WellConnected
  ELSE
    WellConnected := True;
END;

PROCEDURE TBaseComponent.Reset;

BEGIN
  IF Next<>NIL THEN Next^.Reset;
  State := csStandard;
END;

FUNCTION TBaseComponent.Recalculate: Boolean;

BEGIN
  Recalculate := GetState(csStable);
END;

FUNCTION TBaseComponent.RecalculateAll: Boolean;

VAR
  Stable: Boolean;

BEGIN
  Stable := Recalculate;
  IF (Next<>NIL) AND NOT Next^.RecalculateAll THEN
    Stable := False;
  RecalculateAll := Stable;
END;

PROCEDURE TBaseComponent.SetState(AState:Word; Enable:Boolean);

BEGIN
  IF Enable THEN
    State := State OR AState
  ELSE
    State := State AND ($FFFF-AState);
END;

FUNCTION TBaseComponent.GetState(AState:Word): Boolean;

BEGIN
  GetState := State AND AState = AState;
END;

PROCEDURE TBaseComponent.SetFlags(AFlags:Word; Enable:Boolean);

BEGIN
  IF Enable THEN
    Flags := Flags OR AFlags
  ELSE
    Flags := Flags AND ($FFFF-AFlags);
END;

FUNCTION TBaseComponent.GetFlags(AFlags:Word): Boolean;

BEGIN
  GetFlags := Flags AND AFlags = AFlags;
END;

PROCEDURE TBaseComponent.Show;

BEGIN
  IF Next<>NIL THEN Next^.Show;
  Writeln;
  Writeln(Name^, ': State ', State);
END;

{===========================================================================}
{=======                      TConnector                              ======}
{===========================================================================}

CONSTRUCTOR TConnector.Init(AName: STRING; ANumBits:Word; AOwner: PComponent;
  AFlags: Word; ANext:PConnector);

BEGIN
  TBaseComponent.Init(ANAme, ANext);
  Value := 0;
  NumBits := ANumBits;
  Owner := AOwner;
  Flags := AFlags;
  ConnectedTo := NIL;
END;

FUNCTION TConnector.Connect(AConnector: PConnector): Word;

BEGIN
  IF (ConnectedTo<>NIL) OR (AConnector=NIL) OR
    (AConnector^.NumBits<>NumBits) OR NOT
    ((GetFlags(cfInput) AND AConnector^.GetFlags(cfOutput)) OR
     (GetFlags(cfOutput) AND AConnector^.GetFlags(cfInput))) THEN
  BEGIN
    Connect := cpIllegalConnection;
    Exit;
  END;
  ConnectedTo := AConnector;
  Connect := cpOk;
END;

FUNCTION TConnector.DisConnect: Word;

BEGIN
  IF (ConnectedTo=NIL) THEN
    DisConnect := cpIllegalConnection
  ELSE
  BEGIN
    ConnectedTo^.ConnectedTo := NIL;
    ConnectedTo := NIL;
    DisConnect := cpOk;
  END;
END;

FUNCTION TConnector.WellConnected: Boolean;

BEGIN
  WellConnected := (ConnectedTo<>NIL) AND (ConnectedTo^.NumBits=NumBits) AND
    (ConnectedTo^.ConnectedTo=@Self) AND TBaseComponent.WellConnected;
END;

PROCEDURE TConnector.SetState(AState:Word; Enable:Boolean);

VAR
  Dummy: Boolean;

BEGIN
  TBaseComponent.SetState(AState, Enable);
  IF (AState AND csStable<>0) AND Enable THEN
  BEGIN
    IF GetFlags(cfOutput) AND NOT ConnectedTo^.GetState(csStable) THEN
    BEGIN
      ConnectedTo^.Value := Value;
      ConnectedTo^.SetState(State, True);
    END;
    IF GetFlags(cfInput) AND NOT Owner^.GetState(csStable) THEN
      Dummy := Owner^.Recalculate;
  END;
END;

FUNCTION TConnector.Recalculate: Boolean;

BEGIN
  IF NOT GetState(csStable) THEN
  BEGIN
    IF GetFlags(cfInput) AND ConnectedTo^.GetState(csStable) THEN
    BEGIN
      Value := ConnectedTo^.Value;
      SetState(csStable, True);
    END;
  END;
  Recalculate := GetState(csStable);
END;

PROCEDURE TConnector.Show;

BEGIN
  TBaseComponent.Show;
  Writeln('Value: ', Value, '; Numbits: ', NumBits);
  Writeln('Owner: ', Owner^.NAme^, '; Connected to ', ConnectedTo^.Name^, ' of ', ConnectedTo^.Owner^.Name^);
END;

FUNCTION TConnector.GetDescription:STRING;

VAR
  S, s0: STRING;

BEGIN
  S := Name^ + '(';
  IF GetFlags(cfInput) THEN S := S + 'I';
  IF GetFlags(cfOutput) THEN S := S + 'O';
  Str(NumBits, s0);
  S := S + ' ' + s0 + ')';

  IF ConnectedTo=NIL THEN
    S := S + ' not connected'
  ELSE
  BEGIN
    S := S + '->' + ConnectedTo^.Owner^.Name^ + '.' + ConnectedTo^.Name^ + ' ';
    IF NOT GetState(csStable) THEN
      S := S + 'not stable'
    ELSE
      IF GetState(csTriState) THEN
        S := S + 'tri-state'
      ELSE
      BEGIN
        Str(Value, s0);
        S := S + s0
      END
  END;

  GetDescription := S;
END;

{===========================================================================}
{=======                      TComponent                              ======}
{===========================================================================}

CONSTRUCTOR TComponent.Init(AName:STRING; ATypeName:STRING; ANext:PComponent);

BEGIN
  TBaseComponent.Init(AName, ANext);
  TypeName := NewStr(ATypeName);
  Connectors := NIL;
END;

DESTRUCTOR TComponent.Done;

BEGIN
  TBaseComponent.Done;
  IF Connectors<>NIL THEN Dispose(Connectors, Done);
END;

FUNCTION TComponent.ConnectConnector(MyConnector:PConnector; OtherConnector:PConnector): Word;

BEGIN
  IF (MyConnector=NIL) OR (MyConnector^.Owner<>@Self) THEN
    ConnectConnector := cpIllegalConnection
  ELSE
    ConnectConnector := MyConnector^.Connect(OtherConnector);
END;

FUNCTION TComponent.AddConnector(AConnector:PConnector): Word;

VAR
  P: PConnector;

BEGIN
  AConnector^.Owner := @Self;
  IF Connectors=NIL THEN
    Connectors:=AConnector
  ELSE
  BEGIN
    P := Connectors;
    WHILE P^.Next<>NIL DO P:=PConnector(P^.Next);
    PConnector(P^.Next) := AConnector;
  END;
  AddConnector := cpOk;
END;

FUNCTION TComponent.RemoveConnector(AConnector:PConnector): Word;

VAR
  P: PConnector;

BEGIN
  IF (AConnector=NIL) OR (AConnector^.Owner<>@Self) OR
    (AConnector^.ConnectedTo<>NIL) THEN
     RemoveConnector := cpIllegalConnection
  ELSE
  BEGIN
     IF Connectors=AConnector THEN
     BEGIN
        Dispose(Connectors, Done);
        Connectors := NIL
     END
     ELSE
     BEGIN
        P := Connectors;
        WHILE (P^.Next<>NIL) AND (PConnector(P^.Next)<>AConnector) DO P := PConnector(P^.Next);
        IF PConnector(P^.Next)=AConnector THEN
        BEGIN
           P^.Next := AConnector^.Next;
           AConnector^.Next := NIL;
           Dispose(AConnector, Done);
           RemoveConnector := cpOk
        END
        ELSE
          RemoveConnector := cpIllegalConnection;
     END;
  END;
END;

FUNCTION TComponent.GetConnector(AName:STRING):PConnector;

VAR
  Connector: PConnector;

BEGIN
  Connector := Connectors;
  WHILE (Connector<>NIL) AND (Connector^.Name^<>ANAme) DO
    Connector := PConnector(Connector^.Next);
  GetConnector := Connector;
END;

FUNCTION TComponent.StandardConnector(AName:STRING; AFlags:Word):PConnector;

BEGIN
  StandardConnector := NIL;
END;

FUNCTION TComponent.WellConnected: Boolean;

BEGIN
  WellConnected := ( (Connectors=NIL) OR (Connectors^.WellConnected) ) AND
    TBaseComponent.WellConnected;
END;

FUNCTION TComponent.Recalculate: Boolean;

VAR
  Connector: PConnector;
  Stable: Boolean;

BEGIN
  Stable := GetState(csStable);
  IF NOT Stable THEN   { If already stable then no use thinking about it }
  BEGIN
    Stable := True;
    Connector := Connectors;
    WHILE Stable AND (Connector<>NIL) DO
    BEGIN
      IF NOT Connector^.GetFlags(cfOutput) THEN
         Stable := Connector^.Recalculate;
      Connector := PConnector(Connector^.Next);
    END;

    IF Stable THEN
    BEGIN
      SetState(csStable, True);
      CalculateOutputs;
    END;
  END;
  Recalculate := Stable;
END;

PROCEDURE TComponent.CalculateOutputs;

BEGIN
  RunError(255); {Abstract;}
END;

PROCEDURE TComponent.Reset;

BEGIN
  TBaseComponent.Reset;
  IF Connectors<>NIL THEN Connectors^.Reset;
END;

PROCEDURE TComponent.Show;

BEGIN
  TBaseComponent.Show;
  Writeln('Typename: ', TypeName^);
  IF Connectors<>NIL THEN BEGIN
    Writeln('Connectors :');
    Connectors^.Show;
  END;
END;

FUNCTION TComponent.GetComponentWindow(UseNumber:Boolean): PWindow;

VAR
  R: TRect;
  Window: PNumberWindow;
  Connector: PConnector;
  i: Word;

BEGIN
  R.Assign(15,10,65,20);
  Window := New(PNumberWindow, Init(R, Name^, UseNumber));
  Connector := Connectors;
  i := 1;
  WHILE Connector<>NIL DO
  BEGIN
    R.Assign(1, i, 50, i+1);
    Window^.Insert(New(PStaticText, Init(R, Connector^.GetDescription)));
    Connector := PConnector(Connector^.Next);
    Inc(i);
  END;
  GetComponentWindow := Window;
END;

{===========================================================================}
{=======                    TValueComponent                           ======}
{===========================================================================}

CONSTRUCTOR TValueComponent.Init(AName:STRING; ATypeName:STRING; ANumBits:Word;
  ANext:PComponent);

BEGIN
  TComponent.Init(AName, ATypeName, ANext);
  NumBits := ANumBits;
END;

FUNCTION TValueComponent.AddConnector(AConnector:PConnector): Word;

BEGIN
  IF AConnector^.NumBits<>NumBits THEN
    AddConnector := cpIllegalConnection
  ELSE
    AddConnector := TComponent.AddConnector(AConnector);
END;

FUNCTION TValueComponent.StandardConnector(AName:STRING; AFlags:Word):PConnector;

BEGIN
  IF GetFlags(AFlags AND (cfInput+cfOutput)) THEN
     StandardConnector := New(PConnector, Init(AName, NumBits, @Self, AFlags OR cfStandardConnector, NIL))
  ELSE
     StandardConnector := NIL;
END;

PROCEDURE TValueComponent.CalculateOutputs;

VAR
  P: PConnector;

BEGIN
  P := Connectors;
  WHILE P<>NIL DO
  BEGIN
     IF P^.GetFlags(cfOutput) THEN
     BEGIN
        P^.Value := Value;
        P^.SetState(csStable, True);
     END;
     P := PConnector(P^.Next);
  END;
END;

PROCEDURE TValueComponent.Show;

BEGIN
  TComponent.Show;
  Writeln('NumBits: ', NumBits, '; Value :', Value);
END;

{===========================================================================}
{=======                    TTriStateBuffer                           ======}
{===========================================================================}

CONSTRUCTOR TTriStateBuffer.Init(ANAme:STRING; ANumBits:Word; ANExt:PComponent);

BEGIN
  TValueComponent.Init(ANAme, 'TriStateBuffer', ANumBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Connectors := New(PConnector, Init('InBus', NumBits, @Self, cfInput,
    New(PConnector, Init('Enable', 1, @Self, cfInput, NIL))));
  Enable := PConnector(Connectors^.Next);
END;

PROCEDURE TTriStateBuffer.CalculateOutputs;

VAR
  P: PConnector;

BEGIN
  Value := Connectors^.Value;
  P := Connectors;
  WHILE P<>NIL DO
  BEGIN
     IF P^.GetFlags(cfOutput) THEN
     BEGIN
        P^.Value := Value;
        IF Enable^.Value=0 THEN P^.SetState(csTriState, True);
        P^.SetState(csStable, True);
     END;
     P := PConnector(P^.Next);
  END;
END;

CONSTRUCTOR TTriStateBuffer.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TTriStateBuffer.Init(NameParam^, NumBitsParam^, NIL);
END;

{===========================================================================}
{=======                       TConstant                              ======}
{===========================================================================}

CONSTRUCTOR TConstant.Init(AName:STRING; ANumBits:Word; AValue: LongInt;
  ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Constant', ANumBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Value := AValue;
END;

CONSTRUCTOR TConstant.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;
  ValueParam: PLong;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  ValueParam := Parameters^.Next^.Next^.Value^.Data;
  TConstant.Init(NameParam^, NumBitsParam^, ValueParam^, NIL);
END;

PROCEDURE TConstant.Reset;

VAR
  Connector: PConnector;

BEGIN
  TComponent.Reset;
  State := csStable;
  Connector := Connectors;
  WHILE Connector<>NIL DO
  BEGIN
     IF Connector^.GetFlags(cfOutput) THEN
     BEGIN
        Connector^.Value := Value;
        Connector^.State := csStable;
     END;
    Connector := PConnector(Connector^.Next);
  END;
END;

{===========================================================================}
{=======                       TRegister                              ======}
{===========================================================================}

CONSTRUCTOR TRegister.Init(AName:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Register', ANumBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Connectors := New(PConnector, Init('InBus', NumBits, @Self, cfInput,
    New(PConnector, Init('InputEnable', 1, @Self, cfInput, NIL))));
  InputEnable := PConnector(Connectors^.Next);
END;

CONSTRUCTOR TRegister.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TRegister.Init(NameParam^, NumBitsParam^, NIL);
END;


FUNCTION TRegister.Recalculate: Boolean;

VAR
  Connector: PConnector;
  Stable: Boolean;

BEGIN
  Stable := GetState(csStable);
  IF NOT Stable THEN   { If already stable then no use thinking about it }
  BEGIN
    Stable := InputEnable^.Recalculate;
    IF Stable THEN
    BEGIN
      IF (InputEnable^.Value=0) THEN
        Stable := True
      ELSE
      BEGIN
        Stable := Connectors^.Recalculate;
        IF Stable THEN Value := Connectors^.Value;
      END;

      IF Stable THEN
      BEGIN
        SetState(csStable, True);
        CalculateOutputs;
      END;
    END;
  END;
  Recalculate := Stable;
END;

{===========================================================================}
{=======                         TBus                                 ======}
{===========================================================================}

CONSTRUCTOR TBus.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Bus', ANumBits, ANext);
  Flags := Flags OR (cfHasStandardInput + cfHasStandardOutput);
END;

CONSTRUCTOR TBus.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TBus.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TBus.CalculateOutputs;

VAR
  P: PConnector;
  Found: Boolean;

BEGIN
  P := Connectors;
  Found := False;
  WHILE NOT Found AND (P<>NIL) DO
  BEGIN
    IF P^.GetFlags(cfInput) THEN Found := NOT P^.GetState(csTriState);
    IF NOT Found THEN P:=PConnector(P^.Next);
  END;

  IF Found THEN BEGIN
    Value := P^.Value;
    TValueComponent.CalculateOutputs;
  END;
END;

{===========================================================================}
{=======                       TAndPort                               ======}
{===========================================================================}

CONSTRUCTOR TAndPort.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'AndPort', ANumBits, ANext);
  Flags := Flags OR (cfHasStandardInput + cfHasStandardOutput);
END;

CONSTRUCTOR TANDPort.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TANDPort.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TAndPort.CalculateOutputs;

VAR
  Connector: PConnector;

BEGIN
  Value := $FFFFFFFF;
  Connector := Connectors;
  WHILE (Connector<>NIL) DO
  BEGIN
     IF Connector^.GetFlags(cfInput) THEN
        Value := Value AND Connector^.Value;
    Connector:=PConnector(Connector^.Next);
  END;

  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                        TOrPort                               ======}
{===========================================================================}

CONSTRUCTOR TOrPort.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'OrPort', ANumBits, ANext);
  Flags := Flags OR (cfHasStandardInput + cfHasStandardOutput);
END;

CONSTRUCTOR TORPort.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TORPort.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TOrPort.CalculateOutputs;

VAR
  Connector: PConnector;

BEGIN
  Value := $00000000;
  Connector := Connectors;
  WHILE (Connector<>NIL) DO
  BEGIN
     IF Connector^.GetFlags(cfInput) THEN
        Value := Value OR Connector^.Value;
    Connector:=PConnector(Connector^.Next);
  END;

  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                       TXORPort                               ======}
{===========================================================================}

CONSTRUCTOR TXORPort.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'XORPort', ANumBits, ANext);
  Flags := Flags OR (cfHasStandardInput + cfHasStandardOutput);
END;

CONSTRUCTOR TXORPort.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TXORPort.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TXORPort.CalculateOutputs;

VAR
  Connector: PConnector;

BEGIN
  Value := $00000000;
  Connector := Connectors;
  WHILE (Connector<>NIL) DO
  BEGIN
     IF Connector^.GetFlags(cfInput) THEN
        Value := Value XOR Connector^.Value;
    Connector:=PConnector(Connector^.Next);
  END;

  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                     TMultiplexer                             ======}
{===========================================================================}

CONSTRUCTOR TMultiplexer.Init(AName:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Multiplexer', ANumBits, ANext);
  Flags := Flags OR (cfHasStandardInput + cfHasStandardOutput);
  NumInputs := 0;
  Connectors := New(PConnector, Init('Number', NumBits, @Self, cfInput, NIL));
END;

CONSTRUCTOR TMultiplexer.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TMultiplexer.Init(NameParam^, NumBitsParam^, NIL);
END;

FUNCTION TMultiplexer.StandardConnector(AName:STRING; AFlags:Word):PConnector;

BEGIN
  IF NOT (AFlags AND (cfOutput+cfInput)=cfInput+cfOutput) AND
  ((AFlags AND cfOutput<>0) OR (NumInputs<1 SHL NumBits)) THEN
  BEGIN
     StandardConnector := TValueComponent.StandardConnector(AName, AFlags);
     IF AFlags AND cfInput<>0 THEN Inc(NumInputs);
  END
  ELSE
     StandardConnector := NIL;
END;

PROCEDURE TMultiplexer.CalculateOutputs;

VAR
  Connector: PConnector;
  Number: Word;
  i: Word;

BEGIN
  Number := Connectors^.Value;
  Connector := PConnector(Connectors^.Next);
  i := 0;
  WHILE (Connector<>NIL) AND (i<>Number) DO
  BEGIN
    IF Connector^.GetFlags(cfInput) THEN Inc(i);
    Connector := PConnector(Connector^.Next);
  END;

  IF Connector<>NIL THEN
  BEGIN
    Value:=Connector^.Value;
    TValueComponent.CalculateOutputs;
  END;
END;

{===========================================================================}
{=======                         TALU                                 ======}
{===========================================================================}

CONSTRUCTOR TALU.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'ALU', ANumBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Connectors := New(PConnector, Init('FirstInput', NumBits, @Self, cfInput,
    New(PConnector, Init('SecondInput', NumBits, @Self, cfInput,
    New(PConnector, Init('Operation', 2, @Self, cfInput,
    New(PConnector, Init('NFlag', 1, @Self, cfOutput,
    New(PConnector, Init('ZFlag', 1, @Self, cfOutput,
    NIL))))))))));
  SecondInput := PConnector(Connectors^.Next);
  Operation := PConnector(SecondInput^.Next);
  NFlag := PConnector(Operation^.Next);
  ZFlag := PConnector(NFlag^.Next);
END;

CONSTRUCTOR TALU.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TALU.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TALU.CalculateOutputs;

BEGIN
  CASE Operation^.Value OF
    aluAdd:  Value := Connectors^.Value + SecondInput^.Value;
    aluAnd:  Value := Connectors^.Value AND SecondInput^.Value;
    aluCopy: Value := Connectors^.Value;
    aluNot:  Value := NOT Connectors^.Value;
  END;

  TValueComponent.CalculateOutputs;
END;
{===========================================================================}
{=======                       TShifter                               ======}
{===========================================================================}

CONSTRUCTOR TShifter.Init(AName:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Shifter', ANumBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Connectors := New(PConnector, Init('Input', NumBits, @Self, cfInput,
    New(PConnector, Init('Direction', 2, @Self, cfInput,
    NIL))));
  Direction := PConnector(Connectors^.Next);
END;

CONSTRUCTOR TShifter.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TShifter.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TShifter.CalculateOutputs;

BEGIN
  CASE Direction^.Value OF
    shNoShift: Value := Connectors^.Value;
    shShiftRight: Value := Connectors^.Value SHR 1;
    shShiftLeft: Value := Connectors^.Value SHL 1
  END;

  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                    TMicroSeqLogic                            ======}
{===========================================================================}

CONSTRUCTOR TMicroSeqLogic.Init(ANAme:STRING; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'MicroSeqLogic', 1, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Connectors:= New(PConnector, Init('Condition', 2, @Self, cfInput,
    New(PConnector, Init('NFlag', 1, @Self, cfInput,
    New(PConnector, Init('ZFlag', 1, @Self, cfInput,
    NIL))))));
  NFlag := PConnector(Connectors^.Next);
  ZFlag := PConnector(NFlag^.Next);
END;

CONSTRUCTOR TMicroSeqLogic.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;

BEGIN
  NameParam := Parameters^.Value^.Data;
  TMicroSeqLogic.Init(NameParam^, NIL);
END;

PROCEDURE TMicroSeqLogic.CalculateOutputs;

BEGIN
  CASE Connectors^.Value OF
    mslNoJump: Value := 0;
    mslJumpN: Value := NFlag^.Value;
    mslJumpZ: Value := ZFlag^.Value;
    mslJumpAlways: Value := 1;
  END;

  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                          TROM                                ======}
{===========================================================================}

CONSTRUCTOR TROM.Init(ANAme:STRING; AMemory:PMemory; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'ROM', AMemory^.NumDataBits, ANext);
  Flags := Flags OR cfHasStandardOutput;
  Memory := AMemory;
  Connectors := New(PConnector, Init('Address', Memory^.NumAddressBits, @Self, cfInput,
     NIL));
END;

CONSTRUCTOR TROM.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  MemoryParam: PMemory;

BEGIN
  NameParam := Parameters^.Value^.Data;
  MemoryParam := Parameters^.Next^.Value^.Data;
  TROM.Init(NameParam^, MemoryParam, NIL);
END;

PROCEDURE TROM.CalculateOutputs;

BEGIN
  Value := Memory^.Peek(Connectors^.Value);
  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                          TRAM                                ======}
{===========================================================================}

CONSTRUCTOR TRAM.Init(ANAme:STRING; AMemory:PMemory; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'RAM', AMemory^.NumDataBits, ANext);
  Memory := AMemory;
  Connectors := New(PConnector, Init('Address', Memory^.NumAddressBits, @Self, cfInput,
    New(PConnector, Init('Data', Memory^.NumDataBits, @Self, cfInput+cfOutput,
    New(PConnector, Init('Write', 1, @Self, cfInput,
    New(PConnector, Init('Read', 1, @Self, cfInput,
    NIL))))))));
  Address := Connectors;
  Data := PConnector(Address^.Next);
  WriteEnable := PConnector(Data^.Next);
  ReadEnable := PConnector(WriteEnable^.Next);
END;

CONSTRUCTOR TRAM.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  MemoryParam: PMemory;

BEGIN
  NameParam := Parameters^.Value^.Data;
  MemoryParam := Parameters^.Next^.Value^.Data;
  TRAM.Init(NameParam^, MemoryParam, NIL);
END;

FUNCTION TRAM.Recalculate: Boolean;

VAR
  Connector: PConnector;
  Stable: Boolean;

BEGIN
  Stable := GetState(csStable);
  IF NOT Stable THEN   { If already stable then no use thinking about it }
  BEGIN
    Stable := Address^.Recalculate AND ReadEnable^.Recalculate AND WriteEnable^.Recalculate;
    IF Stable AND (WriteEnable^.Value=1) THEN Stable := Data^.Recalculate;

    IF Stable THEN
    BEGIN
      SetState(csStable, True);
      CalculateOutputs;
    END;
  END;
  Recalculate := Stable;
END;

PROCEDURE TRAM.CalculateOutputs;

BEGIN
   IF ReadEnable^.Value=1 THEN
   BEGIN
      Value := Memory^.Peek(Address^.Value);
      Data^.Value := Value;
      Data^.SetState(csStable, True);
   END;

   IF WriteEnable^.Value=1 THEN
   BEGIN
      Value := Data^.Value;
      Memory^.Poke(Address^.Value, Value);
   END;
END;

{===========================================================================}
{=======                      TIncreaser                              ======}
{===========================================================================}

CONSTRUCTOR TIncreaser.Init(ANAme:STRING; ANumBits:Word; ANext:PComponent);

BEGIN
  TValueComponent.Init(AName, 'Increaser', ANumBits, ANext);
  SetFlags(cfHasStandardOutput, True);
  Connectors := New(PConnector, Init('Input', NumBits, @Self, cfInput, NIL));
END;

CONSTRUCTOR TIncreaser.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TIncreaser.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TIncreaser.CalculateOutputs;

BEGIN
  Value := Connectors ^.Value + 1;
  IF Value<0 THEN Value := (1 SHL NumBits) - 1;
  TValueComponent.CalculateOutputs;
END;

{===========================================================================}
{=======                       TDecoder                               ======}
{===========================================================================}

CONSTRUCTOR TDecoder.Init(AName:STRING; ANumBits:Word; ANext:PComponent);

VAR
  i: Byte;
  S: STRING;
  Code: Word;

BEGIN
  TComponent.Init(ANAme, 'Decoder', ANext);
  NumInBits := ANumBits;
  NumOutBits := 1 SHL NumInBits;
  Connectors := New(PConnector, Init('Input', NumInBits, @Self, cfInput, NIL));
  Input := Connectors;
  FOR i:=0 TO NumOutBits-1 DO
  BEGIN
    Str(i, S);
    Code := AddConnector(New(PConnector, Init('Line'+S, 1, NIL, cfOutput, NIL)));
  END;
  Output := PConnector(Connectors^.Next);
END;

CONSTRUCTOR TDecoder.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TDecoder.Init(NameParam^, NumBitsParam^, NIL);
END;

PROCEDURE TDecoder.CalculateOutputs;

VAR
  Value, i: Word;
  Connector: PConnector;

BEGIN
  Value := Input^.Value;
  i:=0;
  Connector := Output;
  FOR i:=0 TO NumOutBits-1 DO
  BEGIN
    IF i=Value THEN Connector^.Value:=1 ELSE Connector^.Value:=0;
    Connector^.SetState(csStable, True);
    Connector := PConnector(Connector^.Next);
  END;
END;

{===========================================================================}
{=======                    TEnableDecoder                            ======}
{===========================================================================}

CONSTRUCTOR TEnableDecoder.Init(AName:STRING; ANumInBits:Word; ANext:PComponent);

VAR
  Code: Word;

BEGIN
  TDecoder.Init(ANAme, ANumInBits, ANext);
  Enable := New(PConnector, Init('Enable', 1, @Self, cfInput, NIL));
  Code := AddConnector(Enable);
END;

CONSTRUCTOR TEnableDecoder.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  TEnableDecoder.Init(NameParam^, NumBitsParam^, NIL);
END;

FUNCTION TEnableDecoder.Recalculate: Boolean;

VAR
  Connector: PConnector;
  Stable: Boolean;

BEGIN
  Stable := GetState(csStable);
  IF NOT Stable THEN   { If already stable then no use thinking about it }
  BEGIN
    Stable := Enable^.Recalculate;
    IF Stable THEN
    BEGIN
      IF (Enable^.Value=0) THEN
      BEGIN
        SetState(csStable, True);
        Connector := Output;          { NOT enabled -> all zeros }
        WHILE Connector<>NIL DO
        BEGIN
          Connector^.Value := 0;
          Connector^.SetState(csStable, True);
          Connector := PConnector(Connector^.Next);
        END;
      END
      ELSE
      BEGIN
        Stable := Input^.Recalculate;
        IF Stable THEN
        BEGIN
          SetState(csStable, True);
          CalculateOutputs;     { calculate decoder outputs }
        END;
      END;
    END;
  END;
  Recalculate := Stable;
END;

{===========================================================================}
{=======                     TBitSplitter                             ======}
{===========================================================================}

CONSTRUCTOR TBitSplitter.Init(AName:STRING; ANumBits:Word; NumOutputs:Word;
   OutputBits:Pointer; ANext:PComponent);

VAR
  i: Word;
  s: STRING;
  Code: Word;

BEGIN
  TComponent.Init(AName, 'BitSplitter', ANext);
  NumBits := ANumBits;
  Connectors := New(PConnector, Init('Input', NumBits, @Self, cfInput, NIL));
  FOR i:=0 TO NumOutputs-1 DO
  BEGIN
     Str(i, s);
     Code := AddConnector(New(PConnector, Init('Output '+s, PByteArray(OutputBits)^[i], @Self, cfOutput, NIL)));
  END;
  Input := Connectors;
END;

CONSTRUCTOR TBitSplitter.ParamInit(Parameters:PValueItem);

VAR
  NameParam: PString;
  NumBitsParam: PWord;
  NumOutputsParam: PWord;
  OutputBitsParam: PByteArray;

BEGIN
  NameParam := Parameters^.Value^.Data;
  NumBitsParam := Parameters^.Next^.Value^.Data;
  NumOutputsParam := Parameters^.Next^.Next^.Value^.Data;
  OutputBitsParam := Parameters^.Next^.Next^.Next^.Value^.Data;
  TBitSplitter.Init(NameParam^, NumBitsParam^, NumOutputsParam^, OutputBitsParam, NIL);
END;

FUNCTION TBitSplitter.WellConnected: Boolean;

VAR
  TotalOutBits: Word;
  Connector: PConnector;

BEGIN
  TotalOutBits := 0;
  Connector := PConnector(Connectors^.Next);
  WHILE Connector<>NIL DO
  BEGIN
    IF Connector^.GetFlags(cfOutput) THEN Inc(TotalOutBits, Connector^.NumBits);
    Connector := PConnector(Connector^.Next);
  END;

  WellConnected := (TotalOutBits=NumBits) AND TComponent.WellConnected;
END;

PROCEDURE TBitSplitter.CalculateOutputs;

VAR
  Connector: PConnector;
  TempValue: LongInt;

BEGIN
  Value := Input^.Value;
  Connector := PConnector(Input^.Next);
  TempValue := Input^.Value;
  WHILE Connector<>NIL DO
  BEGIN
    Connector^.Value := TempValue AND ((1 SHL Connector^.NumBits) - 1);
    TempValue := TempValue SHR Connector^.NumBits;
    Connector^.SetState(csStable, True);
    Connector := PConnector(Connector^.Next);
  END;
END;

PROCEDURE TBitSplitter.Show;

BEGIN
  TComponent.Show;
  Writeln('NumBits: ', NumBits, '; Value :', BinL(Value));
END;

{===========================================================================}
{=======                         Proc                                 ======}
{===========================================================================}

PROCEDURE ConnectTo(OutputComp:PComponent; OutputConn:STRING;
                    InputComp:PComponent; InputConn:STRING);

VAR
  Code: Word;

BEGIN
  Code := OutputComp^.ConnectConnector(OutputComp^.GetConnector(OutputConn), InputComp^.GetConnector(InputConn));
  IF Code<>0 THEN Writeln('Connection error ', Code);
  Code := InputComp^.ConnectConnector(InputComp^.GetConnector(InputConn), OutputComp^.GetConnector(OutputConn));
  IF Code<>0 THEN Writeln('Connection error ', Code);
END;

FUNCTION GetNameNumBitsParameters: PValueItem;

VAR
   NameParam: PString;
   NumBitsParam: PWord;

BEGIN
   GetMem(NameParam, SizeOf(TComponentName));
   GetMem(NumBitsParam, SizeOf(Word));
   NameParam^ := '';
   NumBitsParam^ := 1;
   GetNameNumBitsParameters := NewStringItem('Name', NameParam^, SizeOf(TComponentName)-1, True,
     NewWordItem('NumBits', NumBitsParam^, dfDecimal, True,
     NIL));
END;

FUNCTION GetNameParameters: PValueItem;

VAR
   NameParam: PString;

BEGIN
   GetMem(NameParam, SizeOf(TComponentName));
   NameParam^ := '';
   GetNameParameters := NewStringItem('Name', NameParam^, SizeOf(TComponentName)-1, True,
     NIL);
END;

FUNCTION GetConstantParameters: PValueItem;

VAR
   NameParam: PString;
   NumBitsParam: PWord;
   ValueParam: PLong;

BEGIN
   GetMem(NameParam, SizeOf(TComponentName));
   GetMem(NumBitsParam, SizeOf(Word));
   GetMem(ValueParam, SizeOf(LongInt));
   NameParam^ := '';
   NumBitsParam^ := 1;
   ValueParam^ := 0;
   GetConstantParameters := NewStringItem('Name', NameParam^, SizeOf(TComponentName)-1, True,
     NewWordItem('NumBits', NumBitsParam^, dfDecimal, True,
     NewLongItem('Value', ValueParam^, dfDecimal, True,
     NIL)));
END;

FUNCTION GetBitSplitterParameters: PValueItem;

VAR
  NameParam: PString;
  NumBitsParam: PWord;
  NumOutputsParam: PWord;
  OutBitsParam: PByteArray;

BEGIN
  GetMem(NameParam, SizeOf(TComponentName));
  GetMem(NumBitsParam, SizeOf(Word));
  GetMem(NumOutputsParam, SizeOf(Word));
  GetMem(OutbitsParam, MaxNumSplitterOutputs);
  NameParam^ := '';
  NumBitsParam^ := 1;
  NumOutputsParam^ := 0;
  GetBitSplitterParameters := NewStringItem('Name', NameParam^, SizeOf(TComponentName)-1, True,
    NewWordItem('NumBits', NumBitsParam^, dfDecimal, True,
    NewWordItem('NumOutputs', NumOutputsParam^, dfDecimal, True,
    NewByteItem('OutBits', OutBitsParam^[0], dfDecimal, True, {!!!!!}
    NIL))));
END;


CONST
  crTriStateBuffer:TComponentType = (
     Number: 1;
     Name: 'TriStateBuffer';
     VMTLink: Ofs(TypeOf(TTriStateBuffer)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TTriStateBuffer.ParamInit
  );

  crConstant:TComponentType = (
     Number: 2;
     Name: 'Constant';
     VMTLink: Ofs(TypeOf(TConstant)^);
     GetParameters: @GetConstantParameters;
     ParamInit: @TConstant.ParamInit
  );

  crRegister:TComponentType = (
     Number: 3;
     Name: 'Register';
     VMTLink: Ofs(TypeOf(TRegister)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TRegister.ParamInit
  );

  crBus:TComponentType = (
     Number: 4;
     Name: 'Bus';
     VMTLink: Ofs(TypeOf(TBus)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TBus.ParamInit
  );

  crAndPort:TComponentType = (
     Number: 5;
     Name: 'ANDPort';
     VMTLink: Ofs(TypeOf(TAndPort)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TAndPort.ParamInit
  );

  crOrPort:TComponentType = (
     Number: 6;
     Name: 'ORPort';
     VMTLink: Ofs(TypeOf(TORPort)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TOrPort.ParamInit
  );

  crXORPort:TComponentType = (
     Number: 7;
     Name: 'XORPort';
     VMTLink: Ofs(TypeOf(TXORPort)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TXORPort.ParamInit
  );

  crMultiplexer:TComponentType = (
     Number: 8;
     Name: 'Multiplexer';
     VMTLink: Ofs(TypeOf(TMultiplexer)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TMultiplexer.ParamInit
  );

  crShifter:TComponentType = (
     Number: 9;
     Name: 'Shifter';
     VMTLink: Ofs(TypeOf(TShifter)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TShifter.ParamInit
  );

  crALU:TComponentType = (
     Number: 10;
     Name: 'ALU';
     VMTLink: Ofs(TypeOf(TALU)^);
     GetParameters: @GetNameNumBitsParameters;
     ParamInit: @TALU.ParamInit
  );

  crMicroSeqLogic:TComponentType = (
     Number: 11;
     Name: 'MicroSeqLogic';
     VMTLink: Ofs(TypeOf(TMicroSeqLogic)^);
     GetParameters: @GetNameParameters;
     ParamInit: @TMicroSeqLogic.ParamInit
  );

  crBitSplitter:TComponentType = (
     Number: 12;
     Name: 'BitSplitter';
     VMTLink: Ofs(TypeOf(TBitSplitter)^);
     GetParameters: @GetBitSplitterParameters;
     ParamInit: @TBitSplitter.ParamInit
  );

PROCEDURE RegisterComponent(VAR Rec:TComponentType);

BEGIN
   Rec.Next := ComponentList;
   ComponentList := Addr(Rec);
END;

PROCEDURE RegisterComponents;

BEGIN
  RegisterComponent(crTriStateBuffer);
  RegisterComponent(crConstant);
  RegisterComponent(crRegister);
  RegisterComponent(crBus);
  RegisterComponent(crAndPort);
  RegisterComponent(crOrPort);
  RegisterComponent(crXORPort);
  RegisterComponent(crMultiplexer);
  RegisterComponent(crShifter);
  RegisterComponent(crALU);
  RegisterComponent(crMicroSeqLogic);
END;

FUNCTION GetComponentTypeNames:PStringCollection;

VAR
  P: PComponentType;
  Names: PStringCollection;

BEGIN
  Names := New(PStringCollection, Init(5,5));

  P := ComponentList;
  WHILE P<>NIL DO
  BEGIN
    Names^.Insert(NewStr(P^.Name));
    P := P^.Next;
  END;
  GetComponentTypeNames := Names;
END;

FUNCTION GetComponentType(Name:STRING):PComponentType;

VAR
  P: PComponentType;

BEGIN
  P := ComponentList;
  WHILE (P<>NIL) AND (P^.Name<>Name) DO P:=P^.Next;
  GetComponentType := P;
END;

{===========================================================================}
{=======                         Main                                 ======}
{===========================================================================}

BEGIN
  ComponentList := NIL;
  RegisterComponents;
END.

(*

CONST
  MyMem: ARRAY[0..15] OF Word =
    ($FFFF, $7FFF, $3FFF, $1FFF, $0FFF, $07FF, $03FF, $01FF,
     $00FF, $007F, $003F, $001F, $000F, $0007, $0003, $0001);

VAR
  Memory: PMemory;
  Address: PConstant;
  ROM: PROM;
  Data: PBus;
  Code, i: Word;
  P: Pointer;

BEGIN
  Writeln('Testing ROM');

  P := @MyMem;
  Memory := New(PMemory, Init(P, 16,4, False,False));

  Address := New(PConstant, Init('Address',4,0,
    New(PROM, Init('MyROM', Memory,
    New(PBus, Init('Data',16,
    NIL))))));

  ROM := PROM(Address^.Next);
  Data := PBus(ROM^.Next);

  Code := Address^.AddConnector(Address^.StandardConnector('Output',cfOutput));
  Code := ROM^.AddConnector(ROM^.StandardConnector('Output',cfOutput));
  Code := Data^.AddConnector(Data^.StandardConnector('Input',cfInput));

  ConnectTo(Address, 'Output', ROM, 'Address');
  ConnectTo(ROM, 'Output', Data, 'Input');

  Writeln;
  IF Address^.WellConnected THEN Writeln('Well connected') ELSE
  BEGIN
     Writeln('Not well connected');
     Halt(1);
  END;
  Writeln;

  FOR i:=0 TO 15 DO
  BEGIN
    Address^.Value := i;
    Address^.Reset;
    IF NOT Address^.RecalculateAll THEN Writeln('Not stable');
    Writeln(HexW(MyMem[i]), '    ', HexW(Data^.Value));
  END;

  Dispose(Address, Done);
END.

*)
(*

VAR
  Decoder: PEnableDecoder;
  Input: PConstant;
  Enable: PConstant;
  Outputs: ARRAY[0..15] OF PBus;
  Code: Word;
  i,j: Word;
  S: STRING;

BEGIN
  Writeln('Testing EnableDecoder');
  Decoder := New(PEnableDecoder, Init('A-Decoder', 4,
    New(PConstant, Init('Decoder-input', 4, 0,
    New(PConstant, Init('Decoder enable', 1, 0,
    NIL))))));
  Input := PConstant(Decoder^.Next);
  Enable := PConstant(Input^.Next);


  FOR i:=0 TO 15 DO
  BEGIN
    Str(i,S);
    Outputs[i] := New(PBus, Init('Bus'+S, 1, NIL));
    IF i>0 THEN Outputs[i-1]^.Next := Outputs[i];
  END;

  Enable^.Next := Outputs[0];

  Code := Input^.AddConnector(Input^.StandardConnector('ToDecoder',cfOutput));
  Code := Enable^.AddConnector(Enable^.StandardConnector('ToDecoder',cfOutput));
  FOR i:=0 TO 15 DO
    Code := Outputs[i]^.AddConnector(Outputs[i]^.StandardConnector('Input',cfInput));

  ConnectTo(Input, 'ToDecoder', Decoder, 'Input');
  ConnectTo(Enable, 'ToDecoder', Decoder, 'Enable');
  FOR i:=0 TO 15 DO
  BEGIN
    Str(i, S);
    ConnectTo(Decoder, 'Line'+S, Outputs[i], 'Input');
  END;

  Writeln;
  IF Input^.WellConnected THEN Writeln('Well connected') ELSE
  BEGIN
     Writeln('Not well connected');
     Halt(1);
  END;
  Writeln;

  Enable^.Value := 0;
  Writeln('Not enabled');
  FOR j:=0 TO 15 DO
  BEGIN
    Input^.Value := j;
    Decoder^.Reset;
    IF Decoder^.RecalculateAll THEN Write('All Stable  ') ELSE Write('Not all stable  ');
    FOR i:=0 TO 15 DO Write(Outputs[i]^.Value);
    Writeln;
  END;

  Enable^.Value := 1;
  Writeln('Enabled');
  FOR j:=0 TO 15 DO
  BEGIN
    Input^.Value := j;
    Decoder^.Reset;
    IF Decoder^.Recalculate THEN Write('All Stable  ') ELSE Write('Not all stable  ');
    FOR i:=0 TO 15 DO Write(Outputs[i]^.Value);
    Writeln;
  END;

  Dispose(Input, Done);
END.

*)


VAR
  Register: PRegister;
  Buffer: PTriStateBuffer;
  Bus: PBus;
  InputEnable, OutputEnable: PConstant;
  Splitter: PBitSplitter;
  Bus0, Bus1, Bus2, Bus3: PBus;
  Stable: Boolean;
  Code: WOrd;

CONST
  SplitterBits: ARRAY[0..3] OF Byte = (1,2,5,8);

BEGIN
  RegisterComponents;

  Writeln('Testing Register,Bus,Constant,TriStateBuffer');
  Register := New(PRegister, Init('AC', 16,
    New(PTriStateBuffer, Init('ACBuffer', 16,
    New(PBus, Init('MainBus', 16,
    New(PConstant, Init('InputEnable', 1, 1,
    New(PConstant, Init('OutputEnable', 1, 0,
    New(PBitSplitter, Init('Splitter', 16, 4, Addr(SplitterBits),
    New(PBus, Init('Bus0', 1,
    New(PBus, Init('Bus1', 2,
    New(PBus, Init('Bus2', 5,
    New(PBus, Init('Bus3', 8,
    NIL))))))))))))))))))));

  Buffer := PTriStateBuffer(Register^.Next);
  Bus := PBus(Buffer^.Next);
  InputEnable := PConstant(Bus^.Next);
  OutputEnable := PConstant(InputEnable^.Next);
  Splitter := PBitSplitter(OutputEnable^.Next);
  Bus0 := PBus(Splitter^.Next);
  Bus1 := PBus(Bus0^.Next);
  Bus2 := PBus(Bus1^.Next);
  Bus3 := PBus(Bus2^.Next);

  Code := Register^.AddConnector(Register^.StandardConnector('ToBuffer',cfOutput));
  Code := Buffer^.AddConnector(Buffer^.StandardConnector('ToBus',cfOutput));
  Code := Bus^.AddConnector(Bus^.StandardConnector('FromBuffer',cfInput));
  Code := Bus^.AddConnector(Bus^.StandardConnector('ToAC',cfOutput));
  Code := Bus^.AddConnector(Bus^.StandardConnector('ToSplitter',cfOutput));
  Code := InputEnable^.AddConnector(InputEnable^.StandardConnector('Output',cfOutput));
  Code := OutputEnable^.AddConnector(OutputEnable^.StandardConnector('Output',cfOutput));
  Code := Bus0^.AddConnector(Bus0^.StandardConnector('Input',cfInput));
  Code := Bus1^.AddConnector(Bus1^.StandardConnector('Input',cfInput));
  Code := Bus2^.AddConnector(Bus2^.StandardConnector('Input',cfInput));
  Code := Bus3^.AddConnector(Bus3^.StandardConnector('Input',cfInput));

  ConnectTo(InputEnable, 'Output', Register, 'InputEnable');
  ConnectTo(Register, 'ToBuffer', Buffer, 'InBus');
  ConnectTo(Bus, 'ToAC', Register, 'InBus');
  ConnectTo(OutputEnable, 'Output', Buffer, 'Enable');
  ConnectTo(Buffer, 'ToBus', Bus, 'FromBuffer');
  ConnectTo(Bus, 'ToSplitter', Splitter, 'Input');
  ConnectTo(Splitter, 'Output 0', Bus0, 'Input');
  ConnectTo(Splitter, 'Output 1', Bus1, 'Input');
  ConnectTo(Splitter, 'Output 2', Bus2, 'Input');
  ConnectTo(Splitter, 'Output 3', Bus3, 'Input');

  Writeln;
  IF Register^.WellConnected THEN Writeln('Well connected') ELSE
  BEGIN
     Writeln('Not well connected');
     Halt(1);
  END;
  Writeln;

  Register^.Value := 1234;
  InputEnable^.Value := 0;
  OutputEnable^.Value := 1;
  Register^.Reset;
  Stable := Register^.RecalculateAll;

  Writeln('In: ', InputEnable^.Value, '   Out: ', OutputEnable^.Value);
  Writeln('AC  (', Register^.State, ') : ', BinW(Register^.Value));
  Writeln('Buf (', Buffer^.State, ') : ', Buffer^.Value);
  Writeln('Bus (', Bus^.State, ') : ', Bus^.Value);
  Writeln('Bus0 (', Bus0^.State, ') : ', BinW(Bus0^.Value));
  Writeln('Bus1 (', Bus1^.State, ') : ', BinW(Bus1^.Value));
  Writeln('Bus2 (', Bus2^.State, ') : ', BinW(Bus2^.Value));
  Writeln('Bus3 (', Bus3^.State, ') : ', BinW(Bus3^.Value));
  Writeln;

  Register^.Value := 12;
  OutputEnable^.Value := 0;
  Register^.Reset;
  Stable := Register^.RecalculateAll;

  Writeln('In: ', InputEnable^.Value, '   Out: ', OutputEnable^.Value);
  Writeln('AC  (', Register^.State, ') : ', Register^.Value);
  Writeln('Buf (', Buffer^.State, ') : ', Buffer^.Value);
  Writeln('Bus (', Bus^.State, ') : ', Bus^.Value);
  Writeln;

  InputEnable^.Value := 1;
  Register^.Reset;
  Stable := Register^.RecalculateAll;

  Writeln('In: ', InputEnable^.Value, '   Out: ', OutputEnable^.Value);
  Writeln('AC  (', Register^.State, ') : ', Register^.Value);
  Writeln('Buf (', Buffer^.State, ') : ', Buffer^.Value);
  Writeln('Bus (', Bus^.State, ') : ', Bus^.Value);
  Writeln;

  OutputEnable^.Value := 1;
  Register^.Reset;
  Stable := Register^.RecalculateAll;

  Writeln('In: ', InputEnable^.Value, '   Out: ', OutputEnable^.Value);
  Writeln('AC  (', Register^.State, ') : ', Register^.Value);
  Writeln('Buf (', Buffer^.State, ') : ', Buffer^.Value);
  Writeln('Bus (', Bus^.State, ') : ', Bus^.Value);
  Writeln;

  Dispose(Register, Done);
END.
