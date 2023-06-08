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

UNIT MicroCmd;

INTERFACE

CONST
  cmChangeAddressFormat = 2001;
  cmChangeDataFormat    = 2002;
  cmAutoSize            = 2003;
  cmGoto                = 2004;
  cmLocal		= 2005;

  cmSetSize             = 3000;
  cmSetFormat           = 3001;

  cmUpdate              = 1100;
  cmProcessOutput       = 1101;
  cmNewAddress          = 1102;
  cmGotoCursor          = 1103;

  cmThisIsIt            = 1000;
  cmThisIsSomethingElse = 1001;
  cmTotallyDifferent    = 1002;
  cmReadMicro           = 1003;
  cmReadMacro           = 1004;
  cmDumpMicro           = 1005;
  cmDumpMacro           = 1006;
  cmSystem              = 1007;
  cmChDir               = 1008;
  cmDosShell            = 1009;
  cmCodeMicro           = 1010;
  cmCodeMacro           = 1011;
  cmViewRegisters       = 1012;
  cmViewInternals       = 1013;
  cmClockCycle          = 1014;
  cmMicroInstruction    = 1015;
  cmMacroInstruction    = 1016;
  cmProgramReset        = 1017;
  cmIncreasePC          = 1018;
  cmRunTo90             = 1019;
  cmViewOutput          = 1020;
  cmWriteToOutput       = 1021;
  cmScreenSize          = 1022;
  cmClockPhase          = 1023;
  cmRun                 = 1024;

IMPLEMENTATION

END.
