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

PROGRAM Inprodukt;

CONST
  Max = 20;

TYPE
  Vec       = ARRAY[1..Max] OF Integer;

VAR
  k:   Integer;
  x,y: Vec;

FUNCTION Prod(a,b: Integer): Integer;

VAR
  p,j: Integer;

BEGIN
  IF (a=0) OR (b=0) THEN
    Prod := 0
  ELSE
  BEGIN
    p := 0;
    FOR j:=1 TO a DO
      p := p + b;
    Prod := p
  END
END;

PROCEDURE Inprod(VAR v:vec; VAR Res:Integer);

VAR
  Som, i: Integer;

BEGIN
  Som := 0;
  FOR i:=1 TO Max DO
    Som := Som + Prod(x[i],v[i]);
  Res := Som
END;

BEGIN
  FOR k:=1 TO Max DO
  BEGIN
    x[k] := k;
    y[k] := Prod(2,k) + 1
  END;

  Inprod(y,k);
  Writeln(k);
END.
