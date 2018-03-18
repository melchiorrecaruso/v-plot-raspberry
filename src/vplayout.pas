{
  Description: vPlot layout.

  Copyright (C) 2017-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit vplayout;

{$mode objfpc}
{$i include.inc}

interface

uses
  vpcommon;

type
  tvplayout = packed record
    p00:   tvppoint;
    p01:   tvppoint;
    p02:   tvppoint;
    p03:   tvppoint;
    p04:   tvppoint;
    p05:   tvppoint;
    p08:   tvppoint;
    p09:   tvppoint;
    p10:   tvppoint;
    p11:   tvppoint;
    p12:   tvppoint;
    p13:   tvppoint;
    mode:  longint;
    ratio: double;
  end;

procedure loadlayout(var layout: tvplayout; const filename: rawbytestring);


implementation

uses
  inifiles, sysutils;

procedure loadlayout(var layout: tvplayout; const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  try
    ini.formatsettings.decimalseparator := '.';
    layout.ratio := ini.readfloat  ('VPLOT v1.0', 'RATIO', -1);
    layout.mode  := ini.readinteger('VPLOT v1.0', 'MODE',  -1);
    layout.p00.x := ini.readfloat  ('VPLOT v1.0', 'P00.X', -1);
    layout.p00.y := ini.readfloat  ('VPLOT v1.0', 'P00.Y', -1);
    layout.p01.x := ini.readfloat  ('VPLOT v1.0', 'P01.X', -1);
    layout.p01.y := ini.readfloat  ('VPLOT v1.0', 'P01.Y', -1);
    layout.p02.x := ini.readfloat  ('VPLOT v1.0', 'P02.X', -1);
    layout.p02.y := ini.readfloat  ('VPLOT v1.0', 'P02.Y', -1);
    layout.p03.x := ini.readfloat  ('VPLOT v1.0', 'P03.X', -1);
    layout.p03.y := ini.readfloat  ('VPLOT v1.0', 'P03.Y', -1);
    layout.p04.x := ini.readfloat  ('VPLOT v1.0', 'P04.X', -1);
    layout.p04.y := ini.readfloat  ('VPLOT v1.0', 'P04.Y', -1);
    layout.p05.x := ini.readfloat  ('VPLOT v1.0', 'P05.X', -1);
    layout.p05.y := ini.readfloat  ('VPLOT v1.0', 'P05.Y', -1);

    layout.p08.x := ini.readfloat  ('VPLOT v1.0', 'P08.X', -1);
    layout.p08.y := ini.readfloat  ('VPLOT v1.0', 'P08.Y', -1);
    layout.p09.x := ini.readfloat  ('VPLOT v1.0', 'P09.X', -1);
    layout.p09.y := ini.readfloat  ('VPLOT v1.0', 'P09.Y', -1);

    layout.p10.x := ini.readfloat  ('VPLOT v1.0', 'P10.X', -1);
    layout.p10.y := ini.readfloat  ('VPLOT v1.0', 'P10.Y', -1);
    layout.p11.x := ini.readfloat  ('VPLOT v1.0', 'P11.X', -1);
    layout.p11.y := ini.readfloat  ('VPLOT v1.0', 'P11.Y', -1);
    layout.p12.x := ini.readfloat  ('VPLOT v1.0', 'P12.X', -1);
    layout.p12.y := ini.readfloat  ('VPLOT v1.0', 'P12.Y', -1);
    layout.p13.x := ini.readfloat  ('VPLOT v1.0', 'P13.X', -1);
    layout.p13.y := ini.readfloat  ('VPLOT v1.0', 'P13.Y', -1);
  finally
    ini.destroy;
  end;
  {$ifdef debug}
  writeln('--- VPLOT v1.0 ---');
  writeln(format('P00.X = %-5.3f  P00.Y = %-5.3f', [layout.p00.x, layout.p00.y]));
  writeln(format('P01.X = %-5.3f  P01.Y = %-5.3f', [layout.p01.x, layout.p01.y]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [layout.p02.x, layout.p02.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [layout.p03.x, layout.p03.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [layout.p04.x, layout.p04.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [layout.p05.x, layout.p05.y]));

  writeln(format('P08.X = %-5.3f  P08.Y = %-5.3f', [layout.p08.x, layout.p08.y]));
  writeln(format('P09.X = %-5.3f  P09.Y = %-5.3f', [layout.p09.x, layout.p09.y]));

  writeln(format('P10.X = %-5.3f  P10.Y = %-5.3f', [layout.p10.x, layout.p10.y]));
  writeln(format('P11.X = %-5.3f  P11.Y = %-5.3f', [layout.p11.x, layout.p11.y]));
  writeln(format('P12.X = %-5.3f  P12.Y = %-5.3f', [layout.p12.x, layout.p12.y]));
  writeln(format('P13.X = %-5.3f  P13.Y = %-5.3f', [layout.p13.x, layout.p13.y]));

  writeln(format('MODE  = %-5.3u', [layout.mode]));
  writeln(format('RATIO = %-5.8f', [layout.ratio]));
  {$endif}
end;

end.

