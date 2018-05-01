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

interface

uses
  vpcommon;

type
  tvplayout = class
  private
    fpoint00: tvppoint;
    fpoint01: tvppoint;
    fpoint02: tvppoint;
    fpoint03: tvppoint;
    fpoint04: tvppoint;
    fpoint05: tvppoint;
    fpoint08: tvppoint;
    fpoint09: tvppoint;

    ftop:    double;
    fbottom: double;
    fleft:   double;
    fright:  double;

    fmode:   longint;
    fratio:  double;
    fdelay1: longint;
    fdelay2: longint;
    fdelay3: longint;

    function getheight: double;
    function getwidth:  double;
 public
    constructor create;
    destructor destroy; override;
    procedure load(const filename: rawbytestring);
 public
    property point00: tvppoint read fpoint00;
    property point01: tvppoint read fpoint01;
    property point02: tvppoint read fpoint02;
    property point03: tvppoint read fpoint03;
    property point04: tvppoint read fpoint04;
    property point05: tvppoint read fpoint05;

    property point08: tvppoint read fpoint08;
    property point09: tvppoint read fpoint09;

    property mode:    longint  read fmode;
    property ratio:   double   read fratio;

    property delay1:  longint  read fdelay1;
    property delay2:  longint  read fdelay2;
    property delay3:  longint  read fdelay3;

    property height:  double   read getheight;
    property width:   double   read getwidth;
 end;


implementation

uses
  inifiles, sysutils;

constructor tvplayout.create;
begin
  inherited create;
  fpoint00.x := 0;
  fpoint00.y := 0;
  fpoint01.x := 0;
  fpoint01.y := 0;
  fpoint02.x := 0;
  fpoint02.y := 0;
  fpoint03.x := 0;
  fpoint03.y := 0;
  fpoint04.x := 0;
  fpoint04.y := 0;
  fpoint05.x := 0;
  fpoint05.y := 0;

  fpoint08.x := 0;
  fpoint08.y := 0;
  fpoint09.x := 0;
  fpoint09.y := 0;

  ftop       := 0;
  fbottom    := 0;
  fleft      := 0;
  fright     := 0;

  fmode      := 0;
  fratio     := 0;
  fdelay1    := 0;
  fdelay2    := 0;
  fdelay3    := 0;
end;

destructor tvplayout.destroy;
begin
  inherited destroy;
end;

procedure tvplayout.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';
  try
    fpoint00.x := ini.readfloat  ('Layout',  'P00.X',  0);
    fpoint00.y := ini.readfloat  ('Layout',  'P00.Y',  0);
    fpoint01.x := ini.readfloat  ('Layout',  'P01.X',  0);
    fpoint01.y := ini.readfloat  ('Layout',  'P01.Y',  0);
    fpoint02.x := ini.readfloat  ('Layout',  'P02.X',  0);
    fpoint02.y := ini.readfloat  ('Layout',  'P02.Y',  0);
    fpoint03.x := ini.readfloat  ('Layout',  'P03.X',  0);
    fpoint03.y := ini.readfloat  ('Layout',  'P03.Y',  0);
    fpoint04.x := ini.readfloat  ('Layout',  'P04.X',  0);
    fpoint04.y := ini.readfloat  ('Layout',  'P04.Y',  0);
    fpoint05.x := ini.readfloat  ('Layout',  'P05.X',  0);
    fpoint05.y := ini.readfloat  ('Layout',  'P05.Y',  0);
    fpoint09.x := ini.readfloat  ('Layout',  'P09.X',  0);
    fpoint09.y := ini.readfloat  ('Layout',  'P09.Y',  0);

    ftop       := ini.readfloat  ('Borders', 'TOP',    0);
    fbottom    := ini.readfloat  ('Borders', 'BOTTOM', 0);
    fleft      := ini.readfloat  ('Borders', 'LEFT',   0);
    fright     := ini.readfloat  ('Borders', 'RIGHT',  0);

    fmode      := ini.readinteger('Stepper', 'MODE',   0);
    fratio     := ini.readfloat  ('Stepper', 'RATIO',  0);
    fdelay1    := ini.readinteger('Stepper', 'DELAY1', 0);
    fdelay2    := ini.readinteger('Stepper', 'DELAY2', 0);

    fdelay3    := ini.readinteger('Servo',   'DELAY3', 0);
  finally
    ini.destroy;
  end;
  fpoint08.x   := ((fleft   ) + (fpoint01.x - fright)) / 2;
  fpoint08.y   := ((fbottom ) + (fpoint01.y - ftop  )) / 2;

  if enabledebug then
  begin
    writeln(format('  LAYOUT::P00.X  = %12.5f  P00.Y = %12.5f', [fpoint00.x, fpoint00.y]));
    writeln(format('  LAYOUT::P01.X  = %12.5f  P01.Y = %12.5f', [fpoint01.x, fpoint01.y]));
    writeln(format('  LAYOUT::P02.X  = %12.5f  P02.Y = %12.5f', [fpoint02.x, fpoint02.y]));
    writeln(format('  LAYOUT::P03.X  = %12.5f  P03.Y = %12.5f', [fpoint03.x, fpoint03.y]));
    writeln(format('  LAYOUT::P04.X  = %12.5f  P04.Y = %12.5f', [fpoint04.x, fpoint04.y]));
    writeln(format('  LAYOUT::P05.X  = %12.5f  P05.Y = %12.5f', [fpoint05.x, fpoint05.y]));
    writeln(format('  LAYOUT::P08.X  = %12.5f  P08.Y = %12.5f', [fpoint08.x, fpoint08.y]));
    writeln(format('  LAYOUT::P09.X  = %12.5f  P09.Y = %12.5f', [fpoint09.x, fpoint09.y]));

    writeln(format('  LAYOUT::TOP    = %12.5f', [ftop   ]));
    writeln(format('  LAYOUT::BOTTOM = %12.5f', [fbottom]));
    writeln(format('  LAYOUT::LEFT   = %12.5f', [fleft  ]));
    writeln(format('  LAYOUT::RIGHT  = %12.5f', [fright ]));
    writeln(format('  LAYOUT::MODE   = %12.5u', [fmode]));
    writeln(format('  LAYOUT::RATIO  = %12.5f', [fratio]));

    writeln(format('  LAYOUT::DELAY1 = %12.5u', [fdelay1]));
    writeln(format('  LAYOUT::DELAY2 = %12.5u', [fdelay2]));
    writeln(format('  LAYOUT::DELAY3 = %12.5u', [fdelay3]));
  end;
end;

function tvplayout.getheight: double;
begin
  result := fpoint01.y - (ftop + fbottom);
end;

function tvplayout.getwidth:  double;
begin
  result := fpoint01.x - (fleft + fright);
end;

end.

