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

unit vpsetting;

{$mode objfpc}

interface

uses
  vpcommon;

type
  tvpsetting = class
  private
    flayout00: tvppoint;
    flayout01: tvppoint;
    flayout02: tvppoint;
    flayout03: tvppoint;
    flayout04: tvppoint;
    flayout05: tvppoint;
    flayout08: tvppoint;
    flayout09: tvppoint;

    fwave00:   tvppoint;
    fwave01:   tvppoint;
    fwave02:   tvppoint;
    fwave03:   tvppoint;
    fwave04:   tvppoint;
    fwave05:   tvppoint;
    fwave06:   tvppoint;
    fwave07:   tvppoint;
    fwave08:   tvppoint;

    fxmax:     double;
    fymax:     double;

    fmode:     longint;
    fratio:    double;
    fdelay0:   longint;
    fdelay1:   longint;
    fdelay2:   longint;
 public
    constructor create;
    destructor destroy; override;
    procedure load(const filename: rawbytestring);
    procedure clear;
 public
    property layout00: tvppoint read flayout00;
    property layout01: tvppoint read flayout01;
    property layout02: tvppoint read flayout02;
    property layout03: tvppoint read flayout03;
    property layout04: tvppoint read flayout04;
    property layout05: tvppoint read flayout05;
    property layout08: tvppoint read flayout08;
    property layout09: tvppoint read flayout09;

    property xmax:     double   read fxmax;
    property ymax:     double   read fymax;

    property wave00:   tvppoint read fwave00;
    property wave01:   tvppoint read fwave01;
    property wave02:   tvppoint read fwave02;
    property wave03:   tvppoint read fwave03;
    property wave04:   tvppoint read fwave04;
    property wave05:   tvppoint read fwave05;
    property wave06:   tvppoint read fwave06;
    property wave07:   tvppoint read fwave07;
    property wave08:   tvppoint read fwave08;

    property mode:     longint  read fmode;
    property ratio:    double   read fratio;
    property delay0:   longint  read fdelay0;
    property delay1:   longint  read fdelay1;
    property delay2:   longint  read fdelay2;
 end;


var
  setting: tvpsetting = nil;


implementation

uses
  inifiles, sysutils;

constructor tvpsetting.create;
begin
  inherited create;
  clear;
end;

destructor tvpsetting.destroy;
begin
  inherited destroy;
end;

procedure tvpsetting.clear;
begin
  flayout00.x := 0;  flayout00.y := 0;
  flayout01.x := 0;  flayout01.y := 0;
  flayout02.x := 0;  flayout02.y := 0;
  flayout03.x := 0;  flayout03.y := 0;
  flayout04.x := 0;  flayout04.y := 0;
  flayout05.x := 0;  flayout05.y := 0;
  flayout08.x := 0;  flayout08.y := 0;
  flayout09.x := 0;  flayout09.y := 0;

  fwave00.x   := 0;  fwave00.y   := 0;
  fwave01.x   := 0;  fwave01.y   := 0;
  fwave02.x   := 0;  fwave02.y   := 0;
  fwave03.x   := 0;  fwave03.y   := 0;
  fwave04.x   := 0;  fwave04.y   := 0;
  fwave05.x   := 0;  fwave05.y   := 0;
  fwave06.x   := 0;  fwave06.y   := 0;
  fwave07.x   := 0;  fwave07.y   := 0;
  fwave08.x   := 0;  fwave08.y   := 0;

  fxmax       := 0;
  fymax       := 0;

  fmode       := 0;
  fratio      := 0;
  fdelay2     := 0;
  fdelay1     := 0;
  fdelay0     := 0;
end;

procedure tvpsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';
  try
    flayout00.x := ini.readfloat  ('Layout',  '00.X',  0);
    flayout00.y := ini.readfloat  ('Layout',  '00.Y',  0);
    flayout01.x := ini.readfloat  ('Layout',  '01.X',  0);
    flayout01.y := ini.readfloat  ('Layout',  '01.Y',  0);
    flayout02.x := ini.readfloat  ('Layout',  '02.X',  0);
    flayout02.y := ini.readfloat  ('Layout',  '02.Y',  0);
    flayout03.x := ini.readfloat  ('Layout',  '03.X',  0);
    flayout03.y := ini.readfloat  ('Layout',  '03.Y',  0);
    flayout04.x := ini.readfloat  ('Layout',  '04.X',  0);
    flayout04.y := ini.readfloat  ('Layout',  '04.Y',  0);
    flayout05.x := ini.readfloat  ('Layout',  '05.X',  0);
    flayout05.y := ini.readfloat  ('Layout',  '05.Y',  0);

    flayout08.x := ini.readfloat  ('Layout',  '08.X',  0);
    flayout08.y := ini.readfloat  ('Layout',  '08.Y',  0);
    flayout09.x := ini.readfloat  ('Layout',  '09.X',  0);
    flayout09.y := ini.readfloat  ('Layout',  '09.Y',  0);

    fwave00.x   := ini.readfloat  ('Wave',    '00.X',  0);
    fwave00.y   := ini.readfloat  ('Wave',    '00.Y',  0);
    fwave01.x   := ini.readfloat  ('Wave',    '01.X',  0);
    fwave01.y   := ini.readfloat  ('Wave',    '01.Y',  0);
    fwave02.x   := ini.readfloat  ('Wave',    '02.X',  0);
    fwave02.y   := ini.readfloat  ('Wave',    '02.Y',  0);
    fwave03.x   := ini.readfloat  ('Wave',    '03.X',  0);
    fwave03.y   := ini.readfloat  ('Wave',    '03.Y',  0);
    fwave04.x   := ini.readfloat  ('Wave',    '04.X',  0);
    fwave04.y   := ini.readfloat  ('Wave',    '04.Y',  0);
    fwave05.x   := ini.readfloat  ('Wave',    '05.X',  0);
    fwave05.y   := ini.readfloat  ('Wave',    '05.Y',  0);
    fwave06.x   := ini.readfloat  ('Wave',    '06.X',  0);
    fwave06.y   := ini.readfloat  ('Wave',    '06.Y',  0);
    fwave07.x   := ini.readfloat  ('Wave',    '07.X',  0);
    fwave07.y   := ini.readfloat  ('Wave',    '07.Y',  0);
    fwave08.x   := ini.readfloat  ('Wave',    '08.X',  0);
    fwave08.y   := ini.readfloat  ('Wave',    '08.Y',  0);

    fxmax       := ini.readfloat  ('Area',    'XMAX',   0);
    fymax       := ini.readfloat  ('Area',    'YMAX',   0);

    fmode       := ini.readinteger('Stepper', 'MODE',   0);
    fratio      := ini.readfloat  ('Stepper', 'RATIO',  0);
    fdelay2     := ini.readinteger('Stepper', 'DELAY2', 0);
    fdelay1     := ini.readinteger('Stepper', 'DELAY1', 0);
    fdelay0     := ini.readinteger('Servo',   'DELAY0', 0);
  finally
    ini.destroy;
  end;

  if enabledebug then
  begin
    writeln(format('  LAYOUT::00.X  = %12.5f  00.Y = %12.5f', [flayout00.x, flayout00.y]));
    writeln(format('  LAYOUT::01.X  = %12.5f  01.Y = %12.5f', [flayout01.x, flayout01.y]));
    writeln(format('  LAYOUT::02.X  = %12.5f  02.Y = %12.5f', [flayout02.x, flayout02.y]));
    writeln(format('  LAYOUT::03.X  = %12.5f  03.Y = %12.5f', [flayout03.x, flayout03.y]));
    writeln(format('  LAYOUT::04.X  = %12.5f  04.Y = %12.5f', [flayout04.x, flayout04.y]));
    writeln(format('  LAYOUT::05.X  = %12.5f  05.Y = %12.5f', [flayout05.x, flayout05.y]));
    writeln(format('  LAYOUT::08.X  = %12.5f  08.Y = %12.5f', [flayout08.x, flayout08.y]));
    writeln(format('  LAYOUT::09.X  = %12.5f  09.Y = %12.5f', [flayout09.x, flayout09.y]));

    writeln(format('    WAVE::00.X  = %12.5f  00.Y = %12.5f', [  fwave00.x,   fwave00.y]));
    writeln(format('    WAVE::01.X  = %12.5f  01.Y = %12.5f', [  fwave01.x,   fwave01.y]));
    writeln(format('    WAVE::02.X  = %12.5f  02.Y = %12.5f', [  fwave02.x,   fwave02.y]));
    writeln(format('    WAVE::03.X  = %12.5f  03.Y = %12.5f', [  fwave03.x,   fwave03.y]));
    writeln(format('    WAVE::04.X  = %12.5f  04.Y = %12.5f', [  fwave04.x,   fwave04.y]));
    writeln(format('    WAVE::05.X  = %12.5f  05.Y = %12.5f', [  fwave05.x,   fwave05.y]));
    writeln(format('    WAVE::06.X  = %12.5f  06.Y = %12.5f', [  fwave06.x,   fwave06.y]));
    writeln(format('    WAVE::07.X  = %12.5f  07.Y = %12.5f', [  fwave07.x,   fwave07.y]));
    writeln(format('    WAVE::08.X  = %12.5f  08.Y = %12.5f', [  fwave08.x,   fwave08.y]));

    writeln(format('    AREA::XMAX  = %12.5f', [fxmax  ]));
    writeln(format('    AREA::YMAX  = %12.5f', [fymax  ]));

    writeln(format(' STEPPER::MODE  = %12.5u', [fmode  ]));
    writeln(format(' STEPPER::RATIO = %12.5f', [fratio ]));
    writeln(format(' STEPPER::DLY2  = %12.5u', [fdelay2]));
    writeln(format(' STEPPER::DLY1  = %12.5u', [fdelay1]));
    writeln(format('   SERVO::DLY0  = %12.5u', [fdelay0]));
  end;
end;

end.

