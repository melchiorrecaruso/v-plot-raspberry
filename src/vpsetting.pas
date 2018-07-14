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
  vpcommon, vpmath, vpwave;

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

    fwave:     twavemesh;
    fwavexmax: double;
    fwaveymax: double;

    fmode:     longint;
    fratio:    double;
    fradius:   double;
    fdelaym:   longint;
    fdelayz:   longint;
 public
    constructor create;
    destructor destroy; override;
    procedure load(const filename: rawbytestring);
    procedure clear;
 public
    property layout00: tvppoint  read flayout00;
    property layout01: tvppoint  read flayout01;
    property layout02: tvppoint  read flayout02;
    property layout03: tvppoint  read flayout03;
    property layout04: tvppoint  read flayout04;
    property layout05: tvppoint  read flayout05;
    property layout08: tvppoint  read flayout08;
    property layout09: tvppoint  read flayout09;

    property wave:     twavemesh read fwave;
    property wavexmax: double    read fwavexmax;
    property waveymax: double    read fwaveymax;

    property mode:     longint   read fmode;
    property ratio:    double    read fratio;
    property radius:   double    read fradius;
    property delaym:   longint   read fdelaym;
    property delayz:   longint   read fdelayz;
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

  fwave[0].x  := 0;  fwave[0].y  := 0;
  fwave[1].x  := 0;  fwave[1].y  := 0;
  fwave[2].x  := 0;  fwave[2].y  := 0;
  fwave[3].x  := 0;  fwave[3].y  := 0;
  fwave[4].x  := 0;  fwave[4].y  := 0;
  fwave[5].x  := 0;  fwave[5].y  := 0;
  fwave[6].x  := 0;  fwave[6].y  := 0;
  fwave[7].x  := 0;  fwave[7].y  := 0;
  fwave[8].x  := 0;  fwave[8].y  := 0;
  fwavexmax   := 0;
  fwaveymax   := 0;

  fmode       := 0;
  fratio      := 0;
  fradius     := 0;
  fdelaym     := 0;
  fdelayz     := 0;
end;

procedure tvpsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';
  try
    flayout00.x := ini.readfloat  ('Layout',  '00.X',   0);
    flayout00.y := ini.readfloat  ('Layout',  '00.Y',   0);
    flayout01.x := ini.readfloat  ('Layout',  '01.X',   0);
    flayout01.y := ini.readfloat  ('Layout',  '01.Y',   0);
    flayout02.x := ini.readfloat  ('Layout',  '02.X',   0);
    flayout02.y := ini.readfloat  ('Layout',  '02.Y',   0);
    flayout03.x := ini.readfloat  ('Layout',  '03.X',   0);
    flayout03.y := ini.readfloat  ('Layout',  '03.Y',   0);
    flayout04.x := ini.readfloat  ('Layout',  '04.X',   0);
    flayout04.y := ini.readfloat  ('Layout',  '04.Y',   0);
    flayout05.x := ini.readfloat  ('Layout',  '05.X',   0);
    flayout05.y := ini.readfloat  ('Layout',  '05.Y',   0);

    flayout08.x := ini.readfloat  ('Layout',  '08.X',   0);
    flayout08.y := ini.readfloat  ('Layout',  '08.Y',   0);
    flayout09.x := ini.readfloat  ('Layout',  '09.X',   0);
    flayout09.y := ini.readfloat  ('Layout',  '09.Y',   0);

    fwave[0].x  := ini.readfloat  ('Wave',    '00.X',   0);
    fwave[0].y  := ini.readfloat  ('Wave',    '00.Y',   0);
    fwave[1].x  := ini.readfloat  ('Wave',    '01.X',   0);
    fwave[1].y  := ini.readfloat  ('Wave',    '01.Y',   0);
    fwave[2].x  := ini.readfloat  ('Wave',    '02.X',   0);
    fwave[2].y  := ini.readfloat  ('Wave',    '02.Y',   0);
    fwave[3].x  := ini.readfloat  ('Wave',    '03.X',   0);
    fwave[3].y  := ini.readfloat  ('Wave',    '03.Y',   0);
    fwave[4].x  := ini.readfloat  ('Wave',    '04.X',   0);
    fwave[4].y  := ini.readfloat  ('Wave',    '04.Y',   0);
    fwave[5].x  := ini.readfloat  ('Wave',    '05.X',   0);
    fwave[5].y  := ini.readfloat  ('Wave',    '05.Y',   0);
    fwave[6].x  := ini.readfloat  ('Wave',    '06.X',   0);
    fwave[6].y  := ini.readfloat  ('Wave',    '06.Y',   0);
    fwave[7].x  := ini.readfloat  ('Wave',    '07.X',   0);
    fwave[7].y  := ini.readfloat  ('Wave',    '07.Y',   0);
    fwave[8].x  := ini.readfloat  ('Wave',    '08.X',   0);
    fwave[8].y  := ini.readfloat  ('Wave',    '08.Y',   0);
    fwavexmax   := ini.readfloat  ('Wave',    'XMAX',   0);
    fwaveymax   := ini.readfloat  ('Wave',    'YMAX',   0);

    fmode       := ini.readinteger('Stepper', 'MODE',   0);
    fratio      := ini.readfloat  ('Stepper', 'RATIO',  0);
    fradius     := ini.readfloat  ('Stepper', 'RADIUS', 0);
    fdelaym     := ini.readinteger('Stepper', 'DELAY',  0);
    fdelayz     := ini.readinteger('Stepper', 'DELAY',  0);
  finally
    ini.destroy;
  end;

  if enabledebug then
  begin
    writeln(format('  LAYOUT::00.X   = %12.5f  00.Y = %12.5f', [flayout00.x, flayout00.y]));
    writeln(format('  LAYOUT::01.X   = %12.5f  01.Y = %12.5f', [flayout01.x, flayout01.y]));
    writeln(format('  LAYOUT::02.X   = %12.5f  02.Y = %12.5f', [flayout02.x, flayout02.y]));
    writeln(format('  LAYOUT::03.X   = %12.5f  03.Y = %12.5f', [flayout03.x, flayout03.y]));
    writeln(format('  LAYOUT::04.X   = %12.5f  04.Y = %12.5f', [flayout04.x, flayout04.y]));
    writeln(format('  LAYOUT::05.X   = %12.5f  05.Y = %12.5f', [flayout05.x, flayout05.y]));
    writeln(format('  LAYOUT::08.X   = %12.5f  08.Y = %12.5f', [flayout08.x, flayout08.y]));
    writeln(format('  LAYOUT::09.X   = %12.5f  09.Y = %12.5f', [flayout09.x, flayout09.y]));

    writeln(format('    WAVE::00.X   = %12.5f  00.Y = %12.5f', [ fwave[0].x,  fwave[0].y]));
    writeln(format('    WAVE::01.X   = %12.5f  01.Y = %12.5f', [ fwave[1].x,  fwave[1].y]));
    writeln(format('    WAVE::02.X   = %12.5f  02.Y = %12.5f', [ fwave[2].x,  fwave[2].y]));
    writeln(format('    WAVE::03.X   = %12.5f  03.Y = %12.5f', [ fwave[3].x,  fwave[3].y]));
    writeln(format('    WAVE::04.X   = %12.5f  04.Y = %12.5f', [ fwave[4].x,  fwave[4].y]));
    writeln(format('    WAVE::05.X   = %12.5f  05.Y = %12.5f', [ fwave[5].x,  fwave[5].y]));
    writeln(format('    WAVE::06.X   = %12.5f  06.Y = %12.5f', [ fwave[6].x,  fwave[6].y]));
    writeln(format('    WAVE::07.X   = %12.5f  07.Y = %12.5f', [ fwave[7].x,  fwave[7].y]));
    writeln(format('    WAVE::08.X   = %12.5f  08.Y = %12.5f', [ fwave[8].x,  fwave[8].y]));
    writeln(format('    WAVE::XMAX   = %12.5f', [fwavexmax]));
    writeln(format('    WAVE::YMAX   = %12.5f', [fwaveymax]));

    writeln(format(' STEPPER::MODE   = %12.5u', [fmode    ]));
    writeln(format(' STEPPER::RATIO  = %12.5f', [fratio   ]));
    writeln(format(' STEPPER::RADIUS = %12.5f', [fradius  ]));
    writeln(format(' STEPPER::DLY-M  = %12.5u', [fdelaym  ]));
    writeln(format(' STEPPER::DLY-Z  = %12.5u', [fdelayz  ]));
  end;
end;

end.

