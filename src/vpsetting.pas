{
  Description: vPlot setting class.

  Copyright (C) 2017-2019 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  vpmath, vpwave;

type
  tvpsetting = class
  private
    flayout00: tvppoint;
    flayout01: tvppoint;
    flayout08: tvppoint;
    flayout09: tvppoint;

    fwave:     twavemesh;
    fwavexmax: single;
    fwaveymax: single;

    fxmin:     longint;
    fxmax:     longint;
    fxinc:     longint;
    fxdelay:   longint;
    fxdir:     longint;
    fxradius:  single;
    fxratio:   single;

    fymin:     longint;
    fymax:     longint;
    fyinc:     longint;
    fydelay:   longint;
    fydir:     longint;
    fyradius:  single;
    fyratio:   single;

    fzmin:     longint;
    fzmax:     longint;
    fzinc:     longint;
    fzdelay:   longint;
    fzdir:     longint;
    fzradius:  single;
    fzratio:   single;
 public
    constructor create;
    destructor  destroy; override;
    procedure   load(const filename: rawbytestring);
    procedure   clear;
 public
    property layout00: tvppoint  read flayout00;
    property layout01: tvppoint  read flayout01;
    property layout08: tvppoint  read flayout08;
    property layout09: tvppoint  read flayout09;

    property wave:     twavemesh read fwave;
    property wavexmax: single    read fwavexmax;
    property waveymax: single    read fwaveymax;

    property xmin:     longint   read fxmin;
    property xmax:     longint   read fxmax;
    property xinc:     longint   read fxinc;
    property xdelay:   longint   read fxdelay;
    property xdir:     longint   read fxdir;
    property xradius:  single    read fxradius;
    property xratio:   single    read fxratio;

    property ymin:     longint   read fymin;
    property ymax:     longint   read fymax;
    property yinc:     longint   read fyinc;
    property ydelay:   longint   read fydelay;
    property ydir:     longint   read fydir;
    property yradius:  single    read fyradius;
    property yratio:   single    read fyratio;

    property zmin:     longint   read fzmin;
    property zmax:     longint   read fzmax;
    property zinc:     longint   read fzinc;
    property zdelay:   longint   read fzdelay;
    property zdir:     longint   read fzdir;
    property zradius:  single    read fzradius;
    property zratio:   single    read fzratio;
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

  fxmin       := 0;  fymin       := 0;  fzmin       := 0;
  fxmax       := 0;  fymax       := 0;  fzmax       := 0;
  fxinc       := 0;  fyinc       := 0;  fzinc       := 0;
  fxdelay     := 0;  fydelay     := 0;  fzdelay     := 0;
  fxdir       := 0;  fydir       := 0;  fzdir       := 0;
  fxradius    := 0;  fyradius    := 0;  fzradius    := 0;
  fxratio     := 0;  fyratio     := 0;  fzratio     := 0;
end;

procedure tvpsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';
  try
    flayout00.x  := ini.readfloat ('Layout',  '00.X',  0);
    flayout00.y  := ini.readfloat ('Layout',  '00.Y',  0);
    flayout01.x  := ini.readfloat ('Layout',  '01.X',  0);
    flayout01.y  := ini.readfloat ('Layout',  '01.Y',  0);
    flayout08.x  := ini.readfloat ('Layout',  '08.X',  0);
    flayout08.y  := ini.readfloat ('Layout',  '08.Y',  0);
    flayout09.x  := ini.readfloat ('Layout',  '09.X',  0);
    flayout09.y  := ini.readfloat ('Layout',  '09.Y',  0);

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

    fxmin       := ini.readinteger('X-AXIS',  'MIN',    0);
    fxmax       := ini.readinteger('X-AXIS',  'MAX',    0);
    fxinc       := ini.readinteger('X-AXIS',  'INC',    0);
    fxdelay     := ini.readinteger('X-AXIS',  'DELAY',  0);
    fxdir       := ini.readinteger('X-AXIS',  'DIR',    0);
    fxradius    := ini.readfloat  ('X-AXIS',  'RADIUS', 0);
    fxratio     := ini.readfloat  ('X-AXIS',  'RATIO',  0);

    fymin       := ini.readinteger('Y-AXIS',  'MIN',    0);
    fymax       := ini.readinteger('Y-AXIS',  'MAX',    0);
    fyinc       := ini.readinteger('Y-AXIS',  'INC',    0);
    fydelay     := ini.readinteger('Y-AXIS',  'DELAY',  0);
    fydir       := ini.readinteger('Y-AXIS',  'DIR',    0);
    fyradius    := ini.readfloat  ('Y-AXIS',  'RADIUS', 0);
    fyratio     := ini.readfloat  ('Y-AXIS',  'RATIO',  0);

    fzmin       := ini.readinteger('Z-AXIS',  'MIN',    0);
    fzmax       := ini.readinteger('Z-AXIS',  'MAX',    0);
    fzinc       := ini.readinteger('Z-AXIS',  'INC',    0);
    fzdelay     := ini.readinteger('Z-AXIS',  'DELAY',  0);
    fzdir       := ini.readinteger('Z-AXIS',  'DIR',    0);
    fzradius    := ini.readfloat  ('Z-AXIS',  'RADIUS', 0);
    fzratio     := ini.readfloat  ('Z-AXIS',  'RATIO',  0);
  finally
    ini.destroy;
  end;

  if enabledebug then
  begin
    writeln(format('  LAYOUT::00.X   = %12.5f  00.Y = %12.5f', [flayout00.x, flayout00.y]));
    writeln(format('  LAYOUT::01.X   = %12.5f  01.Y = %12.5f', [flayout01.x, flayout01.y]));
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

    writeln(format('  X-AXIS::MIN    = %12.5f', [fxmin    ]));
    writeln(format('  X-AXIS::MAX    = %12.5f', [fxmax    ]));
    writeln(format('  X-AXIS::INC    = %12.5f', [fxinc    ]));
    writeln(format('  X-AXIS::DELAY  = %12.5u', [fxdelay  ]));
    writeln(format('  X-AXIS::DIR    = %12.5u', [fxdir    ]));
    writeln(format('  X-AXIS::RADIUS = %12.5f', [fxradius ]));
    writeln(format('  X-AXIS::RATIO  = %12.5f', [fxratio  ]));

    writeln(format('  Y-AXIS::MIN    = %12.5f', [fymin    ]));
    writeln(format('  Y-AXIS::MAX    = %12.5f', [fymax    ]));
    writeln(format('  Y-AXIS::INC    = %12.5f', [fyinc    ]));
    writeln(format('  Y-AXIS::DELAY  = %12.5u', [fydelay  ]));
    writeln(format('  Y-AXIS::DIR    = %12.5u', [fydir    ]));
    writeln(format('  Y-AXIS::RADIUS = %12.5f', [fyradius ]));
    writeln(format('  Y-AXIS::RATIO  = %12.5f', [fyratio  ]));

    writeln(format('  Z-AXIS::MIN    = %12.5f', [fzmin    ]));
    writeln(format('  Z-AXIS::MAX    = %12.5f', [fzmax    ]));
    writeln(format('  Z-AXIS::INC    = %12.5f', [fzinc    ]));
    writeln(format('  Z-AXIS::DELAY  = %12.5u', [fzdelay  ]));
    writeln(format('  Z-AXIS::DIR    = %12.5u', [fzdir    ]));
    writeln(format('  Z-AXIS::RADIUS = %12.5f', [fzradius ]));
    writeln(format('  Z-AXIS::RATIO  = %12.5f', [fzratio  ]));
  end;
end;

end.

