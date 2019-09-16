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
  inifiles, sysutils, vpmath;

type
  tvpsetting = class
  private
    // layout
    flayout0:   tvppoint;
    flayout1:   tvppoint;
    flayout8:   tvppoint;
    flayoutx:   vpfloat;
    // x-axis
    fxmin:      longint;
    fxmax:      longint;
    fxinc:      longint;
    fxdelay:    longint;
    fxdir:      longint;
    fxradius:   vpfloat;
    fxratio:    vpfloat;
    // y-axis
    fymin:      longint;
    fymax:      longint;
    fyinc:      longint;
    fydelay:    longint;
    fydir:      longint;
    fyradius:   vpfloat;
    fyratio:    vpfloat;
    // z-axis
    fzmin:      longint;
    fzmax:      longint;
    fzinc:      longint;
    fzdelay:    longint;
    fzdir:      longint;
    fzradius:   vpfloat;
    fzratio:    vpfloat;
    // space wave
    fspacewave0:     tvppoint;
    fspacewave1:     tvppoint;
    fspacewave2:     tvppoint;
    fspacewave3:     tvppoint;
    fspacewave4:     tvppoint;
    fspacewave5:     tvppoint;
    fspacewave6:     tvppoint;
    fspacewave7:     tvppoint;
    fspacewave8:     tvppoint;
    fspacewavedxmax: vpfloat;
    fspacewavedymax: vpfloat;
    fspacewaveoff:   longint;
 public
    constructor create;
    destructor  destroy; override;
    procedure   load(const filename: rawbytestring);
 public
    property layout0:        tvppoint read flayout0;
    property layout1:        tvppoint read flayout1;
    property layout8:        tvppoint read flayout8;
    property layoutx:        vpfloat  read flayoutx;

    property xmin:           longint  read fxmin;
    property xmax:           longint  read fxmax;
    property xinc:           longint  read fxinc;
    property xdelay:         longint  read fxdelay;
    property xdir:           longint  read fxdir;
    property xradius:        vpfloat  read fxradius;
    property xratio:         vpfloat  read fxratio;

    property ymin:           longint  read fymin;
    property ymax:           longint  read fymax;
    property yinc:           longint  read fyinc;
    property ydelay:         longint  read fydelay;
    property ydir:           longint  read fydir;
    property yradius:        vpfloat  read fyradius;
    property yratio:         vpfloat  read fyratio;

    property zmin:           longint  read fzmin;
    property zmax:           longint  read fzmax;
    property zinc:           longint  read fzinc;
    property zdelay:         longint  read fzdelay;
    property zdir:           longint  read fzdir;
    property zradius:        vpfloat  read fzradius;
    property zratio:         vpfloat  read fzratio;

    property spacewave0:     tvppoint read fspacewave0;
    property spacewave1:     tvppoint read fspacewave1;
    property spacewave2:     tvppoint read fspacewave2;
    property spacewave3:     tvppoint read fspacewave3;
    property spacewave4:     tvppoint read fspacewave4;
    property spacewave5:     tvppoint read fspacewave5;
    property spacewave6:     tvppoint read fspacewave6;
    property spacewave7:     tvppoint read fspacewave7;
    property spacewave8:     tvppoint read fspacewave8;
    property spacewavedxmax: vpfloat  read fspacewavedxmax;
    property spacewavedymax: vpfloat  read fspacewavedymax;
    property spacewaveoff:   longint  read fspacewaveoff;
 end;

var
  setting: tvpsetting = nil;

implementation

constructor tvpsetting.create;
begin
  inherited create;
end;

destructor tvpsetting.destroy;
begin
  inherited destroy;
end;

procedure tvpsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';

  flayout0.x   := ini.readfloat   ('LAYOUT',  'L0.X',  0);
  flayout0.y   := ini.readfloat   ('LAYOUT',  'L0.Y',  0);
  flayout1.x   := ini.readfloat   ('LAYOUT',  'L1.X',  0);
  flayout1.y   := ini.readfloat   ('LAYOUT',  'L1.Y',  0);
  flayout8.x   := ini.readfloat   ('LAYOUT',  'L8.X',  0);
  flayout8.y   := ini.readfloat   ('LAYOUT',  'L8.Y',  0);
  flayoutx     := ini.readfloat   ('LAYOUT',  'LX',    0);

  fxmin        := ini.readinteger('X-AXIS',  'MIN',    0);
  fxmax        := ini.readinteger('X-AXIS',  'MAX',    0);
  fxinc        := ini.readinteger('X-AXIS',  'INC',    0);
  fxdelay      := ini.readinteger('X-AXIS',  'DELAY',  0);
  fxdir        := ini.readinteger('X-AXIS',  'DIR',    0);
  fxradius     := ini.readfloat  ('X-AXIS',  'RADIUS', 0);
  fxratio      := ini.readfloat  ('X-AXIS',  'RATIO',  0);

  fymin        := ini.readinteger('Y-AXIS',  'MIN',    0);
  fymax        := ini.readinteger('Y-AXIS',  'MAX',    0);
  fyinc        := ini.readinteger('Y-AXIS',  'INC',    0);
  fydelay      := ini.readinteger('Y-AXIS',  'DELAY',  0);
  fydir        := ini.readinteger('Y-AXIS',  'DIR',    0);
  fyradius     := ini.readfloat  ('Y-AXIS',  'RADIUS', 0);
  fyratio      := ini.readfloat  ('Y-AXIS',  'RATIO',  0);

  fzmin        := ini.readinteger('Z-AXIS',  'MIN',    0);
  fzmax        := ini.readinteger('Z-AXIS',  'MAX',    0);
  fzinc        := ini.readinteger('Z-AXIS',  'INC',    0);
  fzdelay      := ini.readinteger('Z-AXIS',  'DELAY',  0);
  fzdir        := ini.readinteger('Z-AXIS',  'DIR',    0);
  fzradius     := ini.readfloat  ('Z-AXIS',  'RADIUS', 0);

  fspacewave0.x     := ini.readfloat  ('SPACE-WAVE',   '00.X',   0);
  fspacewave0.y     := ini.readfloat  ('SPACE-WAVE',   '00.Y',   0);
  fspacewave1.x     := ini.readfloat  ('SPACE-WAVE',   '01.X',   0);
  fspacewave1.y     := ini.readfloat  ('SPACE-WAVE',   '01.Y',   0);
  fspacewave2.x     := ini.readfloat  ('SPACE-WAVE',   '02.X',   0);
  fspacewave2.y     := ini.readfloat  ('SPACE-WAVE',   '02.Y',   0);
  fspacewave3.x     := ini.readfloat  ('SPACE-WAVE',   '03.X',   0);
  fspacewave3.y     := ini.readfloat  ('SPACE-WAVE',   '03.Y',   0);
  fspacewave4.x     := ini.readfloat  ('SPACE-WAVE',   '04.X',   0);
  fspacewave4.y     := ini.readfloat  ('SPACE-WAVE',   '04.Y',   0);
  fspacewave5.x     := ini.readfloat  ('SPACE-WAVE',   '05.X',   0);
  fspacewave5.y     := ini.readfloat  ('SPACE-WAVE',   '05.Y',   0);
  fspacewave6.x     := ini.readfloat  ('SPACE-WAVE',   '06.X',   0);
  fspacewave6.y     := ini.readfloat  ('SPACE-WAVE',   '06.Y',   0);
  fspacewave7.x     := ini.readfloat  ('SPACE-WAVE',   '07.X',   0);
  fspacewave7.y     := ini.readfloat  ('SPACE-WAVE',   '07.Y',   0);
  fspacewave8.x     := ini.readfloat  ('SPACE-WAVE',   '08.X',   0);
  fspacewave8.y     := ini.readfloat  ('SPACE-WAVE',   '08.Y',   0);
  fspacewavedxmax   := ini.readfloat  ('SPACE-WAVE',   'DXMAX',  0);
  fspacewavedymax   := ini.readfloat  ('SPACE-WAVE',   'DYMAX',  0);
  fspacewaveoff     := ini.readinteger('SPACE-WAVE',   'OFF',    0);

  if enabledebug then
  begin
    writeln(format('  LAYOUT::L0.X   = %12.5f  L0.Y = %12.5f', [flayout0.x, flayout0.y]));
    writeln(format('  LAYOUT::L1.X   = %12.5f  L1.Y = %12.5f', [flayout1.x, flayout1.y]));
    writeln(format('  LAYOUT::L8.X   = %12.5f  L8.Y = %12.5f', [flayout8.x, flayout8.y]));
    writeln(format('  LAYOUT::LX     = %12.5f', [flayoutx ]));

    writeln(format('  X-AXIS::MIN    = %12.5u', [fxmin    ]));
    writeln(format('  X-AXIS::MAX    = %12.5u', [fxmax    ]));
    writeln(format('  X-AXIS::INC    = %12.5u', [fxinc    ]));
    writeln(format('  X-AXIS::DELAY  = %12.5u', [fxdelay  ]));
    writeln(format('  X-AXIS::DIR    = %12.5u', [fxdir    ]));
    writeln(format('  X-AXIS::RADIUS = %12.5f', [fxradius ]));
    writeln(format('  X-AXIS::RATIO  = %12.5f', [fxratio  ]));

    writeln(format('  Y-AXIS::MIN    = %12.5u', [fymin    ]));
    writeln(format('  Y-AXIS::MAX    = %12.5u', [fymax    ]));
    writeln(format('  Y-AXIS::INC    = %12.5u', [fyinc    ]));
    writeln(format('  Y-AXIS::DELAY  = %12.5u', [fydelay  ]));
    writeln(format('  Y-AXIS::DIR    = %12.5u', [fydir    ]));
    writeln(format('  Y-AXIS::RADIUS = %12.5f', [fyradius ]));
    writeln(format('  Y-AXIS::RATIO  = %12.5f', [fyratio  ]));

    writeln(format('  Z-AXIS::MIN    = %12.5u', [fzmin    ]));
    writeln(format('  Z-AXIS::MAX    = %12.5u', [fzmax    ]));
    writeln(format('  Z-AXIS::INC    = %12.5u', [fzinc    ]));
    writeln(format('  Z-AXIS::DELAY  = %12.5u', [fzdelay  ]));
    writeln(format('  Z-AXIS::DIR    = %12.5u', [fzdir    ]));
    writeln(format('  Z-AXIS::RADIUS = %12.5f', [fzradius ]));
    writeln(format('  Z-AXIS::RATIO  = %12.5f', [fzratio  ]));

    writeln(format(' SPACE-W::00.X   = %12.5f  00.Y = %12.5f', [fspacewave0.x, fspacewave0.y]));
    writeln(format(' SPACE-W::01.X   = %12.5f  01.Y = %12.5f', [fspacewave1.x, fspacewave1.y]));
    writeln(format(' SPACE-W::02.X   = %12.5f  02.Y = %12.5f', [fspacewave2.x, fspacewave2.y]));
    writeln(format(' SPACE-W::03.X   = %12.5f  03.Y = %12.5f', [fspacewave3.x, fspacewave3.y]));
    writeln(format(' SPACE-W::04.X   = %12.5f  04.Y = %12.5f', [fspacewave4.x, fspacewave4.y]));
    writeln(format(' SPACE-W::05.X   = %12.5f  05.Y = %12.5f', [fspacewave5.x, fspacewave5.y]));
    writeln(format(' SPACE-W::06.X   = %12.5f  06.Y = %12.5f', [fspacewave6.x, fspacewave6.y]));
    writeln(format(' SPACE-W::07.X   = %12.5f  07.Y = %12.5f', [fspacewave7.x, fspacewave7.y]));
    writeln(format(' SPACE-W::08.X   = %12.5f  08.Y = %12.5f', [fspacewave8.x, fspacewave8.y]));
    writeln(format(' SPACE-W::DXMAX  = %12.5f',                [fspacewavedxmax]));
    writeln(format(' SPACE-W::DYMAX  = %12.5f',                [fspacewavedymax]));
    writeln(format(' SPACE-W::OFF    = %12.5u',                [fspacewaveoff  ]));
  end;
  ini.destroy;
end;

end.

