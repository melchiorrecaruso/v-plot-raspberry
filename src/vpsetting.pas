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
    flayout9:   tvppoint;
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
    // wave
    fwave0:     tvppoint;
    fwave1:     tvppoint;
    fwave2:     tvppoint;
    fwave3:     tvppoint;
    fwave4:     tvppoint;
    fwave5:     tvppoint;
    fwave6:     tvppoint;
    fwave7:     tvppoint;
    fwave8:     tvppoint;
    fwavedxmax: vpfloat;
    fwavedymax: vpfloat;
    fwaveoff:   longint;
 public
    constructor create;
    destructor  destroy; override;
    procedure   load(const filename: rawbytestring);
 public
    property layout0:   tvppoint read flayout0;
    property layout1:   tvppoint read flayout1;
    property layout8:   tvppoint read flayout8;
    property layout9:   tvppoint read flayout9;

    property xmin:      longint  read fxmin;
    property xmax:      longint  read fxmax;
    property xinc:      longint  read fxinc;
    property xdelay:    longint  read fxdelay;
    property xdir:      longint  read fxdir;
    property xradius:   vpfloat   read fxradius;
    property xratio:    vpfloat   read fxratio;

    property ymin:      longint  read fymin;
    property ymax:      longint  read fymax;
    property yinc:      longint  read fyinc;
    property ydelay:    longint  read fydelay;
    property ydir:      longint  read fydir;
    property yradius:   vpfloat   read fyradius;
    property yratio:    vpfloat   read fyratio;

    property zmin:      longint  read fzmin;
    property zmax:      longint  read fzmax;
    property zinc:      longint  read fzinc;
    property zdelay:    longint  read fzdelay;
    property zdir:      longint  read fzdir;
    property zradius:   vpfloat   read fzradius;
    property zratio:    vpfloat   read fzratio;

    property wave0:     tvppoint read fwave0;
    property wave1:     tvppoint read fwave1;
    property wave2:     tvppoint read fwave2;
    property wave3:     tvppoint read fwave3;
    property wave4:     tvppoint read fwave4;
    property wave5:     tvppoint read fwave5;
    property wave6:     tvppoint read fwave6;
    property wave7:     tvppoint read fwave7;
    property wave8:     tvppoint read fwave8;
    property wavedxmax: vpfloat   read fwavedxmax;
    property wavedymax: vpfloat   read fwavedymax;
    property waveoff:   longint  read fwaveoff;
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
  flayout9.x   := ini.readfloat   ('LAYOUT',  'L9.X',  0);
  flayout9.y   := ini.readfloat   ('LAYOUT',  'L9.Y',  0);

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

  fwave0.x     := ini.readfloat  ('WAVE',    'W0.X',   0);
  fwave0.y     := ini.readfloat  ('WAVE',    'W0.Y',   0);
  fwave1.x     := ini.readfloat  ('WAVE',    'W1.X',   0);
  fwave1.y     := ini.readfloat  ('WAVE',    'W1.Y',   0);
  fwave2.x     := ini.readfloat  ('WAVE',    'W2.X',   0);
  fwave2.y     := ini.readfloat  ('WAVE',    'W2.Y',   0);
  fwave3.x     := ini.readfloat  ('WAVE',    'W3.X',   0);
  fwave3.y     := ini.readfloat  ('WAVE',    'W3.Y',   0);
  fwave4.x     := ini.readfloat  ('WAVE',    'W4.X',   0);
  fwave4.y     := ini.readfloat  ('WAVE',    'W4.Y',   0);
  fwave5.x     := ini.readfloat  ('WAVE',    'W5.X',   0);
  fwave5.y     := ini.readfloat  ('WAVE',    'W5.Y',   0);
  fwave6.x     := ini.readfloat  ('WAVE',    'W6.X',   0);
  fwave6.y     := ini.readfloat  ('WAVE',    'W6.Y',   0);
  fwave7.x     := ini.readfloat  ('WAVE',    'W7.X',   0);
  fwave7.y     := ini.readfloat  ('WAVE',    'W7.Y',   0);
  fwave8.x     := ini.readfloat  ('WAVE',    'W8.X',   0);
  fwave8.y     := ini.readfloat  ('WAVE',    'W8.Y',   0);
  fwavedxmax   := ini.readfloat  ('WAVE',    'DXMAX',  0);
  fwavedymax   := ini.readfloat  ('WAVE',    'DYMAX',  0);
  fwaveoff     := ini.readinteger('WAVE',    'OFF',    0);

  if enabledebug then
  begin
    writeln(format('  LAYOUT::L0.X   = %12.5f  L0.Y = %12.5f', [flayout0.x, flayout0.y]));
    writeln(format('  LAYOUT::L1.X   = %12.5f  L1.Y = %12.5f', [flayout1.x, flayout1.y]));
    writeln(format('  LAYOUT::L8.X   = %12.5f  L8.Y = %12.5f', [flayout8.x, flayout8.y]));
    writeln(format('  LAYOUT::L9.X   = %12.5f  L9.Y = %12.5f', [flayout9.x, flayout9.y]));

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

    writeln(format('    WAVE::W0.X   = %12.5f  0.Y = %12.5f', [fwave0.x, fwave0.y]));
    writeln(format('    WAVE::W1.X   = %12.5f  1.Y = %12.5f', [fwave1.x, fwave1.y]));
    writeln(format('    WAVE::W2.X   = %12.5f  2.Y = %12.5f', [fwave2.x, fwave2.y]));
    writeln(format('    WAVE::W3.X   = %12.5f  3.Y = %12.5f', [fwave3.x, fwave3.y]));
    writeln(format('    WAVE::W4.X   = %12.5f  4.Y = %12.5f', [fwave4.x, fwave4.y]));
    writeln(format('    WAVE::W5.X   = %12.5f  5.Y = %12.5f', [fwave5.x, fwave5.y]));
    writeln(format('    WAVE::W6.X   = %12.5f  6.Y = %12.5f', [fwave6.x, fwave6.y]));
    writeln(format('    WAVE::W7.X   = %12.5f  7.Y = %12.5f', [fwave7.x, fwave7.y]));
    writeln(format('    WAVE::W8.X   = %12.5f  8.Y = %12.5f', [fwave8.x, fwave8.y]));
    writeln(format('    WAVE::DXMAX  = %12.5f', [fwavedxmax]));
    writeln(format('    WAVE::DYMAX  = %12.5f', [fwavedymax]));
    writeln(format('    WAVE::OFF    = %12.5u', [fwaveoff  ]));
  end;
  ini.destroy;
end;

end.

