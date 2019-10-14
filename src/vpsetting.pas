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
    // points
    fpoint0:   tvppoint;
    fpoint1:   tvppoint;
    fpoint8:   tvppoint;
    fyfactor:  vpfloat;
    fyoffset:  vpfloat;
    // 0-motor
    fm0min:    longint;
    fm0max:    longint;
    fm0inc:    longint;
    fm0delay:  longint;
    fm0dir:    longint;
    fm0radius: vpfloat;
    fm0ratio:  vpfloat;
    // 1-motor
    fm1min:    longint;
    fm1max:    longint;
    fm1inc:    longint;
    fm1delay:  longint;
    fm1dir:    longint;
    fm1radius: vpfloat;
    fm1ratio:  vpfloat;
    // z-motor
    fmzmin:    longint;
    fmzmax:    longint;
    fmzinc:    longint;
    fmzdelay:  longint;
    fmzdir:    longint;
    fmzradius: vpfloat;
    fmzratio:  vpfloat;
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
    fspacewavescale: vpfloat;
    fspacewaveoff:   longint;
 public
    constructor create;
    destructor destroy; override;
    procedure load(const filename: rawbytestring);
 public
    property point0:         tvppoint read fpoint0    write fpoint0;
    property point1:         tvppoint read fpoint1    write fpoint1;
    property point8:         tvppoint read fpoint8    write fpoint8;
    property yfactor:        vpfloat  read fyfactor   write fyfactor;
    property yoffset:        vpfloat  read fyoffset   write fyoffset;

    property m0min:          longint  read fm0min;
    property m0max:          longint  read fm0max;
    property m0inc:          longint  read fm0inc;
    property m0delay:        longint  read fm0delay;
    property m0dir:          longint  read fm0dir;
    property m0radius:       vpfloat  read fm0radius write fm0radius;
    property m0ratio:        vpfloat  read fm0ratio  write fm0ratio;

    property m1min:          longint  read fm1min;
    property m1max:          longint  read fm1max;
    property m1inc:          longint  read fm1inc;
    property m1delay:        longint  read fm1delay;
    property m1dir:          longint  read fm1dir;
    property m1radius:       vpfloat  read fm1radius write fm1radius;
    property m1ratio:        vpfloat  read fm1ratio  write fm1ratio;

    property mzmin:          longint  read fmzmin;
    property mzmax:          longint  read fmzmax;
    property mzinc:          longint  read fmzinc;
    property mzdelay:        longint  read fmzdelay;
    property mzdir:          longint  read fmzdir;
    property mzradius:       vpfloat  read fmzradius;
    property mzratio:        vpfloat  read fmzratio;

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
    property spacewavescale: vpfloat  read fspacewavescale;
    property spacewaveoff:   longint  read fspacewaveoff;
 end;

var
  setting:  tvpsetting = nil;

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

  fpoint0.x   := ini.readfloat   ('LAYOUT',  'L0.X',  0);
  fpoint0.y   := ini.readfloat   ('LAYOUT',  'L0.Y',  0);
  fpoint1.x   := ini.readfloat   ('LAYOUT',  'L1.X',  0);
  fpoint1.y   := ini.readfloat   ('LAYOUT',  'L1.Y',  0);
  fpoint8.x   := ini.readfloat   ('LAYOUT',  'L8.X',  0);
  fpoint8.y   := ini.readfloat   ('LAYOUT',  'L8.Y',  0);
  fyfactor    := ini.readfloat   ('LAYOUT',  'LY-1',  0);
  fyoffset    := ini.readfloat   ('LAYOUT',  'LY-2',  0);

  fm0min        := ini.readinteger('X-AXIS',  'MIN',    0);
  fm0max        := ini.readinteger('X-AXIS',  'MAX',    0);
  fm0inc        := ini.readinteger('X-AXIS',  'INC',    0);
  fm0delay      := ini.readinteger('X-AXIS',  'DELAY',  0);
  fm0dir        := ini.readinteger('X-AXIS',  'DIR',    0);
  fm0radius     := ini.readfloat  ('X-AXIS',  'RADIUS', 0);
  fm0ratio      := ini.readfloat  ('X-AXIS',  'RATIO',  0);

  fm1min        := ini.readinteger('Y-AXIS',  'MIN',    0);
  fm1max        := ini.readinteger('Y-AXIS',  'MAX',    0);
  fm1inc        := ini.readinteger('Y-AXIS',  'INC',    0);
  fm1delay      := ini.readinteger('Y-AXIS',  'DELAY',  0);
  fm1dir        := ini.readinteger('Y-AXIS',  'DIR',    0);
  fm1radius     := ini.readfloat  ('Y-AXIS',  'RADIUS', 0);
  fm1ratio      := ini.readfloat  ('Y-AXIS',  'RATIO',  0);

  fmzmin        := ini.readinteger('Z-AXIS',  'MIN',    0);
  fmzmax        := ini.readinteger('Z-AXIS',  'MAX',    0);
  fmzinc        := ini.readinteger('Z-AXIS',  'INC',    0);
  fmzdelay      := ini.readinteger('Z-AXIS',  'DELAY',  0);
  fmzdir        := ini.readinteger('Z-AXIS',  'DIR',    0);
  fmzradius     := ini.readfloat  ('Z-AXIS',  'RADIUS', 0);

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
  fspacewavescale   := ini.readfloat  ('SPACE-WAVE',   'SCALE',  0);
  fspacewaveoff     := ini.readinteger('SPACE-WAVE',   'OFF',    0);

  if enabledebug then
  begin
    writeln(format('  LAYOUT::L0.X   = %12.5f  L0.Y = %12.5f', [fpoint0.x, fpoint0.y]));
    writeln(format('  LAYOUT::L1.X   = %12.5f  L1.Y = %12.5f', [fpoint1.x, fpoint1.y]));
    writeln(format('  LAYOUT::L8.X   = %12.5f  L8.Y = %12.5f', [fpoint8.x, fpoint8.y]));
    writeln(format('  LAYOUT::LY-1   = %12.5f', [fyfactor]));
    writeln(format('  LAYOUT::LY-2   = %12.5f', [fyoffset]));

    writeln(format('  X-AXIS::MIN    = %12.5u', [fm0min    ]));
    writeln(format('  X-AXIS::MAX    = %12.5u', [fm0max    ]));
    writeln(format('  X-AXIS::INC    = %12.5u', [fm0inc    ]));
    writeln(format('  X-AXIS::DELAY  = %12.5u', [fm0delay  ]));
    writeln(format('  X-AXIS::DIR    = %12.5u', [fm0dir    ]));
    writeln(format('  X-AXIS::RADIUS = %12.5f', [fm0radius ]));
    writeln(format('  X-AXIS::RATIO  = %12.5f', [fm0ratio  ]));

    writeln(format('  Y-AXIS::MIN    = %12.5u', [fm1min    ]));
    writeln(format('  Y-AXIS::MAX    = %12.5u', [fm1max    ]));
    writeln(format('  Y-AXIS::INC    = %12.5u', [fm1inc    ]));
    writeln(format('  Y-AXIS::DELAY  = %12.5u', [fm1delay  ]));
    writeln(format('  Y-AXIS::DIR    = %12.5u', [fm1dir    ]));
    writeln(format('  Y-AXIS::RADIUS = %12.5f', [fm1radius ]));
    writeln(format('  Y-AXIS::RATIO  = %12.5f', [fm1ratio  ]));

    writeln(format('  Z-AXIS::MIN    = %12.5u', [fmzmin    ]));
    writeln(format('  Z-AXIS::MAX    = %12.5u', [fmzmax    ]));
    writeln(format('  Z-AXIS::INC    = %12.5u', [fmzinc    ]));
    writeln(format('  Z-AXIS::DELAY  = %12.5u', [fmzdelay  ]));
    writeln(format('  Z-AXIS::DIR    = %12.5u', [fmzdir    ]));
    writeln(format('  Z-AXIS::RADIUS = %12.5f', [fmzradius ]));
    writeln(format('  Z-AXIS::RATIO  = %12.5f', [fmzratio  ]));

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
    writeln(format(' SPACE-W::SCALE  = %12.5f',                [fspacewavescale]));
    writeln(format(' SPACE-W::OFF    = %12.5u',                [fspacewaveoff  ]));
  end;
  ini.destroy;
end;

end.

