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
    flayout0:  tvppoint;
    flayout1:  tvppoint;
    flayout8:  tvppoint;
    flayout9:  tvppoint;
    // x-axis
    fxmin:     longint;
    fxmax:     longint;
    fxinc:     longint;
    fxdelay:   longint;
    fxdir:     longint;
    fxradius:  single;
    fxratio:   single;
    // y-axis
    fymin:     longint;
    fymax:     longint;
    fyinc:     longint;
    fydelay:   longint;
    fydir:     longint;
    fyradius:  single;
    fyratio:   single;
    // z-axis
    fzmin:     longint;
    fzmax:     longint;
    fzinc:     longint;
    fzdelay:   longint;
    fzdir:     longint;
    fzradius:  single;
    fzratio:   single;
    // wave
    fwave0:    tvppoint;
    fwave1:    tvppoint;
    fwave2:    tvppoint;
    fwave3:    tvppoint;
    fwave4:    tvppoint;
    fwave5:    tvppoint;
    fwave6:    tvppoint;
    fwave7:    tvppoint;
    fwave8:    tvppoint;
    fwavexmax: single;
    fwaveymax: single;
    // network
    fip:       string;
    fport:     longint;
 public
    constructor create;
    destructor  destroy; override;
    procedure   load(const filename: rawbytestring);
 public
    property layout0: tvppoint read flayout0;
    property layout1: tvppoint read flayout1;
    property layout8: tvppoint read flayout8;
    property layout9: tvppoint read flayout9;

    property xmin:     longint read fxmin;
    property xmax:     longint read fxmax;
    property xinc:     longint read fxinc;
    property xdelay:   longint read fxdelay;
    property xdir:     longint read fxdir;
    property xradius:  single  read fxradius;
    property xratio:   single  read fxratio;

    property ymin:     longint read fymin;
    property ymax:     longint read fymax;
    property yinc:     longint read fyinc;
    property ydelay:   longint read fydelay;
    property ydir:     longint read fydir;
    property yradius:  single  read fyradius;
    property yratio:   single  read fyratio;

    property zmin:     longint read fzmin;
    property zmax:     longint read fzmax;
    property zinc:     longint read fzinc;
    property zdelay:   longint read fzdelay;
    property zdir:     longint read fzdir;
    property zradius:  single  read fzradius;
    property zratio:   single  read fzratio;

    property wave0:   tvppoint read fwave0;
    property wave1:   tvppoint read fwave1;
    property wave2:   tvppoint read fwave2;
    property wave3:   tvppoint read fwave3;
    property wave4:   tvppoint read fwave4;
    property wave5:   tvppoint read fwave5;
    property wave6:   tvppoint read fwave6;
    property wave7:   tvppoint read fwave7;
    property wave8:   tvppoint read fwave8;
    property wavexmax: single  read fwavexmax;
    property waveymax: single  read fwaveymax;

    property ip:       string  read fip;
    property port:     longint read fport;
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

  flayout0.x   := ini.readfloat   ('LAYOUT',  '0.X',   0);
  flayout0.y   := ini.readfloat   ('LAYOUT',  '0.Y',   0);
  flayout1.x   := ini.readfloat   ('LAYOUT',  '1.X',   0);
  flayout1.y   := ini.readfloat   ('LAYOUT',  '1.Y',   0);
  flayout8.x   := ini.readfloat   ('LAYOUT',  '8.X',   0);
  flayout8.y   := ini.readfloat   ('LAYOUT',  '8.Y',   0);
  flayout9.x   := ini.readfloat   ('LAYOUT',  '9.X',   0);
  flayout9.y   := ini.readfloat   ('LAYOUT',  '9.Y',   0);

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

  fwave0.x     := ini.readfloat  ('WAVE',    '0.X',    0);
  fwave0.y     := ini.readfloat  ('WAVE',    '0.Y',    0);
  fwave1.x     := ini.readfloat  ('WAVE',    '1.X',    0);
  fwave1.y     := ini.readfloat  ('WAVE',    '1.Y',    0);
  fwave2.x     := ini.readfloat  ('WAVE',    '2.X',    0);
  fwave2.y     := ini.readfloat  ('WAVE',    '2.Y',    0);
  fwave3.x     := ini.readfloat  ('WAVE',    '3.X',    0);
  fwave3.y     := ini.readfloat  ('WAVE',    '3.Y',    0);
  fwave4.x     := ini.readfloat  ('WAVE',    '4.X',    0);
  fwave4.y     := ini.readfloat  ('WAVE',    '4.Y',    0);
  fwave5.x     := ini.readfloat  ('WAVE',    '5.X',    0);
  fwave5.y     := ini.readfloat  ('WAVE',    '5.Y',    0);
  fwave6.x     := ini.readfloat  ('WAVE',    '6.X',    0);
  fwave6.y     := ini.readfloat  ('WAVE',    '6.Y',    0);
  fwave7.x     := ini.readfloat  ('WAVE',    '7.X',    0);
  fwave7.y     := ini.readfloat  ('WAVE',    '7.Y',    0);
  fwave8.x     := ini.readfloat  ('WAVE',    '8.X',    0);
  fwave8.y     := ini.readfloat  ('WAVE',    '8.Y',    0);
  fwavexmax    := ini.readfloat  ('WAVE',    'XMAX',   0);
  fwaveymax    := ini.readfloat  ('WAVE',    'YMAX',   0);

  fip          := ini.readstring ('NETWORK', 'IP',    '');
  fport        := ini.readinteger('NETWORK', 'PORT',   0);

  if enabledebug then
  begin
    writeln(format('  LAYOUT::0.X    = %12.5f  00.Y = %12.5f', [flayout0.x, flayout0.y]));
    writeln(format('  LAYOUT::1.X    = %12.5f  01.Y = %12.5f', [flayout1.x, flayout1.y]));
    writeln(format('  LAYOUT::8.X    = %12.5f  08.Y = %12.5f', [flayout8.x, flayout8.y]));
    writeln(format('  LAYOUT::9.X    = %12.5f  09.Y = %12.5f', [flayout9.x, flayout9.y]));

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

    writeln(format('    WAVE::0.X    = %12.5f  0.Y = %12.5f', [fwave0.x, fwave0.y]));
    writeln(format('    WAVE::1.X    = %12.5f  1.Y = %12.5f', [fwave1.x, fwave1.y]));
    writeln(format('    WAVE::2.X    = %12.5f  2.Y = %12.5f', [fwave2.x, fwave2.y]));
    writeln(format('    WAVE::3.X    = %12.5f  3.Y = %12.5f', [fwave3.x, fwave3.y]));
    writeln(format('    WAVE::4.X    = %12.5f  4.Y = %12.5f', [fwave4.x, fwave4.y]));
    writeln(format('    WAVE::5.X    = %12.5f  5.Y = %12.5f', [fwave5.x, fwave5.y]));
    writeln(format('    WAVE::6.X    = %12.5f  6.Y = %12.5f', [fwave6.x, fwave6.y]));
    writeln(format('    WAVE::7.X    = %12.5f  7.Y = %12.5f', [fwave7.x, fwave7.y]));
    writeln(format('    WAVE::8.X    = %12.5f  8.Y = %12.5f', [fwave8.x, fwave8.y]));
    writeln(format('    WAVE::XMAX   = %12.5f', [fwavexmax]));
    writeln(format('    WAVE::YMAX   = %12.5f', [fwaveymax]));

    writeln(format(' NETWORK::IP     = %s',     [fip  ]));
    writeln(format(' NETWORK::PORT   = %12u',   [fport]));
  end;
  ini.destroy;
end;

end.

