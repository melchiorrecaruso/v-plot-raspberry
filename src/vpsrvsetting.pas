{
  Description: vPlot server setting class.

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

unit vpsrvsetting;

{$mode objfpc}

interface

type
  tvpsrvsetting = class
  private
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

    fport:     longint;
 public
    constructor create;
    destructor  destroy; override;
    procedure   load(const filename: rawbytestring);
    procedure   clear;
 public
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

    property port:     longint   read fport;
 end;


implementation

uses
  inifiles, sysutils;

constructor tvpsrvsetting.create;
begin
  inherited create;
  clear;
end;

destructor tvpsrvsetting.destroy;
begin
  inherited destroy;
end;

procedure tvpsrvsetting.clear;
begin
  fxmin    := 0;  fymin    := 0;  fzmin    := 0;
  fxmax    := 0;  fymax    := 0;  fzmax    := 0;
  fxinc    := 0;  fyinc    := 0;  fzinc    := 0;
  fxdelay  := 0;  fydelay  := 0;  fzdelay  := 0;
  fxdir    := 0;  fydir    := 0;  fzdir    := 0;
  fxradius := 0;  fyradius := 0;  fzradius := 0;
  fxratio  := 0;  fyratio  := 0;  fzratio  := 0;

  fport    := 0;
end;

procedure tvpsrvsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';

  fxmin    := ini.readinteger('X-AXIS',  'MIN',    0);
  fxmax    := ini.readinteger('X-AXIS',  'MAX',    0);
  fxinc    := ini.readinteger('X-AXIS',  'INC',    0);
  fxdelay  := ini.readinteger('X-AXIS',  'DELAY',  0);
  fxdir    := ini.readinteger('X-AXIS',  'DIR',    0);
  fxradius := ini.readfloat  ('X-AXIS',  'RADIUS', 0);
  fxratio  := ini.readfloat  ('X-AXIS',  'RATIO',  0);

  fymin    := ini.readinteger('Y-AXIS',  'MIN',    0);
  fymax    := ini.readinteger('Y-AXIS',  'MAX',    0);
  fyinc    := ini.readinteger('Y-AXIS',  'INC',    0);
  fydelay  := ini.readinteger('Y-AXIS',  'DELAY',  0);
  fydir    := ini.readinteger('Y-AXIS',  'DIR',    0);
  fyradius := ini.readfloat  ('Y-AXIS',  'RADIUS', 0);
  fyratio  := ini.readfloat  ('Y-AXIS',  'RATIO',  0);

  fzmin    := ini.readinteger('Z-AXIS',  'MIN',    0);
  fzmax    := ini.readinteger('Z-AXIS',  'MAX',    0);
  fzinc    := ini.readinteger('Z-AXIS',  'INC',    0);
  fzdelay  := ini.readinteger('Z-AXIS',  'DELAY',  0);
  fzdir    := ini.readinteger('Z-AXIS',  'DIR',    0);
  fzradius := ini.readfloat  ('Z-AXIS',  'RADIUS', 0);

  fport    := ini.readinteger('NETWORK', 'PORT',   0);

  ini.destroy;

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

  writeln(format(' NETWORK::PORT   = %12.5u', [fport    ]));
end;

end.

