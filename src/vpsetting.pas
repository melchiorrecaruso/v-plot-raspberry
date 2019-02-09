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

    fsrvip:    string;
    fsrvport:  longint;
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

    property srvip:    string    read fsrvip;
    property srvport:  longint   read fsrvport;
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
begin        inherited destroy;
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
  fwavexmax   := 0;  fwaveymax   := 0;

  fsrvip   := '';
  fsrvport :=  0;
end;

procedure tvpsetting.load(const filename: rawbytestring);
var
  ini: tinifile;
begin
  ini := tinifile.create(filename);
  ini.formatsettings.decimalseparator := '.';

  flayout00.x  := ini.readfloat ('LAYOUT',  '00.X',  0);
  flayout00.y  := ini.readfloat ('LAYOUT',  '00.Y',  0);
  flayout01.x  := ini.readfloat ('LAYOUT',  '01.X',  0);
  flayout01.y  := ini.readfloat ('LAYOUT',  '01.Y',  0);
  flayout08.x  := ini.readfloat ('LAYOUT',  '08.X',  0);
  flayout08.y  := ini.readfloat ('LAYOUT',  '08.Y',  0);
  flayout09.x  := ini.readfloat ('LAYOUT',  '09.X',  0);
  flayout09.y  := ini.readfloat ('LAYOUT',  '09.Y',  0);

  fwave[0].x  := ini.readfloat  ('WAVE',    '00.X',   0);
  fwave[0].y  := ini.readfloat  ('WAVE',    '00.Y',   0);
  fwave[1].x  := ini.readfloat  ('WAVE',    '01.X',   0);
  fwave[1].y  := ini.readfloat  ('WAVE',    '01.Y',   0);
  fwave[2].x  := ini.readfloat  ('WAVE',    '02.X',   0);
  fwave[2].y  := ini.readfloat  ('WAVE',    '02.Y',   0);
  fwave[3].x  := ini.readfloat  ('WAVE',    '03.X',   0);
  fwave[3].y  := ini.readfloat  ('WAVE',    '03.Y',   0);
  fwave[4].x  := ini.readfloat  ('WAVE',    '04.X',   0);
  fwave[4].y  := ini.readfloat  ('WAVE',    '04.Y',   0);
  fwave[5].x  := ini.readfloat  ('WAVE',    '05.X',   0);
  fwave[5].y  := ini.readfloat  ('WAVE',    '05.Y',   0);
  fwave[6].x  := ini.readfloat  ('WAVE',    '06.X',   0);
  fwave[6].y  := ini.readfloat  ('WAVE',    '06.Y',   0);
  fwave[7].x  := ini.readfloat  ('WAVE',    '07.X',   0);
  fwave[7].y  := ini.readfloat  ('WAVE',    '07.Y',   0);
  fwave[8].x  := ini.readfloat  ('WAVE',    '08.X',   0);
  fwave[8].y  := ini.readfloat  ('WAVE',    '08.Y',   0);
  fwavexmax   := ini.readfloat  ('WAVE',    'XMAX',   0);
  fwaveymax   := ini.readfloat  ('WAVE',    'YMAX',   0);

  fsrvip      := ini.readstring ('NETWORK', 'IP',    '');
  fsrvport    := ini.readinteger('NETWORK', 'PORT',   0);

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
    writeln(format(' NETWORK::IP     = %s',     [fsrvip   ]));
    writeln(format(' NETWORK::PORT   = %12u',   [fsrvport] ));
  end;
  ini.destroy;
end;

end.

