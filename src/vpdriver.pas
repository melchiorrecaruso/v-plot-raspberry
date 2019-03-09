{
  Description: vPlot driver class.

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

unit vpdriver;

{$mode objfpc}

interface

uses
  classes, sysutils, {$ifdef cpuarm} pca9685, wiringpi, {$endif} vpsetting;

type
  tvpdriver = class
  private
    fxcount:  longint;
    fycount:  longint;
    fzcount:  longint;
    fxdelay:  longint;
    fydelay:  longint;
    fzdelay:  longint;
    fxoff:    boolean;
    fyoff:    boolean;
    fzoff:    boolean;
    procedure setxcount(value: longint);
    procedure setycount(value: longint);
    procedure setzcount(value: longint);
  public
    constructor create;
    destructor destroy; override;
    procedure init(axcount, aycount: longint);
    procedure move(axcount, aycount: longint);
  published
    property xcount: longint read fxcount write setxcount;
    property ycount: longint read fycount write setycount;
    property zcount: longint read fzcount write setzcount;
    property xdelay: longint read fxdelay write fxdelay;
    property ydelay: longint read fydelay write fydelay;
    property zdelay: longint read fzdelay write fzdelay;
    property xoff:   boolean read fxoff   write fxoff;
    property yoff:   boolean read fyoff   write fyoff;
    property zoff:   boolean read fzoff   write fzoff;
  end;

var
  driver:  tvpdriver = nil;

implementation

{$ifdef cpuarm}
const
  motx_on   = P37;
  moty_on   = P37;
  motx_step = P38;
  motx_dir  = P40;
  moty_step = P29;
  moty_dir  = P31;
  motz_freq = 50;
{$endif}

constructor tvpdriver.create;
begin
  inherited create;
  fxcount  := setting.xmin;
  fycount  := setting.ymin;
  fzcount  := setting.zmin;
  fxdelay  := setting.xdelay;
  fydelay  := setting.ydelay;
  fzdelay  := setting.zdelay;
  fxoff    := false;
  fyoff    := false;
  fzoff    := false;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  // enable motors
  pinmode(motx_on,     OUTPUT);
  digitalwrite(motx_on,   LOW);
  // init step motor0
  pinmode(motx_dir,    OUTPUT);
  pinmode(motx_step,   OUTPUT);
  digitalwrite(motx_dir,  LOW);
  digitalwrite(motx_step, LOW);
  // init step motor1
  pinmode(moty_dir,    OUTPUT);
  pinmode(moty_step,   OUTPUT);
  digitalwrite(moty_dir,  LOW);
  digitalwrite(moty_step, LOW);
  {$endif}
  setzcount(setting.zmax);
end;

destructor tvpdriver.destroy;
begin
  inherited destroy;
end;

procedure  tvpdriver.init(axcount, aycount: longint);
begin
  fxcount := axcount;
  fycount := aycount;
end;

procedure tvpdriver.move(axcount, aycount: longint);
{$ifdef cpuarm}
var
  dx: longint;
  dy: longint;
  bx: boolean;
  by: boolean;
{$endif}
begin
  {$ifdef cpuarm}
  dx := axcount - fxcount;
  if setting.xdir = 0 then
  begin
    if dx < 0 then
      digitalwrite(motx_dir,  LOW)
    else
      digitalwrite(motx_dir, HIGH);
  end else
  begin
    if dx < 0 then
      digitalwrite(motx_dir, HIGH)
    else
      digitalwrite(motx_dir,  LOW);
  end;

  dy := aycount - fycount;
  if setting.ydir = 0 then
  begin;
    if dy < 0 then
      digitalwrite(moty_dir,  LOW)
    else
      digitalwrite(moty_dir, HIGH);
  end else
  begin
    if dy < 0 then
      digitalwrite(moty_dir, HIGH)
    else
      digitalwrite(moty_dir,  LOW);
  end;

  dx := abs(dx);
  dy := abs(dy);
  bx := (dx > 0) and (fxoff = false);
  by := (dy > 0) and (fyoff = false);
  while (bx) or (by) do
  begin
    if bx then digitalwrite(motx_step, HIGH);
    if by then digitalwrite(moty_step, HIGH);
    delaymicroseconds(fxdelay);
    dec(dx);

    if bx then digitalwrite(motx_step,  LOW);
    if by then digitalwrite(moty_step,  LOW);
    delaymicroseconds(fydelay);
    dec(dy);

    bx := (dx > 0) and (fxoff = false);
    by := (dy > 0) and (fyoff = false);
  end;
  {$endif}
  fxcount := axcount;
  fycount := aycount;

//writeln(format('  DRIVER::CNT.X  = %12.5u', [fxcount]));
//writeln(format('  DRIVER::CNT.Y  = %12.5u', [fycount]));
end;

procedure tvpdriver.setxcount(value: longint);
begin
  move(value, fycount);
end;

procedure tvpdriver.setycount(value: longint);
begin
  move(fxcount, value);
end;

procedure tvpdriver.setzcount(value: longint);
begin
  if fzoff then exit;
  {$ifdef cpuarm}
  if fzcount > value then
  begin
    if setting.zdir = 0 then
      delaymicroseconds($f*fzdelay);
    while fzcount > value do
    begin
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
      delaymicroseconds(fzdelay);
      dec(fzcount, setting.zinc);
    end;
  end else
    if fzcount < value then
    begin
      if setting.zdir = 1 then
        delaymicroseconds($f*fzdelay);
      while fzcount < value do
      begin
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
        delaymicroseconds(fzdelay);
        inc(fzcount, setting.zinc);
      end;
    end;
  {$endif}
  fzcount := value;

//writeln(format('  DRIVER::CNT.Z  = %12.5u', [fzcount]));
end;

end.

