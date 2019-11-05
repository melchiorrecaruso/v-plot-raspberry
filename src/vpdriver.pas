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
  classes, math, sysutils, {$ifdef cpuarm}
  pca9685, wiringpi, {$endif} vpmath, vpsetting;

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
  driver: tvpdriver = nil;
  driver_resolution: vpfloat;

implementation

{$ifdef cpuarm}

const
  vbr_on    = P7;
  motx_on   = P37;
  moty_on   = P37;
  motx_step = P38;
  motx_dir  = P40;
  moty_step = P29;
  moty_dir  = P31;
  motz_freq = 50;

const
  drivermatrix : array [0..10, 0..18] of longint = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  //  0
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),  //  1
    (0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),  //  2
    (0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0),  //  3
    (1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1),  //  4
    (1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1),  //  5
    (0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0),  //  6
    (1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1),  //  7
    (1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1),  //  8
    (0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),  //  9
    (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)); // 10

{$endif}

constructor tvpdriver.create;
begin
  inherited create;
  fxcount  := setting.m0min;
  fycount  := setting.m1min;
  fzcount  := setting.mzmin;
  fxdelay  := setting.m0delay;
  fydelay  := setting.m1delay;
  fzdelay  := setting.mzdelay;
  fxoff    := false;
  fyoff    := false;
  fzoff    := false;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  // enable sw420 vibration sensor
  pinmode      (vbr_on, INPUT);
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
  setzcount(setting.mzmax);
  // init driver resolution
  driver_resolution := ((setting.m0ratio+setting.m1ratio)/2)*6;
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
        i: longint;
  dx, ddx: longint;
  dy, ddy: longint;
{$endif}
begin
  {$ifdef cpuarm}
  dx := axcount - fxcount;
  if setting.m0dir = 0 then
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
  if setting.m1dir = 0 then
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

  // detect vibrations
  if fzcount = setting.mzmin then
  begin
    if (digitalread(vbr_on) > 0) then
    begin
      fxdelay := fxdelay+setting.m0inc;
      fydelay := fydelay+setting.m0inc;
    end else
    begin
      fxdelay := fxdelay-setting.m0dec;
      fydelay := fydelay-setting.m0dec;
    end;
    fxdelay := max(setting.m0min, min(fxdelay, setting.m0max));
    fydelay := max(setting.m1min, min(fydelay, setting.m1max));
  end else
  begin
    fxdelay := setting.m0min;
    fydelay := setting.m1min;
  end;

  dx := abs(dx);
  dy := abs(dy);
  while (dx > 0) or (dy > 0) do
  begin
    ddx := min(10, dx);
    ddy := min(10, dy);
    for i := 0 to 18 do
    begin

      if drivermatrix[ddx, i] = 1 then
      begin
        digitalwrite(motx_step, HIGH); delaymicroseconds(fxdelay);
        digitalwrite(motx_step,  LOW); delaymicroseconds(fxdelay);
      end;

      if drivermatrix[ddy, i] = 1 then
      begin
        digitalwrite(moty_step, HIGH); delaymicroseconds(fydelay);
        digitalwrite(moty_step,  LOW); delaymicroseconds(fydelay);
      end;
    end;
    dec(dx, ddx);
    dec(dy, ddy);
  end;
  {$endif}
  fxcount := axcount;
  fycount := aycount;
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
    if setting.mzdir = 0 then
      delaymicroseconds($f*fzdelay);
    while fzcount > value do
    begin
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
      delaymicroseconds(fzdelay);
      dec(fzcount, setting.mzinc);
    end;
  end else
    if fzcount < value then
    begin
      if setting.mzdir = 1 then
        delaymicroseconds($f*fzdelay);
      while fzcount < value do
      begin
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
        delaymicroseconds(fzdelay);
        inc(fzcount, setting.mzinc);
      end;
    end;
  {$endif}
  fzcount := value;
end;

end.

