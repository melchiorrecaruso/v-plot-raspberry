{
  Description: vPlot driver library.

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

unit vpdriver;

{$mode objfpc}
{$i include.inc}

interface

uses
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils;

type
  tvpdriver = class
  private
    fcnt0:    longint;
    fcnt1:    longint;
    fdelay1:  longint;
    fdelay2:  longint;
    fdelay3:  longint;
    ffault:   longint;
    fenabled: boolean;
    fpen:     boolean;
    fpenoff:  boolean;
    procedure setpen(value: boolean);
    procedure setpenoff(value: boolean);
    procedure largedisplacements(cnt0, cnt1: longint);
    procedure smalldisplacements(cnt0, cnt1: longint);
  public
    constructor create(mode: longint);
    destructor  destroy; override;
    procedure   init (cnt0, cnt1: longint);
    procedure   move2(cnt0, cnt1: longint);
    procedure   move4(cnt0, cnt1: longint);

  published
    property cnt0:    longint read fcnt0;
    property cnt1:    longint read fcnt1;
    property delay1:  longint read fdelay1 write fdelay1;
    property delay2:  longint read fdelay2 write fdelay2;
    property delay3:  longint read fdelay3 write fdelay3;
    property fault:   longint read ffault;
    property enabled: boolean read fenabled write fenabled;
    property pen:     boolean read fpen     write setpen;
    property penoff:  boolean read fpenoff  write setpenoff;
  end;


implementation


{$ifdef cpuarm}
const
  mot0_step     = P38;
  mot0_dir      = P40;
  mot1_step     = P29;
  mot1_dir      = P31;

  motx_mod0     = P15;
  motx_mod1     = P13;
  motx_mod2     = P11;

  motz_maxvalue = 2.50;
  motz_minvalue = 0.50;
  motz_rstvalue = 1.50;
  motz_incvalue = 0.10;
  motz_freq     = 50;

  vplotmatrix : array [0..10, 0..18] of longint = (
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

constructor tvpdriver.create(mode: longint);
begin
  inherited create;
  {$ifdef cpuarm}
  // setup wiringpi library
  ffault := wiringpisetup;
  // setup pca9685 library
  if ffault <> -1 then
    ffault := pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);

  if ffault <> -1 then
  begin
    // init servo
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
    delaymicroseconds(fdelay3);
    // init mode
    pinmode(motx_mod0,    OUTPUT);
    pinmode(motx_mod1,    OUTPUT);
    pinmode(motx_mod2,    OUTPUT);

    if mode = 1 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 2 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 4 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 8 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2, HIGH);
    end else
    if mode = 16 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 32 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2, HIGH);
    end else
    if mode = 64 then
      begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 128 then
      begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2, HIGH);
    end else
      ffault := -1;

    // init step motor0
    pinmode(mot0_dir,    OUTPUT);
    pinmode(mot0_step,   OUTPUT);
    digitalwrite(mot0_dir,  LOW);
    digitalwrite(mot0_step, LOW);
    // init step motor1
    pinmode(mot1_dir,    OUTPUT);
    pinmode(mot1_step,   OUTPUT);
    digitalwrite(mot1_dir,  LOW);
    digitalwrite(mot1_step, LOW);
  end;
  {$else}
  ffault   := -1;
  {$endif}
  fdelay1  := 1500;
  fdelay2  := 3500;
  fdelay3  := 400000;
  fenabled := false;
  fpen     := false;
  fpenoff  := false;
end;

destructor tvpdriver.destroy;
begin
  inherited destroy;
end;

procedure  tvpdriver.init(cnt0, cnt1: longint);
begin
  fcnt0 := cnt0;
  fcnt1 := cnt1;
end;

procedure tvpdriver.largedisplacements(cnt0, cnt1: longint);
begin
  setpen(false);
  {$ifdef cpuarm}
  inc(fcnt0, cnt0);
  inc(fcnt1, cnt1);

  if cnt0 > 0 then
    digitalwrite(mot0_dir, HIGH)
  else
    digitalwrite(mot0_dir,  LOW);

  if cnt1 > 0 then
    digitalwrite(mot1_dir,  LOW)
  else
    digitalwrite(mot1_dir, HIGH);

  // move step motor0 and motor1
  cnt0 := abs(cnt0);
  cnt1 := abs(cnt1);
  repeat
    if cnt0 > 0 then
    begin
      digitalwrite(mot0_step, HIGH); delaymicroseconds(fdelay1);
      digitalwrite(mot0_step,  LOW); delaymicroseconds(fdelay1);
      dec(cnt0);
    end;

    if cnt1 > 0 then
    begin
      digitalwrite(mot1_step, HIGH); delaymicroseconds(fdelay1);
      digitalwrite(mot1_step,  LOW); delaymicroseconds(fdelay1);
      dec(cnt1);
    end;

  until (cnt0 = 0) and (cnt1 = 0);
  {$endif}
end;

procedure tvpdriver.smalldisplacements(cnt0, cnt1: longint);
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
  setpen(true);
  {$ifdef cpuarm}
  inc(fcnt0, cnt0);
  inc(fcnt1, cnt1);

  if cnt0 > 0 then
    digitalwrite(mot0_dir, HIGH)
  else
    digitalwrite(mot0_dir,  LOW);

  if cnt1 > 0 then
    digitalwrite(mot1_dir,  LOW)
  else
    digitalwrite(mot1_dir, HIGH);

  // move step motor0 and motor1
  cnt0 := abs(cnt0);
  cnt1 := abs(cnt1);
  for i := 0 to 18 do
  begin
    if vplotmatrix[cnt0, i] = 1 then
    begin
      digitalwrite(mot0_step, HIGH); delaymicroseconds(fdelay2);
      digitalwrite(mot0_step,  LOW); delaymicroseconds(fdelay2);
    end;

    if vplotmatrix[cnt1, i] = 1 then
    begin
      digitalwrite(mot1_step, HIGH); delaymicroseconds(fdelay2);
      digitalwrite(mot1_step,  LOW); delaymicroseconds(fdelay2);
    end;
  end;
  {$endif}
end;

procedure tvpdriver.setpen(value: boolean);
begin
  if fpenoff = false then
    if fpen <> value then
    begin
      fpen := value;
      {$ifdef cpuarm}
      if fpen then
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq))
      else
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
      delaymicroseconds(fdelay3);
      {$endif}
    end;
end;

procedure tvpdriver.setpenoff(value: boolean);
begin
  fpenoff := value;
  if fpenoff then
    if fpen then
    begin
      fpen := false;
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
      delaymicroseconds(fdelay3);
      {$endif}
    end;
end;

procedure tvpdriver.move2(cnt0, cnt1: longint);
begin
  {$ifdef cpuarm}
  move4(cnt0 - fcnt0, cnt1 - fcnt1);
  {$endif}
end;

procedure tvpdriver.move4(cnt0, cnt1: longint);
begin
  if not fenabled then exit;
  if ffault  = -1 then exit;
  {$ifdef cpuarm}
  // mode step motors
  if (cnt0 <> 0) or
     (cnt1 <> 0) then
  begin
    if (not fpenoff) then
    begin
      if (abs(cnt0) < 11) and
         (abs(cnt1) < 11) then
        smalldisplacements(cnt0, cnt1)
      else
        largedisplacements(cnt0, cnt1);

    end else
      largedisplacements(cnt0, cnt1);
  end;
  {$ifdef debug}
  writeln('fcount0 = ', fcnt0);
  writeln('fcount1 = ', fcnt1);
  {$endif}
  {$endif}
end;

end.

