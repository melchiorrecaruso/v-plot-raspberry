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
{$define debug}

interface

uses
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils;

type
  tvpdriver = class
  private
    fcnt0:  longint;
    fcnt1:  longint;
    fcntz:  longint;
    fdelayms: longint;
    fenabled: boolean;
    ffault:   longint;
    procedure largedisplacements(cnt0, cnt1: longint);
    procedure smalldisplacements(cnt0, cnt1: longint);
  public
    constructor create(mode: longint);
    destructor  destroy; override;
    procedure   init (cnt0, cnt1, cntz: longint);
    procedure   move2(cnt0, cnt1, cntz: longint);
    procedure   move4(cnt0, cnt1, cntz: longint);
  published
    property delayms: longint read fdelayms write fdelayms;
    property enabled: boolean read fenabled write fenabled;
    property fault:   longint read ffault;
  end;


implementation

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
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_minvalue, motz_freq)); delay(500);
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq)); delay(500);
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq)); delay(500);
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
  fdelayms := 2000;
  fenabled := false;
end;

destructor tvpdriver.destroy;
begin
  inherited destroy;
end;

procedure  tvpdriver.init(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  fcnt0 := cnt0;
  fcnt1 := cnt1;
  fcntz := cntz;

  if fcntz < 0 then
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq))
  else
  if fcntz > 0 then
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_minvalue, motz_freq))
  else
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
  delay(500);
  {$endif}
end;

procedure tvpdriver.move2(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  move4(cnt0 - fcnt0, cnt1 - fcnt1, cntz - fcntz);
  {$endif}
end;

procedure tvpdriver.largedisplacements(cnt0, cnt1: longint);
begin
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

  // move step motor0
  cnt0 := abs(cnt0);
  while cnt0 > 0 do
  begin
    digitalwrite(mot0_step, HIGH);  delaymicroseconds(fdelayms);
    digitalwrite(mot0_step,  LOW);  delaymicroseconds(fdelayms);
    dec(cnt0);
  end;
  // move step motor1
  cnt1 := abs(cnt1);
  while cnt1 > 0 do
  begin
    digitalwrite(mot1_step, HIGH);  delaymicroseconds(fdelayms);
    digitalwrite(mot1_step,  LOW);  delaymicroseconds(fdelayms);
    dec(cnt1);
  end;
  {$endif}
end;

procedure tvpdriver.smalldisplacements(cnt0, cnt1: longint);
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
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

  cnt0 := abs(cnt0);
  cnt1 := abs(cnt1);
  for i := 0 to 18 do
  begin
    // move step motor0
    if vplotmatrix[cnt0, i] = 1 then
    begin
      digitalwrite(mot0_step, HIGH);  delaymicroseconds(fdelayms);
      digitalwrite(mot0_step,  LOW);  delaymicroseconds(fdelayms);
    end;
    // move step motor1
    if vplotmatrix[cnt1, i] = 1 then
    begin
      digitalwrite(mot1_step, HIGH);  delaymicroseconds(fdelayms);
      digitalwrite(mot1_step,  LOW);  delaymicroseconds(fdelayms);
    end;
  end;
  {$endif}
end;

procedure tvpdriver.move4(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  if not fenabled then exit;
  if ffault  = -1 then exit;
  // move pwm motz
  if fcntz <> min(1, max(-1, fcntz + cntz)) then
  begin
    fcntz := min(1, max(-1, fcntz + cntz));
    if fcntz < 0 then
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq))
    else
    if fcntz > 0 then
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_minvalue, motz_freq))
    else
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
    delay(500);
  end;
  // mode step motors
  if (cnt0 <> 0) or
     (cnt1 <> 0) then
  begin
    if (abs(cnt0) < 11) and
       (abs(cnt1) < 11) then
      smalldisplacements(cnt0, cnt1)
    else
      largedisplacements(cnt0, cnt1);
  end;
  {$ifdef debug}
  writeln('fcount0 = ', fcnt0);
  writeln('fcount1 = ', fcnt1);
  writeln('fcountz = ', fcntz);
  {$endif}
  {$endif}
end;

end.

