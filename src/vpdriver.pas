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

interface

uses
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils, vpcommon;

type
  tvpdriver = class
  private
    fcount0:     longint;
    fcount1:     longint;
    fclockwise0: boolean;
    fclockwise1: boolean;
    fdelay0:     longint;
    fdelay1:     longint;
    fenabled:    boolean;
    fmode:       longint;
    fpen:        boolean;
    fpenoff:     boolean;
    procedure setmode(value: longint);
    procedure setpen(value: boolean);
    procedure setpenoff(value: boolean);
    procedure setclockwise0(value: boolean);
    procedure setclockwise1(value: boolean);
  public
    constructor create;
    destructor  destroy; override;
    procedure   init(acount0, acount1: longint);
    procedure   step(acount0, acount1: longint);
  published
    property count0:     longint read fcount0;
    property count1:     longint read fcount1;
    property clockwise0: boolean read fclockwise0 write setclockwise0;
    property clockwise1: boolean read fclockwise1 write setclockwise1;
    property delay0:     longint read fdelay0     write fdelay0;
    property delay1:     longint read fdelay1     write fdelay1;
    property enabled:    boolean read fenabled    write fenabled;
    property mode:       longint read fmode       write setmode;
    property pen:        boolean read fpen        write setpen;
    property penoff:     boolean read fpenoff     write setpenoff;
  end;


var
  driver: tvpdriver = nil;


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

  motz_up       = 1.80;
  motz_low      = 2.50;
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

constructor tvpdriver.create;
begin
  inherited create;
  fcount0  := 0;
  fcount1  := 0;
  fdelay0  := 300000;
  fdelay1  := 3000;
  fenabled := false;
  fpenoff  := false;
  fpen     := true;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  // init servo
  pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_up, motz_freq));
  delaymicroseconds(fdelay0);
  // init mode
  pinmode(motx_mod0, OUTPUT);
  pinmode(motx_mod1, OUTPUT);
  pinmode(motx_mod2, OUTPUT);
  digitalwrite(motx_mod0, LOW);
  digitalwrite(motx_mod1, LOW);
  digitalwrite(motx_mod2, LOW);
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
  {$endif}
  setclockwise0(true);
  setclockwise1(true);
  setclockwise0(false);
  setclockwise1(false);
  setpenoff(false);
  setpen(false);
  setmode(1);
  setmode(4);
end;

destructor tvpdriver.destroy;
begin
  inherited destroy;
end;

procedure  tvpdriver.init(acount0, acount1: longint);
begin
  fcount0 := acount0;
  fcount1 := acount1;
end;

procedure tvpdriver.setpen(value: boolean);
begin
  if not fpenoff then
    if fpen <> value then
    begin
      fpen := value;
      {$ifdef cpuarm}
      if fpen then
      begin
        delaymicroseconds(fdelay0);
        delaymicroseconds(fdelay0);
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_low, motz_freq))
      end else
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_up,  motz_freq));
      delaymicroseconds(fdelay0);
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
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_up, motz_freq));
      delaymicroseconds(fdelay0);
      {$endif}
    end;
end;

procedure tvpdriver.setmode(value: longint);
begin
  if value <> fmode then
  begin
    {$ifdef cpuarm}
    if value = 1 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
      fmode := value;
    end else
    if value = 2 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
      fmode := value;
    end else
    if value = 4 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
      fmode := value;
    end else
    if value = 8 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
      fmode := value;
    end else
    if value = 16 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2, HIGH);
      fmode := value;
    end else
    if value = 32 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2, HIGH);
      fmode := value;
    end;
    {$endif}
  end;
end;

procedure tvpdriver.setclockwise0(value: boolean);
begin
  if value <> fclockwise0 then
  begin
    fclockwise0 := value;
    {$ifdef cpuarm}
    if fclockwise0 then
      digitalwrite(mot0_dir,  LOW)
    else
      digitalwrite(mot0_dir, HIGH);
    {$endif}
  end;
end;

procedure tvpdriver.setclockwise1(value: boolean);
begin
  if value <> fclockwise1 then
  begin
    fclockwise1 := value;
    {$ifdef cpuarm}
    if fclockwise1 then
      digitalwrite(mot1_dir,  LOW)
    else
      digitalwrite(mot1_dir, HIGH);
    {$endif}
  end;
end;

procedure tvpdriver.step(acount0, acount1: longint);
{$ifdef cpuarm}
var
   i: longint;
  b0: boolean;
  b1: boolean;
{$endif}
begin
  if enabledebug then
  begin
    if acount0 < 0 then raise exception.create('"acount0 < 0" exception');
    if acount1 < 0 then raise exception.create('"acount1 < 0" exception');
  end;

  if fenabled and ((acount0 > 0) or (acount1 > 0)) then
  begin
    {$ifdef cpuarm}
    for i := 0 to 18 do
    begin
      b0 := (vplotmatrix[acount0, i] = 1);
      b1 := (vplotmatrix[acount1, i] = 1);
      if b0 then digitalwrite(mot0_step, HIGH);
      if b1 then digitalwrite(mot1_step, HIGH);

      if b0 or
         b1 then delaymicroseconds(fdelay1);

      if b0 then digitalwrite(mot0_step,  LOW);
      if b1 then digitalwrite(mot1_step,  LOW);

      if b0 or
         b1 then delaymicroseconds(fdelay1);
    end;
    {$endif}
    if fclockwise0 then
      inc(fcount0, acount0)
    else
      dec(fcount0, acount0);

    if fclockwise1 then
      dec(fcount1, acount1)
    else
      inc(fcount1, acount1);
  end;

  if enabledebug then
  begin
    writeln(format('  DRIVER::CNT.0  = %12.5u', [fcount0]));
    writeln(format('  DRIVER::CNT.1  = %12.5u', [fcount1]));
  end;
end;

end.

