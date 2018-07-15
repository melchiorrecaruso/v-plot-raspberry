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
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils, vpcommon, vpmath;

type
  tvpdriver = class
  private
    fcount0:     longint;
    fcount1:     longint;
    fdelaym:     longint;
    fdelayz:     longint;
    fenabled:    boolean;
    fmode:       longint;
    fpen:        boolean;
    fpenoff:     boolean;
    procedure setmode(value: longint);
    procedure setpen(value: boolean);
    procedure setpenoff(value: boolean);
  public
    constructor create;
    destructor  destroy; override;
    procedure   init(acount0, acount1: longint);
    procedure   step(acount0, acount1: longint);
  published
    property count0:     longint read fcount0;
    property count1:     longint read fcount1;
    property delaym:     longint read fdelaym     write fdelaym;
    property delayz:     longint read fdelayz     write fdelayz;
    property enabled:    boolean read fenabled    write fenabled;
    property mode:       longint read fmode       write setmode;
    property pen:        boolean read fpen        write setpen;
    property penoff:     boolean read fpenoff     write setpenoff;
  end;

  tvpdriverthread = class(tthread)
  private
    fenabled:  boolean;
    fpaths:    tvppaths;
    fposition: tvpposition;
    fonstart:  tthreadmethod;
    fonstop:   tthreadmethod;
    fontick:   tthreadmethod;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor  destroy; override;
  public
    property enabled:  boolean       read fenabled   write fenabled;
    property position: tvpposition   read fposition;
    property onstart:  tthreadmethod read fonstart   write fonstart;
    property onstop:   tthreadmethod read fonstop    write fonstop;
    property ontick:   tthreadmethod read fontick    write fontick;
  end;

var
  driver:       tvpdriver       = nil;
  driverthread: tvpdriverthread = nil;

implementation

uses
  math;

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
  motz_inc      = 0.10;
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
  fdelayz  := 300000;
  fdelaym  := 3000;
  fenabled := false;
  fmode    := 0;
  fpenoff  := false;
  fpen     := true;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  // init servo
  pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_up, motz_freq));
  delaymicroseconds(fdelayz);
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
  setpenoff(false);
  setpen   (false);
  setmode(1);
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
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
  if not fpenoff then
    if fpen <> value then
    begin
      fpen := value;
      {$ifdef cpuarm}
      if fpen then
      begin
        i := motz_up;
        repeat
          i := min(i + motz_inc, motz_low);;
          pwmwrite(PCA9685_PIN_BASE + 0, calcticks(i, motz_freq));
          delaymicroseconds(fdelayz);
        until i >= motz_low;
      end else
      begin
        i := motz_low;
        repeat
          i := max(i - motz_inc, motz_up);
          pwmwrite(PCA9685_PIN_BASE + 0, calcticks(i, motz_freq));
          delaymicroseconds(fdelayz);
        until i <= motz_up;
      end;
      {$endif}
    end;
end;

procedure tvpdriver.setpenoff(value: boolean);
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
  fpenoff := value;
  if fpenoff then
    if fpen then
    begin
      fpen := false;
      {$ifdef cpuarm}
      i := motz_low;
      repeat
        i := max(i - motz_inc, motz_up);
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(i, motz_freq));
        delaymicroseconds(fdelayz);
      until i <= motz_up;
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
    end else
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
      fmode := 1;
    end;
    {$endif}
  end;
end;

procedure tvpdriver.step(acount0, acount1: longint);
{$ifdef cpuarm}
var
          i: longint;
  dm0, ddm0: longint;
  dm1, ddm1: longint;
{$endif}
begin
  {$ifdef cpuarm}
  if enabled then
  begin
    dm0 := acount0 - fcount0;
    if dm0 > 0 then
      digitalwrite(mot0_dir,  LOW)
    else
      digitalwrite(mot0_dir, HIGH);

    dm1 := acount1 - fcount1;
    if dm1 > 0 then
      digitalwrite(mot1_dir, HIGH)
    else
      digitalwrite(mot1_dir,  LOW);

    dm0 := abs(dm0);
    dm1 := abs(dm1);
    setpen(max(dm0, dm1) < 8);

    while (dm0 > 0) or (dm1 > 0) do
    begin
      ddm0 := min(10, dm0);
      ddm1 := min(10, dm1);

      for i := 0 to 18 do
      begin
        if (vplotmatrix[ddm0, i] = 1) then
        begin
          digitalwrite(mot0_step, HIGH);
          delaymicroseconds(fdelaym);
          digitalwrite(mot0_step,  LOW);
        end;

        if (vplotmatrix[ddm1, i] = 1) then
        begin
          digitalwrite(mot1_step, HIGH);
          delaymicroseconds(fdelaym);
          digitalwrite(mot1_step,  LOW);
        end;
        delaymicroseconds(fdelaym*(1+byte(fpen)));
      end;
      dec (dm0, ddm0);
      dec (dm1, ddm1);
    end;
  end;
  {$endif}
  fcount0 := acount0;
  fcount1 := acount1;
  if enabledebug then
  begin
    writeln(format('  DRIVER::CNT.0  = %12.5u', [fcount0]));
    writeln(format('  DRIVER::CNT.1  = %12.5u', [fcount1]));
  end;
end;

// tvpdriverthread

constructor tvpdriverthread.create(paths: tvppaths);
begin
  fenabled := true;
  fpaths   := paths;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvpdriverthread.destroy;
begin
  fpaths := nil;
  inherited destroy;
end;

procedure tvpdriverthread.execute;
var
     i: longint;
     j: longint;
  path: tvppath;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  for i := 0 to fpaths.count - 1 do
  begin
    path := fpaths.item[i];
    for j := 0 to path.count - 1 do
      if not terminated then
      begin
        fposition := path.item[j];
        if fposition.c then
        begin
          driver.step(fposition.m0, fposition.m1);
        end;

        if assigned(ontick) then
          synchronize(ontick);

        while not enabled do sleep(250);
      end;
    end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

