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
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils, vpcommon, vpmath, vpsetting;

type
  tvpdriver = class
  private
    fcount0:  longint;
    fcount1:  longint;
    fcountz:  double;
    fdelaym:  longint;
    fdelayz:  longint;
    fenabled: boolean;
    fmode:    longint;
    fzoff:    boolean;
    procedure setcount0(value: longint);
    procedure setcount1(value: longint);
    procedure setcountz(value: double);
    procedure setmode  (value: longint);
    procedure setzoff  (value: boolean);
  public
    constructor create;
    destructor  destroy; override;
    procedure   init(acount0, acount1: longint);
    procedure   move(acount0, acount1: longint);
  published
    property count0:  longint read fcount0  write setcount0;
    property count1:  longint read fcount1  write setcount1;
    property countz:  double  read fcountz  write setcountz;
    property delaym:  longint read fdelaym  write fdelaym;
    property delayz:  longint read fdelayz  write fdelayz;
    property enabled: boolean read fenabled write fenabled;
    property mode:    longint read fmode    write setmode;
    property zoff:    boolean read fzoff    write setzoff;
  end;

  tvpdriverthread = class(tthread)
  private
    fenabled: boolean;
    fonstart: tthreadmethod;
    fonstop:  tthreadmethod;
    fontick:  tthreadmethod;
    fpaths:   tvppaths;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor  destroy; override;
  public
    property enabled: boolean       read fenabled write fenabled;
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop:  tthreadmethod read fonstop  write fonstop;
    property ontick:  tthreadmethod read fontick  write fontick;
  end;

var
  driver:       tvpdriver       = nil;
  driverthread: tvpdriverthread = nil;

implementation

uses
  math;

const
  motz_hi       = 2.50;
  motz_lo       = 0.00;
  motz_inc      = 0.05;
  motz_freq     = 60;

{$ifdef cpuarm}
  mot0_step     = P38;
  mot0_dir      = P40;
  mot1_step     = P29;
  mot1_dir      = P31;

  motx_mod0     = P15;
  motx_mod1     = P13;
  motx_mod2     = P11;

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
  fcountz  := motz_lo;
  fdelaym  := 1000;
  fdelayz  := 30000;
  fenabled := false;
  fmode    := 0;
  fzoff    := false;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fcountz , motz_freq));
  // init step mode
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

procedure tvpdriver.move(acount0, acount1: longint);
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
    if max(dm0, dm1) <= 10 then
      setcountz(setting.srvdown)
    else
      setcountz(setting.srvup);

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
          delaymicroseconds(fdelaym);
        end;

        if (vplotmatrix[ddm1, i] = 1) then
        begin
          digitalwrite(mot1_step, HIGH);
          delaymicroseconds(fdelaym);
          digitalwrite(mot1_step,  LOW);
          delaymicroseconds(fdelaym);
        end;
      end;
      dec(dm0, ddm0);
      dec(dm1, ddm1);
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

procedure tvpdriver.setcount0(value: longint);
begin
  move(value, fcount1);
end;

procedure tvpdriver.setcount1(value: longint);
begin
  move(fcount0, value);
end;

procedure tvpdriver.setcountz(value: double);
begin
  if fzoff = true    then exit;
  if value > motz_hi then exit;
  if value < motz_lo then exit;

  if fcountz <> value then
  begin
    if fcountz < value then
      while fcountz < value do
      begin
        fcountz := min(value, fcountz + motz_inc);
        {$ifdef cpuarm}
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fcountz , motz_freq));
        delaymicroseconds(fdelayz);
        {$endif}
      end
    else
      while fcountz > value do
      begin
        fcountz := max(value, fcountz - motz_inc);
        {$ifdef cpuarm}
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fcountz , motz_freq));
        delaymicroseconds(fdelayz);
        {$endif}
      end;
  end;
end;

procedure tvpdriver.setzoff(value: boolean);
begin
  fzoff := value;
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
   pos: tvpposition;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  for i := 0 to fpaths.count - 1 do
  begin
    path := fpaths.item[i];
    for j := 0 to path.count - 1 do
    begin
      if not terminated then
      begin
        pos := path.item[j];
        if pos.c then
          driver.move(pos.m0, pos.m1);

        while not enabled do sleep(250);
      end;
    end;
    if assigned(ontick) then
      synchronize(ontick);
  end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

