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
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils,
  vpcommon, vpmath, vpsetting, vpwave;

type
  tvpdriver = class
  private
    fcount0:  longint;
    fcount1:  longint;
    fdelaym:  longint;
    fdelayz:  longint;
    fenabled: boolean;
    fmode:    longint;
    fpen:     boolean;
    fzoff:    boolean;
    procedure setcount0(value: longint);
    procedure setcount1(value: longint);
    procedure setmode  (value: longint);
    procedure setpen   (value: boolean);
    procedure setzoff  (value: boolean);
  public
    constructor create;
    destructor  destroy; override;
    procedure   init(acount0, acount1: longint);
    procedure   move(acount0, acount1: longint);
  published
    property count0:  longint read fcount0  write setcount0;
    property count1:  longint read fcount1  write setcount1;
    property delaym:  longint read fdelaym  write fdelaym;
    property delayz:  longint read fdelayz  write fdelayz;
    property enabled: boolean read fenabled write fenabled;
    property mode:    longint read fmode    write setmode;
    property pen:     boolean read fpen     write setpen;
    property zoff:    boolean read fzoff    write setzoff;
  end;

  tvpdriverthread = class(tthread)
  private
    fenabled:  boolean;
    fmaxdx:    double;
    fmaxdy:    double;
    fmidx:     double;
    fmidy:     double;
    foffsetx:  double;
    foffsety:  double;
    fonstart:  tthreadmethod;
    fonstop:   tthreadmethod;
    fontick:   tthreadmethod;
    fpaths:    tvppaths;
    fprogress: longint;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor  destroy; override;
  public
    property enabled:  boolean       read fenabled  write fenabled;
    property maxdx:    double        read fmaxdx    write fmaxdx;
    property maxdy:    double        read fmaxdy    write fmaxdy;
    property midx:     double        read fmidx     write fmidx;
    property midy:     double        read fmidy     write fmidy;
    property offsetx:  double        read foffsetx  write foffsetx;
    property offsety:  double        read foffsety  write foffsety;
    property onstart:  tthreadmethod read fonstart  write fonstart;
    property onstop:   tthreadmethod read fonstop   write fonstop;
    property ontick:   tthreadmethod read fontick   write fontick;
    property progress: longint       read fprogress;
  end;

  procedure optimize(const p: tvppoint; var m0, m1: longint);

var
  driver:       tvpdriver       = nil;
  driverthread: tvpdriverthread = nil;

implementation

uses
  math;

const
  motz_hi       = 2.50;
  motz_lo       = 0.00;
  motz_inc      = 0.02;
  motz_freq     = 50;

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
  fdelaym  := 1000;
  fdelayz  := 30000;
  fenabled := false;
  fmode    := 0;
  fpen     := true;
  fzoff    := false;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE,               PCA9685_ADDRESS, motz_freq);
  pwmwrite    (PCA9685_PIN_BASE + 0, calcticks(setting.srvdef0, motz_freq));
  pwmwrite    (PCA9685_PIN_BASE + 1, calcticks(setting.srvdef1, motz_freq));
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
  setpen(false);
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

procedure tvpdriver.setpen(value: boolean);
var
  cnt0: double;
  cnt1: double;
     i: longint;
begin
  if fzoff = true  then exit;
  if fpen  = value then exit;

  fpen := value;
  if fpen then
  begin
    cnt0 := setting.srvdef0;
    cnt1 := setting.srvdef1;
    //writeln('DOWN0 = ', cnt0);
    //writeln('DOWN1 = ', cnt1);
    for i := 1 to setting.srvcount do
    begin
      cnt0 := cnt0 - motz_inc;
      cnt1 := cnt1 + motz_inc;
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(cnt0, motz_freq));
      pwmwrite(PCA9685_PIN_BASE + 1, calcticks(cnt1, motz_freq));
      delaymicroseconds(fdelayz);
      {$endif}
      //writeln('DOWN0 -> ', cnt0);
      //writeln('DOWN1 -> ', cnt1);
      //readln;
    end;

  end else
  begin
    cnt0 := setting.srvdef0 - setting.srvcount * motz_inc;
    cnt1 := setting.srvdef1 + setting.srvcount * motz_inc;
    //writeln('UP0 = ', cnt0);
    //writeln('UP1 = ', cnt1);
    for i := 1 to setting.srvcount do
    begin
      cnt0 := cnt0 + motz_inc;
      cnt1 := cnt1 - motz_inc;
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(cnt0, motz_freq));
      pwmwrite(PCA9685_PIN_BASE + 1, calcticks(cnt1, motz_freq));
      delaymicroseconds(fdelayz);
      {$endif}
      //writeln('UP0 -> ', cnt0);
      //writeln('UP1 -> ', cnt1);
      //readln;
    end;

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

procedure tvpdriver.setzoff(value: boolean);
begin
  fzoff := value;
end;

// tvpdriverthread

constructor tvpdriverthread.create(paths: tvppaths);
begin
  fenabled  := true;
  fmaxdx    := 0;
  fmaxdy    := 0;
  fmidx     := 0;
  fmidy     := 0;
  foffsetx  := 0;
  foffsety  := 0;
  fpaths    := paths;
  fprogress := 0;

  freeonterminate := true;
  inherited create(true);
end;

destructor tvpdriverthread.destroy;
begin
  fpaths := nil;
  inherited destroy;
end;

procedure optimize(const p: tvppoint; var m0, m1: longint);
var
   c0,  c1,  c2: tvpcircle;
       f01, fxx: tvpline;
        l0,  l1: double;
  s00, s01, sxx: tvppoint;
  t00, t01, txx: tvppoint;
begin
  t00 := setting.layout00;
  t01 := setting.layout01;
  //find tangent point t00
  l0  := sqrt(sqr(distance_between_two_points(t00, p))-sqr(setting.radius));
  c0  := circle_by_center_and_radius(t00, setting.radius);
  c2  := circle_by_center_and_radius(p, l0);
  if intersection_of_two_circles(c0, c2, s00, sxx) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  l0  := l0 + get_line_angle(line_by_two_points(s00, t00))*setting.radius;
  //find tangent point t01
  l1  := sqrt(sqr(distance_between_two_points(t01, p))-sqr(setting.radius));
  c1  := circle_by_center_and_radius(t01, setting.radius);
  c2  := circle_by_center_and_radius(p, l1);
  if intersection_of_two_circles(c1, c2, s01, sxx) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  l1  := l1 + (pi-get_line_angle(line_by_two_points(s01, t01)))*setting.radius;
  // calculate steps
  m0 := round(setting.mode*(l0/setting.ratio));
  m1 := round(setting.mode*(l1/setting.ratio));

  if enabledebug then
  begin
    txx.x := s00.x;
    txx.y := p.y;

    f01 := line_by_two_points(s01, p);
    fxx := line_by_two_points(s00, txx);
    txx := intersection_of_two_lines(f01, fxx);

    writeln(format('OPTIMIZE::P.X    = %12.5f  P.Y   = %12.5f', [p.x, p.y]));
    writeln(format('OPTIMIZE::LEN0   = %12.5f  LEN1  = %12.5f', [l0, l1]));
    writeln(format('OPTIMIZE::CNT0   = %12.5u  CNT1  = %12.5u', [m0, m1]));

    writeln(format('OPTIMIZE::F0     = %12.5u  F1    = %12.5u', [
      trunc(setting.weight/
        distance_between_two_points(s00, txx)*
        distance_between_two_points(s00, p)),
      trunc(setting.weight/
        distance_between_two_points(s00, txx)*
        distance_between_two_points(txx, p))]));
  end;
end;

procedure tvpdriverthread.execute;
var
     i: longint;
     j: longint;
    m0: longint;
    m1: longint;
  path: tvppath;
   pos: tvpposition;
     p: tvppoint;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  if enabledebug then
  begin
    writeln(format('DRIVER.THREAD::OFF-X  = %12.5f', [foffsetx]));
    writeln(format('DRIVER.THREAD::OFF-Y  = %12.5f', [foffsety]));
    writeln(format('DRIVER.THREAD::MAX-DX = %12.5f', [fmaxdx  ]));
    writeln(format('DRIVER.THREAD::MAX-DY = %12.5f', [fmaxdy  ]));
    writeln(format('DRIVER.THREAD::MID-X  = %12.5f', [fmidx   ]));
    writeln(format('DRIVER.THREAD::MID-Y  = %12.5f', [fmidy   ]));
  end;

  fprogress := 0;
  for i := 0 to fpaths.count - 1 do
  begin
    path := fpaths.item[i];
    for j := 0 to path.count - 1 do
    begin
      pos   := path.item[j];
      p.x   := pos.x + foffsetx;
      p.y   := pos.y + foffsety;
      p     := wave.update(p);
      pos.b := (abs(p.x) <= (fmaxdx)) and
               (abs(p.y) <= (fmaxdy));
    end;
    fprogress := (100*i) div fpaths.count;
  end;

  fprogress := 0;
  for i := 0 to fpaths.count - 1 do
  begin
    path := fpaths.item[i];
    for j := 0 to path.count - 1 do
    begin
      if not terminated then
      begin
        pos := path.item[j];
        if pos.b then
        begin
          p.x := pos.x + foffsetx;
          p.y := pos.y + foffsety;
          p   := wave.update(p);
          p.x := p.x + fmidx;
          p.y := p.y + fmidy;
          optimize(p, m0, m1);
          driver.move(m0, m1);
          if assigned(ontick) then
            synchronize(ontick);
        end;
        while not enabled do sleep(250);

      end;
      fprogress := (100*i) div fpaths.count;
    end;
  end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

