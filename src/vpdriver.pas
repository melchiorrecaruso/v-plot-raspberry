{
  Description: vPlot driver library.

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
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils,
  vppaths, vpmath, vpsetting, vpwave;

type
  tvpdriver = class
  private
    fcountx:  longint;
    fcounty:  longint;
    fdelaym:  longint;
    fdelayz:  longint;
    fenabled: boolean;
    fpen:     boolean;
    fzoff:    boolean;
    fxyoff:   boolean;
    procedure setcountx(value: longint);
    procedure setcounty(value: longint);
    procedure setpen   (value: boolean);
    procedure setzoff  (value: boolean);
    procedure setxyoff (value: boolean);
  public
    constructor create;
    destructor  destroy; override;
    procedure   init(acount0, acount1: longint);
    procedure   move(acount0, acount1: longint);
  published
    property count0:  longint read fcountx  write setcountx;
    property count1:  longint read fcounty  write setcounty;
    property delaym:  longint read fdelaym  write fdelaym;
    property delayz:  longint read fdelayz  write fdelayz;
    property enabled: boolean read fenabled write fenabled;
    property pen:     boolean read fpen     write setpen;
    property zoff:    boolean read fzoff    write setzoff;
    property xyoff:   boolean read fxyoff   write setxyoff;
  end;

  tvpdriverthread = class(tthread)
  private
    fenabled:  boolean;
    fmaxdx:    double;
    fmaxdy:    double;
    fmidx:     double;
    fmidy:     double;
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
    property onstart:  tthreadmethod read fonstart  write fonstart;
    property onstop:   tthreadmethod read fonstop   write fonstop;
    property ontick:   tthreadmethod read fontick   write fontick;
    property progress: longint       read fprogress;
  end;

  procedure optimize(const p: tvppoint; out m0, m1: longint);

var
  driver:        tvpdriver       = nil;
  driverthread:  tvpdriverthread = nil;

implementation

uses
  math;

const
  motz_hi       = 2.50;
  motz_lo       = 0.00;
  motz_inc      = 0.02;
  motz_freq     = 50;

{$ifdef cpuarm}
  mot_en        = P37;
  mot0_step     = P38;
  mot0_dir      = P40;
  mot1_step     = P29;
  mot1_dir      = P31;
{$endif}

constructor tvpdriver.create;
begin
  inherited create;
  fcountx  := 0;
  fcounty  := 0;
  fdelaym  := trunc(setting.delaym/setting.mode);
  fdelayz  :=       setting.delayz;
  fenabled := false;
  fpen     := true;
  fzoff    := false;
  {$ifdef cpuarm}
  // setup wiringpi library
  wiringpisetup;
  // setup pca9685 library
  pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);
  // enable motors
  pinmode(mot_en,      OUTPUT);
  digitalwrite(mot_en,    LOW);
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
  setxyoff(false);
  setpen(false);
end;

destructor tvpdriver.destroy;
begin
  setxyoff(true);
  inherited destroy;
end;

procedure  tvpdriver.init(acount0, acount1: longint);
begin
  fcountx := acount0;
  fcounty := acount1;
end;

procedure tvpdriver.move(acount0, acount1: longint);
{$ifdef cpuarm}
var
  dmx: longint;
  dmy: longint;
{$endif}
begin
  {$ifdef cpuarm}
  if fxyoff  then exit;
  if enabled then
  begin
    dmx := acount0 - fcountx;
    if dmx < 0 then
      digitalwrite(mot0_dir,  LOW)
    else
      digitalwrite(mot0_dir, HIGH);

    dmy := acount1 - fcounty;
    if dmy < 0 then
      digitalwrite(mot1_dir, HIGH)
    else
      digitalwrite(mot1_dir,  LOW);

    dmx := abs(dmx);
    dmy := abs(dmy);
    while (dmx > 0) or (dmy > 0) do
    begin
      if dmx > 0 then digitalwrite(mot0_step, HIGH);
      if dmy > 0 then digitalwrite(mot1_step, HIGH);

      delaymicroseconds(fdelaym);

      if dmx > 0 then digitalwrite(mot0_step,  LOW);
      if dmy > 0 then digitalwrite(mot1_step,  LOW);

      delaymicroseconds(fdelaym);

      if dmx > 0 then dec(dmx);
      if dmy > 0 then dec(dmy);
    end;
  end;
  {$endif}
  fcountx := acount0;
  fcounty := acount1;
  if enabledebug then
  begin
    writeln(format('  DRIVER::CNT.0  = %12.5u', [fcountx]));
    writeln(format('  DRIVER::CNT.1  = %12.5u', [fcounty]));
  end;
end;

procedure tvpdriver.setpen(value: boolean);
var
  cnt0: double;
  cnt1: double;
     i: longint;

 function clockwise(value: boolean): boolean;
 begin
   if setting.srvdir = 0 then
     result := value
   else
     result := not value;
 end;

begin
  if fpen  = value then exit;
  if fzoff = true  then exit;

  fpen := value;
  {$ifdef cpuarm}
  if fpen then delaymicroseconds($F*fdelayz);
  {$endif}
  if clockwise(fpen) then
  begin
    cnt0 := setting.srvdef0;
    cnt1 := setting.srvdef1;
    for i := 1 to setting.srvcount do
    begin
      cnt0 := cnt0 - motz_inc;
      cnt1 := cnt1 + motz_inc;
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(cnt0, motz_freq));
      pwmwrite(PCA9685_PIN_BASE + 1, calcticks(cnt1, motz_freq));
      delaymicroseconds(fdelayz);
      {$endif}
    end;
  end else
  begin
    cnt0 := setting.srvdef0 - setting.srvcount * motz_inc;
    cnt1 := setting.srvdef1 + setting.srvcount * motz_inc;
    for i := 1 to setting.srvcount do
    begin
      cnt0 := cnt0 + motz_inc;
      cnt1 := cnt1 - motz_inc;
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(cnt0, motz_freq));
      pwmwrite(PCA9685_PIN_BASE + 1, calcticks(cnt1, motz_freq));
      delaymicroseconds(fdelayz);
      {$endif}
    end;
  end;

end;

procedure tvpdriver.setcountx(value: longint);
begin
  move(value, fcounty);
end;

procedure tvpdriver.setcounty(value: longint);
begin
  move(fcountx, value);
end;

procedure tvpdriver.setzoff(value: boolean);
begin
  fzoff := value;
end;

procedure tvpdriver.setxyoff(value: boolean);
begin
  {$ifdef cpuarm}
  fxyoff := value;
  if fxyoff then
    digitalwrite(mot_en, HIGH)
  else
    digitalwrite(mot_en,  LOW);
  {$endif}
end;

// tvpdriverthread

constructor tvpdriverthread.create(paths: tvppaths);
begin
  fenabled  := true;
  fmaxdx    := 0;
  fmaxdy    := 0;
  fmidx     := 0;
  fmidy     := 0;
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

procedure optimize(const p: tvppoint; out m0, m1: longint); inline;
var
   c0,  c1,  c2: tvpcircleimp;
        l0,  l1: double;
  s00, s01, sxx: tvppoint;
       t00, t01: tvppoint;
begin
  t00 := setting.layout00;
  t01 := setting.layout01;
  //find tangent point t00
  l0  := sqrt(sqr(distance_between_two_points(t00, p))-sqr(setting.radius));
  c0  := circle_by_center_and_radius(t00, setting.radius);
  c2  := circle_by_center_and_radius(p, l0);
  if intersection_of_two_circles(c0, c2, s00, sxx) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  l0  := l0 + get_angle(line_by_two_points(s00, t00))*setting.radius;
  //find tangent point t01
  l1  := sqrt(sqr(distance_between_two_points(t01, p))-sqr(setting.radius));
  c1  := circle_by_center_and_radius(t01, setting.radius);
  c2  := circle_by_center_and_radius(p, l1);
  if intersection_of_two_circles(c1, c2, s01, sxx) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  l1  := l1 + (pi-get_angle(line_by_two_points(s01, t01)))*setting.radius;
  // calculate steps
  m0 := round(setting.mode*(l0/setting.ratio));
  m1 := round(setting.mode*(l1/setting.ratio));
end;

procedure tvpdriverthread.execute;
var
   i, j: longint;
     m0: longint = 0;
     m1: longint = 0;
   path: tvppath;
  point: tvppoint;
   list: tfplist;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  if enabledebug then
  begin
    writeln(format('DRIVER.THREAD::MAX-DX = %12.5f', [fmaxdx]));
    writeln(format('DRIVER.THREAD::MAX-DY = %12.5f', [fmaxdy]));
    writeln(format('DRIVER.THREAD::MID-X  = %12.5f', [fmidx ]));
    writeln(format('DRIVER.THREAD::MID-Y  = %12.5f', [fmidy ]));
  end;

  list := tfplist.create;
  for i := 0 to fpaths.count -1 do
  begin
    path := fpaths.items[i];
    if path.enabled then
      for j := 0 to path.count -1 do
      begin
        point:= path.items[j]^;
        point:= wave.update(point);

        if (abs(point.x) <= (fmaxdx)) and
           (abs(point.y) <= (fmaxdy)) then
          list.add(path.items[j]);
      end;
  end;

  if list.count > 0 then
  begin
    path.items[0]^.z := 0;
    for i := 1 to list.count -1 do
      if distance_between_two_points(
        path.items[i]^, path.items[i-1]^) < 0.2 then
        path.items[i]^.z := -1
      else
        path.items[i]^.z := +1;
  end;

  fprogress := 0;
  for i := 0 to list.count -1 do
  begin
    point := pvppoint(list[i])^;
    if not terminated then
    begin
      point   := wave.update(point);
      point.x := point.x + fmidx;
      point.y := point.y + fmidy;
      optimize(point, m0, m1);

      driver.pen := point.z < 0;
      driver.move(m0, m1);
      if assigned(ontick) then
        synchronize(ontick);

      while not enabled do sleep(250);
    end;
    fprogress := (100*i) div list.count;
  end;
  list.destroy;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

