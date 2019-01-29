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
  classes, {$ifdef cpuarm} pca9685, wiringpi, {$endif}
  sysutils, vppaths, vpmath, vpsetting, vpwave;

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
    destructor  destroy; override;
    procedure   init(axcount, aycount: longint);
    procedure   move(axcount, aycount: longint);
  published
    property xcount: longint read fxcount  write setxcount;
    property ycount: longint read fycount  write setycount;
    property zcount: longint read fzcount  write setzcount;
    property xdelay: longint read fxdelay  write fxdelay;
    property ydelay: longint read fydelay  write fydelay;
    property zdelay: longint read fzdelay  write fzdelay;
    property xoff:   boolean read fxoff    write fxoff;
    property yoff:   boolean read fyoff    write fyoff;
    property zoff:   boolean read fzoff    write fzoff;
  end;

  tvpdriverthread = class(tthread)
  private
    fenabled:  boolean;
    fxcenter:  double;
    fycenter:  double;
    fxmax:     double;
    fymax:     double;
    fpaths:    tvppaths;
    fprogress: longint;
    fonstart:  tthreadmethod;
    fonstop:   tthreadmethod;
    fontick:   tthreadmethod;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor destroy; override;
  public
    property enabled:  boolean       read fenabled  write fenabled;
    property xcenter:  double        read fxcenter  write fxcenter;
    property ycenter:  double        read fycenter  write fycenter;
    property xmax:     double        read fxmax     write fxmax;
    property ymax:     double        read fymax     write fymax;
    property onstart:  tthreadmethod read fonstart  write fonstart;
    property onstop:   tthreadmethod read fonstop   write fonstop;
    property ontick:   tthreadmethod read fontick   write fontick;
    property progress: longint       read fprogress;
  end;

  procedure optimize(const p: tvppoint; out mx, my: longint);

var
  driver:        tvpdriver       = nil;
  driverthread:  tvpdriverthread = nil;

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
  while (dx > 0) or (dy > 0) do
  begin
    if (fxoff = false) and (dx > 0) then
    begin
      digitalwrite(motx_step, HIGH); delaymicroseconds(fxdelay);
      digitalwrite(motx_step,  LOW); delaymicroseconds(fxdelay);
    end;

    if (fyoff = false) and (dy > 0) then
    begin
      digitalwrite(moty_step, HIGH); delaymicroseconds(fydelay);
      digitalwrite(moty_step,  LOW); delaymicroseconds(fydelay);
    end;
    dec(dx);
    dec(dy);
  end;
  {$endif}
  fxcount := axcount;
  fycount := aycount;
  if enabledebug then
  begin
    writeln(format('  DRIVER::CNT.X  = %12.5u', [fxcount]));
    writeln(format('  DRIVER::CNT.Y  = %12.5u', [fycount]));
  end;
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

  if fzcount > value then
  begin
    if setting.zdir = 0 then delaymicroseconds($f*fzdelay);
    while fzcount > value do
    begin
      dec(fzcount, setting.zinc);
      {$ifdef cpuarm}
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
      delaymicroseconds(fzdelay);
      {$endif}
    end;
  end else
    if fzcount < value then
    begin
      if setting.zdir = 1 then delaymicroseconds($f*fzdelay);
      while fzcount < value do
      begin
        inc(fzcount, setting.zinc);
        {$ifdef cpuarm}
        pwmwrite(PCA9685_PIN_BASE + 0, calcticks(fzcount/100, motz_freq));
        delaymicroseconds(fzdelay);
        {$endif}
      end;
    end;

  fzcount := value;
  if enabledebug then
  begin
    writeln(format('  DRIVER::CNT.Z  = %12.5u', [fzcount]));
  end;
end;

// tvpdriverthread

constructor tvpdriverthread.create(paths: tvppaths);
begin
  fenabled  := true;
  fxcenter  := 0;
  fycenter  := 0;
  fxmax     := 0;
  fymax     := 0;
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

procedure optimize(const p: tvppoint; out mx, my: longint); inline;
var
  cx, cy, ct: tvpcircleimp;
      lx, ly: double;
  sx, sy, st: tvppoint;
      tx, ty: tvppoint;
begin
  tx := setting.layout00;
  ty := setting.layout01;
  //find tangent point tx
  lx := sqrt(sqr(distance_between_two_points(tx, p))-sqr(setting.xradius));
  cx := circle_by_center_and_radius(tx, setting.xradius);
  ct := circle_by_center_and_radius(p, lx);
  if intersection_of_two_circles(cx, ct, sx, st) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  lx := lx + get_angle(line_by_two_points(sx, tx))*setting.xradius;
  //find tangent point ty
  ly := sqrt(sqr(distance_between_two_points(ty, p))-sqr(setting.yradius));
  cy := circle_by_center_and_radius(ty, setting.yradius);
  ct := circle_by_center_and_radius(p, ly);
  if intersection_of_two_circles(cy, ct, sy, st) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  ly := ly + (pi-get_angle(line_by_two_points(sy, ty)))*setting.yradius;
  // calculate steps
  mx := round(lx/setting.xratio);
  my := round(ly/setting.yratio);
end;

procedure tvpdriverthread.execute;
var
   i, j: longint;
     mx: longint = 0;
     my: longint = 0;
   path: tvppath;
  point: tvppoint;
   list: tfplist;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  if enabledebug then
  begin
    writeln(format('DRIVER.THREAD::XMAX   = %12.5f', [fxmax   ]));
    writeln(format('DRIVER.THREAD::YMAX   = %12.5f', [fymax   ]));
    writeln(format('DRIVER.THREAD::X-CNTR = %12.5f', [fxcenter]));
    writeln(format('DRIVER.THREAD::Y-CNTR = %12.5f', [fycenter]));
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

        if (abs(point.x) <= (fxmax)) and
           (abs(point.y) <= (fymax)) then
          list.add(path.items[j]);
      end;
  end;

  fprogress := 0;
  if list.count > 0 then
  begin
    pvppoint(list[0])^.z := setting.zmax;
    for i := 1 to list.count -1 do
      if distance_between_two_points(
        pvppoint(list[i])^, pvppoint(list[i-1])^) < 0.2 then
        pvppoint(list[i])^.z := setting.zmin
      else
        pvppoint(list[i])^.z := setting.zmax;

    for i := 0 to list.count -1 do
    begin
      point := pvppoint(list[i])^;
      if not terminated then
      begin
        point   := wave.update(point);
        point.x := point.x + fxcenter;
        point.y := point.y + fycenter;

        driver.zcount := trunc(point.z);
        optimize(point, mx, my);
        driver.move(mx, my);
        if assigned(ontick) then
          synchronize(ontick);

        while not enabled do sleep(250);
      end;
      fprogress := (100*i) div list.count;
    end;
  end;
  list.destroy;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

