{
  Description: vPlot driver-thread class.

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

unit vpdriverthread;

{$mode objfpc}

interface

uses
  classes, sysutils, vpdriver, vpmath, vppaths, vpsetting, vpwave;

type
  tvpdriverthread = class(tthread)
  private
    fenabled: boolean;
    fonstart: tthreadmethod;
    fonstop:  tthreadmethod;
    fontick:  tthreadmethod;
    fxcenter: double;
    fycenter: double;
    fpath:    tvppath;
  protected
    procedure execute; override;
  public
    constructor create(path: tvppath);
    destructor destroy; override;
  public
    property enabled: boolean       read fenabled write fenabled;
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop:  tthreadmethod read fonstop  write fonstop;
    property ontick:  tthreadmethod read fontick  write fontick;
    property xcenter: double        read fxcenter write fxcenter;
    property ycenter: double        read fycenter write fycenter;
  end;

  procedure calc_ (const p: tvppoint; out l0, l1: vpfloat); overload;
  procedure calc_ (const p: tvppoint; out m0, m1: longint); overload;

  function calc_l0(const p, t0: tvppoint; r0: vpfloat): vpfloat;
  function calc_l1(const p, t1: tvppoint; r1: vpfloat): vpfloat;

var
  driverthread: tvpdriverthread = nil;

implementation

function calc_l0(const p, t0: tvppoint; r0: vpfloat): vpfloat;
var
      a0: vpfloat;
  c0, cx: tvpcircleimp;
  s0, sx: tvppoint;
begin
  //find tangent point t0
  result := sqrt(sqr(distance_between_two_points(t0, p))-sqr(r0));
  c0 := circle_by_center_and_radius(t0, r0);
  cx := circle_by_center_and_radius(p, result);
  if intersection_of_two_circles(c0, cx, s0, sx) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  a0 := angle(line_by_two_points(s0, t0));
  result := result + a0*r0;
end;

function calc_l1(const p, t1: tvppoint; r1: vpfloat): vpfloat;
var
      a1: vpfloat;
  c1, cx: tvpcircleimp;
  s1, sx: tvppoint;
begin
  //find tangent point t1
  result := sqrt(sqr(distance_between_two_points(t1, p))-sqr(r1));
  c1 := circle_by_center_and_radius(t1, r1);
  cx := circle_by_center_and_radius(p, result);
  if intersection_of_two_circles(c1, cx, s1, sx) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  a1 := pi-angle(line_by_two_points(s1, t1));
  result := result + a1*r1;
end;

procedure calc_ (const p: tvppoint; out l0, l1: vpfloat);
var
      a0, a1: vpfloat;
  c0, c1, cx: tvpcircleimp;
  s0, s1, sx: tvppoint;
      t0, t1: tvppoint;
begin
  //find tangent point t0
  t0 := setting.point0;
  l0 := sqrt(sqr(distance_between_two_points(t0, p))-sqr(setting.m0radius));
  c0 := circle_by_center_and_radius(t0, setting.m0radius);
  cx := circle_by_center_and_radius(p, l0);
  if intersection_of_two_circles(c0, cx, s0, sx) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  a0 := angle(line_by_two_points(s0, t0));
  l0 := l0 + a0*setting.m0radius;
  //find tangent point t1
  t1 := setting.point1;
  l1 := sqrt(sqr(distance_between_two_points(t1, p))-sqr(setting.m1radius));
  c1 := circle_by_center_and_radius(t1, setting.m1radius);
  cx := circle_by_center_and_radius(p, l1);
  if intersection_of_two_circles(c1, cx, s1, sx) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  a1 := pi-angle(line_by_two_points(s1, t1));
  l1 := l1 + a1*setting.m1radius;
end;

procedure calc_ (const p: tvppoint; out m0, m1: longint);
var
  l0, l1: vpfloat;
begin
  calc_(p, l0, l1);
  // calculate steps
  m0 := round(l0/setting.m0ratio);
  m1 := round(l1/setting.m1ratio);
end;

// tvpdriverthread

constructor tvpdriverthread.create(path: tvppath);
begin
  fenabled := true;
  fxcenter := 0;
  fycenter := 0;
  fpath    := path;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvpdriverthread.destroy;
begin
  fpath := nil;
  inherited destroy;
end;

procedure tvpdriverthread.execute;
var
   i: longint;
  mx: longint = 0;
  my: longint = 0;
  p0,
  p1: tvppoint;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  p0 := setting.point8;
  for i := 0 to fpath.count -1 do
  begin
    if not terminated then
    begin
      p1   := spacewave.update(fpath.items[i]^);
      p1.x := p1.x + fxcenter;
      p1.y := p1.y + fycenter;

      if distance_between_two_points(p0, p1) < 0.25 then
        driver.zcount := setting.mzmin
      else
        driver.zcount := setting.mzmax;

      calc_(p1, mx, my);
      driver.move(mx, my);
      if assigned(ontick) then
        synchronize(ontick);

      while not enabled do
      begin
        sleep(250);
      end;
      p0 := p1;
    end;
  end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

