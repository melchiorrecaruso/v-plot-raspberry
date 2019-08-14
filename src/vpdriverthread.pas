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

  procedure optimize (const p: tvppoint; out mx, my: longint);

var
  driverthread: tvpdriverthread = nil;

implementation

procedure optimize(const p: tvppoint; out mx, my: longint); inline;
var
      a0, a1: vpfloat;
  cx, cy, ct: tvpcircleimp;
      lx, ly: vpfloat;
  sx, sy, st: tvppoint;
      tx, ty: tvppoint;
begin
  tx := setting.layout0;
  ty := setting.layout1;
  //find tangent point tx
  lx := sqrt(sqr(distance_between_two_points(tx, p))-sqr(setting.xradius));
  cx := circle_by_center_and_radius(tx, setting.xradius);
  ct := circle_by_center_and_radius(p, lx);
  if intersection_of_two_circles(cx, ct, sx, st) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  a0 := angle(line_by_two_points(sx, tx));
  lx := lx + a0*setting.xradius;
  //find tangent point ty
  ly := sqrt(sqr(distance_between_two_points(ty, p))-sqr(setting.yradius));
  cy := circle_by_center_and_radius(ty, setting.yradius);
  ct := circle_by_center_and_radius(p, ly);
  if intersection_of_two_circles(cy, ct, sy, st) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  a1 := pi-angle(line_by_two_points(sy, ty));
  ly := ly + a1*setting.yradius;
  // calculate steps
  mx := round(lx/setting.xratio);
  my := round(ly/setting.yratio);
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
  p1,
  p2: tvppoint;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  p0 := setting.layout9;
  p1 := setting.layout8;
  for i := 0 to fpath.count -1 do
  begin
    if not terminated then
    begin
      p2 := spacewave.update(fpath.items[i]^);
      driver.xdelay := timewave.getdxdelay(p2);
      driver.ydelay := timewave.getdydelay(p2);

      p2.x := p2.x + fxcenter;
      p2.y := p2.y + fycenter;

      if distance_between_two_points(p1, p2) < 0.2 then
        driver.zcount := setting.zmin
      else
        driver.zcount := setting.zmax;

      optimize(p2, mx, my);
      driver.move(mx, my);
      if assigned(ontick) then
        synchronize(ontick);

      while not enabled do
      begin
        sleep(250);
      end;
      p0 := p1;
      p1 := p2;
    end;
  end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

