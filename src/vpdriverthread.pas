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
    fenabled:   boolean;
    fxcenter:   double;
    fycenter:   double;
    fdxmax:     double;
    fdymax:     double;
    fpaths:     tvppaths;
    ftick:      longint;
    fonstart:   tthreadmethod;
    fonstop:    tthreadmethod;
    fontick:    tthreadmethod;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor destroy; override;
  public
    property enabled:   boolean       read fenabled   write fenabled;
    property xcenter:   double        read fxcenter   write fxcenter;
    property ycenter:   double        read fycenter   write fycenter;
    property dxmax:     double        read fdxmax     write fdxmax;
    property dymax:     double        read fdymax     write fdymax;
    property tick:      longint       read ftick;
    property onstart:   tthreadmethod read fonstart   write fonstart;
    property onstop:    tthreadmethod read fonstop    write fonstop;
    property ontick:    tthreadmethod read fontick    write fontick;
  end;

  procedure optimize(const p: tvppoint; out mx, my: longint);

var
  driverthread: tvpdriverthread = nil;


implementation


procedure optimize(const p: tvppoint; out mx, my: longint); inline;
var
  cx, cy, ct: tvpcircleimp;
      lx, ly: double;
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

// tvpdriverthread

constructor tvpdriverthread.create(paths: tvppaths);
begin
  fenabled := true;
  fxcenter := 0;
  fycenter := 0;
  fdxmax   := 0;
  fdymax   := 0;
  fpaths   := paths;
  ftick    := 0;

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
   i, j: longint;
     mx: longint = 0;
     my: longint = 0;
   path: tvppath;
  point: tvppoint;
   list: tfplist;
begin
  if assigned(onstart) then
    synchronize(fonstart);

  list := tfplist.create;
  for i := 0 to fpaths.count -1 do
  begin
    path := fpaths.items[i];
    if path.enabled then
      if not path.hidden then
        for j := 0 to path.count -1 do
        begin
          point:= path.items[j]^;
          point:= wave.update(point);

          if (abs(point.x) <= (fdxmax)) and
             (abs(point.y) <= (fdymax)) then
            list.add(path.items[j]);
        end;
  end;

  if list.count > 0 then
  begin
    pvppoint(list[0])^.z := setting.zmax;
    for i := 1 to list.count -1 do
      if distance_between_two_points(
        pvppoint(list[i])^, pvppoint(list[i-1])^) < 0.25 then
        pvppoint(list[i])^.z := setting.zmin
      else
        pvppoint(list[i])^.z := setting.zmax;

    ftick := list.count;
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
      dec(ftick);
    end;
  end;
  list.destroy;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

