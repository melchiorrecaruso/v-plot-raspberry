{
  Description: vPlot coder.

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

unit vpcoder;

{$mode objfpc}

interface

uses
  classes, fpvectorial, vpcommon, vpmath, vpsetting;

  function interpolate_line(const p0, p1: tvppoint): tvppath;
  function interpolate_circle(const entity: tvcircle ): tvppath;
  function interpolate_circlearc(const entity: tvcirculararc): tvppath;
  function interpolate_path(const entity: tpath): tvppath;

  procedure load_paths(paths: tvppaths; vec: tvvectorialdocument);
  procedure optimize_paths(paths: tvppaths; offsetx, offsety, midx, midy, hmax, wmax: double);
  procedure optimize_point(const p: tvppoint; var m0, m1: longint);

implementation

uses
  math, sysutils;

// toolpath routines

function interpolate_line(const p0, p1: tvppoint): tvppath;
var
  dx, dy: double;
  i, len: longint;
       p: tvppoint;
begin
  len := max(1, round(distance_between_two_points(p0, p1) / 0.15));
   dx := (p1.x - p0.x) / len;
   dy := (p1.y - p0.y) / len;

  result := tvppath.create;
  for i := 0 to len do
  begin
    p.x := i * dx;
    p.y := i * dy;
    p   := translate_point(p0, p);
    result.add(p);
  end;
end;

function interpolate_circle(const entity: tvcircle): tvppath;
var
      i: longint;
    len: longint;
     cc: tvppoint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  cc.x    := entity.x;
  cc.y    := entity.y;
  start.x := entity.radius;
  start.y := +0.0;
  sweep   := 2 * pi;

  len := max(1, round(abs(sweep) * entity.radius / 0.15));

  result := tvppath.create;
  for i := 0 to len do
  begin
    p := rotate_point(start, (i * (sweep / len)));
    p := translate_point(cc, p);
    result.insert(0, p);
  end;
end;

function interpolate_circlearc(const entity: tvcirculararc): tvppath;
var
      i: longint;
    len: longint;
     cc: tvppoint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  cc.x    := entity.x;
  cc.y    := entity.y;
  start.x := entity.radius;
  start.y := +0.0;
  start   := rotate_point(start, degtorad(entity.startangle));
  sweep   := degtorad(entity.endangle - entity.startangle);

  len := max(1, round(abs(sweep) * entity.radius / 0.15));

  result := tvppath.create;
  for i := 0 to len do
  begin
    p := rotate_point(start, (i * (sweep / len)));
    p := translate_point(cc, p);
    result.insert(0, p);
  end;
end;

function interpolate_path(const entity: tpath): tvppath;
var
   dx, dy: double;
     i, j: longint;
      len: longint;
        p: tvppoint;
   p0, p1: tvppoint;
  segment: tpathsegment;
begin
  result := tvppath.create;
  entity.prepareforsequentialreading;
  for i := 0 to entity.len - 1 do
  begin
    segment := tpathsegment(entity.next);
    case segment.segmenttype of
      stmoveto:
      begin
        p0.x := t2dsegment(segment).x;
        p0.y := t2dsegment(segment).y;
      end;
      st2dlinewithpen, st2dline, st3dline:
      begin
        p1.x := t2dsegment(segment).x;
        p1.y := t2dsegment(segment).y;

        len := max(1, round(distance_between_two_points(p0, p1) / 0.15));
        dx := (p1.x - p0.x) / len;
        dy := (p1.y - p0.y) / len;

        for j := 0 to len do
        begin
          p.x := j * dx;
          p.y := j * dy;
          p   := translate_point(p0, p);
          result.add(p);
        end;
        p0 := p1;
      end;
      else
        writeln(segment.segmenttype);
    end;
  end;
end;

procedure optimize_point(const p: tvppoint; var m0, m1: longint);
var
  c0, c1, c3, c4: tvpcircle;
  alpha, alphalo, alphahi, lsx, ldx: double;
  s00, s01, sxx, t00, t01, t02, t03, t04, t05, t06: tvppoint;
begin
  alphalo := -pi/2;
  alphahi := +pi/2;
  repeat
    alpha := (alphalo+alphahi)/2;
    t00 := setting.layout00;
    t01 := setting.layout01;
    t02 := translate_point(p, rotate_point(setting.layout02, alpha));
    t03 := translate_point(p, rotate_point(setting.layout03, alpha));
    t04 := translate_point(p, rotate_point(setting.layout04, alpha));
    t05 := translate_point(p, rotate_point(setting.layout05, alpha));
    //find tangent point t00
    lsx := sqrt(sqr(distance_between_two_points(t00, t03))-sqr(setting.radius));
    c0  := circle_by_center_and_radius(setting.layout00, setting.radius);
    c3  := circle_by_center_and_radius(t03, lsx);
    if intersection_of_two_circles(c0, c3, s00, sxx) = 0 then
      raise exception.create('intersection_of_two_circles [c0c3]');
    lsx := lsx + get_line_angle(line_by_two_points(s00, t00))*setting.radius;
    //find tangent point t01
    ldx := sqrt(sqr(distance_between_two_points(t01, t04))-sqr(setting.radius));
    c1  := circle_by_center_and_radius(setting.layout01, setting.radius);
    c4  := circle_by_center_and_radius(t04, ldx);
    if intersection_of_two_circles(c1, c4, s01, sxx) = 0 then
      raise exception.create('intersection_of_two_circles [c1c4]');
    ldx := ldx + (pi-get_line_angle(line_by_two_points(s01, t01)))*setting.radius;
    //find intersection point
    t06 := intersection_of_two_lines(line_by_two_points(s00, t03),
                                     line_by_two_points(s01, t04));
    if (t06.x - t05.x) < -0.000001 then
      alphahi := alpha
    else
      if (t06.x - t05.x) > +0.000001 then
        alphalo := alpha
      else
        break;
  until false;
  // optimized
  m0 := round(setting.mode * (lsx/setting.ratio));
  m1 := round(setting.mode * (ldx/setting.ratio));
  if enabledebug then
  begin
    writeln(format('OPTIMIZE::ALPHA  = %12.5f',              [radtodeg(alpha)]));
    writeln(format('OPTIMIZE::P02.X  = %12.5f  P02.Y = %12.5f', [t02.x, t02.y]));
    writeln(format('OPTIMIZE::P03.X  = %12.5f  P03.Y = %12.5f', [t03.x, t03.y]));
    writeln(format('OPTIMIZE::P04.X  = %12.5f  P04.Y = %12.5f', [t04.x, t04.y]));
    writeln(format('OPTIMIZE::P05.X  = %12.5f  P05.Y = %12.5f', [t05.x, t05.y]));
    writeln(format('OPTIMIZE::P06.X  = %12.5f  P06.Y = %12.5f', [t06.x, t06.y]));
    writeln(format('OPTIMIZE::LSX    = %12.5f', [lsx]));
    writeln(format('OPTIMIZE::LDX    = %12.5f', [ldx]));
    writeln(format('OPTIMIZE::CNT.0  = %12.5u', [m0]));
    writeln(format('OPTIMIZE::CNT.1  = %12.5u', [m1]));
    readln;
  end;
end;

procedure load_paths(paths: tvppaths; vec: tvvectorialdocument);
var
    i, j: longint;
  entity: tventity;
    page: tvpage;
    path: tvppath;
begin
  paths.clear;
  for i := 0 to vec.getpagecount - 1 do
  begin
    page := vec.getpageasvectorial(i);
    for j := 0 to page.getentitiescount - 1 do
    begin
      path   := nil;
      entity := page.getentity(j);
      if entity is tvcircle then
      begin
        path := interpolate_circle(tvcircle(entity))
      end else
      if entity is tvcirculararc then
      begin
        path := interpolate_circlearc(tvcirculararc(entity))
      end else
      if entity is tpath then
      begin
        path := interpolate_path(tpath(entity))
      end else
      begin
        if enabledebug then
          writeln(entity.classname);
      end;

      if assigned(path) then
        paths.add(path);
    end;
  end;
  paths.createtoolpath;
  paths.zerocenter;
end;

procedure optimize_paths(paths: tvppaths; offsetx, offsety, midx, midy, hmax, wmax: double);
var
  i, j: longint;
  path: tvppath;
   pos: tvpposition;
begin
  if enabledebug then
  begin
    writeln(format(' EXECUTE::OFF-X  = %12.5f', [offsetx]));
    writeln(format(' EXECUTE::OFF-Y  = %12.5f', [offsety]));
    writeln(format(' EXECUTE::MID-X  = %12.5f', [midx   ]));
    writeln(format(' EXECUTE::MID-Y  = %12.5f', [midy   ]));
    writeln(format(' EXECUTE::MAX-H  = %12.5f', [hmax   ]));
    writeln(format(' EXECUTE::MAX-W  = %12.5f', [wmax   ]));
  end;

  for i := 0 to paths.count - 1 do
  begin
    path := paths.item[i];
    for j := 0 to path.count - 1 do
    begin
      pos := path.item[j];
      pos.pp.x := pos.p.x + offsetx + midx;
      pos.pp.y := pos.p.y + offsety + midy;
      optimize_point(pos.pp, pos.m0, pos.m1);

      pos.c    := true;
    end;
  end;

end;



end.

