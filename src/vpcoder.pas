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

  procedure vec2paths(vec: tvvectorialdocument; paths: tvppaths);

implementation

uses
  math, sysutils, vpwave;

const
  min_len = 0.15/2;

// toolpath routines

function interpolate_line(const p0, p1: tvppoint): tvppath;
var
  dx, dy: double;
  i, len: longint;
       p: tvppoint;
begin
  len := max(1, round(distance_between_two_points(p0, p1) / min_len));
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

  len := max(1, round(abs(sweep) * entity.radius / min_len));

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

  len := max(1, round(abs(sweep) * entity.radius / min_len));

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

        len := max(1, round(distance_between_two_points(p0, p1) / min_len));
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

procedure vec2paths(vec: tvvectorialdocument; paths: tvppaths);
var
    i, j: longint;
  entity: tventity;
    page: tvpage;
    path: tvppath;
begin
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

end.

