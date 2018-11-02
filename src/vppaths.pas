{
  Description: vPlot paths class.

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

unit vppaths;

{$mode objfpc}

interface

uses
  classes, fpvectorial, vpmath, vpsetting;

type
  tvpposition = class(tobject)
  private
    fb: boolean;
    fx: double;
    fy: double;
  public
    constructor create(p: tvppoint);
  published
    property b: boolean read fb write fb;
    property x: double  read fx;
    property y: double  read fy;
  end;

  tvppath = class(tobject)
  private
    flist:   tlist;
    function getfirst: tvpposition;
    function getlast:  tvpposition;
    function getcount: longint;
    function get(index: longint): tvpposition;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const p: tvppoint);
    procedure   insert(index: longint; const p: tvppoint);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   invert;
    function    getlen: double;
  public
    property item[index: longint]: tvpposition read get;
    property count: longint read getcount;
  end;

  tvppaths = class(tobject)
  private
    flist:   tlist;
    function getcount:  longint;
    function get(index: longint): tvppath;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(path: tvppath);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   createtoolpath;
    procedure   zerocenter;
  public
    property item[index: longint]: tvppath read get;
    property count: longint read getcount;
  end;

  procedure vec2paths(vec: tvvectorialdocument; paths: tvppaths);

implementation

uses
  math, sysutils;

const
  smallest = 0.05;

// internal toolpath routines

function interpolate_line(const p0, p1: tvppoint): tvppath;
var
  dx, dy: double;
  i, len: longint;
       p: tvppoint;
begin
  len := max(1, round(distance_between_two_points(p0, p1)/(1.2/setting.mode)));
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

  len := max(1, round(abs(sweep) * entity.radius/(1.2/setting.mode)));

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

  len := max(1, round(abs(sweep)*entity.radius/(1.2/setting.mode)));

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

        len := max(1, round(distance_between_two_points(p0, p1)/(1.2/setting.mode)));
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

// internal commont routines

function distance_between_two_position(p0, p1: tvpposition): double;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function compare_position(pos0, pos1: tvpposition): boolean;
begin
  result := abs(pos1.x - pos0.x) < smallest;
  if result then
  begin
    result := abs(pos1.y - pos0.y) < smallest;
  end;
end;

function compare_path(path0, path1: pointer): longint;
begin
  result := round(tvppath(path1).getlen - tvppath(path0).getlen);
end;

function walk_back(pos0: tvpposition; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if compare_position(pos0, tvppath(list[i]).getlast) then
    begin
      result := i;
      exit;
    end else
    if compare_position(pos0, tvppath(list[i]).getfirst) then
    begin
      tvppath(list[i]).invert;
      result := i;
      exit;
    end;
end;

function walk_next(pos0: tvpposition; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if compare_position(pos0, tvppath(list[i]).getfirst) then
    begin
      result := i;
      exit;
    end else
    if compare_position(pos0, tvppath(list[i]).getlast) then
    begin
      tvppath(list[i]).invert;
      result := i;
      exit;
    end;
end;

function walk_near(path: tvppath; list: tlist): longint;
var
     i: longint;
  curr: double;
  best: double;
begin
  result := 0;
  if path <> nil then
  begin
    best := distance_between_two_position(
      path.getlast, tvppath(list[0]).getfirst);

    for i := 1 to list.count - 1 do
    begin
      curr := distance_between_two_position(
        path.getlast, tvppath(list[i]).getfirst);

      if curr < best then
      begin
        best   := curr;
        result := i;
      end else
      begin
        curr := distance_between_two_position(
          path.getlast, tvppath(list[i]).getlast);

        if curr < best then
        begin
          best   := curr;
          result := i;

          tvppath(list[i]).invert;
        end;
      end;
    end;
  end;
end;

function is_closed(path: tvppath): boolean;
begin
  result := false;
  if path.count > 1 then
  begin
    result := compare_position(path.getfirst, path.getlast);
  end;
end;

// tvpposition

constructor tvpposition.create(p: tvppoint);
begin
  inherited create;
  fb := true;
  fx := p.x;
  fy := p.y;
end;

// tvppath

constructor tvppath.create;
begin
  inherited create;
  flist := tlist.create;
end;

destructor tvppath.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvppath.delete(index: longint);
begin
  tvpposition(flist[index]).destroy;
  flist.delete(index);
end;

procedure tvppath.clear;
begin
  while flist.count > 0 do delete(0);
end;

procedure tvppath.add(const p: tvppoint);
begin
  flist.add(tvpposition.create(p));
end;

procedure tvppath.insert(index: longint; const p: tvppoint);
begin
  flist.insert(index, tvpposition.create(p));
end;

procedure tvppath.invert;
var
      i: longint;
  alist: tlist;
begin
  alist := tlist.create;
  for i := flist.count - 1 downto 0 do alist.add(flist[i]);
  for i := flist.count - 1 downto 0 do flist[i] := alist[i];
  alist.destroy;
end;

function tvppath.getlen: double;
var
  i: longint;
begin
  result := 0;
  for i := 1 to flist.count - 1 do
  begin
    result := result +
      distance_between_two_position(
        tvpposition(flist[i    ]),
        tvpposition(flist[i - 1]));
  end;
end;

function tvppath.getfirst: tvpposition;
begin
  if flist.count > 0 then
    result := tvpposition(flist.first)
  else
    result := nil;
end;

function tvppath.getlast: tvpposition;
begin
  if flist.count > 0 then
    result := tvpposition(flist.last)
  else
    result := nil;
end;

function tvppath.getcount: longint;
begin
  result := flist.count;
end;

function tvppath.get(index: longint): tvpposition;
begin
  result := tvpposition(flist[index]);
end;

// tvppaths

constructor tvppaths.create;
begin
  inherited create;
  flist := tlist.create;
end;

destructor tvppaths.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvppaths.delete(index: longint);
begin
  tvppath(flist[index]).destroy;
  flist.delete(index);
end;

procedure tvppaths.clear;
begin
  while flist.count > 0 do delete(0);
end;

procedure tvppaths.add(path: tvppath);
begin
  flist.add(path)
end;

procedure tvppaths.zerocenter;
var
     i, j: longint;
     xmin: double;
     xmax: double;
     ymin: double;
     ymax: double;
  offsetx: double;
  offsety: double;
     path: tvppath;
      pos: tvpposition;
begin
  xmin  := + maxint;
  xmax  := - maxint;
  ymin  := + maxint;
  ymax  := - maxint;
  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
        pos := path.item[j];
       xmin := min(xmin, pos.x);
       xmax := max(xmax, pos.x);
       ymin := min(ymin, pos.y);
       ymax := max(ymax, pos.y);
    end;
  end;
  offsetx := - (xmin + xmax) / 2;
  offsety := - (ymin + ymax) / 2;

  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      pos    := path.item[j];
      pos.fx := pos.fx + offsetx;
      pos.fy := pos.fy + offsety;
    end;
  end;
end;

procedure tvppaths.createtoolpath;
var
      i: longint;
  index: longint;
  list1: tlist;
  list2: tlist;
  list3: tlist;
   path: tvppath;
begin
  list1 := tlist.create;
  list2 := tlist.create;
  list3 := tlist.create;
  for i := 0 to flist.count - 1 do
    list1.add(flist[i]);
  // create toolpath
  path := nil;
  while list1.count > 0 do
  begin
    index := walk_near(path, list1);
    path  := tvppath(list1[index]);
    list1.delete(index);
    list2.add(path);
    if not is_closed(tvppath(flist[i])) then
    begin
      repeat
        index := walk_back(path.getfirst, list1);
        if index <> -1 then
        begin
          path := tvppath(list1[index]);
          list1.delete(index);
          list2.insert(0, path);
        end;
      until index = -1;

      path := tvppath(list2.last);
      repeat
        index := walk_next(path.getlast, list1);
        if index <> -1 then
        begin
          path := tvppath(list1[index]);
          list1.delete(index);
          list2.add(path);
        end;
      until index = -1;
    end;
    // move toolpath
    for i := 0 to list2.count - 1 do
      list3.add(list2[i]);
    list2.clear;
  end;

  for i := 0 to flist.count - 1 do
    flist[i] := list3[i];

  list3.destroy;
  list2.destroy;
  list1.destroy;
end;

function tvppaths.getcount: longint;
begin
  result := flist.count;
end;

function tvppaths.get(index: longint): tvppath;
begin
  result := tvppath(flist[index]);
end;

// vectorial2paths

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
        path := interpolate_circle(tvcircle(entity));
      end else
      if entity is tvcirculararc then
      begin
        path := interpolate_circlearc(tvcirculararc(entity));
      end else
      if entity is tpath then
      begin
        path := interpolate_path(tpath(entity));
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

