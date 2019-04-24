{
  Description: vPlot math unit.

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

unit vpmath;

{$mode objfpc}

interface

uses
  classes, sysutils, math;

type
  pvppoint = ^tvppoint;
  tvppoint = packed record
    x: single;
    y: single;
  end;

  pvpline = ^tvpline;
  tvpline = packed record
    p0: tvppoint;
    p1: tvppoint;
  end;

  pvplineimp = ^tvplineimp;
  tvplineimp = packed record
    a: single;
    b: single;
    c: single;
  end;

  pvpcircle = ^tvpcircle;
  tvpcircle = packed record
    center: tvppoint;
    radius: single;
  end;

  pvpcircleimp = ^tvpcircleimp;
  tvpcircleimp = packed record
    a: single;
    b: single;
    c: single;
  end;

  pvpcirclearc = ^tvpcirclearc;
  tvpcirclearc = packed record
    center:     tvppoint;
    startangle: single;
    endangle:   single;
    radius:     single;
  end;

  pvpellipse = ^tvpellipse;
  tvpellipse = packed record
    center:  tvppoint;
    radiusx: single;
    radiusy: single;
      angle: single;
  end;

  pvppolygonal = ^tvppolygonal;
  tvppolygonal = array of tvppoint;

// MOVE
procedure move(var point:     tvppoint;     dx, dy: single);
procedure move(var line:      tvpline;      dx, dy: single);
procedure move(var circle:    tvpcircle;    dx, dy: single);
procedure move(var circlearc: tvpcirclearc; dx, dy: single);
procedure move(var polygonal: tvppolygonal; dx, dy: single);

// ROTATE
procedure rotate(var point:     tvppoint;     angle: single);
procedure rotate(var line:      tvpline;      angle: single);
procedure rotate(var circle:    tvpcircle;    angle: single);
procedure rotate(var circlearc: tvpcirclearc; angle: single);
procedure rotate(var polygonal: tvppolygonal; angle: single);

// SCALE
procedure scale(var point:     tvppoint;     factor: single);
procedure scale(var line:      tvpline;      factor: single);
procedure scale(var circle:    tvpcircle;    factor: single);
procedure scale(var circlearc: tvpcirclearc; factor: single);
procedure scale(var polygonal: tvppolygonal; factor: single);

// MIRROR X
procedure mirrorx(var point:     tvppoint    );
procedure mirrorx(var line:      tvpline     );
procedure mirrorx(var circle:    tvpcircle   );
procedure mirrorx(var circlearc: tvpcirclearc);
procedure mirrorx(var polygonal: tvppolygonal);

// MIRROR Y
procedure mirrory(var point:     tvppoint    );
procedure mirrory(var line:      tvpline     );
procedure mirrory(var circle:    tvpcircle   );
procedure mirrory(var circlearc: tvpcirclearc);
procedure mirrory(var polygonal: tvppolygonal);

// INVERT
procedure invert(var line:      tvpline     );
procedure invert(var circle:    tvpcircle   );
procedure invert(var circlearc: tvpcirclearc);
procedure invert(var polygonal: tvppolygonal);

// TRANSLATE
procedure translate(var point: tvppoint; const cc: tvppoint);

// LENGTH
function length(const line:      tvpline     ): single;
function length(const circle:    tvpcircle   ): single;
function length(const circlearc: tvpcirclearc): single;
function length(const path:      tvppolygonal     ): single;

// ANGLE
function angle(const line: tvplineimp): single;

// INTERPOLATE
procedure interpolate(const line:      tvpline;      var path: tvppolygonal; value: single);
procedure interpolate(const circle:    tvpcircle;    var path: tvppolygonal; value: single);
procedure interpolate(const circlearc: tvpcirclearc; var path: tvppolygonal; value: single);
procedure interpolate(const polygonal: tvppolygonal; var path: tvppolygonal; value: single);

// ---
function line_by_two_points(const p0, p1: tvppoint): tvplineimp;
function distance_between_two_points(const p0, p1: tvppoint): single;
function intersection_of_two_lines(const l0, l1: tvplineimp): tvppoint;
function circle_by_three_points(const p0, p1, p2: tvppoint): tvpcircleimp;
function circle_by_center_and_radius(const cc: tvppoint; radius: double): tvpcircleimp;
function circlearc_by_three_points(cc, p0, p1: tvppoint): tvpcirclearc;
function intersection_of_two_circles(const c0, c1: tvpcircleimp; var p1, p2: tvppoint): longint;

var
  enabledebug: boolean = false;

implementation

// MOVE

procedure move(var point: tvppoint; dx, dy: single);
begin
  point.x := point.x + dx;
  point.y := point.y + dy;
end;

procedure move(var line: tvpline; dx, dy: single);
begin
  move(line.p0, dx, dy);
  move(line.p1, dx, dy);
end;

procedure move(var circle: tvpcircle; dx, dy: single);
begin
  move(circle.center, dx, dy);
end;

procedure move(var circlearc: tvpcirclearc; dx, dy: single);
begin
  move(circlearc.center, dx, dy);
end;

procedure move(var polygonal: tvppolygonal; dx, dy: single);
var
  i: longint;
begin
  for i := 0 to high(polygonal) do
  begin
    move(polygonal[i], dx, dy);
  end;
end;

// ROTATE

procedure rotate(var point: tvppoint; angle: single);
var
  px, py: single;
  sn, cs: single;
begin
  sincos(angle, sn, cs);
  begin
    px := point.x * cs - point.y * sn;
    py := point.x * sn + point.y * cs;
  end;
  point.x := px;
  point.y := py;;
end;

procedure rotate(var line: tvpline; angle: single);
begin
  rotate(line.p0, angle);
  rotate(line.p1, angle);
end;

procedure rotate(var circle: tvpcircle; angle: single);
begin
  rotate(circle.center, angle);
end;

procedure rotate(var circlearc: tvpcirclearc; angle: single);
begin
  rotate(circlearc.center, angle);
  circlearc.startangle := circlearc.startangle + angle;
  circlearc.endangle   := circlearc.endangle   + angle;
end;

procedure rotate(var polygonal: tvppolygonal; angle: single);
var
  i: longint;
begin
  for i := 0 to high(polygonal) do
  begin
    rotate(polygonal[i], angle);
  end;
end;

// SCALE

procedure scale(var point: tvppoint; factor: single);
begin
  point.x := point.x * factor;
  point.y := point.y * factor;
end;

procedure scale(var line: tvpline; factor: single);
begin
  scale(line.p0, factor);
  scale(line.p1, factor);
end;

procedure scale(var circle: tvpcircle; factor: single);
begin
  scale(circle.center, factor);
  circle.radius := circle.radius * factor;
end;

procedure scale(var circlearc: tvpcirclearc; factor: single);
begin
  scale(circlearc.center, factor);
  circlearc.radius := circlearc.radius * factor;
end;

procedure scale(var polygonal: tvppolygonal; factor: single);
var
  i: longint;
begin
  for i := 0 to high(polygonal) do
  begin
    scale(polygonal[i], factor);
  end;
end;

// MIRROR X
procedure mirrorx(var point: tvppoint);
begin
  point.y := -point.y;
end;

procedure mirrorx(var line: tvpline);
begin
  mirrorx(line.p0);
  mirrorx(line.p1);
end;

procedure mirrorx(var circle: tvpcircle);
begin
  mirrorx(circle.center);
end;

procedure mirrorx(var circlearc: tvpcirclearc);
begin
  mirrorx(circlearc.center);
  circlearc.startangle := -circlearc.startangle + 360;
  circlearc.endangle   := -circlearc.endangle   + 360;
end;

procedure mirrorx(var polygonal: tvppolygonal);
var
  i: longint;
begin
  for i := 0 to high(polygonal) do
  begin
    mirrorx(polygonal[i]);
  end;
end;

// MIRROR Y
procedure mirrory(var point: tvppoint);
begin
  point.x := -point.x;
end;

procedure mirrory(var line: tvpline);
begin
  mirrory(line.p0);
  mirrory(line.p1);
end;

procedure mirrory(var circle: tvpcircle);
begin
  mirrory(circle.center);
end;

procedure mirrory(var circlearc: tvpcirclearc);
begin
  mirrory(circlearc.center);
  circlearc.startangle := -circlearc.startangle + 180;
  circlearc.endangle   := -circlearc.endangle   + 180;
end;

procedure mirrory(var polygonal: tvppolygonal);
var
  i: longint;
begin
  for i := 0 to high(polygonal) do
  begin
    mirrory(polygonal[i]);
  end;
end;

// INVERT

procedure invert(var line: tvpline);
var
  t: tvppoint;
begin
  t       := line.p0;
  line.p0 := line.p1;
  line.p1 := t;
end;

procedure invert(var circle: tvpcircle);
begin
  // nothing to do
end;

procedure invert(var circlearc: tvpcirclearc);
var
  t: single;
begin
  t                    := circlearc.startangle;
  circlearc.startangle := circlearc.endangle;
  circlearc.endangle   := t;
end;

procedure invert(var polygonal: tvppolygonal);
var
  i, j: longint;
     t: tvppolygonal;
begin
  j := high(polygonal);

  setlength(t, j + 1);
  for i := 0 to high(polygonal) do t[i] := polygonal[j - i];
  for i := 0 to high(polygonal) do t[i] := polygonal[    i];
  setlength(t, 0);
end;

// TRANSLATE

procedure translate(var point: tvppoint; const cc: tvppoint);
begin
  point.x := point.x + cc.x;
  point.y := point.y + cc.y;
end;

// LENGTH

function length(const line: tvpline): single;
begin
  result := distance_between_two_points(line.p0, line.p1);
end;

function length(const circle: tvpcircle): single;
const
  sweep = 2*pi;
begin
  result := sweep*circle.radius;
end;

function length(const circlearc: tvpcirclearc): single;
var
  sweep: single;
begin
  sweep  := degtorad(abs(circlearc.endangle-circlearc.startangle));
  result := sweep*circlearc.radius;
end;

function length(const path: tvppolygonal): single;
var
  i: longint;
begin
  result := 0;
  for i := 1 to high(path) do
  begin
    result := result + distance_between_two_points(path[i-1], path[i]);
  end;
end;

// ANGLE

function angle(const line: tvplineimp): single;
begin
  if line.b = 0 then
  begin
    if line.a > 0 then
      result := +pi / 2
    else
      result := -pi / 2;
  end else
    result := arctan2(line.a, -line.b);
end;

// INTERPOLATE

procedure interpolate(const line: tvpline; var path: tvppolygonal; value: single);
var
  dx, dy: double;
   i,  j: longint;
begin
   j := max(1, round(distance_between_two_points(line.p0, line.p1)/value));
  dx := (line.p1.x-line.p0.x)/j;
  dy := (line.p1.y-line.p0.y)/j;
  setlength(path, j+1);
  for i := 0 to j do
  begin
    path[i].x := i*dx;
    path[i].y := i*dy;
    translate(path[i], line.p0);
  end;
end;

procedure interpolate(const circle: tvpcircle; var path: tvppolygonal; value: single);
var
  i, j: longint;
    ds: single;
    dz: boolean;
begin
   j := max(1, round(length(circle)/value));
  ds := (2*pi)/j;

  setlength(path, j+1);
  for i := 0 to j do
  begin
    path[i].x := circle.radius;
    path[i].y := 0.0;
    rotate(path[i], i*ds);
    translate(path[i], circle.center);
  end;
end;

procedure interpolate(const circlearc: tvpcirclearc; var path: tvppolygonal; value: single);
var
  i, j: longint;
    ds: single;
begin
   j := max(1, round(length(circlearc)/value));
  ds := (circlearc.endangle-circlearc.startangle)/j;

  setlength(path, j+1);
  for i := 0 to j do
  begin
    path[i].x := circlearc.radius;
    path[i].y := 0.0;
    rotate(path[i], degtorad(circlearc.startangle+(i*ds)));
    translate(path[i], circlearc.center);
  end;
end;

procedure interpolate(const polygonal: tvppolygonal; var path: tvppolygonal; value: single);
var
   i, j: longint;
  aline: tvpline;
  alist: tfplist;
  apath: tvppolygonal;
     ap: pvppoint;
begin
  alist := tfplist.create;

  new(ap);
  alist.add(ap);
  ap^ := polygonal[0];

  for i := 0 to system.length(polygonal) - 2 do
  begin
    aline.p0 := polygonal[i  ];
    aline.p1 := polygonal[i+1];

    interpolate(aline, apath, value);
    for j := 1 to high(apath) do
    begin
      new(ap);
      alist.add(ap);
      ap^ := apath[j];
    end;
  end;

  setlength(path, alist.count);
  for i := 0 to alist.count -1 do
  begin
    path[i] := pvppoint(alist[i])^;
    dispose(pvppoint(alist[i]));
  end;
  alist.destroy;
end;

// ---

function line_by_two_points(const p0, p1: tvppoint): tvplineimp;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y -(p1.y - p0.y) * p0.x;
end;

function distance_between_two_points(const p0, p1: tvppoint): single;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function intersection_of_two_lines(const l0, l1: tvplineimp): tvppoint;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    result.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    result.y := (-l0.c - l0.a * result.x) / (l0.b);
  end else
    raise exception.create('intersection_of_two_lines exception');
end;

function circle_by_three_points(const p0, p1, p2: tvppoint): tvpcircleimp;
begin
   raise exception.create('circle_by_three_points exception');
end;

function circle_by_center_and_radius(const cc: tvppoint; radius: double): tvpcircleimp;
begin
  result.a :=  -2*cc.x;
  result.b :=  -2*cc.y;
  result.c := sqr(cc.x)+
              sqr(cc.y)-
              sqr(radius);
end;

function circlearc_by_three_points(cc, p0, p1: tvppoint): tvpcirclearc;
begin




end;

function intersection_of_two_circles(const c0, c1: tvpcircleimp; var p1, p2: tvppoint): longint;
var
  aa, bb, cc, dd: double;
begin
  aa := 1+sqr((c0.b-c1.b)/(c1.a-c0.a));
  bb := 2*(c0.b-c1.b)/(c1.a-c0.a)*(c0.c-c1.c)/(c1.a-c0.a)+c1.a*(c0.b-c1.b)/(c1.a-c0.a)+c1.b;
  cc := c1.a*(c0.c-c1.c)/(c1.a-c0.a)+sqr((c0.c-c1.c)/(c1.a-c0.a))+c1.c;
  dd := sqr(bb)-4*aa*cc;

  if dd > 0 then
  begin
    result := 2;
    p1.y   := (-bb-sqrt(dd))/(2*aa);
    p2.y   := (-bb+sqrt(dd))/(2*aa);
    p1.x   := ((c0.c-c1.c)+(c0.b-c1.b)*p1.y)/(c1.a-c0.a);
    p2.x   := ((c0.c-c1.c)+(c0.b-c1.b)*p2.y)/(c1.a-c0.a);
  end else
    if dd = 0 then
    begin
      result := 1;
      p1.y   := -bb;
      p1.x   := ((c0.c-c1.c)+(c0.b-c1.b)*p1.y)/(c1.a-c0.a);
      p2     := p1;
    end else
      result := 0;
end;

function intersection_of_line_and_circles(const l0: tvplineimp; const c1: tvpcircleimp; var p1, p2: tvppoint): longint;
var
  aa, bb, cc, dd: double;
begin
  aa := 1+sqr((l0.b-c1.b)/(c1.a-l0.a));
  bb := 2*(l0.b-c1.b)/(c1.a-l0.a)*(l0.c-c1.c)/(c1.a-l0.a)+c1.a*(l0.b-c1.b)/(c1.a-l0.a)+c1.b;
  cc := c1.a*(l0.c-c1.c)/(c1.a-l0.a)+sqr((l0.c-c1.c)/(c1.a-l0.a))+c1.c;
  dd := sqr(bb)-4*aa*cc;

  if dd > 0 then
  begin
    result := 2;
    p1.y   := (-bb-sqrt(dd))/(2*aa);
    p2.y   := (-bb+sqrt(dd))/(2*aa);
    p1.x   := ((l0.c-c1.c)+(l0.b-c1.b)*p1.y)/(c1.a-l0.a);
    p2.x   := ((l0.c-c1.c)+(l0.b-c1.b)*p2.y)/(c1.a-l0.a);
  end else
    if dd = 0 then
    begin
      result := 1;
      p1.y   := -bb;
      p1.x   := ((l0.c-c1.c)+(l0.b-c1.b)*p1.y)/(c1.a-l0.a);
      p2     := p1;
    end else
      result := 0;
end;

// init unit

procedure initializedebug;
begin
  if paramcount = 1 then
  begin
    enabledebug := (paramstr(1) =  '-debug') or
                   (paramstr(1) = '--debug');
    if enabledebug then
      writeln('VPLOTTER::START-DEBUGGER');
  end;
end;

procedure finalizedebug;
begin
  if enabledebug then
    writeln('VPLOTTER::END-DEBUGGER');
end;


initialization

  initializedebug;

finalization

  finalizedebug;

end.

