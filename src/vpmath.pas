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
    z: single;
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
    center:   tvppoint;
    radiusx:  single;
    radiusy:  single;
      angle:  single;
  end;

function translate_point(const p, cc: tvppoint): tvppoint;
function line_by_two_points(const p0, p1: tvppoint): tvplineimp;
function rotate_point(const p: tvppoint; const alpha: single): tvppoint;
function distance_between_two_points(const p0, p1: tvppoint): single;
function intersection_of_two_lines(const l0, l1: tvplineimp): tvppoint;
function circle_by_three_points(const p0, p1, p2: tvppoint): tvpcircleimp;
function circle_by_center_and_radius(const cc: tvppoint; radius: double): tvpcircleimp;
function intersection_of_two_circles(const c0, c1: tvpcircleimp; var p1, p2: tvppoint): longint;

function len_segment(segment: pvpline): single;

function get_angle(const line: tvplineimp): single;

var
  enabledebug: boolean = false;

implementation

function translate_point(const p, cc: tvppoint): tvppoint;
begin
  result.x := p.x + cc.x;
  result.y := p.y + cc.y;
end;

function line_by_two_points(const p0, p1: tvppoint): tvplineimp;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y -(p1.y - p0.y) * p0.x;
end;

function rotate_point(const p: tvppoint; const alpha: single): tvppoint;
var
  sinus, cosinus: double;
begin
  sincos(alpha, sinus, cosinus);
  result.x := p.x * cosinus - p.y *   sinus;
  result.y := p.x *   sinus + p.y * cosinus;
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

function get_angle(const line: tvplineimp): single;
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

function len_segment(segment: pvpline): single;
begin
  result := distance_between_two_points(segment^.p0, segment^.p1);
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

