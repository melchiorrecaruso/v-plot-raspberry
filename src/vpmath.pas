{
  Description: vPlot common unit.

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

unit vpmath;

{$mode objfpc}

interface

uses
  classes, sysutils;

type
  tvppoint = packed record
    x: double;
    y: double;
  end;
  pvppoint = ^tvppoint;

  tvpline = packed record
    a: double;
    b: double;
    c: double;
  end;
  pvpline = ^tvpline;

  tvpcircle = packed record
    a: double;
    b: double;
    c: double;
  end;
  pvpcircle = ^tvpcircle;


function translate_point(const p, cc: tvppoint): tvppoint;
function line_by_two_points(const p0, p1: tvppoint): tvpline;
function rotate_point(const p: tvppoint; const alpha: double): tvppoint;
function distance_between_two_points(const p0, p1: tvppoint): double;
function intersection_of_two_lines(const l0, l1: tvpline): tvppoint;
function circle_by_three_points(const p0, p1, p2: tvppoint): tvpcircle;
function circle_by_center_and_radius(const cc: tvppoint; radius: double): tvpcircle;

function intersection_of_two_circles(const c0, c1: tvpcircle; var p1, p2: tvppoint): longint;

function get_line_angle(const line: tvpline): double;


implementation

uses
  math;

function translate_point(const p, cc: tvppoint): tvppoint;
begin
  result.x := p.x + cc.x;
  result.y := p.y + cc.y;
end;

function line_by_two_points(const p0, p1: tvppoint): tvpline;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y -(p1.y - p0.y) * p0.x;
end;

function rotate_point(const p: tvppoint; const alpha: double): tvppoint;
var
  sinus, cosinus: double;
begin
  sincos(alpha, sinus, cosinus);
  result.x := p.x * cosinus - p.y *   sinus;
  result.y := p.x *   sinus + p.y * cosinus;
end;

function distance_between_two_points(const p0, p1: tvppoint): double;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function intersection_of_two_lines(const l0, l1: tvpline): tvppoint;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    result.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    result.y := (-l0.c - l0.a * result.x) / (l0.b);
  end else
    raise exception.create('intersection_of_two_lines exception');
end;

function circle_by_three_points(const p0, p1, p2: tvppoint): tvpcircle;
begin
   raise exception.create('circle_by_three_points exception');
end;

function circle_by_center_and_radius(const cc: tvppoint; radius: double): tvpcircle;
begin
  result.a :=  -2*cc.x;
  result.b :=  -2*cc.y;
  result.c := sqr(cc.x)+
              sqr(cc.y)-
              sqr(radius);
end;

function intersection_of_two_circles(const c0, c1: tvpcircle; var p1, p2: tvppoint): longint;
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

function intersection_of_line_and_circles(const l0: tvpline; const c1: tvpcircle; var p1, p2: tvppoint): longint;
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

function get_line_angle(const line: tvpline): double;
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

end.

