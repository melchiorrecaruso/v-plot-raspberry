{
  Description: vPlot sketcher form class.

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

unit vpsketcher;

{$mode objfpc}{$H+}

interface

uses
  bgrabitmap, classes, fpimage, sysutils, vpmath, vppaths;

type
  tvpsketcher = class
  private
   fbit:       tbgrabitmap;
   fdotsize:   single;
   fpatternw:  longint;
   fpatternh:  longint;
   fpatternbw: longint;
   fpatternbh: longint;
   function getdarkness(x, y, w, h: longint): longint;
   function getpattern(n, width, height: single): tvppath;
  public
    constructor create(bit: tbgrabitmap);
    destructor destroy; override;
    procedure run(paths: tvppaths);
  public
    property dotsize:   single  read fdotsize   write fdotsize;
    property patternw:  longint read fpatternw  write fpatternw;
    property patternh:  longint read fpatternh  write fpatternh;
    property patternbw: longint read fpatternbw write fpatternbw;
    property patternbh: longint read fpatternbh write fpatternbh;
  end;


implementation


constructor tvpsketcher.create(bit: tbgrabitmap);
begin
  inherited create;
  fbit       := bit;
  fdotsize   := 0.5;
  fpatternw  := 10;
  fpatternh  := 10;
  fpatternbw := 10;
  fpatternbh := 10;
end;

destructor tvpsketcher.destroy;
begin
  inherited destroy;
end;

function tvpsketcher.getdarkness(x, y, w, h: longint): longint;
var
  i: longint;
  j: longint;
  k: longint;
  c: tfpcolor;
begin
  k := 0;
  for j := 0 to h -1 do
    for i := 0 to w -1 do
    begin
      c := fbit.colors[x+i, y+j];
      k := k + c.blue ;
      k := k + c.green;
      k := k + c.red;
      k := k + c.alpha;
    end;

  result := round( (patternw/dotsize) - ((patternw/dotsize) * (k/(4*w*h))/$FFFF ));
  writeln('darkness = ', result);
end;

function tvpsketcher.getpattern(n, width, height: single): tvppath;
var
  p0: tvppoint;
  p1: tvppoint;
begin
  result := tvppath.create;
  if n > 0 then
  begin
    p0.x := 0;
    p0.y := 0;
    p1.x := width/(n*2);
    p1.y := 0;
    interpolate_line(@p0, @p1, result);

    while width > p1.x do
    begin

      if p1.x - p0.x > 0  then
      begin
        p0 := p1;
        if p1.y = 0 then
          p1.y := p1.y + height
        else
          p1.y := 0
      end else
      begin
        p0   := p1;
        p1.x := p1.x + width/n;
        if p1.x > width then
          p1.x := width;
      end;
      interpolate_line(@p0, @p1, result);
    end;
  end else
  begin
    p0.x   := 0;
    p0.y   := 0;
    p1.x   := width;
    p1.y   := 0;
    interpolate_line(@p0, @p1, result);
  end;
end;

procedure tvpsketcher.run(paths: tvppaths);
var
  i, j, k: longint;
   aw, ah: longint;
   mirror: boolean;
    path1: tvppath;
    path2: tvppath;
begin
  aw := (fbit.width  div fpatternbw);
  ah := (fbit.height div fpatternbh);
  mirror := false;

  j := 0;
  while j < ah do
  begin
    path1 := tvppath.create;

    i := 0;
    while i < aw do
    begin
      path2 := getpattern(getdarkness(
        fpatternbw*i, fpatternbh*j,
        fpatternbw,   fpatternbh),
        fpatternw,    fpatternh);

      if path2.count > 0 then
      begin
        if mirror then
          for k := 0 to path2.count -1 do
          begin
            path2.items[k]^.y := -path2.items[k]^.y + patternh;
          end;

        mirror := path2.items[path2.count -1]^.y > 0;
        for k := 0 to path2.count -1 do
        begin
          path2.items[k]^.x := path2.items[k]^.x + patternw*i;
          path2.items[k]^.y := path2.items[k]^.y + patternh*j;
        end;
        path1.copyfrom(path2);
      end;

      path2.destroy;
      inc(i, 1);
    end;
    paths.addpath(path1);
    inc(j, 1);
  end;
  paths.zerocenter;
  paths.mirror(true);
end;

end.

