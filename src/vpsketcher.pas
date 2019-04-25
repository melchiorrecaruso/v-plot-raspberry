{
  Description: vPlot sketcher class.

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
   fpatternw:  single;
   fpatternbw: longint;
   function getdarkness(x, y, width: longint): longint;
  public
    constructor create(bit: tbgrabitmap);
    destructor destroy; override;
    procedure update(elements: tvpelementlist); virtual abstract;
  public
    property dotsize:   single  read fdotsize   write fdotsize;
    property patternw:  single  read fpatternw  write fpatternw;
    property patternbw: longint read fpatternbw write fpatternbw;
  end;

  tvpsketcher1 = class(tvpsketcher)
  private
    function step1(n, width: single): tvpelementlist; virtual;
  public
    procedure update(elements: tvpelementlist); override;
  end;

  tvpsketcher2 = class(tvpsketcher1)
  private
    function step1(n, width: single): tvpelementlist; override;
    function step2(elements: tvpelementlist; radius: single): tvpelementlist;
  end;

  tvpsketcher3 = class(tvpsketcher1)
  private
    function step1(n, width: single): tvpelementlist; override;
  end;


implementation

// tvpsketcher

constructor tvpsketcher.create(bit: tbgrabitmap);
begin
  inherited create;
  fbit       := bit;
  fdotsize   := 0.5;
  fpatternw  := 10;
  fpatternbw := 10;
end;

destructor tvpsketcher.destroy;
begin
  inherited destroy;
end;

function tvpsketcher.getdarkness(x, y, width: longint): longint;
var
  i: longint;
  j: longint;
  k: longint;
  c: tfpcolor;
begin
  k := 0;
  for j := 0 to width -1 do
    for i := 0 to width -1 do
    begin
      c := fbit.colors[x+i, y+j];
      k := k + c.blue;
      k := k + c.green;
      k := k + c.red;
    end;

  result := round((patternw/dotsize)-(patternw/dotsize)*(k/((3*$FFFF)*width*width)));
end;

// tvpsketcher1

function tvpsketcher1.step1(n, width: single): tvpelementlist;
var
  line: tvpline;
begin
  result := tvpelementlist.create;
  if n > 0 then
  begin
    line.p0.x := 0;
    line.p0.y := 0;
    line.p1.x := width/(n*2);
    line.p1.y := 0;
    result.add(line);

    while line.p1.x < width do
    begin
      if line.p1.x - line.p0.x > 0  then
      begin
        line.p0 := line.p1;
        if line.p1.y = 0 then
          line.p1.y := line.p1.y + width
        else
          line.p1.y := 0
      end else
      begin
        line.p0   := line.p1;
        line.p1.x := line.p1.x + width/n;
        if line.p1.x > width then
          line.p1.x := width;
      end;
      result.add(line);
    end;
  end else
  begin
    line.p0.x := 0;
    line.p0.y := 0;
    line.p1.x := width;
    line.p1.y := 0;
    result.add(line);
  end;
  result.interpolate(0.5);
end;

procedure tvpsketcher1.update(elements: tvpelementlist);
var
  i, j, k: longint;
   aw, ah: longint;
    list1: tvpelementlist;
    list2: tvpelementlist;
       mx: boolean;
begin
  list1 := tvpelementlist.create;
     aw := (fbit.width  div fpatternbw);
     ah := (fbit.height div fpatternbw);
     mx := false;

  j := 0;
  while j < ah do
  begin
    i := 0;
    while i < aw do
    begin
      list2 := step1(getdarkness(
        fpatternbw*i, fpatternbw*j,
        fpatternbw),  fpatternw);

      if mx then
      begin
        list2.mirrorx;
        list2.move(0, patternw);
      end;
      mx := list2.items[list2.count -1].last^.y > 0;

      for k := 0 to list2.count -1 do
      begin
        list2.items[k].move(patternw*i, patternw*j);
        list2.items[k].layer := j;
      end;
      list1.add(list2);
      list2.destroy;
      inc(i, 1);
    end;

    if j mod 2 = 1 then
      list1.invert;
    elements.add(list1);
    inc(j, 1);
  end;
  list1.destroy;
  elements.mirrorx;
  elements.movetoorigin;
  elements.interpolate(0.5);
end;

// tvpsketcher2

function tvpsketcher2.step1(n, width: single): tvpelementlist;
begin
  result := step2(inherited step1(n, width), width/(2*n));
end;

function tvpsketcher2.step2(elements: tvpelementlist; radius: single): tvpelementlist;
var
   i: longint;
  l0: tvpline;
  l1: tvpline;
  a0: tvpcirclearc;
begin
  result := tvpelementlist.create;

  if elements.count = 1 then
  begin
    result.add(elements.extract(0));
  end else
  begin
    l0.p0 := tvpelementline(elements.items[0]).first^;
    l0.p1 := tvpelementline(elements.items[0]).last^;

    for i := 1 to elements.count -1 do
    begin
      l1.p0 := tvpelementline(elements.items[i]).first^;
      l1.p1 := tvpelementline(elements.items[i]).last^;

      if (l0.p1.y = 0) and
         (l1.p1.y > 0) then // left-bottom corner
      begin
        a0.radius     := radius;
        a0.center.x   := l0.p1.x - radius;
        a0.center.y   := l0.p1.y + radius;
        a0.startangle := 270;
        a0.endangle   := 360;
        l0.p1.x       := a0.center.x;
        l1.p0.y       := a0.center.y;
        result.add(a0);
      end else
      if (l0.p1.y > 0) and
         (l1.p1.y > 0) then // left-top corner
      begin
        a0.radius     := radius;
        a0.center.x   := l0.p1.x + radius;
        a0.center.y   := l0.p1.y - radius;
        a0.startangle := 180;
        a0.endangle   :=  90;
        l1.p0.x       := a0.center.x;
        l0.p1.y       := a0.center.y;
        result.add(l0);
        result.add(a0);
      end else
      if (l0.p1.y > 0) and
         (l1.p1.y = 0) then // right-top corner
      begin
        a0.radius     := radius;
        a0.center.x   := l0.p1.x - radius;
        a0.center.y   := l0.p1.y - radius;
        a0.startangle := 90;
        a0.endangle   :=  0;
        l0.p1.x       := a0.center.x;
        l1.p0.y       := a0.center.y;
        result.add(a0);
      end else
      if (l0.p1.y = 0) and
         (l1.p1.y = 0) then // right-bottom corner
      begin
        a0.radius     := radius;
        a0.center.x   := l0.p1.x + radius;
        a0.center.y   := l0.p1.y + radius;
        a0.startangle := 180;
        a0.endangle   := 270;
        l1.p0.x       := a0.center.x;
        l0.p1.y       := a0.center.y;
        result.add(l0);
        result.add(a0);
      end;

      l0 := l1;
    end;
  end;
  elements.destroy;
  result.interpolate(0.5);
end;

// tvpsketcher3

function tvpsketcher3.step1(n, width: single): tvpelementlist;
begin
  result := inherited step1(n, width);
end;

end.

