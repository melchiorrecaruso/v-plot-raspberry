{
  Description: vPlot list class.

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

unit vpcommon;

{$mode objfpc}

interface

uses
  classes, sysutils;

type
  tvppoint = packed record
    x: double;
    y: double;
    z: double;
  end;

  pvppoint = ^tvppoint;

  tvpline = packed record
    a: double;
    b: double;
    c: double;
  end;

  tvppath = class(tobject)
  private
    flist: tlist;
    function getcount: longint;
    function get(index: longint): pvppoint;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const point: tvppoint);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   invert;
    function    firstis(point: pvppoint): boolean;
    function    lastis (point: pvppoint): boolean;
  public
    property count:                longint  read getcount;
    property item[index: longint]: pvppoint read get;
  end;

  tvppaths = class(tobject)
  private
    flist:   tlist;
    fheight: double;
    fwidth:  double;
    function getcount:  longint;
    function get(index: longint): tvppath;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const path: tvppath);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   update;
  public
    property height:               double  read fheight;
    property width:                double  read fwidth;
    property count:                longint read getcount;
    property item[index: longint]: tvppath read get;
  end;


implementation


uses
  math;

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
  dispose(pvppoint(flist[index]));
  flist.delete(index);
end;

procedure tvppath.clear;
begin
  while flist.count > 0 do
  begin
    dispose(pvppoint(flist[0]));
    flist.delete(0);
  end;
end;

procedure tvppath.add(const point: tvppoint);
var
  p: pvppoint;
begin
  new(p);
  p^.x := point.x;
  p^.y := point.y;
  p^.z := point.z;
  flist.add(p);
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

function tvppath.firstis(point: pvppoint): boolean;
var
  dx, dy: double;
begin
  result := false;
  if flist.count > 0 then
  begin
    dx := abs(point^.x - pvppoint(flist.first)^.x);
    dy := abs(point^.y - pvppoint(flist.first)^.y);
    result := (dx < 0.1) and (dy < 0.1);
  end;
end;

function tvppath.lastis(point: pvppoint): boolean;
var
  dx, dy: double;
begin
  result := false;
  if flist.count > 0 then
  begin
    dx := abs(point^.x - pvppoint(flist.last)^.x);
    dy := abs(point^.y - pvppoint(flist.last)^.y);
    result := (dx < 0.1) and (dy < 0.1);
  end;
end;

function tvppath.getcount: longint;
begin
  result := flist.count;
end;

function tvppath.get(index: longint): pvppoint;
begin
  result := pvppoint(flist[index]);
end;

// tvppaths

constructor tvppaths.create;
begin
  inherited create;
  flist   := tlist.create;
  fheight := 0;
  fwidth  := 0;
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
  while flist.count > 0 do
  begin
    tvppath(flist[0]).destroy;
    flist.delete(0);
  end;
end;

procedure tvppaths.add(const path: tvppath);
begin
  if path.count > 0 then
  begin
    flist.add(path);
  end;
end;

procedure tvppaths.update;
var
     i, j: longint;
        p: pvppoint;
     xmin: double;
     xmax: double;
     ymin: double;
     ymax: double;
  offsetx: double;
  offsety: double;
     path: tvppath;
    point: pvppoint;
    list1:  tlist;
    list2:  tlist;

begin
  xmin  := + maxint;
  xmax  := - maxint;
  ymin  := + maxint;
  ymax  := - maxint;

  for i := 0 to flist.count - 1 do
  begin
    path := get(i);
    for j := 0 to path.count - 1 do
    begin
      point := path.item[j];
       xmin := min(xmin, point^.x);
       xmax := max(xmax, point^.x);
       ymin := min(ymin, point^.y);
       ymax := max(ymax, point^.y);
    end;
  end;
  offsetx := - (xmin + xmax) / 2;
  offsety := - (ymin + ymax) / 2;

  for i := 0 to flist.count - 1 do
  begin
    path := get(i);
    for j := 0 to path.count - 1 do
    begin
      point    := path.item[j];
      point^.x := point^.x + offsetx;
      point^.y := point^.y + offsety;
    end;
  end;

  fheight := ymax - ymin;
  fwidth  := xmax - xmin;


  // sort path

  list1 := tlist.create;
  list2 := tlist.create;
  for i := 0 to flist.count - 1 do
    list1.add(flist[i]);

  while list1.count > 0 do
  begin
    path := tvppath(list1[0]);
    list2.add(path);
    list1.delete(0);

    repeat
      j := -1;

      for i := 0 to list1.count - 1 do
        if tvppath(list1[i]).firstis(path.get(path.count - 1)) then
        begin
          j := i;
          break;
        end else
        if tvppath(list1[i]).lastis(path.get(path.count - 1)) then
        begin
          tvppath(list1[i]).invert;
          j := i;
          break;
        end;

      if j <> -1 then
      begin
        path := tvppath(list1[j]);
        list2.add(path);
        list1.delete(j);
      end;

    until j = -1;

  end;

  for i := 0 to flist.count - 1 do
    flist[i] := list2[i];

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

end.

