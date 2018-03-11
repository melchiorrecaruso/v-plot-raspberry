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
    function getfirst: pvppoint;
    function getlast:  pvppoint;
    function getcount: longint;
    function get(index: longint): pvppoint;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const point: tvppoint);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   invert;
    function    isaloop: boolean;
  public
    property count:                longint  read getcount;
    property item[index: longint]: pvppoint read get;
  end;

  tvppaths = class(tobject)
  private
    flist:    tlist;
    fheight:  double;
    fwidth:   double;
    function  getcount:  longint;
    function  get(index: longint): tvppath;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const path: tvppath);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   update;
    procedure zerocenter;
  public
    property height:               double  read fheight;
    property width:                double  read fwidth;
    property count:                longint read getcount;
    property item[index: longint]: tvppath read get;
  end;

  function comparepoint(p1, p2: pvppoint): boolean;


implementation


uses
  math;

const
  toll = 0.25;

function comparepoint(p1, p2: pvppoint): boolean;
begin
  result := abs(p2^.x - p1^.x) < toll;
  if result then
  begin
    result := abs(p2^.y - p1^.y) < toll;
  end;
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

function tvppath.isaloop: boolean;
begin
  result := comparepoint(pvppoint(flist.first), pvppoint(flist.last));
end;

function tvppath.getfirst: pvppoint;
begin
  if flist.count > 0 then
    result := pvppoint(flist.first)
  else
    result := nil;
end;

function tvppath.getlast:  pvppoint;
begin
  if flist.count > 0 then
    result := pvppoint(flist.last)
  else
    result := nil;
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
    point: pvppoint;
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
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      point    := path.item[j];
      point^.x := point^.x + offsetx;
      point^.y := point^.y + offsety;
    end;
  end;
  fheight := ymax - ymin;
  fwidth  := xmax - xmin;
end;

procedure tvppaths.update;
var
       i: longint;
  index1: longint;
  index2: longint;
   list1: tlist;
   list2: tlist;
    path: tvppath;
begin
  writeln('update start');
  list1 := tlist.create;
  list2 := tlist.create;

  //move loop path
  for i := 0 to flist.count - 1 do
    if tvppath(flist[i]).isaloop then
      list2.add(flist[i])
    else
      list1.add(flist[i]);

  // move other path
  while list1.count > 0 do
  begin
    index1 := 0;
    path   := tvppath(list1[index1]);

    writeln('walkback');
    for i := 1 to list1.count - 1 do
    begin
      if comparepoint(path.getfirst, tvppath(list1[i]).getlast) then
      begin
        index1 := i;
        path   := tvppath(list1[index1]);
      end else
      if comparepoint(path.getfirst, tvppath(list1[i]).getfirst) then
      begin
        index1 := i;
        path   := tvppath(list1[index1]);
        path.invert;
      end;
    end;
    list2.add(path);
    list1.delete(index1);

    writeln('walknext');
    repeat
      index2 := -1;
      for i := 0 to list1.count - 1 do
        if comparepoint(path.getlast, tvppath(list1[i]).getfirst) then
        begin
          index2 := i;
          break;
        end else
        if comparepoint(path.getlast, tvppath(list1[i]).getlast) then
        begin
          tvppath(list1[i]).invert;
          index2 := i;
          break;
        end;

      if index2 <> -1 then
      begin
        path := tvppath(list1[index2]);
        list2.add(path);
        list1.delete(index2);
      end;
    until index2 = -1;

  end;

  for i := 0 to flist.count - 1 do
    flist[i] := list2[i];

  list2.destroy;
  list1.destroy;
  writeln('update end');
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

