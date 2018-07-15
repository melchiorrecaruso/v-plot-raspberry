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

unit vpcommon;

{$mode objfpc}

interface

uses
  classes, sysutils, vpmath;

type
  tvpposition = class(tobject)
  public
     c: boolean;
     p: tvppoint;
    pp: tvppoint;
    m0: longint;
    m1: longint;
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
    fheight: double;
    fwidth:  double;
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
    procedure   deletesmallpaths;
  public
    property height:               double  read fheight;
    property width:                double  read fwidth;
    property count:                longint read getcount;
    property item[index: longint]: tvppath read get;
  end;

var
  enabledebug: boolean = false;

implementation

uses
  math;

const
  smallest = 0.05;

function compareposition(pos0, pos1: tvpposition): boolean;
begin
  result := abs(pos1.p.x - pos0.p.x) < smallest;
  if result then
  begin
    result := abs(pos1.p.y - pos0.p.y) < smallest;
  end;
end;

function comparepath(path0, path1: pointer): longint;
begin
  result := round(tvppath(path1).getlen - tvppath(path0).getlen);
end;

function walkback(pos0: tvpposition; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if compareposition(pos0, tvppath(list[i]).getlast) then
    begin
      result := i;
      exit;
    end else
    if compareposition(pos0, tvppath(list[i]).getfirst) then
    begin
      tvppath(list[i]).invert;
      result := i;
      exit;
    end;
end;

function walknext(pos0: tvpposition; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if compareposition(pos0, tvppath(list[i]).getfirst) then
    begin
      result := i;
      exit;
    end else
    if compareposition(pos0, tvppath(list[i]).getlast) then
    begin
      tvppath(list[i]).invert;
      result := i;
      exit;
    end;
end;

function walknear(path: tvppath; list: tlist): longint;
var
     i: longint;
  curr: double;
  best: double;
begin
  result := 0;
  if path <> nil then
  begin
    best := distance_between_two_points(
      path.getlast.p, tvppath(list[0]).getfirst.p);

    for i := 1 to list.count - 1 do
    begin
      curr := distance_between_two_points(
        path.getlast.p, tvppath(list[i]).getfirst.p);

      if curr < best then
      begin
        best   := curr;
        result := i;
      end else
      begin
        curr := distance_between_two_points(
          path.getlast.p, tvppath(list[i]).getlast.p);

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

function isclosed(path: tvppath): boolean;
begin
  result := false;
  if path.count > 1 then
  begin
    result := compareposition(path.getfirst, path.getlast);
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
  tvpposition(flist[index]).destroy;
  flist.delete(index);
end;

procedure tvppath.clear;
begin
  while flist.count > 0 do delete(0);
end;

procedure tvppath.add(const p: tvppoint);
var
  pos: tvpposition;
begin
  pos   := tvpposition.create;
  pos.p := p;
  flist.add(pos);
end;

procedure tvppath.insert(index: longint; const p: tvppoint);
var
  pos: tvpposition;
begin
  pos   := tvpposition.create;
  pos.p := p;
  flist.insert(index, pos);
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
      distance_between_two_points(
        tvpposition(flist[i    ]).p,
        tvpposition(flist[i - 1]).p);
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
  while flist.count > 0 do delete(0);
end;

procedure tvppaths.add(path: tvppath);
//var
//  i: longint;
//  j: longint;
//  k: longint;
begin
// DELETE OVERLAP POINT
//for i := path.count - 1 downto 0 do
//  for j := flist.count - 1 downto 0 do
//  begin
//    k := tvppath(flist[j]).find(path.item[i]);
//    if k <> - 1 then
//    begin
//      path.delete(i);
//      break;
//    end;
//  end;
//if path.getlen = 0 then
//begin
//  path.destroy;
//  path := nil;

  if assigned(path) then
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
      point := @(path.item[j].p);
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
      point    := @(path.item[j].p);
      point^.x := point^.x + offsetx;
      point^.y := point^.y + offsety;
    end;
  end;
  fheight := ymax - ymin;
  fwidth  := xmax - xmin;
end;

procedure tvppaths.deletesmallpaths;
var
  i: longint;
begin
  for i := flist.count - 1 downto 0 do
    if tvppath(flist[i]).getlen < smallest then
    begin
      delete(i);
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
    index := walknear(path, list1);
    path  := tvppath(list1[index]);
    list1.delete(index);
    list2.add(path);
    if not isclosed(tvppath(flist[i])) then
    begin
      repeat
        index := walkback(path.getfirst, list1);
        if index <> -1 then
        begin
          path := tvppath(list1[index]);
          list1.delete(index);
          list2.insert(0, path);
        end;
      until index = -1;

      path := tvppath(list2.last);
      repeat
        index := walknext(path.getlast, list1);
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

