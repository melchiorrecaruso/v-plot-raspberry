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
  classes, matrix, sysutils;

type
  tdegres = 0..10;

  tpolynome = packed record
    deg: tdegres;
    coefs: array[tdegres] of double;
  end;

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

  tvppath = class(tobject)
  private
    flist:   tlist;
    function getfirst: pvppoint;
    function getlast:  pvppoint;
    function getcount: longint;
    function get(index: longint): pvppoint;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const point: tvppoint);
    procedure   insert(index: longint; const point: tvppoint);
    procedure   delete(index: longint);
    function    find(p: pvppoint): longint;
    procedure   clear;
    procedure   invert;
    function    getlen: double;
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

type
  twavemesh = array[0..8] of tvppoint;

  twave = class
  private
    lax, lay: tpolynome;
    lbx, lby: tpolynome;
    lcx, lcy: tpolynome;
  public
    constructor create(xmax, ymax: double; const mesh: twavemesh);
    destructor destroy; override;
    function   update(const p: tvppoint): tvppoint;
  end;


  function  translatepoint(const cc, p: tvppoint): tvppoint;
  function  rotatepoint(const p: tvppoint; const alpha: double): tvppoint;
  function  distancebetween(const p0, p1: tvppoint): double;
  function  linebetween(const p0, p1: tvppoint): tvpline;
  function  lineangle(var line: tvpline): double;
  function  intersectlines(const l0, l1: tvpline): tvppoint;

  function polyeval(const apoly: tpolynome; x: double): double;

var
  enabledebug: boolean = false;

implementation

uses
  math;

const
  smallest = 0.05;

// polynomial evaluation

function polyeval(const apoly: tpolynome; x: double): double;
var
  i: tdegres;
begin
  with apoly do
  begin
    result := 0;
    for i := deg downto low(coefs) do
      result := result * x + coefs[i];
  end;
end;

// geometry routines

function translatepoint(const cc, p: tvppoint): tvppoint;
begin
  result.x := cc.x + p.x;
  result.y := cc.y + p.y;
end;

function rotatepoint(const p: tvppoint; const alpha: double): tvppoint;
var
  sinus, cosinus : double;
begin
  sincos(alpha, sinus, cosinus);
  result.x := p.x * cosinus - p.y *   sinus;
  result.y := p.x *   sinus + p.y * cosinus;
end;

function distancebetween(const p0, p1: tvppoint): double;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function linebetween(const p0, p1: tvppoint): tvpline;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y - (p1.y - p0.y) * p0.x;
end;

function lineangle(var line: tvpline): double;
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

function intersectlines(const l0, l1: tvpline): tvppoint;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    result.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    result.y := (-l0.c - l0.a * result.x) / (l0.b);
  end else
    raise exception.create('Intersectlines routine exception');
end;

// ---

function comparepoint(p0, p1: pvppoint): boolean;
begin
  result := abs(p1^.x - p0^.x) < smallest;
  if result then
  begin
    result := abs(p1^.y - p0^.y) < smallest;
  end;
end;

function comparepath(p0, p1: pointer): longint;
begin
  result := round(tvppath(p1).getlen - tvppath(p0).getlen);
end;

function walkback(p0: pvppoint; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if comparepoint(p0, tvppath(list[i]).getlast) then
    begin
      result := i;
      exit;
    end else
    if comparepoint(p0, tvppath(list[i]).getfirst) then
    begin
      tvppath(list[i]).invert;
      result := i;
      exit;
    end;
end;

function walknext(p0: pvppoint; list: tlist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if comparepoint(p0, tvppath(list[i]).getfirst) then
    begin
      result := i;
      exit;
    end else
    if comparepoint(p0, tvppath(list[i]).getlast) then
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
    best := distancebetween(path.getlast^, tvppath(list[0]).getfirst^);

    for i := 1 to list.count - 1 do
    begin
      curr := distancebetween(path.getlast^, tvppath(list[i]).getfirst^);
      if curr < best then
      begin
        best   := curr;
        result := i;
      end else
      begin
        curr := distancebetween(path.getlast^, tvppath(list[i]).getlast^);
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
    result := comparepoint(path.getfirst, path.getlast);
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

function tvppath.find(p: pvppoint): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to flist.count - 1 do
    if comparepoint(p, pvppoint(flist[i])) then
    begin
      result := i;
      exit;
    end;
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
  flist.add(p);
end;

procedure tvppath.insert(index: longint; const point: tvppoint);
var
  p: pvppoint;
begin
  new(p);
  p^.x := point.x;
  p^.y := point.y;
  flist.insert(index, p);
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
      distancebetween(pvppoint(flist[i    ])^,
                      pvppoint(flist[i - 1])^);
  end;
end;

function tvppath.getfirst: pvppoint;
begin
  if flist.count > 0 then
    result := pvppoint(flist.first)
  else
    result := nil;
end;

function tvppath.getlast: pvppoint;
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

// twave

constructor twave.create(xmax, ymax: double; const mesh: twavemesh);
var
   a, aa: tvector3_double;
   b, bb: tvector3_double;
   c, cc: tvector3_double;
  dy: tvector3_double;
  dx: tvector3_double;
   y: tmatrix3_double;
   x: tmatrix3_double;
begin
  inherited create;
  xmax := abs(xmax);
  ymax := abs(ymax);

  x.init(1, -xmax, sqr(-xmax), 1, 0, 0, 1, +xmax, sqr(+xmax));
  y.init(1, +ymax, sqr(+ymax), 1, 0, 0, 1, -ymax, sqr(-ymax));
  x := x.inverse(x.determinant);
  y := y.inverse(y.determinant);

  // calculate y-mirror
  dy.init(mesh[0].y, mesh[1].y, mesh[2].y);   a := x * dy;
  dy.init(mesh[3].y, mesh[4].y, mesh[5].y);   b := x * dy;
  dy.init(mesh[6].y, mesh[7].y, mesh[8].y);   c := x * dy;

  dx.init(a.data[0], b.data[0], c.data[0]);  cc := y * dx;
  dx.init(a.data[1], b.data[1], c.data[1]);  bb := y * dx;
  dx.init(a.data[2], b.data[2], c.data[2]);  aa := y * dx;

  lay.deg :=2;
  lay.coefs[2] := aa.data[2];
  lay.coefs[1] := aa.data[1];
  lay.coefs[0] := aa.data[0];

  lby.deg :=2;
  lby.coefs[2] := bb.data[2];
  lby.coefs[1] := bb.data[1];
  lby.coefs[0] := bb.data[0];

  lcy.deg :=2;
  lcy.coefs[2] := cc.data[2];
  lcy.coefs[1] := cc.data[1];
  lcy.coefs[0] := cc.data[0];

  // calculate x-mirror
  dx.init(mesh[0].x, mesh[3].x, mesh[6].x);   a := y * dx;
  dx.init(mesh[1].x, mesh[4].x, mesh[7].x);   b := y * dx;
  dx.init(mesh[2].x, mesh[5].x, mesh[8].x);   c := y * dx;

  dy.init(a.data[0], b.data[0], c.data[0]);  cc := x * dy;
  dy.init(a.data[1], b.data[1], c.data[1]);  bb := x * dy;
  dy.init(a.data[2], b.data[2], c.data[2]);  aa := x * dy;

  lax.deg :=2;
  lax.coefs[2] := aa.data[2];
  lax.coefs[1] := aa.data[1];
  lax.coefs[0] := aa.data[0];

  lbx.deg :=2;
  lbx.coefs[2] := bb.data[2];
  lbx.coefs[1] := bb.data[1];
  lbx.coefs[0] := bb.data[0];

  lcx.deg :=2;
  lcx.coefs[2] := cc.data[2];
  lcx.coefs[1] := cc.data[1];
  lcx.coefs[0] := cc.data[0];
end;

destructor twave.destroy;
begin
  inherited destroy;
end;

function twave.update(const p: tvppoint): tvppoint;
var
  ly, lx: tpolynome;
begin
  ly.deg :=2;
  ly.coefs[2] := polyeval(lay, p.y);
  ly.coefs[1] := polyeval(lby, p.y);
  ly.coefs[0] := polyeval(lcy, p.y);

  lx.deg :=2;
  lx.coefs[2] := polyeval(lax, p.x);
  lx.coefs[1] := polyeval(lbx, p.x);
  lx.coefs[0] := polyeval(lcx, p.x);

  result.x := p.x + polyeval(lx, p.y);
  result.y := p.y + polyeval(ly, p.x);

  if enabledebug then
  begin
    writeln(format('  WAVING::P.X    = %12.5f  P''.X = %12.5f', [p.x, result.x]));
    writeln(format('  WAVING::P.Y    = %12.5f  P''.Y = %12.5f', [p.x, result.y]));
  end;
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

