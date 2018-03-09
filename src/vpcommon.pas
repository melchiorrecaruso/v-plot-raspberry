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

  tvppointlist = class(tobject)
  private
    flist:   tlist;
    fheight: double;
    fwidth:  double;
    function getcount:  longint;
    function get(index: longint): pvppoint;
  public
    constructor create;
    destructor  destroy; override;
    procedure   add(const point: tvppoint);
    procedure   delete(index: longint);
    procedure   clear;
    procedure   update;
  public
    property count:  longint read getcount;
    property height: double  read fheight;
    property width:  double  read fwidth;
    property items[index: longint]: pvppoint read get; default;
  end;

implementation

uses
  math;

{ tvppointlist }

function compare(item1, item2: pvppoint): longint;
begin
  result := 1;
end;

constructor tvppointlist.create;
begin
  flist := tlist.create;
end;

destructor tvppointlist.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvppointlist.delete(index: longint);
begin
  dispose(pvppoint(flist[index]));
  flist.delete(index);
end;

procedure tvppointlist.clear;
var
  i: longint;
begin
  for i := flist.count - 1 downto 0 do
  begin
    dispose(pvppoint(flist[i]));
    flist.delete(i);
  end;
end;

procedure tvppointlist.add(const point: tvppoint);
var
  p: pvppoint;
begin
  new(p);
  p^.x := point.x;
  p^.y := point.y;
  p^.z := point.z;
  flist.add(p);
end;

procedure tvppointlist.update;
var
        i: longint;
        p: pvppoint;
     xmin: double;
     xmax: double;
     ymin: double;
     ymax: double;
  offsetx: double;
  offsety: double;
begin
  xmin  := + maxint;
  xmax  := - maxint;
  ymin  := + maxint;
  ymax  := - maxint;
  for i := 0 to flist.count - 1 do
  begin
    xmin := min(xmin, get(i)^.x);
    xmax := max(xmax, get(i)^.x);
    ymin := min(ymin, get(i)^.y);
    ymax := max(ymax, get(i)^.y);
  end;
  offsetx := - (xmin + xmax) / 2;
  offsety := - (ymin + ymax) / 2;
  for i := 0 to flist.count - 1 do
  begin
    p    := flist.items[i];
    p^.x := p^.x + offsetx;
    p^.y := p^.y + offsety;
  end;

  fheight := ymax - ymin;
  fwidth  := xmax - xmin;
end;

function tvppointlist.getcount: longint;
begin
  result := flist.count;
end;

function tvppointlist.get(index: longint): pvppoint;
begin
  result := pvppoint(flist[index]);
end;

end.

