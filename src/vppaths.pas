{
  Description: vPlot paths class.

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

unit vppaths;

{$mode objfpc}

interface

uses
  classes, sysutils, vpmath, vpwave;

type
  tvpelement = class(tobject)
  private
    fhidden: boolean;
    flayer: longword;
    fpath: tvppolygonal;
    fselected:  boolean;
    function getfirst: pvppoint;
    function getlast: pvppoint;
    function getcount: longint;
    function getitem(index: longint): pvppoint;
  public
    constructor create;
    destructor destroy; override;
    procedure invert; virtual;
    procedure interpolate(value: vpfloat); virtual; abstract;
    procedure move(dx, dy: vpfloat); virtual;
    procedure rotate(angle: vpfloat); virtual;
    procedure scale(value: vpfloat); virtual;
    procedure mirrorx; virtual;
    procedure mirrory; virtual;
    procedure read(stream: tstream); virtual;
    procedure write(stream: tstream); virtual;
  public
    property first: pvppoint read getfirst;
    property hidden: boolean read fhidden write fhidden;
    property layer: longword read flayer  write flayer;
    property last: pvppoint read getlast;
    property selected: boolean read fselected write fselected;
    property items[index: longint]: pvppoint read getitem;
    property count: longint read getcount;
  end;

  tvpelementline = class(tvpelement)
  private
    fline: tvpline;
  public
    constructor create;
    constructor create(const aline: tvpline);
    procedure invert; override;
    procedure interpolate(value: vpfloat); override;
    procedure move(dx, dy: vpfloat); override;
    procedure rotate(angle: vpfloat); override;
    procedure scale(value: vpfloat); override;
    procedure mirrorx; override;
    procedure mirrory; override;
    procedure read(stream: tstream); override;
    procedure write(stream: tstream); override;
  end;

  tvpelementcircle = class(tvpelement)
  private
    fcircle: tvpcircle;
  public
    constructor create;
    constructor create(const acircle: tvpcircle);
    procedure invert; override;
    procedure interpolate(value: vpfloat); override;
    procedure move(dx, dy: vpfloat); override;
    procedure rotate(angle: vpfloat); override;
    procedure scale(value: vpfloat); override;
    procedure mirrorx; override;
    procedure mirrory; override;
    procedure read(stream: tstream); override;
    procedure write(stream: tstream); override;
  end;

  tvpelementcirclearc = class(tvpelement)
  private
    fcirclearc: tvpcirclearc;
  public
    constructor create;
    constructor create(const acirclearc: tvpcirclearc);
    procedure invert; override;
    procedure interpolate(value: vpfloat); override;
    procedure move(dx, dy: vpfloat); override;
    procedure rotate(angle: vpfloat); override;
    procedure scale(value: vpfloat); override;
    procedure mirrorx; override;
    procedure mirrory; override;
    procedure read(stream: tstream); override;
    procedure write(stream: tstream); override;
  end;

  tvpelementpolygonal = class(tvpelement)
  private
    fpolygonal: tvppolygonal;
  public
    constructor create;
    constructor create(const apolygonal: tvppolygonal);
    procedure invert; override;
    procedure interpolate(value: vpfloat); override;
    procedure move(dx, dy: vpfloat); override;
    procedure rotate(angle: vpfloat); override;
    procedure scale(value: vpfloat); override;
    procedure mirrorx; override;
    procedure mirrory; override;
    procedure read(stream: tstream); override;
    procedure write(stream: tstream); override;
  end;

  tvpelementlist = class(tobject)
  private
    flist: tfplist;
    procedure createtoolpath4layer(list: tfplist);
    function getitem(index: longint): tvpelement;
    function getcount: longint;
  public
    constructor create;
    destructor destroy; override;
    procedure add(const line: tvpline);
    procedure add(const circle: tvpcircle);
    procedure add(const circlearc: tvpcirclearc);
    procedure add(const polygonal: tvppolygonal);
    procedure add(element: tvpelement);
    procedure add(elements: tvpelementlist);
    procedure insert(index: longint; element: tvpelement);
    function  extract(index: longint): tvpelement;
    procedure delete(index: longint);
    procedure clear;
    //
    procedure interpolate(value: vpfloat);
    procedure offset(var x, y: vpfloat);
    procedure move(dx, dy: vpfloat);
    procedure rotate(angle: vpfloat);
    procedure scale(value: vpfloat);
    procedure mirrorx;
    procedure mirrory;
    procedure invert;
    procedure invert(index: longint);
    procedure createtoolpath;
    procedure movetoorigin;
    procedure smooth;

    //
    procedure hide(value: boolean);
    procedure hide(value: boolean; layer: longint);
    procedure hideselected;
    procedure inverthidden;
    //
    procedure select(value: boolean);
    procedure select(value: boolean; layer: longint);
    procedure selectattached;
    procedure invertselected;

    procedure mergeselected;
    procedure unmergeselected;

    procedure load(const filename: string);
    procedure save(const filename: string);
  public
    property count: longint read getcount;
    property items[index: longint]: tvpelement read getitem;
  end;

  tvppath = class
  private
    flist: tfplist;
    fpathlength: vpfloat;
    fpathraises: longint;
    fpathvertex: longint;
    function getcount: longint;
    function getitem(index: longint): pvppoint;
    procedure add(const p: tvppoint);
  public
    constructor create;
    destructor destroy; override;
    procedure clear;
    procedure update(elemlist: tvpelementlist; dxmax, dymax: vpfloat);
  public
    property count: longint read getcount;
    property items[index: longint]: pvppoint read getitem;
    property pathlength: vpfloat  read fpathlength;
    property pathraises: longint read fpathraises;
    property pathvertex: longint read fpathvertex;
  end;

implementation

uses
  math, vpsetting;

// tvpelement routines

function getfirst(const p: tvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count -1 do
  begin
    if distance_between_two_points(p, tvpelement(list[i]).getfirst^) < 0.02 then
    begin
      result := i;
      exit;
    end
  end;
end;

function getlast(const p: tvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count -1 do
  begin
    if distance_between_two_points(p, tvpelement(list[i]).getlast^) < 0.02 then
    begin
      result := i;
      exit;
    end
  end;
end;

function getnear(const p: tvppoint; list: tfplist): longint;
var
     i: longint;
  len1: vpfloat = $FFFFFFF;
  len2: vpfloat = $FFFFFFF;
  elem: tvpelement;
begin
  result := -1;
  for i := 0 to list.count -1 do
  begin
    elem := tvpelement(list[i]);

    len2 := distance_between_two_points(p, elem.getfirst^);
    if len1 > len2 then
    begin
      len1   := len2;
      result := i;
    end;

    len2 := distance_between_two_points(p, elem.getlast^);
    if len1 > len2 then
    begin
      elem.invert;
      len1   := len2;
      result := i;
    end;

  end;
end;

function isaloop(const element: tvpelement): boolean;
begin
  result := distance_between_two_points(element.getfirst^, element.getlast^) < 0.02;
end;

// tvpelement

constructor tvpelement.create;
begin
  inherited create;
  fhidden   := false;
  flayer    := 0;
  setlength(fpath, 0);
  fselected := false;
end;

destructor tvpelement.destroy;
begin
  setlength(fpath, 0);
  inherited destroy;
end;

function tvpelement.getfirst: pvppoint;
begin
  result := @fpath[0];
end;

function tvpelement.getlast: pvppoint;
begin
  result := @fpath[high(fpath)];
end;

function tvpelement.getitem(index: longint): pvppoint;
begin
  result := @fpath[index];
end;

function tvpelement.getcount: longint;
begin
  result := system.length(fpath);
end;

procedure tvpelement.invert;
begin
  vpmath.invert(fpath);
end;

procedure tvpelement.move(dx, dy: vpfloat);
begin
  vpmath.move(fpath, dx, dy);
end;

procedure tvpelement.rotate(angle: vpfloat);
begin
  vpmath.rotate(fpath, angle);
end;

procedure tvpelement.scale(value: vpfloat);
begin
  vpmath.scale(fpath, value);
end;

procedure tvpelement.mirrorx;
begin
  vpmath.mirrorx(fpath);
end;

procedure tvpelement.mirrory;
begin
  vpmath.mirrory(fpath);
end;

procedure tvpelement.read(stream: tstream);
begin
  stream.read(fhidden,   sizeof(boolean));
  stream.read(flayer,    sizeof(longint));
  stream.read(fselected, sizeof(boolean));
end;

procedure tvpelement.write(stream: tstream);
begin
  stream.write(fhidden,   sizeof(boolean));
  stream.write(flayer,    sizeof(longint));
  stream.write(fselected, sizeof(boolean));
end;

// tvpelementline

constructor tvpelementline.create;
begin
  inherited create;
end;

constructor tvpelementline.create(const aline: tvpline);
begin
  inherited create;
  fline := aline;
end;

procedure tvpelementline.invert;
begin
  vpmath.invert(fline);
  inherited invert;
end;

procedure tvpelementline.interpolate(value: vpfloat);
begin
  vpmath.interpolate(fline, fpath, value);
end;

procedure tvpelementline.move(dx, dy: vpfloat);
begin
  vpmath.move(fline, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementline.rotate(angle: vpfloat);
begin
  vpmath.rotate(fline, angle);
  inherited rotate(angle);
end;

procedure tvpelementline.scale(value: vpfloat);
begin
  vpmath.scale(fline, value);
  inherited scale(value);
end;

procedure tvpelementline.mirrorx;
begin
  vpmath.mirrorx(fline);
  inherited mirrorx;
end;

procedure tvpelementline.mirrory;
begin
  vpmath.mirrory(fline);
  inherited mirrory;
end;

procedure tvpelementline.read(stream: tstream);
begin
  inherited read(stream);
  stream.read(fline, sizeof(tvpline));
end;

procedure tvpelementline.write(stream: tstream);
begin
  inherited write(stream);
  stream.write(fline, sizeof(tvpline));
end;

// tvpelementcircle

constructor tvpelementcircle.create;
begin
  inherited create;
end;

constructor tvpelementcircle.create(const acircle: tvpcircle);
begin
  inherited create;
  fcircle := acircle;
end;

procedure tvpelementcircle.invert;
begin
  vpmath.invert(fcircle);
  inherited invert;
end;

procedure tvpelementcircle.interpolate(value: vpfloat);
begin
  vpmath.interpolate(fcircle, fpath, value);
end;

procedure tvpelementcircle.move(dx, dy: vpfloat);
begin
  vpmath.move(fcircle, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementcircle.rotate(angle: vpfloat);
begin
  vpmath.rotate(fcircle, angle);
  inherited rotate(angle);
end;

procedure tvpelementcircle.scale(value: vpfloat);
begin
  vpmath.scale(fcircle, value);
  inherited scale(value);
end;

procedure tvpelementcircle.mirrorx;
begin
  vpmath.mirrorx(fcircle);
  inherited mirrorx;
end;

procedure tvpelementcircle.mirrory;
begin
  vpmath.mirrory(fcircle);
  inherited mirrory;
end;

procedure tvpelementcircle.read(stream: tstream);
begin
  inherited read(stream);
  stream.read(fcircle, sizeof(tvpcircle));
end;

procedure tvpelementcircle.write(stream: tstream);
begin
  inherited write(stream);
  stream.write(fcircle, sizeof(tvpcircle));
end;

// tvpelementcirclearc

constructor tvpelementcirclearc.create;
begin
  inherited create;
end;

constructor tvpelementcirclearc.create(const acirclearc: tvpcirclearc);
begin
  inherited create;
  fcirclearc := acirclearc;
end;

procedure tvpelementcirclearc.invert;
begin
  vpmath.invert(fcirclearc);
  inherited invert;
end;

procedure tvpelementcirclearc.interpolate(value: vpfloat);
begin
  vpmath.interpolate(fcirclearc, fpath, value);
end;

procedure tvpelementcirclearc.move(dx, dy: vpfloat);
begin
  vpmath.move(fcirclearc, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementcirclearc.rotate(angle: vpfloat);
begin
  vpmath.rotate(fcirclearc, angle);
  inherited rotate(angle);
end;

procedure tvpelementcirclearc.scale(value: vpfloat);
begin
  vpmath.scale(fcirclearc, value);
  inherited scale(value);
end;

procedure tvpelementcirclearc.mirrorx;
begin
  vpmath.mirrorx(fcirclearc);
  inherited mirrorx;
end;

procedure tvpelementcirclearc.mirrory;
begin
  vpmath.mirrory(fcirclearc);
  inherited mirrory;
end;

procedure tvpelementcirclearc.read(stream: tstream);
begin
  inherited read(stream);
  stream.read(fcirclearc, sizeof(tvpcirclearc));
end;

procedure tvpelementcirclearc.write(stream: tstream);
begin
  inherited write(stream);
  stream.write(fcirclearc, sizeof(tvpcirclearc));
end;

// tvpelementpolygonal

constructor tvpelementpolygonal.create;
begin
  inherited create;
end;

constructor tvpelementpolygonal.create(const apolygonal: tvppolygonal);
var
  i: longint;
begin
  inherited create;
  setlength(fpolygonal, system.length(apolygonal));
  for i := 0 to high(apolygonal) do
  begin
    fpolygonal[i] := apolygonal[i];
  end;
end;

procedure tvpelementpolygonal.invert;
begin
  vpmath.invert(fpolygonal);
  inherited invert;
end;

procedure tvpelementpolygonal.interpolate(value: vpfloat);
begin
  vpmath.interpolate(fpolygonal, fpath, value);
end;

procedure tvpelementpolygonal.move(dx, dy: vpfloat);
begin
  vpmath.move(fpolygonal, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementpolygonal.rotate(angle: vpfloat);
begin
  vpmath.rotate(fpolygonal, angle);
  inherited rotate(angle);
end;

procedure tvpelementpolygonal.scale(value: vpfloat);
begin
  vpmath.scale(fpolygonal, value);
  inherited scale(value);
end;

procedure tvpelementpolygonal.mirrorx;
begin
  vpmath.mirrorx(fpolygonal);
  inherited mirrorx;
end;

procedure tvpelementpolygonal.mirrory;
begin
  vpmath.mirrory(fpolygonal);
  inherited mirrory;
end;

procedure tvpelementpolygonal.read(stream: tstream);
var
  i, j: longint;
begin
  inherited read(stream);

  setlength(fpolygonal, 0);
  if stream.read(j, sizeof(longint)) = sizeof(longint) then
  begin
    setlength(fpolygonal, j);
    for i := 0 to high(fpolygonal) do
    begin
      stream.read(fpolygonal[i], sizeof(tvppoint));
    end;
  end;
end;

procedure tvpelementpolygonal.write(stream: tstream);
var
  i, j: longint;
begin
  inherited write(stream);

  j := system.length(fpolygonal);
  stream.write(j, sizeof(longint));
  for i := 0 to high(fpolygonal) do
  begin
    stream.write(fpolygonal[i], sizeof(tvppoint));
  end;
end;

// tvpelementslist

constructor tvpelementlist.create;
begin
  inherited create;
  flist := tfplist.create;
end;

destructor tvpelementlist.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvpelementlist.clear;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).destroy;
  end;
  flist.clear;
end;

function tvpelementlist.getcount: longint;
begin
  result := flist.count;
end;

function tvpelementlist.getitem(index: longint): tvpelement;
begin
  result := tvpelement(flist[index]);
end;

procedure tvpelementlist.add(const line: tvpline);
begin
   flist.add(tvpelementline.create(line));
end;

procedure tvpelementlist.add(const circle: tvpcircle);
begin
  flist.add(tvpelementcircle.create(circle));
end;

procedure tvpelementlist.add(const circlearc: tvpcirclearc);
begin
  flist.add(tvpelementcirclearc.create(circlearc));
end;

procedure tvpelementlist.add(const polygonal: tvppolygonal);
begin
  flist.add(tvpelementpolygonal.create(polygonal));
end;

procedure tvpelementlist.add(element: tvpelement);
begin
  flist.add(element);
end;

procedure tvpelementlist.add(elements: tvpelementlist);
var
  i: longint;
begin
  for i := 0 to elements.count -1 do
  begin
    flist.add(elements.items[i]);
  end;
  elements.flist.clear;
end;

procedure tvpelementlist.insert(index: longint; element: tvpelement);
begin
  flist.Insert(index, element);

end;

function tvpelementlist.extract(index: longint): tvpelement;
begin
  result := tvpelement(flist[index]);
  flist.delete(index);
end;

procedure tvpelementlist.delete(index: longint);
begin
  tvpelement(flist[index]).destroy;
  flist.delete(index);
end;

//---

procedure tvpelementlist.interpolate(value: vpfloat);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).interpolate(value);
  end;
end;

procedure tvpelementlist.offset(var x, y: vpfloat);
var
     i, j: longint;
     xmin: vpfloat;
     xmax: vpfloat;
     ymin: vpfloat;
     ymax: vpfloat;
  element: tvpelement;
    point: tvppoint;
begin
  xmin  := + maxint;
  xmax  := - maxint;
  ymin  := + maxint;
  ymax  := - maxint;
  for i := 0 to flist.count -1 do
  begin
    element := tvpelement(flist[i]);
    for j := 0 to high(element.fpath) do
    begin
      point := element.fpath[j];
       xmin := min(xmin, point.x);
       xmax := max(xmax, point.x);
       ymin := min(ymin, point.y);
       ymax := max(ymax, point.y);
    end;
  end;
  x := -(xmin + xmax)/2;
  y := -(ymin + ymax)/2;
end;

procedure tvpelementlist.move(dx, dy: vpfloat);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).move(dx, dy);
  end;
end;

procedure tvpelementlist.rotate(angle: vpfloat);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).rotate(angle);
  end;
end;

procedure tvpelementlist.scale(value: vpfloat);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).scale(value);
  end;
end;

procedure tvpelementlist.mirrorx;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).mirrorx;
  end;
end;

procedure tvpelementlist.mirrory;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).mirrory;
  end;
end;

procedure tvpelementlist.invert;
var
  i, cnt: longint;
begin
  cnt := flist.count -1;
  for i := 0 to cnt do
  begin
    tvpelement(flist[0]).invert;
    flist.move(0, cnt-i);
  end;
end;

procedure tvpelementlist.invert(index: longint);
begin
  tvpelement(flist[index]).invert;
end;

//---

procedure tvpelementlist.hide(value: boolean);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).fhidden := value;;
  end;
end;

procedure tvpelementlist.hide(value: boolean; layer: longint);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvpelement(flist[i]).flayer = layer then
    begin
      tvpelement(flist[i]).fhidden := value;
    end;
end;

procedure tvpelementlist.hideselected;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvpelement(flist[i]).fselected then
    begin
      tvpelement(flist[i]).fhidden := true;
    end;
end;

procedure tvpelementlist.inverthidden;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).fhidden :=
      not tvpelement(flist[i]).fhidden;
  end;
end;

procedure tvpelementlist.select(value: boolean);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).fselected := value;
  end;
end;

procedure tvpelementlist.select(value: boolean; layer: longint);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvpelement(flist[i]).flayer = layer then
    begin
      tvpelement(flist[i]).fselected := value;
    end;
end;

procedure tvpelementlist.selectattached;
var
     i, j: longint;
  element: tvpelement;
begin
  for i := 0 to flist.count -1 do
  begin
    element := tvpelement(flist[i]);

    if element.fselected then
    begin
      repeat
        j := getfirst(element.getlast^, flist);
        if j <> -1 then
        begin
          element := tvpelement(flist[j]);
          if element.fselected then
            break;
          element.fselected := true;
        end;
      until (j = -1) or (j = i);

      element := tvpelement(flist[i]);
      repeat
        j := getlast(element.getfirst^, flist);
        if j <> -1 then
        begin
          element := tvpelement(flist[j]);
          if element.fselected then
            break;
          element.fselected := true;
        end;
      until (j = -1) or (j = i);

    end;
  end;
end;

procedure tvpelementlist.invertselected;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).fselected :=
      not tvpelement(flist[i]).fselected;
  end;
end;

procedure tvpelementlist.mergeselected;
var
     i, j: longint;
  element: tvpelement;
begin
  randomize;
  repeat
    j := random($FFFFFFF);
    for i := 0 to flist.count -1 do
      if j = tvpelement(flist[i]).flayer then
      begin
        j := 0;
      end;
  until j <> 0;

  for i := 0 to flist.count -1 do
  begin
    element := tvpelement(flist[i]);
    if element.fselected then
      element.flayer := j;
  end;
end;

procedure tvpelementlist.unmergeselected;
var
        i: longint;
  element: tvpelement;
begin
  for i := 0 to flist.count -1 do
  begin
    element := tvpelement(flist[i]);
    if element.fselected then
    begin
      element.flayer := 0;
    end;
  end;
end;

procedure tvpelementlist.save(const filename: string);
var
        i: longint;
  element: tvpelement;
        s: tmemorystream;
begin
  s := tmemorystream.create;
  s.writeansistring('vpl3.1');
  s.write(flist.count, sizeof(longint));
  for i := 0 to flist.count -1 do
  begin
    element := tvpelement(flist[i]);

    if element is tvpelementline      then s.writeansistring('line')      else
    if element is tvpelementcircle    then s.writeansistring('circle')    else
    if element is tvpelementcirclearc then s.writeansistring('circlearc') else
    if element is tvpelementpolygonal then s.writeansistring('polygonal') else
      raise exception.create('tvpelementlist.save');

    element.write(s);
  end;
  s.savetofile(filename);
  s.destroy;
end;

procedure tvpelementlist.load(const filename: string);
var
     i, j: longint;
  element: tvpelement;
     sign: string;
        s: tmemorystream;
begin
  clear;

  s := tmemorystream.create;
  s.loadfromfile(filename);
  if s.readansistring = 'vpl3.1' then
  begin

    if s.read(j, sizeof(longint))= sizeof(longint) then
      for i := 0 to j -1 do
      begin
        sign := s.readansistring;

        if sign = 'line'      then element := tvpelementline.create      else
        if sign = 'circle'    then element := tvpelementcircle.create    else
        if sign = 'circlearc' then element := tvpelementcirclearc.create else
        if sign = 'polygonal' then element := tvpelementpolygonal.create else
          raise exception.create('tvpelementlist.load');

        element.read(s);
        add(element);
      end;
  end;
  s.destroy;
end;

procedure tvpelementlist.createtoolpath;
var
   i, j: longint;
  list1: tfplist;
  list2: tfplist;
begin
  list1 := tfplist.create;
  list2 := tfplist.create;
  for i := 0 to flist.count -1 do
    list1.add(flist[i]);
  flist.clear;

  while list1.count > 0 do
  begin
    j :=  tvpelement(list1[0]).flayer;
    for i := list1.count -1 downto 0 do
    begin
      if j = tvpelement(list1[i]).flayer then
      begin
        list2.add(list1[i]);
        list1.delete(i);
      end;
    end;

    createtoolpath4layer(list2);
    for i := 0 to list2.count -1 do
      flist.add(list2[i]);
    list2.clear;
  end;
  list2.destroy;
  list1.destroy;
end;

procedure tvpelementlist.createtoolpath4layer(list: tfplist);
var
      i: longint;
   elem: tvpelement;
  point: pvppoint;
  list1: tfplist;
  list2: tfplist;
begin
  list1 := tfplist.create;
  list2 := tfplist.create;
  for i := 0 to list.count -1 do
    list1.add(list[i]);
  list.clear;

  point := nil;
  // create toolpath
  while list1.count > 0 do
  begin
    if point = nil then
      i := 0
    else
      i := getnear(point^, list1);

    list2.add(list1[i]);
    list1.delete(i);

    if isaloop(tvpelement(list2[0])) = false then
    begin

      elem := tvpelement(list2[0]);
      repeat
        i := getfirst(elem.getlast^, list1);
        if i = -1 then
        begin
          i := getlast(elem.getlast^, list1);
          if i <> -1 then tvpelement(list1[i]).invert;
        end;

        if i <> -1 then
        begin
          elem := tvpelement(list1[i]);
          list2.add(elem);
          list1.delete(i);
        end;
      until i = -1;

      elem := tvpelement(list2[0]);
      repeat
        i := getlast(elem.getfirst^, list1);
        if i = -1 then
        begin
          i := getfirst(elem.getfirst^, list1);
          if i <> -1 then tvpelement(list1[i]).invert;
        end;

        if i <> -1 then
        begin
          elem := tvpelement(list1[i]);
          list2.insert(0, elem);
          list1.delete(i);
        end;
      until i = -1;

    end;

    // move toolpath
    i := list.count;
    point := tvpelement(list2[list2.count-1]).getlast;
    while list2.count > 0 do
    begin
      tvpelement(list2[0]).flayer := i;
      list.add(tvpelement(list2[0]));
      list2.delete(0);
    end;
  end;
  list2.destroy;
  list1.destroy;
end;

procedure tvpelementlist.movetoorigin;
var
  dx: vpfloat = 0;
  dy: vpfloat = 0;
begin
  offset(dx, dy);
  move(dx, dy);
end;

procedure tvpelementlist.smooth;
var
      i: longint;
  elem0: tvpelement;
  elem1: tvpelement;
  elem2: tvpelement;
     p0: tvppoint;
     p1: tvppoint;
     p2: tvppoint;
     p3: tvppoint;
     a0: tvpcirclearc;
  list1: tfplist;
begin
  list1 := tfplist.create;
  if flist.count > 0 then
  begin

    elem0 := tvpelement(flist[0]);
    for i := 1 to flist.count -1 do
    begin
      elem1 := tvpelement(flist[i]);

      list1.add(elem0);
      if (elem0 is tvpelementline) and
         (elem1 is tvpelementline) then
      begin
        if itsavertex(
          tvpelementline(elem0).getfirst^,
          tvpelementline(elem0).getlast^,
          tvpelementline(elem1).getlast^) then
        begin
          vpmath.smooth(
            tvpelementline(elem0).fline,
            tvpelementline(elem1).fline, a0);
          elem0.interpolate(0.5);
          elem1.interpolate(0.5);

          elem2        := tvpelementcirclearc.create(a0);
          elem2.flayer := elem0.flayer;
          list1.add(elem2);
        end;
      end;
      elem0:= elem1;
    end;

    //clear;
    for i := 0 to list1.count -1 do
      flist.add(list1[i]);
    list1.destroy;
    interpolate(0.5);
  end;
end;

//  tvppath

constructor tvppath.create;
begin
  inherited create;
  flist       := tfplist.create;
  fpathlength := 0;
  fpathraises := 0;
  fpathvertex := 0;
end;

destructor tvppath.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvppath.clear;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    dispose(pvppoint(flist[i]));
  end;
  flist.clear;
  fpathlength := 0;
  fpathraises := 0;
  fpathvertex := 0;
end;

procedure tvppath.add(const p: tvppoint);
var
  pp: pvppoint;
begin
  new(pp);
  pp^ := p;
  flist.add(pp);
end;

procedure tvppath.update(elemlist: tvpelementlist; dxmax, dymax: vpfloat);
var
  i, j: longint;
  elem: tvpelement;
    p0,
    p1,
    p2: tvppoint;
    pz: vpfloat;
begin
  clear;

  pz := setting.zmax;
  p0 := setting.layout9;
  for i := 0 to elemlist.count -1 do
  begin
    elem := elemlist.items[i];
    if elem.hidden = false then
    begin
      elem.interpolate(0.1);

      for j := 0 to elem.count -1 do
      begin
        p1 := wave.update(elem.items[j]^);
        if not itsthesame(p0, p1) then
          if (abs(p1.x) <= (dxmax)) and
             (abs(p1.y) <= (dymax)) then
          begin
            if distance_between_two_points(p0, p1) < 0.2 then
            begin
              fpathlength := fpathlength
                + distance_between_two_points(p0, p1);

              if pz <> setting.zmin then
              begin
                pz := setting.zmin;
                inc(fpathraises);
              end;
            end else
            begin
              if pz <> setting.zmax then
              begin
                pz := setting.zmax;
                inc(fpathraises);
              end;
            end;
            add(elem.items[j]^);
            p0 := p1;
          end;
      end;

      elem.interpolate(0.5);
    end;
  end;
  inc(fpathraises);

  if flist.count > 2 then
  begin
    p0 := wave.update(pvppoint(flist[0])^);
    p1 := wave.update(pvppoint(flist[1])^);
    for i := 2 to flist.count -1 do
    begin
      p2 := wave.update(pvppoint(flist[i])^);

      if itsavertex(p0, p1, p2) then
      begin
        inc(fpathvertex);
      end;
      p0 := p1;
      p1 := p2;
    end;
  end;
end;

function tvppath.getcount: longint;
begin
  result := flist.count;
end;

function tvppath.getitem(index: longint): pvppoint;
begin
  result := pvppoint(flist[index]);
end;

end.



