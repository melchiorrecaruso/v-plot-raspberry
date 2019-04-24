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
    finverted: boolean;
    flayer: longword;
    flen: single;
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
    procedure interpolate(value: single); virtual;
    procedure move(dx, dy: single); virtual;
    procedure rotate(angle: single); virtual;
    procedure scale(value: single); virtual;
    procedure mirrorx; virtual;
    procedure mirrory; virtual;
    procedure read(stream: tstream); virtual;
    procedure write(stream: tstream); virtual;
  public
    property first: pvppoint read getfirst;
    property hidden: boolean read fhidden write fhidden;
    property layer: longword read flayer  write flayer;
    property last: pvppoint read getlast;
    property len: single read flen;
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
    procedure interpolate(value: single); override;
    procedure move(dx, dy: single); override;
    procedure rotate(angle: single); override;
    procedure scale(value: single); override;
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
    procedure interpolate(value: single); override;
    procedure move(dx, dy: single); override;
    procedure rotate(angle: single); override;
    procedure scale(value: single); override;
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
    procedure interpolate(value: single); override;
    procedure move(dx, dy: single); override;
    procedure rotate(angle: single); override;
    procedure scale(value: single); override;
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
    procedure interpolate(value: single); override;
    procedure move(dx, dy: single); override;
    procedure rotate(angle: single); override;
    procedure scale(value: single); override;
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
    procedure interpolate(value: single);
    procedure offset(var x, y: single);
    procedure move(dx, dy: single);
    procedure rotate(angle: single);
    procedure scale(value: single);
    procedure mirrorx;
    procedure mirrory;
    procedure invert;
    procedure invert(index: longint);
    procedure movetoorigin;
    procedure createtoolpath;
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
    function getcount: longint;
    function getitem(index: longint): pvppoint;
  public
    constructor create;
    destructor destroy; override;
    procedure clear;
    procedure update(elemlist: tvpelementlist; dxmax, dymax: single);
    function getraises: longint;
    function getlength: single;
  public
    property items[index: longint]: pvppoint read getitem;
    property count: longint read getcount;
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
    if distance_between_two_points(p, tvpelement(list[i]).getfirst^) < 0.05 then
    begin
      result := i;
      exit;
    end;
end;

function getlast(const p: tvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count -1 do
    if distance_between_two_points(p, tvpelement(list[i]).getlast^) < 0.05 then
    begin
      result := i;
      exit;
    end;
end;

function getnear(const p: tvppoint; list: tfplist): longint;
var
        i: longint;
     len1: double = $FFFFFFF;
     len2: double = $FFFFFFF;
  element: tvpelement;
begin
  result := -1;
  for i := 0 to list.count -1 do
  begin
    element := tvpelement(list[i]);

    len2 := distance_between_two_points(p, element.getfirst^);
    if len1 > len2 then
    begin
      len1   := len2;
      result := i;
    end;

    len2 := distance_between_two_points(p, element.getlast^);
    if len1 > len2 then
    begin
      element.invert;
      len1   := len2;
      result := i;
    end;

  end;
end;

function isaloop(const element: tvpelement): boolean;
begin
  result := distance_between_two_points(element.getfirst^, element.getlast^) < 0.05;
end;

// tvpelement

constructor tvpelement.create;
begin
  inherited create;
  fhidden   := false;
  finverted := false;
  flayer    := 0;
  flen      := 0;;
  fselected := false;
  setlength(fpath, 0);
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
  finverted := not finverted;
  vpmath.invert(fpath);
end;

procedure tvpelement.interpolate(value: single);
var
  i: longint;
begin
  flen := 0;
  for i := 0 to high(fpath) -1 do
  begin
    flen := flen + distance_between_two_points(fpath[i], fpath[i+1]);
  end;
end;

procedure tvpelement.move(dx, dy: single);
begin
  vpmath.move(fpath, dx, dy);
end;

procedure tvpelement.rotate(angle: single);
begin
  vpmath.rotate(fpath, angle);
end;

procedure tvpelement.scale(value: single);
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
  stream.read(finverted, sizeof(boolean));
  stream.read(flayer,    sizeof(longint));
  stream.read(flen,      sizeof(single ));
  stream.read(fselected, sizeof(boolean));
end;

procedure tvpelement.write(stream: tstream);
begin
  stream.write(fhidden,   sizeof(boolean));
  stream.write(finverted, sizeof(boolean));
  stream.write(flayer,    sizeof(longint));
  stream.write(flen,      sizeof(single ));
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

procedure tvpelementline.interpolate(value: single);
begin
  vpmath.interpolate(fline, fpath, value);
  inherited interpolate(value);
end;

procedure tvpelementline.move(dx, dy: single);
begin
  vpmath.move(fline, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementline.rotate(angle: single);
begin
  vpmath.rotate(fline, angle);
  inherited rotate(angle);
end;

procedure tvpelementline.scale(value: single);
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

procedure tvpelementcircle.interpolate(value: single);
begin
  vpmath.interpolate(fcircle, fpath, value);
  inherited interpolate(value);
end;

procedure tvpelementcircle.move(dx, dy: single);
begin
  vpmath.move(fcircle, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementcircle.rotate(angle: single);
begin
  vpmath.rotate(fcircle, angle);
  inherited rotate(angle);
end;

procedure tvpelementcircle.scale(value: single);
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

procedure tvpelementcirclearc.interpolate(value: single);
begin
  vpmath.interpolate(fcirclearc, fpath, value);
  inherited interpolate(value);
end;

procedure tvpelementcirclearc.move(dx, dy: single);
begin
  vpmath.move(fcirclearc, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementcirclearc.rotate(angle: single);
begin
  vpmath.rotate(fcirclearc, angle);
  inherited rotate(angle);
end;

procedure tvpelementcirclearc.scale(value: single);
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

procedure tvpelementpolygonal.interpolate(value: single);
begin
  vpmath.interpolate(fpolygonal, fpath, value);
  inherited interpolate(value);
end;

procedure tvpelementpolygonal.move(dx, dy: single);
begin
  vpmath.move(fpolygonal, dx, dy);
  inherited move(dx, dy);
end;

procedure tvpelementpolygonal.rotate(angle: single);
begin
  vpmath.rotate(fpolygonal, angle);
  inherited rotate(angle);
end;

procedure tvpelementpolygonal.scale(value: single);
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

procedure tvpelementlist.interpolate(value: single);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).interpolate(value);
  end;
end;

procedure tvpelementlist.offset(var x, y: single);
var
     i, j: longint;
     xmin: double;
     xmax: double;
     ymin: double;
     ymax: double;
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

procedure tvpelementlist.move(dx, dy: single);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).move(dx, dy);
  end;
end;

procedure tvpelementlist.rotate(angle: single);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).rotate(angle);
  end;
end;

procedure tvpelementlist.scale(value: single);
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
  i: longint;
begin
  for i := 0 to flist.count -1 do
  begin
    tvpelement(flist[i]).invert;
  end;
end;

procedure tvpelementlist.invert(index: longint);
begin
  tvpelement(flist[index]).invert;
end;

procedure tvpelementlist.movetoorigin;
var
  dx, dy: single;
begin
  offset(dx, dy);
  move(dx, dy);
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
  element: tvpelement;
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

      element := tvpelement(list2[0]);
      repeat
        i := getfirst(element.getlast^, list1);
        if i <> -1 then
        begin
          element := tvpelement(list1[i]);
          list2.add(element);
          list1.delete(i);
        end;
      until i = -1;

      element := tvpelement(list2[0]);
      repeat
        i := getlast(element.getfirst^, list1);
        if i <> -1 then
        begin
          element := tvpelement(list1[i]);
          list2.insert(0, element);
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

//  tvppath

constructor tvppath.create;
begin
  inherited create;
  flist := tfplist.create;
end;

destructor tvppath.destroy;
begin
  clear;
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
end;

procedure tvppath.update(elemlist: tvpelementlist; dxmax, dymax: single);
var
  i, j: longint;
  elem: tvpelement;
    pp: pvppoint;
     p: tvppoint;
begin
  clear;
  for i := 0 to elemlist.count -1 do
  begin
    elem := elemlist.items[i];
    if elem.hidden = false then
    begin
      elem.interpolate(0.1);
      for j := 0 to elem.count -1 do
      begin
        p := wave.update(elem.items[j]^);

        if (abs(p.x) <= (dxmax)) and
           (abs(p.y) <= (dymax)) then
        begin
          new(pp);
          pp^ := elem.items[j]^;
          flist.add(pp);
        end;
      end;
      elem.interpolate(0.5);
    end;
  end;
end;

function tvppath.getlength: single;
var
       i: longint;
  p0, p1: tvppoint;
begin
  result := 0;

  p0 := setting.layout9;
  for i := 0 to flist.count -1 do
  begin
    p1 := wave.update(pvppoint(flist[i])^);
    result := result + distance_between_two_points(p0, p1);

    p0 := p1;
  end;
end;


function tvppath.getraises: longint;
var
       i: longint;
  p0, p1: tvppoint;
       z: single;
begin
  result := 0;

   z := setting.zmax;
  p0 := setting.layout9;
  for i := 0 to flist.count -1 do
  begin
    p1 := wave.update(pvppoint(flist[i])^);

    if distance_between_two_points(p0, p1) < 0.2 then
    begin
      if z <> setting.zmin then
      begin
        z := setting.zmin;
        inc(result);
      end;
    end else
    begin
      if z <> setting.zmax then
      begin
        z := setting.zmax;
        inc(result);
      end;
    end;

    p0 := p1;
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

//

function info(elements: tvpelementlist): single;
var
  i: longint;
begin
  result := 0;

  elements.interpolate(0.5);
  for i := 0 to elements.count -1 do
  begin
    result := result + elements.items[i].len;
  end;
end;

end.



