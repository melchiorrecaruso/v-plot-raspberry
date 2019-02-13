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
  classes, sysutils, vpmath, vpsetting, vpwave;

type
  tvppath = class(tobject)
  private
    fenabled:  boolean;
    fhidden:   boolean;
    flist:     tfplist;
    flayer:    longword;
    fselected: boolean;
    function get(index: longint): pvppoint;
    function getcount: longint;
    function getfirst: pvppoint;
    function getlast: pvppoint;
  public
    constructor create;
    destructor destroy; override;
    procedure add(p: pvppoint);
    procedure clear;
    procedure delete(index: longint);
    procedure invert;
  public
    property count:    longint  read getcount;
    property enabled:  boolean  read fenabled  write fenabled;
    property hidden:   boolean  read fhidden   write fhidden;
    property layer:    longword read flayer    write flayer;
    property selected: boolean  read fselected write fselected;
    property items[index: longint]: pvppoint  read get;
  end;

  tvppaths = class(tobject)
  private
    flist: tfplist;
    function get(index: longint): tvppath;
    function getcount:  longint;
    procedure createtoolpath4layer(list: tfplist);
  public
    constructor create;
    destructor destroy; override;
    procedure addline(entity: pvpline);
    procedure addcircle(entity: pvpcircle);
    procedure addcirclearc(entity: pvpcirclearc);
    procedure addellipse(entity: pvpellipse);
    procedure addpath(entity: tvppath);
    procedure delete(index: longint);
    procedure clear;
    procedure clean;
    procedure createtoolpath;
    procedure zerocenter;
    procedure mirror(xaxis: boolean);
    procedure scale(const factor: single);
    procedure rotate(const alpha: single);
    procedure offset(const x, y: single);
    //
    procedure showall(value: boolean);
    procedure showlayer(value: longint);
    procedure hidelayer(value: longint);
    procedure hideselected;
    procedure inverthidden;
    //
    procedure selectall(value: boolean);
    procedure selectlayer(value: longint);
    procedure selectattached;
    procedure invertselected;
    //
    procedure mergeselected;
    procedure unmergeselected;
    //
    procedure load(const filename: rawbytestring);
    procedure save(const filename: rawbytestring);
  public
    property count: longint read getcount;
    property items[index: longint]: tvppath read get;
  end;

  procedure optimize2(paths: tvppaths; const xcenter, ycenter, xmax, ymax: double; var s: ansistring); overload;
  procedure optimize(paths: tvppaths; const xcenter, ycenter, xmax, ymax: double; buf: tstringlist); overload;
  procedure optimize(const p: tvppoint; out mx, my: longint);


implementation

uses
  math, vpdxfreader;

const
  minlen = 0.15;

// internal toolpath routines

function interpolate_line(entity: pvpline): tvppath;
var
  dx, dy: double;
   i,  j: longint;
       p: tvppoint;
begin
    j := max(1, round(len_segment(entity)/minlen));
   dx := (entity^.p1.x - entity^.p0.x)/j;
   dy := (entity^.p1.y - entity^.p0.y)/j;

  result := tvppath.create;
  for i := 0 to j do
  begin
    p.x := i * dx;
    p.y := i * dy;
    p   := translate_point(entity^.p0, p);
    result.add(@p);
  end;
end;

function interpolate_circle(entity: pvpcircle): tvppath;
var
   i, j: longint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  start.x := entity^.radius;
  start.y := 0.0;
  sweep   := 2 * pi;

  j := max(1, round((abs(sweep)*entity^.radius)/minlen));

  result := tvppath.create;
  for i := 0 to j do
  begin
    p := rotate_point(start, (i * (sweep / j)));
    p := translate_point(entity^.center, p);
    result.add(@p);
  end;
end;

function interpolate_circlearc(entity: pvpcirclearc): tvppath;
var
   i, j: longint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  start.x := entity^.radius;
  start.y := 0.0;
  start   := rotate_point(start, degtorad(entity^.startangle));
  sweep   := degtorad(entity^.endangle - entity^.startangle);

  j := max(1, round(abs(sweep)*entity^.radius/minlen));

  result := tvppath.create;
  for i := 0 to j do
  begin
    p := rotate_point(start, (i * (sweep / j)));
    p := translate_point(entity^.center, p);
    result.add(@p);
  end;
end;

function interpolate_elipse(entity: pvpellipse): tvppath;
begin
  // ...
end;

// internal commont routines

function distance_between_two_position(p0, p1: pvppoint): double;
begin
  result := sqrt(sqr(p1^.x - p0^.x) + sqr(p1^.y - p0^.y));
end;

function compare_position(p0, p1: pvppoint): boolean;
begin
  result := distance_between_two_position(p0, p1) < 0.02;
end;

function getfirst(p0: pvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count -1 do
    if compare_position(p0, tvppath(list[i]).getfirst) then
    begin
      result := i;
      exit;
    end;
end;

function getlast(p0: pvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count -1 do
    if compare_position(p0, tvppath(list[i]).getlast) then
    begin
      result := i;
      exit;
    end;
end;

function get_near(p0: pvppoint; list: tfplist): longint;
var
     i: longint;
  len1: double = $FFFFFFF;
  len2: double = $FFFFFFF;
begin
  result := 0;
  for i := 0 to list.count - 1 do
  begin

    len2 := distance_between_two_position(p0, tvppath(list[i]).getfirst);
    if len1 > len2 then
    begin
      len1 := len2;
      result := i;
    end;

    len2 := distance_between_two_position(p0, tvppath(list[i]).getlast);
    if len1 > len2 then
    begin
      tvppath(list[i]).invert;
      len1 := len2;
      result := i;
    end;

  end;
end;

function is_closed(entity: tvppath): boolean;
begin
  result := true;
  if entity.count > 1 then
  begin
    result := compare_position(entity.get(0), entity.get(entity.count-1));
  end;
end;

// tvppath

constructor tvppath.create;
begin
  inherited create;
  fenabled  := true;
  fhidden   := false;
  flayer    := 0;
  flist     := tfplist.create;
  fselected := false;
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
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
    dispose(pvppoint(flist[i]));
  flist.clear;
end;

procedure tvppath.add(p: pvppoint);
var
  point: pvppoint;
begin
  new(point);
  point^.x := p^.x;
  point^.y := p^.y;
  point^.z := p^.z;
  flist.add(point);
end;

procedure tvppath.invert;
var
    i: longint;
  tmp: tlist;
begin
  tmp := tlist.create;
  for i := flist.count - 1 downto 0 do tmp.add(flist[i]);
  for i := flist.count - 1 downto 0 do flist[i] := tmp[i];
  tmp.destroy;
end;

function tvppath.getfirst: pvppoint;
begin
  result := pvppoint(flist[0]);
end;

function tvppath.getlast: pvppoint;
begin
  result := pvppoint(flist[flist.count-1]);
end;

function tvppath.get(index: longint): pvppoint;
begin
   result := pvppoint(flist[index]);
end;

function tvppath.getcount: longint;
begin
  result := flist.count;
end;

// tvppaths

constructor tvppaths.create;
begin
  inherited create;
  flist := tfplist.create;
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
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
    tvppath(flist[i]).destroy;
  flist.clear;
end;

procedure tvppaths.clean;
begin
  //...
end;

procedure tvppaths.addline(entity: pvpline);
begin
  flist.add(interpolate_line(entity));
end;

procedure tvppaths.addcircle(entity: pvpcircle);
begin
  flist.add(interpolate_circle(entity));
end;

procedure tvppaths.addcirclearc(entity: pvpcirclearc);
begin
  flist.add(interpolate_circlearc(entity));
end;

procedure tvppaths.addellipse(entity: pvpellipse);
begin
  // ...
end;

procedure tvppaths.addpath(entity: tvppath);
begin
  flist.add(entity);
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
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
    begin
      point := path.items[j];
       xmin := min(xmin, point^.x);
       xmax := max(xmax, point^.x);
       ymin := min(ymin, point^.y);
       ymax := max(ymax, point^.y);
    end;
  end;
  offsetx := - (xmin + xmax) / 2;
  offsety := - (ymin + ymax) / 2;

  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
    begin
      point    := path.items[j];
      point^.x := point^.x + offsetx;
      point^.y := point^.y + offsety;
    end;
  end;
end;

procedure tvppaths.mirror(xaxis: boolean);
var
   i, j: longint;
   path: tvppath;
  point: pvppoint;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
      if xaxis then
      begin
        point    := path.items[j];
        point^.y := -point^.y;
      end else
      begin
        point    := path.items[j];
        point^.x := -point^.x;
      end;
  end;
end;

procedure tvppaths.rotate(const alpha: single);
var
   i, j: longint;
   path: tvppath;
  point: pvppoint;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
    begin
      point  := path.items[j];
      point^ := rotate_point(point^, alpha);
    end;
  end;
end;

procedure tvppaths.offset(const x, y: single);
var
   i, j: longint;
   path: tvppath;
  point: pvppoint;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
    begin
      point    := path.items[j];
      point^.x := point^.x + x;
      point^.y := point^.y + y;
    end;
  end;
end;

procedure tvppaths.scale(const factor: single);
var
   i, j: longint;
   path: tvppath;
  point: pvppoint;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count -1 do
    begin
      point    := path.items[j];
      point^.x := point^.x * factor;
      point^.y := point^.y * factor;
    end;
  end;
end;

procedure tvppaths.mergeselected;
var
  i, j: longint;
  path: tvppath;
begin
  randomize;
  repeat
    j := random($FFFFFFF);
    for i := 0 to flist.count -1 do
      if j = tvppath(flist[i]).layer then j := 0;
  until j <> 0;

  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    if path.selected then
      path.layer := j;
  end;
end;

procedure tvppaths.unmergeselected;
var
     i: longint;
  path: tvppath;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    if path.selected then
    begin
      path.layer := 0;
    end;
  end;
end;

//

procedure tvppaths.showall(value: boolean);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    tvppath(flist[i]).fhidden := not value;
end;

procedure tvppaths.showlayer(value: longint);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvppath(flist[i]).flayer = value then
      tvppath(flist[i]).fhidden := false;
end;

procedure tvppaths.hidelayer(value: longint);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvppath(flist[i]).flayer = value then
      tvppath(flist[i]).fhidden := true;
end;

procedure tvppaths.hideselected;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvppath(flist[i]).fselected then
      tvppath(flist[i]).fhidden := true;
end;

procedure tvppaths.inverthidden;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    tvppath(flist[i]).fhidden := not tvppath(flist[i]).fhidden;
end;

//

procedure tvppaths.selectall(value: boolean);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    tvppath(flist[i]).fselected := value;
end;

procedure tvppaths.selectlayer(value: longint);
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    if tvppath(flist[i]).flayer = value then
      tvppath(flist[i]).fselected := true;
end;

procedure tvppaths.selectattached;
var
  i, j: longint;
  path: tvppath;
begin
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);

    if path.selected then
    begin
      repeat
        j := getfirst(path.getlast, flist);
        if j <> -1 then
        begin
          path := tvppath(flist[j]);
          if path.selected then
            break;
          path.selected := true;
        end;
      until (j = -1) or (j = i);

      path := tvppath(flist[i]);
      repeat
        j := getlast(path.getfirst, flist);
        if j <> -1 then
        begin
          path := tvppath(flist[j]);
          if path.selected then
            break;
          path.selected := true;
        end;
      until (j = -1) or (j = i);

    end;
  end;
end;

procedure tvppaths.invertselected;
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    tvppath(flist[i]).fselected := not tvppath(flist[i]).fselected;
end;

//

procedure tvppaths.save(const filename: rawbytestring);
var
   i, j: longint;
   path: tvppath;
  point: pvppoint;
      s: tfilestream;
begin
  s := tfilestream.create(filename, fmcreate);
  s.writeansistring('vpl3.0');
  s.write(flist.count, sizeof(longint));
  for i := 0 to flist.count -1 do
  begin
    path := tvppath(flist[i]);
    s.write(path.layer, sizeof(longint));
    s.write(path.count, sizeof(longint));
    for j := 0 to path.count -1 do
    begin
      point := path.items[j];
      s.write(point^.x, sizeof(single));
      s.write(point^.y, sizeof(single));
    end;
  end;
  s.destroy;
end;

procedure tvppaths.load(const filename: rawbytestring);
var
  count1: longint = 0;
  count2: longint = 0;
    i, j: longint;
    path: tvppath;
   point: tvppoint;
       s: tfilestream;
begin
  clear;

  s := tfilestream.create(filename, fmopenread or fmsharedenywrite);
  if s.readansistring = 'vpl3.0' then
  begin
    s.read(count1, sizeof(longint));
    for i := 0 to count1 -1 do
    begin
      path := tvppath(flist[flist.add(tvppath.create)]);
      s.read(path.flayer, sizeof(longint));
      s.read(count2,      sizeof(longint));
      for j := 0 to count2 -1 do
      begin
        s.read(point.x, sizeof(single));
        s.read(point.y, sizeof(single));
        path.add(@point);
      end;
    end;
  end;
  s.destroy;
end;

procedure tvppaths.createtoolpath;
var
   i, j: longint;
  list1: tfplist;
  list2: tfplist;
begin
  list1 := tfplist.create;
  list2 := tfplist.create;
  while flist.count > 0 do
  begin
    list1.add(flist[0]);
    flist.delete(0);
  end;

  while list1.count > 0 do
  begin
    j :=  tvppath(list1[0]).layer;
    for i := list1.count -1 downto 0 do
    begin
      if j = tvppath(list1[i]).layer then
      begin
        list2.add(list1[j]);
        list1.delete(j);
      end;
    end;

    createtoolpath4layer(list2);
    while list2.count > 0 do
    begin
      flist.add(list2[0]);
      list2.delete(0);
    end;
  end;
  list2.destroy;
  list1.destroy;
end;

procedure tvppaths.createtoolpath4layer(list: tfplist);
var
      i: longint;
   path: tvppath;
  point: pvppoint;
  list1: tfplist;
  list2: tfplist;
begin
  list1 := tfplist.create;
  list2 := tfplist.create;
  while list.count > 0 do
  begin
    list1.add(list[0]);
    list.delete(0);
  end;
  point := nil;

  // create toolpath
  while list1.count > 0 do
  begin
    if point = nil then
      i := 0
    else
      i := get_near(point, list1);

    list2.add(list1[i]);
    list1.delete(i);

    if is_closed(tvppath(list2[0])) = false then
    begin
      path := tvppath(list2[0]);
      repeat
        i := getfirst(path.getlast, list1);
        if i = -1 then
        begin
          i := getlast(path.getlast, list1);
          if i <> -1 then
            tvppath(list1[i]).invert;
        end;

        if i <> -1 then
        begin
          path := tvppath(list1[i]);
          list2.add(path);
          list1.delete(i);
        end;
      until i = -1;

      path := tvppath(list2[0]);
      repeat
        i := getlast(path.getfirst, list1);
        if i = -1 then
        begin
          i := getfirst(path.getfirst, list1);
          if i <> -1 then
            tvppath(list1[i]).invert;
        end;

        if i <> -1 then
        begin
          path := tvppath(list1[i]);
          list2.insert(0, path);
          list1.delete(i);
        end;
      until i = -1;
    end;

    // move toolpath
    i := list.count;
    point := tvppath(list2[list2.count-1]).getlast;
    while list2.count > 0 do
    begin
      tvppath(list2[0]).layer := i;
      list.add(tvppath(list2[0]));
      list2.delete(0);
    end;

  end;
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



procedure optimize(const p: tvppoint; out mx, my: longint); inline;
var
  cx, cy, ct: tvpcircleimp;
      lx, ly: double;
  sx, sy, st: tvppoint;
      tx, ty: tvppoint;
begin
  tx := setting.layout0;
  ty := setting.layout1;
  //find tangent point tx
  lx := sqrt(sqr(distance_between_two_points(tx, p))-sqr(setting.xradius));
  cx := circle_by_center_and_radius(tx, setting.xradius);
  ct := circle_by_center_and_radius(p, lx);
  if intersection_of_two_circles(cx, ct, sx, st) = 0 then
    raise exception.create('intersection_of_two_circles [c0c2]');
  lx := lx + get_angle(line_by_two_points(sx, tx))*setting.xradius;
  //find tangent point ty
  ly := sqrt(sqr(distance_between_two_points(ty, p))-sqr(setting.yradius));
  cy := circle_by_center_and_radius(ty, setting.yradius);
  ct := circle_by_center_and_radius(p, ly);
  if intersection_of_two_circles(cy, ct, sy, st) = 0 then
    raise exception.create('intersection_of_two_circles [c1c2]');
  ly := ly + (pi-get_angle(line_by_two_points(sy, ty)))*setting.yradius;
  // calculate steps
  mx := round(lx/setting.xratio);
  my := round(ly/setting.yratio);
end;


procedure optimize(paths: tvppaths; const xcenter, ycenter, xmax, ymax: double; buf: tstringlist); overload;
var
   i, j: longint;
     mx: longint = 0;
     my: longint = 0;
   path: tvppath;
  point: tvppoint;
   list: tfplist;
begin
  list := tfplist.create;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.enabled then
      for j := 0 to path.count -1 do
      begin
        point:= path.items[j]^;
        point:= wave.update(point);

        if (abs(point.x) <= (xmax)) and
           (abs(point.y) <= (ymax)) then
          list.add(path.items[j]);
      end;
  end;

  if list.count > 0 then
  begin
    pvppoint(list[0])^.z := setting.zmax;
    for i := 1 to list.count -1 do
      if distance_between_two_points(
        pvppoint(list[i])^, pvppoint(list[i-1])^) < 0.25 then
        pvppoint(list[i])^.z := setting.zmin
      else
        pvppoint(list[i])^.z := setting.zmax;

    for i := 0 to list.count -1 do
    begin
      point   := pvppoint(list[i])^;
      point   := wave.update(point);
      point.x := point.x + xcenter;
      point.y := point.y + ycenter;

      optimize(point, mx, my);
      buf.add(format('MOVE X%d Y%d Z%d', [mx, my, trunc(point.z)]));
    end;
  end;
  list.destroy;
end;

procedure optimize2(paths: tvppaths; const xcenter, ycenter, xmax, ymax: double; var s: ansistring); overload;
var
   i, j: longint;
     mx: longint = 0;
     my: longint = 0;
   path: tvppath;
  point: tvppoint;
   list: tfplist;
begin
  list := tfplist.create;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.enabled then
      for j := 0 to path.count -1 do
      begin
        point:= path.items[j]^;
        point:= wave.update(point);

        if (abs(point.x) <= (xmax)) and
           (abs(point.y) <= (ymax)) then
          list.add(path.items[j]);
      end;
  end;

  if list.count > 0 then
  begin
    pvppoint(list[0])^.z := setting.zmax;
    for i := 1 to list.count -1 do
      if distance_between_two_points(
        pvppoint(list[i])^, pvppoint(list[i-1])^) < 0.25 then
        pvppoint(list[i])^.z := setting.zmin
      else
        pvppoint(list[i])^.z := setting.zmax;

    for i := 0 to list.count -1 do
    begin
      point   := pvppoint(list[i])^;
      point   := wave.update(point);
      point.x := point.x + xcenter;
      point.y := point.y + ycenter;

      optimize(point, mx, my);
      s := s + format('MOVE X%d Y%d Z%d ;', [mx, my, trunc(point.z)]);
    end;
  end;
  list.destroy;
end;

end.

