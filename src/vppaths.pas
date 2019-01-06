{
  Description: vPlot paths class.

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

unit vppaths;

{$mode objfpc}

interface

uses
  classes, vpmath, vpsetting, bgrabitmap, bgrasvg, bgrasvgshapes, bgrasvgtype,
  bgrabitmaptypes, bgratransform, bgrapath, bgravectorize, bcsvgviewer;

type
  tvpentity = class(tobject)
  private
    flist:   tfplist;
    function getcount: longint;
    function get(index: longint): pvppoint;
  public
    constructor create;
    destructor destroy; override;
    procedure add(p: pvppoint);
    procedure delete(index: longint);
    procedure clear;
    procedure invert;
    function first: pvppoint;
    function last: pvppoint;
  public
    property items[index: longint]: pvppoint read get;
    property count: longint read getcount;
  end;

  tvppath = class(tobject)
  private
    flist: tfplist;
    fenabled: boolean;
    function getcount: longint;
    function get(index: longint): tvpentity;
  public
    constructor create;
    destructor destroy; override;
    procedure add(entity: tvpentity);
    procedure delete(index: longint);
    procedure clear;
    procedure reorder;
  public
    property enabled: boolean read fenabled write fenabled;
    property items[index: longint]: tvpentity read get;
    property count: longint read getcount;
  end;

  tvppaths = class(tobject)
  private
    flist: tfplist;
    fpathnames: tstringlist;
    function getcount:  longint;
    function get(index: longint): tvppath;
    function getname(index: longint): string;
  public
    constructor create;
    destructor destroy; override;
    procedure addline     (entity: pvpline;      const pathname: string);
    procedure addcircle   (entity: pvpcircle;    const pathname: string);
    procedure addcirclearc(entity: pvpcirclearc; const pathname: string);
    procedure addellipse  (entity: pvpellipse;   const pathname: string);
    procedure delete(index: longint);
    procedure clear;
    procedure createtoolpath;
    procedure zerocenter;
    procedure mirror_x;
    procedure mirror_y;
    procedure rotate(const alpha: double);
  public
    property items[index: longint]: tvppath read get;
    property itemname[index: longint]: string  read getname;
    property count: longint read getcount;
  end;


  TPather = class(TBCSVGViewer)


  public



  end;

  //procedure dxf2paths(const afilename: string; apaths: tvppaths);
  //procedure vec2paths(const afilename: string; apaths: tvppaths);
  procedure svg2paths(const afilename: string; apaths: tvppaths);

implementation

uses
  math, sysutils;

// internal toolpath routines

function interpolate_line(entity: pvpline): tvpentity;
var
  dx, dy: double;
   i,  j: longint;
       p: tvppoint;
begin
    j := max(1, round(len_segment(entity)/(1.2/setting.mode)));
   dx := (entity^.p1.x - entity^.p0.x)/j;
   dy := (entity^.p1.y - entity^.p0.y)/j;

  result := tvpentity.create;
  for i := 0 to j do
  begin
    p.x := i * dx;
    p.y := i * dy;
    p   := translate_point(entity^.p0, p);
    result.add(@p);
  end;
end;

function interpolate_circle(entity: pvpcircle): tvpentity;
var
   i, j: longint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  start.x := entity^.radius;
  start.y := 0.0;
  sweep   := 2 * pi;

  j := max(1, round((abs(sweep)*entity^.radius)/(1.2/setting.mode)));

  result := tvpentity.create;
  for i := 0 to j do
  begin
    p := rotate_point(start, (i * (sweep / j)));
    p := translate_point(entity^.center, p);
    result.add(@p);
  end;
end;

function interpolate_circlearc(entity: pvpcirclearc): tvpentity;
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

  j := max(1, round(abs(sweep)*entity^.radius/(1.2/setting.mode)));

  result := tvpentity.create;
  for i := 0 to j do
  begin
    p := rotate_point(start, (i * (sweep / j)));
    p := translate_point(entity^.center, p);
    result.add(@p);
  end;
end;

function interpolate_elipse(entity: pvpellipse): tvpentity;
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
  result := distance_between_two_position(p0, p1) < 0.001;
end;

function get_next(p0: pvppoint; list: tfplist): longint;
var
  i: longint;
begin
  result := -1;
  for i := 0 to list.count - 1 do
    if compare_position(p0, tvpentity(list[i]).first) or
       compare_position(p0, tvpentity(list[i]).last ) then
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

    len2 := distance_between_two_position(p0, tvpentity(list[i]).first);
    if len1 > len2 then
    begin
      len1 := len2;
      result := i;
    end;

    len2 := distance_between_two_position(p0, tvpentity(list[i]).last);
    if len1 > len2 then
    begin
      tvpentity(list[i]).invert;
      len1 := len2;
      result := i;
    end;

  end;
end;

function is_closed(entity: tvpentity): boolean;
begin
  result := true;
  if entity.count > 1 then
  begin
    result := compare_position(entity.get(0), entity.get(entity.count-1));
  end;
end;

// tvpentity

constructor tvpentity.create;
begin
  inherited create;
  flist := tfplist.create;
end;

destructor tvpentity.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvpentity.delete(index: longint);
begin
  dispose(pvppoint(flist[index]));
  flist.delete(index);
end;

procedure tvpentity.clear;
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
  begin
    dispose(pvppoint(flist[i]));
  end;
  flist.clear;
end;

procedure tvpentity.add(p: pvppoint);
var
  point: pvppoint;
begin
  new(point);
  point^.x := p^.x;
  point^.y := p^.y;
  point^.z := p^.z;
  flist.add(point);
end;

procedure tvpentity.invert;
var
    i: longint;
  tmp: tlist;
begin
  tmp := tlist.create;
  for i := flist.count - 1 downto 0 do tmp.add(flist[i]);
  for i := flist.count - 1 downto 0 do flist[i] := tmp[i];
  tmp.destroy;
end;

function tvpentity.first: pvppoint;
begin
  result := pvppoint(flist[0]);
end;

function tvpentity.last: pvppoint;
begin
  result := pvppoint(flist[flist.count-1]);
end;

function tvpentity.get(index: longint): pvppoint;
begin
   result := pvppoint(flist[index]);
end;

function tvpentity.getcount: longint;
begin
  result := flist.count;
end;

// tvppath

constructor tvppath.create;
begin
  inherited create;
  flist    := tfplist.create;
  fenabled := true;
end;

destructor tvppath.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure tvppath.delete(index: longint);
begin
  tvpentity(flist[index]).destroy;
  flist.delete(index);
end;

procedure tvppath.clear;
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
  begin
    tvpentity(flist[i]).destroy;
  end;
  flist.clear;
end;

procedure tvppath.reorder;
var
       i: longint;
  entity: tvpentity;
   point: pvppoint = nil;
   list1: tfplist;
   list2: tfplist;
begin
  list1 := tfplist.create;
  list2 := tfplist.create;

  // move all path to list1
  while flist.count > 0 do
  begin
    list1.add(flist[0]);
    flist.delete(0);
  end;

  // create toolpath
  while list1.count > 0 do
  begin
    if point = nil then
      i := 0
    else
      i := get_near(point, list1);

    list2.add(list1[i]);
    list1.delete(i);

    if not is_closed(tvpentity(list2[0])) then
    begin
      entity := tvpentity(list2[0]);
      repeat
        i := get_next(entity.last, list1);
        if i <> -1 then
        begin
          if compare_position(entity.last,
            tvpentity(list1[i]).last) then
            tvpentity(list1[i]).invert;

          entity := tvpentity(list1[i]);
          list2.add(entity);
          list1.delete(i);
        end;
      until i = -1;

      entity := tvpentity(list2[0]);
      repeat
        i := get_next(entity.first, list1);
        if i <> -1 then
        begin
          if compare_position(entity.first,
            tvpentity(list1[i]).first) then
            tvpentity(list1[i]).invert;

          entity := tvpentity(list1[i]);
          list2.insert(0, entity);
          list1.delete(i);
        end;
      until i = -1;
    end;

    // move toolpath
    point := tvpentity(list2[list2.count-1]).last;
    while list2.count > 0 do
    begin
      flist.add(tvppath(list2[0]));
      list2.delete(0);
    end;

  end;
  list2.destroy;
  list1.destroy;
end;

procedure tvppath.add(entity: tvpentity);
begin
  flist.add(entity);
end;

function tvppath.getcount: longint;
begin
  result := flist.count;
end;

function tvppath.get(index: longint): tvpentity;
begin
  result := tvpentity(flist[index]);
end;

// tvppaths

constructor tvppaths.create;
begin
  inherited create;
  flist      := tfplist.create;
  fpathnames := tstringlist.create;
end;

destructor tvppaths.destroy;
begin
  clear;
  flist.destroy;
  fpathnames.destroy;
  inherited destroy;
end;

procedure tvppaths.delete(index: longint);
begin
  tvppath(flist[index]).destroy;
  flist.delete(index);
  fpathnames.delete(index);
end;

procedure tvppaths.clear;
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
  begin
    tvppath(flist[i]).destroy;
  end;
  flist.clear;
  fpathnames.clear;
end;

procedure tvppaths.addline(entity: pvpline; const pathname: string);
var
  i: longint;
begin
  i := fpathnames.indexof(pathname);
  if i = -1 then
  begin
    i :=
      flist     .add(tvppath.create);
      fpathnames.add(pathname);
  end;
  tvppath(flist[i]).add(interpolate_line(entity));
end;

procedure tvppaths.addcircle(entity: pvpcircle; const pathname: string);
var
  i: longint;
begin
  i := fpathnames.indexof(pathname);
  if i = -1 then
  begin
    i :=
      flist .add(tvppath.create);
      fpathnames.add(pathname);
  end;
  tvppath(flist[i]).add(interpolate_circle(entity));
end;

procedure tvppaths.addcirclearc(entity: pvpcirclearc; const pathname: string);
var
  i: longint;
begin
  i := fpathnames.indexof(pathname);
  if i = -1 then
  begin
    i :=
      flist .add(tvppath.create);
      fpathnames.add(pathname);
  end;
  tvppath(flist[i]).add(interpolate_circlearc(entity));
end;

procedure tvppaths.addellipse(entity: pvpellipse; const pathname: string);
begin
  // ...
end;

procedure tvppaths.zerocenter;
var
  i, j, k: longint;
     xmin: double;
     xmax: double;
     ymin: double;
     ymax: double;
  offsetx: double;
  offsety: double;
   entity: tvpentity;
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
      entity := path.items[j];
      for k := 0 to entity.count - 1 do
      begin
        point := entity.items[k];
         xmin := min(xmin, point^.x);
         xmax := max(xmax, point^.x);
         ymin := min(ymin, point^.y);
         ymax := max(ymax, point^.y);
      end;
    end;
  end;
  offsetx := - (xmin + xmax) / 2;
  offsety := - (ymin + ymax) / 2;

  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      entity := path.items[j];
      for k := 0 to entity.count - 1 do
      begin
        point    := entity.items[k];
        point^.x := point^.x + offsetx;
        point^.y := point^.y + offsety;
      end;
    end;
  end;
end;

procedure tvppaths.mirror_x;
var
  i, j, k: longint;
   entity: tvpentity;
     path: tvppath;
    point: pvppoint;
begin
  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      entity := path.items[j];
      for k := 0 to entity.count - 1 do
      begin
         point    := entity.items[k];
         point^.y := -point^.y;
      end;
    end;
  end;
end;

procedure tvppaths.mirror_y;
var
  i, j, k: longint;
   entity: tvpentity;
     path: tvppath;
    point: pvppoint;
begin
  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      entity := path.items[j];
      for k := 0 to entity.count - 1 do
      begin
         point    := entity.items[k];
         point^.x := -point^.x;
      end;
    end;
  end;
end;

procedure tvppaths.rotate(const alpha: double);
var
  i, j, k: longint;
   entity: tvpentity;
     path: tvppath;
    point: pvppoint;
begin
  for i := 0 to flist.count - 1 do
  begin
    path := tvppath(flist[i]);
    for j := 0 to path.count - 1 do
    begin
      entity := path.items[j];
      for k := 0 to entity.count - 1 do
      begin
         point  := entity.items[k];
         point^ := rotate_point(point^, alpha);
      end;
    end;
  end;
end;

procedure tvppaths.createtoolpath;
var
  i: longint;
begin
  for i := 0 to flist.count - 1 do
    get(i).reorder;
end;

function tvppaths.getcount: longint;
begin
  result := flist.count;
end;

function tvppaths.get(index: longint): tvppath;
begin
  result := tvppath(flist[index]);
end;

function tvppaths.getname(index: longint): string;
begin
  result := fpathnames[index];
end;

// dxf2paths
(*
procedure dxf2paths(const afilename: string; apaths: tvppaths);
var
  reader: tvdxfreader;
begin
  reader := tvdxfreader.create;
  reader.readfromfile(afilename, apaths);
  reader.destroy;
  apaths.zerocenter;
  apaths.createtoolpath;
end;

// vectorial2paths

procedure entity2paths(entity: tventity; apaths: tvppaths);
var
          i: longint;
     circle: tvpcircle;
  circlearc: tvpcirclearc;
       line: tvpline;
    segment: tpathsegment;
begin
  if entity is tvcircle then
  begin
    circle.center.x := tvcircle(entity).x;
    circle.center.y := tvcircle(entity).y;
    circle.radius   := tvcircle(entity).radius;
    apaths.addcircle(@circle, '0');
  end else
  if entity is tvcirculararc then
  begin
    circlearc.center.x   := tvcirculararc(entity).x;
    circlearc.center.y   := tvcirculararc(entity).y;
    circlearc.radius     := tvcirculararc(entity).radius;
    circlearc.startangle := tvcirculararc(entity).startangle;
    circlearc.endangle   := tvcirculararc(entity).endangle;
    apaths.addcirclearc(@circlearc, '0');
  end else
  if entity is tpath then
  begin
    tpath(entity).prepareforsequentialreading;
    for i := 0 to tpath(entity).len - 1 do
    begin
      segment := tpathsegment(tpath(entity).next);
      case segment.segmenttype of
        stmoveto:
        begin
          line.p0.x := t2dsegment(segment).x;
          line.p0.y := t2dsegment(segment).y;
        end;
        st2dlinewithpen, st2dline, st3dline:
        begin
          line.p1.x := t2dsegment(segment).x;
          line.p1.y := t2dsegment(segment).y;
          apaths.addline(@line, '0');
          line.p0 := line.p1;
        end;
        else
          writeln(segment.segmenttype);
      end;
    end;
  end else
  if entity is tvlayer then
  begin
    for i := 0 to tvlayer(entity).getentitiescount -1 do
    begin
      entity2paths(tvlayer(entity).getentity(i), apaths);
    end;
  end else
  begin
    if enabledebug then
      writeln(entity.classname);
  end;
end;

procedure vec2paths(const afilename: string; apaths: tvppaths);
var
  i, j: longint;
  page: tvpage;
   vec: tvvectorialdocument;
begin
  vec := tvvectorialdocument.create;
  vec.readfromfile(afilename);
  for i := 0 to vec.getpagecount - 1 do
  begin
    page := vec.getpageasvectorial(i);
    for j := 0 to page.getentitiescount - 1 do
      entity2paths(page.getentity(j), apaths);
  end;
  vec.destroy;

  apaths.createtoolpath;
  apaths.mirror_x;
  apaths.zerocenter;
end;
*)
// svg2paths

procedure element2paths(element: tsvgelement; apaths: tvppaths);
var
     bmp: tbgrabitmap;
    i, j: longint;
    line: tvpline;
  points: arrayoftpointf;
begin
  bmp := tbgrabitmap.create;
  bmp.canvas2d.fontrenderer := tbgravectorizedfontrenderer.create;
  if (element is tsvgline      ) or
     (element is tsvgrectangle ) or
     (element is tsvgcircle    ) or
     (element is tsvgellipse   ) or
     (element is tsvgpath      ) or
     (element is tsvgtext      ) or
     (element is tsvgpolypoints) then
  begin
    element.draw(bmp.canvas2d, cumillimeter);
    points := bmp.canvas2d.currentpath;

    i := 0;
    while i < length(points) do
    begin
      writeln('index** =', i, ' p.x=', points[i].x:2:2);
      writeln('index** =', i, ' p.x=', points[i].y:2:2);
      if (points[i].x < 0) or
         (points[i].y < 0) then
      begin
        points[i].x := -1;
        points[i].y := -1;
      end;
      inc(i);
    end;

    for i := 0 to length(points) - 2 do
    begin
      line.p0.x := points[i].x;
      line.p0.y := points[i].y;
      line.p1.x := points[i+1].x;
      line.p1.y := points[i+1].y;

      if (line.p0.x > 0) and
         (line.p0.y > 0) and
         (line.p1.x > 0) and
         (line.p1.y > 0) then
        apaths.addline(@line, element.id);
    end;
    setlength(points, 0);
  end else
  if element is tsvggroup then
  begin
    with tsvggroup(element).content do
    begin
      for i := 0 to elementcount - 1 do
        element2paths(element[i], apaths);
    end;
  end else
  if enabledebug then
  begin
    writeln(element.classname);
  end;

  bmp.destroy;
end;

procedure svg2paths(const afilename: string; apaths: tvppaths);
var
    i: longint;
  svg: tbgrasvg;
begin
  svg := tbgrasvg.create(afilename);
  for i := 0 to svg.content.elementcount - 1 do
  begin
    element2paths(svg.content.element[i], apaths);
  end;
  svg.destroy;

  apaths.mirror_x;
  apaths.createtoolpath;
  apaths.zerocenter;
end;

end.

