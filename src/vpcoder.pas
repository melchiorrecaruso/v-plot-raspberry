{
  Description: vPlot coder.

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

unit vpcoder;

{$mode objfpc}

interface

uses
  classes, vpcommon, vplayout, fpvectorial;

type
  tvplotter = class(tthread)
  private
    fpx:       double;
    fpy:       double;
    fpaths:    tvppaths;
    fplot:     boolean;
    findex:    longint;
    fcount:    longint;
    fonstart:  tthreadmethod;
    fonstop:   tthreadmethod;
    fontick:   tthreadmethod;
  protected
    procedure execute; override;
  public
    constructor create(paths: tvppaths);
    destructor  destroy; override;
  public
    property px:       double        read fpx;
    property py:       double        read fpy;
    property plot:     boolean       read fplot      write fplot;
    property index:    longint       read findex;
    property count:    longint       read fcount;
    property onstart:  tthreadmethod read fonstart   write fonstart;
    property onstop:   tthreadmethod read fonstop    write fonstop;
    property ontick:   tthreadmethod read fontick    write fontick;
  end;


  function interpolate_line(const p0, p1: tvppoint): tvppath;
  function interpolate_circle(const entity: tvcircle ): tvppath;
  function interpolate_circlearc(const entity: tvcirculararc): tvppath;
  function interpolate_path(const entity: tpath): tvppath;

  function  createpaths(
           vec: tvvectorialdocument;
    zerocenter: boolean;
     skipsmall: boolean): tvppaths;

  procedure optimize(const p: tvppoint; const l: tvplayout; var m0, m1: longint);


implementation


uses
  math, sysutils;


// toolpath routines

function interpolate_line(const p0, p1: tvppoint): tvppath;
var
  dx, dy: double;
  i, len: longint;
       p: tvppoint;
begin
  len := max(1, round(distancebetween(p0, p1) / 0.15));
   dx := (p1.x - p0.x) / len;
   dy := (p1.y - p0.y) / len;

  result := tvppath.create;
  for i := 0 to len do
  begin
    p.x := i * dx;
    p.y := i * dy;
    p   := translatepoint(p0, p);
    result.add(p);
  end;
end;

function interpolate_circle(const entity: tvcircle ): tvppath;
var
      i: longint;
    len: longint;
     cc: tvppoint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  cc.x    := entity.x;
  cc.y    := entity.y;
  start.x := entity.radius;
  start.y := +0.0;
  sweep   := 2 * pi;

  len := max(1, round(abs(sweep) * entity.radius / 0.15));

  result := tvppath.create;
  for i := 0 to len do
  begin
    p := rotatepoint(start, (i * (sweep / len)));
    p := translatepoint(cc, p);
    result.insert(0, p);
  end;
end;

function interpolate_circlearc(const entity: tvcirculararc): tvppath;
var
      i: longint;
    len: longint;
     cc: tvppoint;
      p: tvppoint;
  start: tvppoint;
  sweep: double;
begin
  cc.x    := entity.x;
  cc.y    := entity.y;
  start.x := entity.radius;
  start.y := +0.0;
  start   := rotatepoint(start, degtorad(entity.startangle));
  sweep   := degtorad(entity.endangle - entity.startangle);

  len := max(1, round(abs(sweep) * entity.radius / 0.15));

  result := tvppath.create;
  for i := 0 to len do
  begin
    p := rotatepoint(start, (i * (sweep / len)));
    p := translatepoint(cc, p);
    result.insert(0, p);
  end;
end;

function interpolate_path(const entity: tpath): tvppath;
var
   dx, dy: double;
     i, j: longint;
      len: longint;
        p: tvppoint;
   p0, p1: tvppoint;
  segment: tpathsegment;
begin
  result := tvppath.create;
  entity.prepareforsequentialreading;
  for i := 0 to entity.len - 1 do
  begin
    segment := tpathsegment(entity.next());
    case segment.segmenttype of
      stmoveto:
      begin
        p0.x := t2dsegment(segment).x;
        p0.y := t2dsegment(segment).y;
      end;
      st2dlinewithpen, st2dline, st3dline:
      begin
        p1.x := t2dsegment(segment).x;
        p1.y := t2dsegment(segment).y;

        len := max(1, round(distancebetween(p0, p1) / 0.15));
        dx := (p1.x - p0.x) / len;
        dy := (p1.y - p0.y) / len;

        for j := 0 to len do
        begin
          p.x := j * dx;
          p.y := j * dy;
          p   := translatepoint(p0, p);
          result.add(p);
        end;
        p0 := p1;
      end;
      else
        writeln(segment.segmenttype);
    end;
  end;
end;

// ---

procedure optimize(const p: tvppoint; const l: tvplayout; var m0, m1: longint);
var
  alpha: double;
  err:   double;
  tmp00: tvppoint;
  tmp01: tvppoint;
  tmp02: tvppoint;
  tmp03: tvppoint;
  tmp04: tvppoint;
  tmp05: tvppoint;
  tmp06: tvppoint;
begin
  alpha    := 0;
  repeat
    tmp00  := l.point00;
    tmp01  := l.point01;
    tmp02  := translatepoint(p, rotatepoint(l.point02, alpha));
    tmp03  := translatepoint(p, rotatepoint(l.point03, alpha));
    tmp04  := translatepoint(p, rotatepoint(l.point04, alpha));
    tmp05  := translatepoint(p, rotatepoint(l.point05, alpha));
    tmp06  := intersectlines(linebetween(tmp00, tmp03),
                             linebetween(tmp01, tmp04));

    err := abs(tmp06.x - tmp05.x);
    if  err > 0.001 then
    begin
      if tmp06.x < tmp05.x then
        alpha := alpha - (err / 100)
      else
      if tmp06.x > tmp05.x then
        alpha := alpha + (err / 100);
    end else
      break;
  until false;

  m0 := round(l.mode * (distancebetween(tmp00, tmp03) / l.ratio));
  m1 := round(l.mode * (distancebetween(tmp01, tmp04) / l.ratio));

  if enabledebug then
  begin
    writeln(format('OPTIMIZE::ALPHA  = %12.5f',                 [radtodeg(alpha) ]));
    writeln(format('OPTIMIZE::P02.X  = %12.5f  P02.Y = %12.5f', [tmp02.x, tmp02.y]));
    writeln(format('OPTIMIZE::P03.X  = %12.5f  P03.Y = %12.5f', [tmp03.x, tmp03.y]));
    writeln(format('OPTIMIZE::P04.X  = %12.5f  P04.Y = %12.5f', [tmp04.x, tmp04.y]));
    writeln(format('OPTIMIZE::P05.X  = %12.5f  P05.Y = %12.5f', [tmp05.x, tmp05.y]));
    writeln(format('OPTIMIZE::P06.X  = %12.5f  P06.Y = %12.5f', [tmp06.x, tmp06.y]));
    writeln(format('OPTIMIZE::D03    = %12.5f', [distancebetween(tmp00, tmp03)]));
    writeln(format('OPTIMIZE::D14    = %12.5f', [distancebetween(tmp01, tmp04)]));
    writeln(format('OPTIMIZE::CNT.0  = %12.5u', [m0]));
    writeln(format('OPTIMIZE::CNT.1  = %12.5u', [m1]));
    writeln(format('OPTIMIZE::MODE   = %12.3u', [l.mode]));
    writeln(format('OPTIMIZE::RATIO  = %12.5f', [l.ratio]));
  end;
end;

// tvplotter

constructor tvplotter.create(paths: tvppaths);
var
  i: longint;
begin
  fpaths   := paths;
  fplot    := true;
  findex   := 0;
  fcount   := 0;

  for i := 0 to fpaths.count - 1 do
    inc(fcount, fpaths.item[i].count);

  fonstart := nil;
  fonstop  := nil;
  fontick  := nil;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvplotter.destroy;
begin
  fpaths := nil;
  inherited destroy;
end;

procedure tvplotter.execute;
var
      i: longint;
      j: longint;
   page: tvppath;
  point: pvppoint;
begin
  if assigned(fonstart) then
    synchronize(fonstart);

  findex := 0;
  if assigned(fontick) then
    for i := 0 to fpaths.count - 1 do
    begin
      page := fpaths.item[i];
      for j := 0 to page.count - 1 do
      begin
        point := page.item[j];
        if not terminated then
        begin
          inc(findex);
          fpx := point^.x;
          fpy := point^.y;
          synchronize(fontick);
          while not fplot do sleep(250);
        end;
      end;
    end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

// createpaths

function  createpaths(vec: tvvectorialdocument;
  zerocenter: boolean;
  skipsmall:  boolean): tvppaths;
var
    i, j: longint;
  entity: tventity;
    page: tvpage;
    path: tvppath;
begin
  result := tvppaths.create;
  for i := 0 to vec.getpagecount - 1 do
  begin
    page := vec.getpageasvectorial(i);
    for j := 0 to page.getentitiescount - 1 do
    begin
      path   := nil;
      entity := page.getentity(j);
      if entity is tvcircle then
      begin
        path := interpolate_circle(tvcircle(entity))
      end else
      if entity is tvcirculararc then
      begin
        path := interpolate_circlearc(tvcirculararc(entity))
      end else
      if entity is tpath then
      begin
        path := interpolate_path(tpath(entity))
      end else
      begin
        {$ifdef debug}
        writeln(entity.classname);
        {$endif}
      end;

      if assigned(path) then
        result.add(path);
    end;
  end;

  result.createtoolpath;
  if zerocenter then
    result.zerocenter;
  if skipsmall then
    result.deletesmallpaths;
end;

end.

