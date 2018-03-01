{
  Description: vPlot coder.

  Copyright (C) 2014-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
{$define debug}

interface

uses
  classes, vplayout;

type
  tvpcoder = class(tthread)
  private
    fpx:      double;
    fpy:      double;
    fpz:      double;
    fxmin:    double;
    fxmax:    double;
    fymin:    double;
    fymax:    double;
    foffsetx: double;
    foffsety: double;
    findex:   longint;
    flist:    tstringlist;
    fonstart: tthreadmethod;
    fonstop:  tthreadmethod;
    fontick:  tthreadmethod;
    fenabled: boolean;
  protected
    procedure execute; override;
  public
    constructor create(list: tstringlist);
    destructor destroy; override;
  public
    property px:      double        read fpx;
    property py:      double        read fpy;
    property pz:      double        read fpz;
    property xmin:    double        read fxmin;
    property xmax:    double        read fxmax;
    property ymin:    double        read fymin;
    property ymax:    double        read fymax;
    property offsetx: double        read foffsetx;
    property offsety: double        read foffsety;

    property index:   longint       read findex;
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop:  tthreadmethod read fonstop  write fonstop;
    property ontick:  tthreadmethod read fontick  write fontick;
    property enabled: boolean       read fenabled write fenabled;
  end;

function  translatepoint(const cc, p: tvppoint): tvppoint; inline;
function  rotatepoint(const p: tvppoint; const alpha: double): tvppoint; inline;
function  distancebetween(const p0, p1: tvppoint): double; inline;
function  linebetween(const p0, p1: tvppoint): tvpline; inline;
function  lineangle(var line: tvpline): double; inline;
function  intersectlines(const l0, l1: tvpline): tvppoint; inline;

procedure optimize(const p: tvppoint; const l: tvplayout; var m0, m1: longint); inline;


implementation

uses
  math, sysutils;

//  parser routines //

function  parse_comment(const gcode: rawbytestring): rawbytestring;
var
  i, j: longint;
begin
  result := gcode;
  i := pos('(', result);
  if i > 0 then
  begin
    j := pos(')', result);
    delete(result, i, j - 1);
  end;

  i := pos(';', result);
  if i > 0 then
    setlength(result, i - 1);
end;

procedure parse_prefix(const prefix, code: rawbytestring; var value: double);
var
  i: longint;
  s: rawbytestring = '';
begin
  i := pos(prefix, code);
  if i > 0 then
  begin
    while (i < length(code)) and (code[i] <> ' ') do
    begin
      s := s + code[i];
      inc(i);
    end;
    delete(s, 1, 1);
  end;

  if length(s) > 0 then
  begin
    for i := 1 to length(s) do
      if s[i] in ['.', ','] then
        s[i] := decimalseparator;
    value := strtofloat(s);
  end;
end;

procedure parse_line(gcode: rawbytestring; var vcode: tvpcode);
begin
  vcode.c := '';
  gcode   := parse_comment(gcode);
  if length(gcode) > 0 then
  begin
    if pos('G0 ' , gcode) = 1 then vcode.c := 'G00 ' else
    if pos('G00 ', gcode) = 1 then vcode.c := 'G00 ' else
    if pos('G1 ' , gcode) = 1 then vcode.c := 'G01 ' else
    if pos('G01 ', gcode) = 1 then vcode.c := 'G01 ' else
    if pos('G2 ' , gcode) = 1 then vcode.c := 'G02 ' else
    if pos('G02 ', gcode) = 1 then vcode.c := 'G02 ' else
    if pos('G3 ' , gcode) = 1 then vcode.c := 'G03 ' else
    if pos('G03 ', gcode) = 1 then vcode.c := 'G03 ';

    if vcode.c <> '' then
    begin
      parse_prefix('X', gcode, vcode.x);
      parse_prefix('Y', gcode, vcode.y);
      parse_prefix('Z', gcode, vcode.z);
      parse_prefix('F', gcode, vcode.f);
      parse_prefix('E', gcode, vcode.e);
      parse_prefix('I', gcode, vcode.i);
      parse_prefix('J', gcode, vcode.j);
      parse_prefix('K', gcode, vcode.k);
      parse_prefix('R', gcode, vcode.r);
    end;
  end;
end;

// geometry routines //

function translatepoint(const cc, p: tvppoint): tvppoint;
begin
  result.x := cc.x + p.x;
  result.y := cc.y + p.y;
end;

function rotatepoint(const p: tvppoint; const alpha: double): tvppoint;
begin
  result.x := p.x * cos(alpha) - p.y * sin(alpha);
  result.y := p.x * sin(alpha) + p.y * cos(alpha);
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

procedure interpolateline(const p0, p1: tvppoint; var path: tvppath);
var
  dx, dy: double;
  i, j: longint;
begin
  i := max(1, round(distancebetween(p0, p1) / 0.15));
  dx := (p1.x - p0.x) / i;
  dy := (p1.y - p0.y) / i;

  setlength(path, i + 1);
  for j := 0 to i do
  begin
    path[j].x := j * dx;
    path[j].y := j * dy;
    path[j]   := translatepoint(p0, path[j]);
  end;
end;

procedure interpolatearc(const p0, p1, cc: tvppoint; clockwise: boolean; var path: tvppath);
var
  i, j: longint;
  angle0: double;
  angle1: double;
  line0: tvpline;
  line1: tvpline;
  sweep: double;
  tmp0: tvppoint;
  tmp1: tvppoint;
  tmp2: tvppoint;
begin
  tmp0.x := p0.x - cc.x;
  tmp0.y := p0.y - cc.y;
  tmp1.x := p1.x - cc.x;
  tmp1.y := p1.y - cc.y;
  tmp2.x := 0;
  tmp2.y := 0;

  line0  := linebetween(tmp2, tmp0);
  line1  := linebetween(tmp2, tmp1);
  angle0 := lineangle(line0);
  angle1 := lineangle(line1);
  sweep  := angle1 - angle0;

  if (clockwise) and (sweep >= 0) then
    sweep := sweep - (2 * pi)
  else
    if (not clockwise) and (sweep <= 0) then
      sweep := sweep + (2 * pi);

  i := max(1, round(abs(sweep) * distancebetween(tmp2, tmp0) / 0.15));
  setlength(path, i + 1);
  for j := 0 to i do
  begin
    path[j] := rotatepoint(tmp0, (j * (sweep / i)));
    path[j] := translatepoint(cc, path[j]);
  end;
end;

//

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
    tmp00  := l.p00;
    tmp01  := l.p01;
    tmp02  := translatepoint(p, rotatepoint(l.p02, alpha));
    tmp03  := translatepoint(p, rotatepoint(l.p03, alpha));
    tmp04  := translatepoint(p, rotatepoint(l.p04, alpha));
    tmp05  := translatepoint(p, rotatepoint(l.p05, alpha));
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
  {$ifdef debug}
  writeln('--- OPTIMIZE ---');
  writeln(format('alpha = %-5.3f', [radtodeg(alpha)]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [tmp02.x, tmp02.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [tmp03.x, tmp03.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [tmp04.x, tmp04.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [tmp05.x, tmp05.y]));
  writeln(format('P06.X = %-5.3f  P06.Y = %-5.3f', [tmp06.x, tmp06.y]));
  writeln(format('D03   = %-5.3f', [distancebetween(tmp00, tmp03)]));
  writeln(format('D14   = %-5.3f', [distancebetween(tmp01, tmp04)]));
  writeln(format('CNT0  = %-5.3u', [m0]));
  writeln(format('CNT1  = %-5.3u', [m1]));

  writeln(format('MODE  = %-5.3u', [l.mode]));
  writeln(format('R     = %-5.8f', [l.ratio]));
  {$endif}
end;

// tvpcoder //

constructor tvpcoder.create(list: tstringlist);
begin
  fpx      := 0;
  fpy      := 0;
  fpz      := 0;
  fxmin    := + maxint;
  fxmax    := - maxint;
  fymin    := + maxint;
  fymax    := - maxint;
  foffsetx := 0;
  foffsety := 0;

  flist    := list;
  fenabled := false;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvpcoder.destroy;
begin
  flist  := nil;
  inherited destroy;
end;

procedure tvpcoder.execute;
var
  code: tvpcode;
     i: longint;
     j: longint;
    p0: tvppoint;
    p1: tvppoint;
    cc: tvppoint;
  path: tvppath;
begin
  code.c := '';
  code.x := 0; code.y := 0; code.z := 0;
  code.f := 0; code.e := 0; code.i := 0;
  code.j := 0; code.k := 0; code.r := 0;
  // ---
  findex := 0;
  while (findex < flist.count) do
  begin
    parse_line(flist[findex], code);
    if (code.c ='G01 ') or (code.c ='G01 ') or
       (code.c ='G02 ') or (code.c ='G03 ') then
      if code.z < 0 then;
      begin
        fxmin := min(fxmin, code.x);
        fxmax := max(fxmax, code.x);
        fymin := min(fymin, code.y);
        fymax := max(fymax, code.y);
      end;
    inc(findex);
  end;
  foffsetx := - (fxmin + fxmax) / 2;
  foffsety := - (fymin + fymax) / 2;
  // ---
  findex := 0;
  if assigned(fonstart) then
    synchronize(fonstart);

  while (findex < flist.count) and (not terminated) do
  begin
    if fenabled then
    begin
      parse_line(flist[findex], code);

      setlength(path, 0);
      if (code.c = 'G00 ') or
         (code.c = 'G01 ') then
      begin
        p0.x := fpx;
        p0.y := fpy;
        p1.x := code.x + foffsetx;
        p1.y := code.y + foffsety;
        interpolateline(p0, p1, path);
      end else
      if (code.c = 'G02 ') or
         (code.c = 'G03 ') then
      begin
        p0.x := fpx;
        p0.y := fpy;
        p1.x := code.x + foffsetx;
        p1.y := code.y + foffsety;
        cc.x := p0.x + code.i;
        cc.y := p0.y + code.j;
        interpolatearc(p0, p1, cc, code.c = 'G02 ', path);
      end;

      j := length(path);
      if j > 0 then
        for i := 0 to j - 1 do
          if not terminated then
          begin
            fpx := path[i].x;
            fpy := path[i].y;
            fpz :=    code.z;
            if assigned(fontick) then
              synchronize(fontick);
          end;
      inc(findex);
    end else
      sleep(250);
  end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

end.

