{ Description: vPlot library.

  Copyright (C) 2014-2017 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit libvplot;

{$mode objfpc}{$H+}
{$define debug}

interface

uses
  classes, inifiles;

type
  tvplotpoint = packed record
    x: double;
    y: double;
  end;

  tvplotline = packed record
    a: double;
    b: double;
    c: double;
  end;

  tvplotposition = packed record
    m0: longint;
    m1: longint;
    p:  tvplotpoint;
  end;

  tvplotcode = packed record
    c: rawbytestring;
    x: double;
    y: double;
    z: double;
    f: double;
    e: double;
    i: double;
    j: double;
    k: double;
    r: double;
  end;

  tvplotsetup = packed record
    point0:  tvplotpoint;
    point1:  tvplotpoint;
    point2:  tvplotpoint;
    point3:  tvplotpoint;
    point4:  tvplotpoint;
    point5:  tvplotpoint;
    point9:  tvplotpoint;
    ratio:   double;
    mode:    byte;
  end;

  tvplotdriver = class
  private
    fcount0:  longint;
    fcount1:  longint;
    fcountz:  longint;
    fdelayms: longword;
    fenabled: boolean;
    ffault:   longint;
    procedure largedisplacements(cnt0, cnt1: longint);
    procedure smalldisplacements(cnt0, cnt1: longint);
  public
    constructor create(mode: longint);
    destructor destroy; override;
    procedure  init (cnt0, cnt1, cntz: longint);
    procedure  move2(cnt0, cnt1, cntz: longint);
    procedure  move4(cnt0, cnt1, cntz: longint);
  published
    property fault:   longint  read ffault;
    property delayms: longword read fdelayms write fdelayms;
    property enabled: boolean  read fenabled write fenabled;
  end;

  tvplotcoder = class(tthread)
  private
    fpx: double;
    fpy: double;
    fpz: double;
    fmot0: longint;
    fmot1: longint;
    fmotz: longint;
    foffsetx: double;
    foffsety: double;
    fpath: array of tvplotposition;
    flist: tstringlist;
    flistindex: longint;
    flistcount: longint;
    fenabled: boolean;
    fontick:  tthreadmethod;
    procedure interpolateline(const p0, p1: tvplotpoint);
    procedure interpolatearc (const p0, p1, cc: tvplotpoint; clockwise: boolean);
    procedure encode(const code: tvplotcode);
    procedure dotick(const position: tvplotposition; const z: double);
  protected
    procedure execute; override;
  public
    constructor create(inlist: tstringlist);
    destructor destroy; override;
  published
    property enabled:   boolean       read fenabled write fenabled;
    property listindex: longint       read flistindex;
    property listcount: longint       read flistcount;
    property px:        double        read fpx;
    property py:        double        read fpy;
    property pz:        double        read fpz;
    property mot0:      longint       read fmot0;
    property mot1:      longint       read fmot1;
    property motz:      longint       read fmotz;
    property offsetx:   double        read foffsetx;
    property offsety:   double        read foffsety;
    property ontick:    tthreadmethod read fontick write fontick;
  end;

function  translatepoint(const cc, p: tvplotpoint): tvplotpoint; inline;
function  rotatepoint(const p: tvplotpoint; const alpha: double): tvplotpoint; inline;
function  distancebetween(const p0, p1: tvplotpoint): double; inline;
function  linebetween(const p0, p1: tvplotpoint): tvplotline; inline;
function  lineangle(var line: tvplotline): double; inline;
function  intersectlines(const l0, l1: tvplotline): tvplotpoint; inline;
function  optimize(var position: tvplotposition; const l: tvplotsetup): double; inline;
procedure loadsetup(inifile: tinifile; var setup: tvplotsetup);

var
  vplotcoder:  tvplotcoder  = nil;
  vplotdriver: tvplotdriver = nil;
  vplotsetup:  tvplotsetup;
  vplothome:   tvplotposition;

implementation

uses
  math, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils;

{$ifdef cpuarm}
const
  mot0_step     = P38;
  mot0_dir      = P40;
  mot1_step     = P29;
  mot1_dir      = P31;

  motx_mod0     = P15;
  motx_mod1     = P13;
  motx_mod2     = P11;

  motz_maxvalue = 2.50;
  motz_minvalue = 0.50;
  motz_rstvalue = 1.50;
  motz_incvalue = 0.10;
  motz_freq     = 50;

  vplotmatrix : array [0..10, 0..18] of longint = (
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  //  0
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),  //  1
    (0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0),  //  2
    (0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0),  //  3
    (1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1),  //  4
    (1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1),  //  5
    (0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0),  //  6
    (1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1),  //  7
    (1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1),  //  8
    (0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),  //  9
    (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)); // 10
{$endif}

//  gcode parser //

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

procedure parse_prefix(const prefix, gcode: rawbytestring; var value: double);
var
  i: longint;
  s: rawbytestring = '';
begin
  i := pos(prefix, gcode);
  if i > 0 then
  begin
    while (i < length(gcode)) and (gcode[i] <> ' ') do
    begin
      s := s + gcode[i];
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

procedure parse_line(gcode: rawbytestring; var vcode: tvplotcode);
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

// routines //

function translatepoint(const cc, p: tvplotpoint): tvplotpoint;
begin
  result.x := cc.x + p.x;
  result.y := cc.y + p.y;
end;

function rotatepoint(const p: tvplotpoint; const alpha: double): tvplotpoint;
begin
  result.x := p.x * cos(alpha) - p.y * sin(alpha);
  result.y := p.x * sin(alpha) + p.y * cos(alpha);
end;

function distancebetween(const p0, p1: tvplotpoint): double;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function linebetween(const p0, p1: tvplotpoint): tvplotline;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y - (p1.y - p0.y) * p0.x;
end;

function lineangle(var line: tvplotline): double;
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

function intersectlines(const l0, l1: tvplotline): tvplotpoint;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    result.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    result.y := (-l0.c - l0.a * result.x) / (l0.b);
  end else
    raise exception.create('Intersectlines routine exception');
end;

procedure loadsetup(inifile: tinifile; var setup: tvplotsetup);
var
  offsetx: double;
  offsety: double;
begin
  inifile.formatsettings.decimalseparator := '.';
  setup.ratio    := inifile.readfloat  ('VPLOT v1.0', 'R'   ,  -1);
  setup.mode     := inifile.readinteger('VPLOT v1.0', 'MODE',  -1);
  setup.point0.x := inifile.readfloat  ('VPLOT v1.0', 'P00.X', -1);
  setup.point0.y := inifile.readfloat  ('VPLOT v1.0', 'P00.Y', -1);
  setup.point1.x := inifile.readfloat  ('VPLOT v1.0', 'P01.X', -1);
  setup.point1.y := inifile.readfloat  ('VPLOT v1.0', 'P01.Y', -1);
  setup.point2.x := inifile.readfloat  ('VPLOT v1.0', 'P02.X', -1);
  setup.point2.y := inifile.readfloat  ('VPLOT v1.0', 'P02.Y', -1);
  offsetx        := inifile.readfloat('VPLOT v1.0', 'D23.X', -1);
  offsety        := inifile.readfloat('VPLOT v1.0', 'D23.Y', -1);
  setup.point3.x := setup.point2.x + offsetx;
  setup.point3.y := setup.point2.y + offsety;
  offsetx        := inifile.readfloat('VPLOT v1.0', 'D24.X', -1);
  offsety        := inifile.readfloat('VPLOT v1.0', 'D24.Y', -1);
  setup.point4.x := setup.point2.x + offsetx;
  setup.point4.y := setup.point2.y + offsety;
  offsetx        := inifile.readfloat('VPLOT v1.0', 'D25.X', -1);
  offsety        := inifile.readfloat('VPLOT v1.0', 'D25.Y', -1);
  setup.point5.x := setup.point2.x + offsetx;
  setup.point5.y := setup.point2.y + offsety;
  setup.point9.x := -setup.point2.x;
  setup.point9.y := -setup.point2.y;
  setup.point2   :=  translatepoint(setup.point9, setup.point2);
  setup.point3   :=  translatepoint(setup.point9, setup.point3);
  setup.point4   :=  translatepoint(setup.point9, setup.point4);
  setup.point5   :=  translatepoint(setup.point9, setup.point5);
  setup.point9.x := -setup.point9.x;
  setup.point9.y := -setup.point9.y;
  {$ifdef debug}
  writeln('--- VPLOT v1.0 ---');
  writeln(format('P00.X = %-5.3f  P00.Y = %-5.3f', [setup.point0.x, setup.point0.y]));
  writeln(format('P01.X = %-5.3f  P01.Y = %-5.3f', [setup.point1.x, setup.point1.y]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [setup.point2.x, setup.point2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [setup.point3.x, setup.point3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [setup.point4.x, setup.point4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [setup.point5.x, setup.point5.y]));
  writeln(format('P09.X = %-5.3f  P09.Y = %-5.3f', [setup.point9.x, setup.point9.y]));

  writeln(format('MODE  = %-5.3u', [setup.mode]));
  writeln(format('R     = %-5.3f', [setup.ratio]));
  {$endif}
end;

function optimize(var position: tvplotposition; const l: tvplotsetup): double;
var
  err:  double;
  tmp0: tvplotpoint;
  tmp1: tvplotpoint;
  tmp2: tvplotpoint;
  tmp3: tvplotpoint;
  tmp4: tvplotpoint;
  tmp5: tvplotpoint;
  tmp6: tvplotpoint;
begin
  result  := 0;
  repeat
    tmp0  := l.point0;
    tmp1  := l.point1;
    tmp2  := translatepoint(position.p, rotatepoint(l.point2, result));
    tmp3  := translatepoint(position.p, rotatepoint(l.point3, result));
    tmp4  := translatepoint(position.p, rotatepoint(l.point4, result));
    tmp5  := translatepoint(position.p, rotatepoint(l.point5, result));
    tmp6  := intersectlines(linebetween(tmp0, tmp3),
                            linebetween(tmp1, tmp4));

    err := abs(tmp6.x - tmp5.x);
    if  err > 0.001 then
    begin
      if tmp6.x < tmp5.x then
        result := result - (err / 100)
      else
      if tmp6.x > tmp5.x then
        result := result + (err / 100);
    end else
      break;
  until false;

  position.m0 := round(l.mode * (distancebetween(tmp0, tmp3) / l.ratio));
  position.m1 := round(l.mode * (distancebetween(tmp1, tmp4) / l.ratio));
  {$ifdef debug}
  writeln('--- OPTIMIZE ---');
  writeln(format('alpha = %-5.3f', [radtodeg(result)]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [tmp2.x, tmp2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [tmp3.x, tmp3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [tmp4.x, tmp4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [tmp5.x, tmp5.y]));
  writeln(format('P06.X = %-5.3f  P06.Y = %-5.3f', [tmp6.x, tmp6.y]));
  writeln(format('D03   = %-5.3f', [distancebetween(tmp0, tmp3)]));
  writeln(format('D14   = %-5.3f', [distancebetween(tmp1, tmp4)]));
  writeln(format('CNT0  = %-5.3u', [position.m0]));
  writeln(format('CNT1  = %-5.3u', [position.m1]));
  {$endif}
end;

// tvplotcoder //

constructor tvplotcoder.create(inlist: tstringlist);
begin
  fenabled := false;
  flist    := inlist;
  // ---
  freeonterminate := true;
  inherited create(true);
end;

destructor tvplotcoder.destroy;
begin
  flist      := nil;
  vplotcoder := nil;
  inherited destroy;
end;

procedure tvplotcoder.dotick(const position: tvplotposition; const z: double);
begin
  fpx   := position.p.x;
  fpy   := position.p.y;
  fpz   := z;
  fmot0 := position.m0;
  fmot1 := position.m1;
  fmotz := round(z);
  if assigned(fontick) then
  begin
    synchronize(fontick);
  end;
end;

procedure tvplotcoder.interpolateline(const p0, p1: tvplotpoint);
var
  dx, dy: double;
  i, j: longint;
begin
  i := max(1, round(distancebetween(p0, p1) / 0.15));

  dx := (p1.x - p0.x) / i;
  dy := (p1.y - p0.y) / i;

  setlength(fpath, i + 1);
  for j := 0 to i do
  begin
    fpath[j].p.x := j * dx;
    fpath[j].p.y := j * dy;
    fpath[j].p   := translatepoint(p0, fpath[j].p);
  end;
end;

procedure tvplotcoder.interpolatearc(const p0, p1, cc: tvplotpoint; clockwise: boolean);
var
  line0: tvplotline;
  line1: tvplotline;
  angle0: double;
  angle1: double;
  sweep:  double;
  i, j: longint;
  tmp0: tvplotpoint;
  tmp1: tvplotpoint;
  tmp2: tvplotpoint;
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
  setlength(fpath, i + 1);
  for j := 0 to i do
  begin
    fpath[j].p := rotatepoint(tmp0, (j * (sweep / i)));
    fpath[j].p := translatepoint(cc, fpath[j].p);
  end;
end;

procedure tvplotcoder.encode(const code: tvplotcode);
var
  i, j: longint;
  p0, p1, cc: tvplotpoint;
begin
  setlength(fpath, 0);
  if (code.c ='G00 ') or (code.c = 'G01 ') then
  begin
    p0.x := fpx;
    p0.y := fpy;
    p1.x := code.x + foffsetx;
    p1.y := code.y + foffsety;
    interpolateline(p0, p1);
  end else
  if (code.c ='G02 ') or (code.c = 'G03 ') then
  begin
    p0.x := fpx;
    p0.y := fpy;
    p1.x := code.x + foffsetx;
    p1.y := code.y + foffsety;
    cc.x := p0.x + code.i;
    cc.y := p0.y + code.j;
    interpolatearc(p0, p1, cc, code.c = 'G02 ');
  end;

  j := length(fpath);
  if j > 0 then
    for i := 0 to j - 1 do
      if not terminated then
      begin
        optimize(fpath[i], vplotsetup);
        dotick(fpath[i], code.z);
      end;
end;

procedure tvplotcoder.execute;
var
  code: tvplotcode;
  xmin, xmax: double;
  ymin, ymax: double;
begin
  code.c := '';
  code.x := 0;
  code.y := 0;
  code.z := 0;
  code.f := 0;
  code.e := 0;
  code.i := 0;
  code.j := 0;
  code.k := 0;
  code.r := 0;
  // ---
  xmin   := vplotsetup.point1.x;
  xmax   := 0;
  ymin   := vplotsetup.point1.y;
  ymax   := 0;
  // ---
  flistindex := 0;
  flistcount := flist.count;
  while (flistindex < flistcount) do
  begin
    parse_line(flist[flistindex], code);
    if (code.c ='G00 ') or (code.c ='G01 ') or
       (code.c ='G02 ') or (code.c ='G03 ') then
    begin
      xmin := min(xmin, code.x);
      xmax := max(xmax, code.x);
      ymin := min(ymin, code.y);
      ymax := max(ymax, code.y);
    end;
    inc(flistindex);
  end;
  foffsetx := (vplotsetup.point1.x / 2) - ((xmax + xmin) / 2);
  foffsety := (vplotsetup.point1.y / 2) - ((ymax + ymin) / 2);
  {$ifdef debug}
  writeln('--- CENTRE DRAWING ---');
  writeln('offset x = ', foffsetx:2:2);
  writeln('offset y = ', foffsety:2:2);
  {$endif}
  // ---
  flistindex := 0;
  flistcount := flist.count;
  dotick(vplothome, 1);
  while (flistindex < flistcount) and (not terminated) do
  begin
    if enabled then
    begin
      parse_line(flist[flistindex], code);
      if (code.c ='G00 ') or (code.c ='G01 ') or
         (code.c ='G02 ') or (code.c ='G03 ') then encode(code);
         inc(flistindex);
    end else
      sleep(250);
  end;
  dotick(vplothome, 1);
end;

// tvplotdriver //

constructor tvplotdriver.create(mode: longint);
begin
  inherited create;
  {$ifdef cpuarm}
  // setup wiringpi library
  ffault := wiringpisetup;
  // setup pca9685 library
  if ffault <> -1 then
    ffault := pca9685setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, motz_freq);

  if ffault <> -1 then
  begin
    // init servo
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_minvalue, motz_freq)); delay(2000);
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq)); delay(2000);
    pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq)); delay(2000);
    // init mode
    pinmode(motx_mod0,    OUTPUT);
    pinmode(motx_mod1,    OUTPUT);
    pinmode(motx_mod2,    OUTPUT);

    if mode = 1 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 2 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 4 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 8 then
    begin
      digitalwrite(motx_mod0,  LOW);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2, HIGH);
    end else
    if mode = 16 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 32 then
    begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1,  LOW);
      digitalwrite(motx_mod2, HIGH);
    end else
    if mode = 64 then
      begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2,  LOW);
    end else
    if mode = 128 then
      begin
      digitalwrite(motx_mod0, HIGH);
      digitalwrite(motx_mod1, HIGH);
      digitalwrite(motx_mod2, HIGH);
    end else
      ffault := -1;

    // init step motor0
    pinmode(mot0_dir,    OUTPUT);
    pinmode(mot0_step,   OUTPUT);
    digitalwrite(mot0_dir,  LOW);
    digitalwrite(mot0_step, LOW);
    // init step motor1
    pinmode(mot1_dir,    OUTPUT);
    pinmode(mot1_step,   OUTPUT);
    digitalwrite(mot1_dir,  LOW);
    digitalwrite(mot1_step, LOW);
  end;
  {$else}
  ffault   := -1;
  {$endif}
  fdelayms := 1000;
  fenabled := false;
end;

destructor tvplotdriver.destroy;
begin
  vplotdriver := nil;
  inherited destroy;
end;

procedure  tvplotdriver.init(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  fcount0  := cnt0;
  fcount1  := cnt1;
  fcountz  := cntz;
  {$endif}
end;

procedure tvplotdriver.move2(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  move4(cnt0 - fcount0, cnt1 - fcount1, cntz - fcountz);
  {$endif}
end;

procedure tvplotdriver.largedisplacements(cnt0, cnt1: longint);
begin
  {$ifdef cpuarm}
  inc(fcount0, cnt0);
  inc(fcount1, cnt1);

  if cnt0 > 0 then
    digitalwrite(mot0_dir, HIGH)
  else
    digitalwrite(mot0_dir,  LOW);

  if cnt1 > 0 then
    digitalwrite(mot1_dir,  LOW)
  else
    digitalwrite(mot1_dir, HIGH);

  // move step motor0
  cnt0 := abs(cnt0);
  while cnt0 > 0 do
  begin
    digitalwrite(mot0_step, HIGH);  delaymicroseconds(fdelayms);
    digitalwrite(mot0_step,  LOW);  delaymicroseconds(fdelayms);
    dec(cnt0);
  end;
  // move step motor1
  cnt1 := abs(cnt1);
  while cnt1 > 0 do
  begin
    digitalwrite(mot1_step, HIGH);  delaymicroseconds(fdelayms);
    digitalwrite(mot1_step,  LOW);  delaymicroseconds(fdelayms);
    dec(cnt1);
  end;
  {$endif}
end;

procedure tvplotdriver.smalldisplacements(cnt0, cnt1: longint);
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
  {$ifdef cpuarm}
  inc(fcount0, cnt0);
  inc(fcount1, cnt1);

  if cnt0 > 0 then
    digitalwrite(mot0_dir, HIGH)
  else
    digitalwrite(mot0_dir,  LOW);

  if cnt1 > 0 then
    digitalwrite(mot1_dir,  LOW)
  else
    digitalwrite(mot1_dir, HIGH);

  cnt0 := abs(cnt0);
  cnt1 := abs(cnt1);
  for i := 0 to 18 do
  begin
    // move step motor0
    if vplotmatrix[cnt0, i] = 1 then
    begin
      digitalwrite(mot0_step, HIGH);  delaymicroseconds(fdelayms);
      digitalwrite(mot0_step,  LOW);  delaymicroseconds(fdelayms);
    end;
    // move step motor1
    if vplotmatrix[cnt1, i] = 1 then
    begin
      digitalwrite(mot1_step, HIGH);  delaymicroseconds(fdelayms);
      digitalwrite(mot1_step,  LOW);  delaymicroseconds(fdelayms);
    end;
  end;
  {$endif}
end;

procedure tvplotdriver.move4(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  if not enabled  then exit;
  if ffault  = -1 then exit;
  // move pwm motz
  if cntz <> 0 then
  begin
    fcountz := min(1, max(-1, fcountz + cntz));
    if fcountz < 0 then
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq))
    else
    if fcountz > 0 then
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_minvalue, motz_freq))
    else
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_rstvalue, motz_freq));
    delay(1000);
  end;
  // mode step motors
  if (cnt0 <> 0) or
     (cnt1 <> 0) then
  begin
    if (abs(cnt0) < 11) and
       (abs(cnt1) < 11) then
      smalldisplacements(cnt0, cnt1)
    else
      largedisplacements(cnt0, cnt1);
  end;
  {$ifdef debug}
  writeln('fcountz = ', fcountz);
  writeln('fcount1 = ', fcount0);
  writeln('fcount1 = ', fcount1);
  {$endif}
  {$endif}
end;

end.

