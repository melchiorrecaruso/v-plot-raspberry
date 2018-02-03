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
{*$define debug}

interface

uses
  classes, inifiles;

type
  tvppoint = packed record
    x: double;
    y: double;
  end;

  tvpline = packed record
    a: double;
    b: double;
    c: double;
  end;

  tvplayout = packed record
    point0:  tvppoint;
    point1:  tvppoint;
    point2:  tvppoint;
    point3:  tvppoint;
    point4:  tvppoint;
    point5:  tvppoint;
    point8:  tvppoint;
    point9:  tvppoint;
    ratio:   double;
    mode:    byte;
  end;

  tvpcode = packed record
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

  tvpdriver = class
  private
    fcnt0:  longint;
    fcnt1:  longint;
    fcntz:  longint;
    fdelayms: longword;
    ffault:   longint;
    procedure largedisplacements(cnt0, cnt1: longint);
    procedure smalldisplacements(cnt0, cnt1: longint);
  public
    constructor create(mode: longint);
    destructor  destroy; override;
    procedure   init (cnt0, cnt1, cntz: longint);
    procedure   move2(cnt0, cnt1, cntz: longint);
    procedure   move4(cnt0, cnt1, cntz: longint);
  published
    property delayms: longword read fdelayms write fdelayms;
    property fault:   longint  read ffault;
  end;

  tvpcoder = class(tthread)
  private
    fpx: double;
    fpy: double;
    fpz: double;
    findex: longint;
    fitems: tstringlist;
    fpath: array of tvppoint;
    fonstart: tthreadmethod;
    fonstop:  tthreadmethod;
    fontick:  tthreadmethod;
    function getcount: longint;
    function getitem(index: longint): rawbytestring;
    procedure interpolateline(const p0, p1: tvppoint);
    procedure interpolatearc (const p0, p1, cc: tvppoint; clockwise: boolean);
    procedure dotick(const p: tvppoint; const z: double);
    procedure encode(const code: tvpcode);
  protected
    procedure execute; override;
  public
    constructor create(list: tstringlist);
    destructor destroy; override;
  public
    property px: double read fpx;
    property py: double read fpy;
    property pz: double read fpz;
    property count: integer read getcount;
    property index: longint read findex;
    property items[i: longint]: string read getitem;
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop:  tthreadmethod read fonstop  write fonstop;
    property ontick:  tthreadmethod read fontick  write fontick;
  end;


function  translatepoint(const cc, p: tvppoint): tvppoint; inline;
function  rotatepoint(const p: tvppoint; const alpha: double): tvppoint; inline;
function  distancebetween(const p0, p1: tvppoint): double; inline;
function  linebetween(const p0, p1: tvppoint): tvpline; inline;
function  lineangle(var line: tvpline): double; inline;
function  intersectlines(const l0, l1: tvpline): tvppoint; inline;



procedure loadlayout(ini: tinifile; var layout: tvplayout);
procedure optimize(const p: tvppoint; const l: tvplayout; var m0, m1: longint); inline;

var
  vpcoder:  tvpcoder = nil;
  vplayout: tvplayout;

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

// routines //

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

// ---

procedure loadlayout(ini: tinifile; var layout: tvplayout);
begin
  ini.formatsettings.decimalseparator := '.';
  layout.ratio    := ini.readfloat  ('VPLOT v1.0', 'R'   ,  -1);
  layout.mode     := ini.readinteger('VPLOT v1.0', 'MODE',  -1);
  layout.point0.x := ini.readfloat  ('VPLOT v1.0', 'P00.X', -1);
  layout.point0.y := ini.readfloat  ('VPLOT v1.0', 'P00.Y', -1);
  layout.point1.x := ini.readfloat  ('VPLOT v1.0', 'P01.X', -1);
  layout.point1.y := ini.readfloat  ('VPLOT v1.0', 'P01.Y', -1);
  layout.point2.x := ini.readfloat  ('VPLOT v1.0', 'P02.X', -1);
  layout.point2.y := ini.readfloat  ('VPLOT v1.0', 'P02.Y', -1);
  layout.point3.x := ini.readfloat  ('VPLOT v1.0', 'P03.X', -1);
  layout.point3.y := ini.readfloat  ('VPLOT v1.0', 'P03.Y', -1);
  layout.point4.x := ini.readfloat  ('VPLOT v1.0', 'P04.X', -1);
  layout.point4.y := ini.readfloat  ('VPLOT v1.0', 'P04.Y', -1);
  layout.point5.x := ini.readfloat  ('VPLOT v1.0', 'P05.X', -1);
  layout.point5.y := ini.readfloat  ('VPLOT v1.0', 'P05.Y', -1);
  layout.point8.x := ini.readfloat  ('VPLOT v1.0', 'P08.X', -1);
  layout.point8.y := ini.readfloat  ('VPLOT v1.0', 'P08.Y', -1);
  layout.point9.x := ini.readfloat  ('VPLOT v1.0', 'P09.X', -1);
  layout.point9.y := ini.readfloat  ('VPLOT v1.0', 'P09.Y', -1);
  {$ifdef debug}
  writeln('--- VPLOT v1.0 ---');
  writeln(format('P00.X = %-5.3f  P00.Y = %-5.3f', [layout.point0.x, layout.point0.y]));
  writeln(format('P01.X = %-5.3f  P01.Y = %-5.3f', [layout.point1.x, layout.point1.y]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [layout.point2.x, layout.point2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [layout.point3.x, layout.point3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [layout.point4.x, layout.point4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [layout.point5.x, layout.point5.y]));
  writeln(format('P08.X = %-5.3f  P08.Y = %-5.3f', [layout.point8.x, layout.point8.y]));
  writeln(format('P09.X = %-5.3f  P09.Y = %-5.3f', [layout.point9.x, layout.point9.y]));

  writeln(format('MODE  = %-5.3u', [layout.mode]));
  writeln(format('R     = %-5.3f', [layout.ratio]));
  {$endif}
end;

procedure optimize(const p: tvppoint; const l: tvplayout; var m0, m1: longint);
var
  alpha: double;
  err:   double;
  tmp0:  tvppoint;
  tmp1:  tvppoint;
  tmp2:  tvppoint;
  tmp3:  tvppoint;
  tmp4:  tvppoint;
  tmp5:  tvppoint;
  tmp6:  tvppoint;
begin
  alpha   := 0;
  repeat
    tmp0  := l.point0;
    tmp1  := l.point1;
    tmp2  := translatepoint(p, rotatepoint(l.point2, alpha));
    tmp3  := translatepoint(p, rotatepoint(l.point3, alpha));
    tmp4  := translatepoint(p, rotatepoint(l.point4, alpha));
    tmp5  := translatepoint(p, rotatepoint(l.point5, alpha));
    tmp6  := intersectlines(linebetween(tmp0, tmp3),
                            linebetween(tmp1, tmp4));

    err := abs(tmp6.x - tmp5.x);
    if  err > 0.001 then
    begin
      if tmp6.x < tmp5.x then
        alpha := alpha - (err / 100)
      else
      if tmp6.x > tmp5.x then
        alpha := alpha + (err / 100);
    end else
      break;
  until false;

  m0 := round(l.mode * (distancebetween(tmp0, tmp3) / l.ratio));
  m1 := round(l.mode * (distancebetween(tmp1, tmp4) / l.ratio));
  {$ifdef debug}
  writeln('--- OPTIMIZE ---');
  writeln(format('alpha = %-5.3f', [radtodeg(alpha)]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [tmp2.x, tmp2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [tmp3.x, tmp3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [tmp4.x, tmp4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [tmp5.x, tmp5.y]));
  writeln(format('P06.X = %-5.3f  P06.Y = %-5.3f', [tmp6.x, tmp6.y]));
  writeln(format('D03   = %-5.3f', [distancebetween(tmp0, tmp3)]));
  writeln(format('D14   = %-5.3f', [distancebetween(tmp1, tmp4)]));
  writeln(format('CNT0  = %-5.3u', [m0]));
  writeln(format('CNT1  = %-5.3u', [m1]));
  {$endif}
end;

// tvplotcoder //

constructor tvpcoder.create(list: tstringlist);
begin
  findex := 0;
  fitems := list;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvpcoder.destroy;
begin
  vpcoder := nil;
  fitems  := nil;
  inherited destroy;
end;

procedure tvpcoder.dotick(const p: tvppoint; const z: double);
begin
  fpx := p.x;
  fpy := p.y;
  fpz :=   z;
  if assigned(fontick) then
    synchronize(fontick);
end;

procedure tvpcoder.interpolateline(const p0, p1: tvppoint);
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
    fpath[j].x := j * dx;
    fpath[j].y := j * dy;
    fpath[j]   := translatepoint(p0, fpath[j]);
  end;
end;

procedure tvpcoder.interpolatearc(const p0, p1, cc: tvppoint; clockwise: boolean);
var
  angle0: double;
  angle1: double;
  i, j: longint;
  sweep: double;
  line0: tvpline;
  line1: tvpline;
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
  setlength(fpath, i + 1);
  for j := 0 to i do
  begin
    fpath[j] := rotatepoint(tmp0, (j * (sweep / i)));
    fpath[j] := translatepoint(cc, fpath[j]);
  end;
end;

procedure tvpcoder.encode(const code: tvpcode);
var
  i, j: longint;
  p0, p1, cc: tvppoint;
begin
  setlength(fpath, 0);
  if (code.c ='G00 ') or (code.c = 'G01 ') then
  begin
    p0.x := fpx;
    p0.y := fpy;
    p1.x := code.x;
    p1.y := code.y;
    interpolateline(p0, p1);
  end else
  if (code.c ='G02 ') or (code.c = 'G03 ') then
  begin
    p0.x := fpx;
    p0.y := fpy;
    p1.x := code.x;
    p1.y := code.y;
    cc.x := p0.x + code.i;
    cc.y := p0.y + code.j;
    interpolatearc(p0, p1, cc, code.c = 'G02 ');
  end;

  j := length(fpath);
  if j > 0 then
    for i := 0 to j - 1 do
    begin
      if not terminated then
        dotick(fpath[i], code.z);
    end;
end;

procedure tvpcoder.execute;
var
  code:    tvpcode;
  codeoff: tvpcode;
  xmin, xmax: double;
  ymin, ymax: double;
  offsetx: double;
  offsety: double;
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
  xmin   := +maxint;
  xmax   := -maxint;
  ymin   := +maxint;
  ymax   := -maxint;
  // ---
  findex := 0;
  while (findex < fitems.count) do
  begin
    parse_line(fitems[findex], code);
    if (code.c ='G00 ') or (code.c ='G01 ') or
       (code.c ='G02 ') or (code.c ='G03 ') then
      if code.z < 0 then;
      begin
        xmin := min(xmin, code.x);
        xmax := max(xmax, code.x);
        ymin := min(ymin, code.y);
        ymax := max(ymax, code.y);
      end;
    inc(findex);
  end;
  offsetx := - (xmax + xmin) / 2;
  offsety := - (ymax + ymin) / 2;
  {$ifdef debug}
  writeln('--- CENTRE DRAWING ---');
  writeln('xmin = ', xmin:2:2);
  writeln('xmax = ', xmax:2:2);
  writeln('ymin = ', ymin:2:2);
  writeln('ymax = ', ymax:2:2);
  writeln('offset x = ', offsetx:2:2);
  writeln('offset y = ', offsety:2:2);
  {$endif}
  // ---
  findex := 0;
  if assigned(fonstart) then fonstart;
  while (findex < fitems.count) and (not terminated) do
  begin
    parse_line(fitems[findex], code);
    if (code.c ='G00 ') or (code.c ='G01 ') or
       (code.c ='G02 ') or (code.c ='G03 ') then
    begin
      codeoff   := code;
      codeoff.x := codeoff.x + offsetx;
      codeoff.y := codeoff.y + offsety;
      encode(codeoff);
    end;
    inc(findex);
  end;
  if assigned(fonstop) then fonstop;
end;

function tvpcoder.getitem(index: longint): rawbytestring;
begin
  result := fitems[index];
end;

function tvpcoder.getcount: longint;
begin
  result := fitems.count;
end;

// tvplotdriver //

constructor tvpdriver.create(mode: longint);
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
end;

destructor tvpdriver.destroy;
begin
  inherited destroy;
end;

procedure  tvpdriver.init(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  fcnt0  := cnt0;
  fcnt1  := cnt1;
  fcntz  := cntz;
  {$endif}
end;

procedure tvpdriver.move2(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  move4(cnt0 - fcnt0, cnt1 - fcnt1, cntz - fcntz);
  {$endif}
end;

procedure tvpdriver.largedisplacements(cnt0, cnt1: longint);
begin
  {$ifdef cpuarm}
  inc(fcnt0, cnt0);
  inc(fcnt1, cnt1);

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

procedure tvpdriver.smalldisplacements(cnt0, cnt1: longint);
{$ifdef cpuarm}
var
  i: longint;
{$endif}
begin
  {$ifdef cpuarm}
  inc(fcnt0, cnt0);
  inc(fcnt1, cnt1);

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

procedure tvpdriver.move4(cnt0, cnt1, cntz: longint);
begin
  {$ifdef cpuarm}
  if ffault  = -1 then exit;
  // move pwm motz
  if cntz <> 0 then
  begin
    fcntz := min(1, max(-1, fcntz + cntz));
    if fcntz < 0 then
      pwmwrite(PCA9685_PIN_BASE + 0, calcticks(motz_maxvalue, motz_freq))
    else
    if fcntz > 0 then
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

