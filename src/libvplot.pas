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
  classes, inifiles, {$ifdef raspberrypi} pca9685, wiringpi, {$endif} sysutils;

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

type
  tvplotcoder = class(tthread)
  private

    fpx:     double;
    fpy:     double;
    fpz:     double;
    fmot0:   longint;
    fmot1:   longint;
    fmotz:   longint;

    finlist:  tstringlist;
    finifile: tinifile;
    findex:   longint;

    fontick: tthreadmethod;

    fpoint0:    tvplotpoint;
    fpoint1:    tvplotpoint;
    fpoint2:    tvplotpoint;
    fpoint3:    tvplotpoint;
    fpoint4:    tvplotpoint;
    fpoint5:    tvplotpoint;

    fpoint9:    tvplotpoint;
    fratio:     double;

    fcurrpath:  array of tvplotposition;
    procedure interpolateline(const p0, p1: tvplotpoint);
    procedure interpolatearc (const p0, p1, cc: tvplotpoint; clockwise: boolean);
    procedure optimizexy(var position: tvplotposition);
    procedure encode(const code: tvplotcode);
    procedure dotick(const position: tvplotposition; const z: double);
  protected
    procedure execute; override;
  public
    constructor create(inlist: tstringlist; inifile: tinifile);
    destructor destroy; override;
  published
    property px:      double        read fpx;
    property py:      double        read fpy;
    property pz:      double        read fpz;
    property mot0:    longint       read fmot0;
    property mot1:    longint       read fmot1;
    property motz:    longint       read fmotz;
    property index:   longint       read findex;
  published
    property ontick:  tthreadmethod read fontick write fontick;
  end;

type
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
    constructor create;
    destructor destroy; override;
    procedure  init (cnt0, cnt1, cntz: longint);
    procedure  move2(cnt0, cnt1, cntz: longint);
    procedure  move4(cnt0, cnt1, cntz: longint);
  published
    property fault:   longint  read ffault;
    property delayms: longword read fdelayms write fdelayms;
    property enabled: boolean  read fenabled write fenabled;
  end;

var
  vplotcoder:  tvplotcoder  = nil;
  vplotdriver: tvplotdriver = nil;

implementation

uses
  math;

{$ifdef raspberrypi}
const
  mot0_step     = P38;
  mot0_dir      = P40;
  mot1_step     = P16;
  mot1_dir      = P18;

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
    value := strtofloat(s);
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

function translatepoint(const cc, p: tvplotpoint): tvplotpoint; inline;
begin
  result.x := cc.x + p.x;
  result.y := cc.y + p.y;
end;

function rotatepoint(const p: tvplotpoint; const alpha: double): tvplotpoint; inline;
begin
  result.x := p.x * cos(alpha) - p.y * sin(alpha);
  result.y := p.x * sin(alpha) + p.y * cos(alpha);
end;

function distancebetween(const p0, p1: tvplotpoint): double; inline;
begin
  result := sqrt(sqr(p1.x - p0.x) + sqr(p1.y - p0.y));
end;

function linebetween(const p0, p1: tvplotpoint): tvplotline; inline;
begin
  result.a :=  p1.y - p0.y;
  result.b :=  p0.x - p1.x;
  result.c := (p1.x - p0.x) * p0.y - (p1.y - p0.y) * p0.x;
end;

function lineangle(var line: tvplotline): double; inline;
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

function intersectlines(const l0, l1: tvplotline): tvplotpoint; inline;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    result.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    result.y := (-l0.c - l0.a * result.x) / (l0.b);
  end else
    raise exception.create('Intersectlines routine exception');
end;

// tvplotcoder //

constructor tvplotcoder.create(inlist: tstringlist; inifile: tinifile);
begin
  finlist         := inlist;
  finifile        := inifile;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvplotcoder.destroy;
begin
  finlist    := nil;
  finifile   := nil;
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

  setlength(fcurrpath, i + 1);
  for j := 0 to i do
  begin
    fcurrpath[j].p.x := j * dx;
    fcurrpath[j].p.y := j * dy;
    fcurrpath[j].p   := translatepoint(p0, fcurrpath[j].p);
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
  setlength(fcurrpath, i + 1);
  for j := 0 to i do
  begin
    fcurrpath[j].p := rotatepoint(tmp0, (j * (sweep / i)));
    fcurrpath[j].p := translatepoint(cc, fcurrpath[j].p);
  end;
end;

procedure tvplotcoder.optimizexy(var position: tvplotposition);
var
  ang:  double;
  err:  double;
  tmp2: tvplotpoint;
  tmp3: tvplotpoint;
  tmp4: tvplotpoint;
  tmp5: tvplotpoint;
  tmp6: tvplotpoint;
begin
  ang := 0;
  repeat
    tmp2  := translatepoint(position.p, rotatepoint(fpoint2, ang));
    tmp3  := translatepoint(position.p, rotatepoint(fpoint3, ang));
    tmp4  := translatepoint(position.p, rotatepoint(fpoint4, ang));
    tmp5  := translatepoint(position.p, rotatepoint(fpoint5, ang));
    tmp6  := intersectlines(linebetween(fpoint0, tmp3),
                            linebetween(fpoint1, tmp4));

    err := abs(tmp6.y - tmp5.y);
    if  err > 0.001 then
    begin
      if tmp6.y < tmp5.y then
        ang := ang - (err / 100)
      else
      if tmp6.y > tmp5.y then
        ang := ang + (err / 100);
    end else
      break;
  until false;

  position.m0 := round(distancebetween(fpoint0, tmp3) / fratio);
  position.m1 := round(distancebetween(fpoint1, tmp4) / fratio);

  {$ifdef debug}
  writeln('--- OPTIMIZED ---');
  writeln('alpha ', radtodeg(ang):2:2);
  writeln('P2.x  ', tmp2.x:2:2);
  writeln('P2.y  ', tmp2.y:2:2);
  writeln('P3.x  ', tmp3.x:2:2);
  writeln('P3.y  ', tmp3.y:2:2);
  writeln('P4.x  ', tmp4.x:2:2);
  writeln('P4.y  ', tmp4.y:2:2);
  writeln('P5.x  ', tmp5.x:2:2);
  writeln('P5.y  ', tmp5.y:2:2);
  writeln('P6.x  ', tmp6.x:2:2);
  writeln('P6.y  ', tmp6.y:2:2);
  writeln('D03   ', distancebetween(fpoint0, tmp3):2:2);
  writeln('D14   ', distancebetween(fpoint1, tmp4):2:2);
  {$endif}
end;

procedure tvplotcoder.encode(const code: tvplotcode);
var
  i, j: longint;
  p0, p1, cc: tvplotpoint;
begin
  setlength(fcurrpath, 0);
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

  j := length(fcurrpath);
  if j > 0 then
    for i := 0 to j - 1 do
    begin
      optimizexy(fcurrpath[i]);
      dotick(fcurrpath[i], code.z);
    end;
end;

procedure tvplotcoder.execute;
var
  code: tvplotcode;
  home: tvplotposition;
  xmin, xmax: double;
  ymin, ymax: double;
  offsetx: double;
  offsety: double;
begin
  fpoint0.x := -1;  fpoint0.y := -1;
  fpoint1.x := -1;  fpoint1.y := -1;
  fpoint2.x := -1;  fpoint2.y := -1;
  fpoint3.x := -1;  fpoint3.y := -1;
  fpoint4.x := -1;  fpoint4.y := -1;
  fpoint5.x := -1;  fpoint5.y := -1;
  fpoint9.x := -1;  fpoint9.y := -1;
  fratio    := -1;

  if assigned(finifile) then
  begin
    fpoint0.x := finifile.readfloat('VPLOT v1.0', 'P0.X', -1);
    fpoint0.y := finifile.readfloat('VPLOT v1.0', 'P0.Y', -1);
    fpoint1.x := finifile.readfloat('VPLOT v1.0', 'P1.X', -1);
    fpoint1.y := finifile.readfloat('VPLOT v1.0', 'P1.Y', -1);
    fpoint2.x := finifile.readfloat('VPLOT v1.0', 'P2.X', -1);
    fpoint2.y := finifile.readfloat('VPLOT v1.0', 'P2.Y', -1);
    fpoint3.x := finifile.readfloat('VPLOT v1.0', 'P3.X', -1);
    fpoint3.y := finifile.readfloat('VPLOT v1.0', 'P3.Y', -1);
    fpoint4.x := finifile.readfloat('VPLOT v1.0', 'P4.X', -1);
    fpoint4.y := finifile.readfloat('VPLOT v1.0', 'P4.Y', -1);
    fpoint5.x := finifile.readfloat('VPLOT v1.0', 'P5.X', -1);
    fpoint5.y := finifile.readfloat('VPLOT v1.0', 'P5.Y', -1);
    fpoint9.x := finifile.readfloat('VPLOT v1.0', 'P9.X', -1);
    fpoint9.y := finifile.readfloat('VPLOT v1.0', 'P9.Y', -1);
    fratio    := finifile.readfloat('VPLOT v1.0', 'R'   , -1);
  end;

  writeln('VPLOT v1.0');
  writeln(format(' P0.X = %-5.3f  P1.X = %-5.3f', [fpoint0.x, fpoint1.x]));
  writeln(format(' P0.Y = %-5.3f  P1.Y = %-5.3f', [fpoint0.y, fpoint1.y]));
  writeln(format(' P2.X = %-5.3f  P3.X = %-5.3f  P4.X = %-5.3f  P5.X = %-5.3f', [fpoint2.x, fpoint3.x, fpoint4.x, fpoint5.x]));
  writeln(format(' P2.Y = %-5.3f  P3.Y = %-5.3f  P4.Y = %-5.3f  P5.Y = %-5.3f', [fpoint2.y, fpoint3.y, fpoint4.y, fpoint5.y]));
  writeln(format(' P9.X = %-5.3f', [fpoint9.x]));
  writeln(format(' P9.Y = %-5.3f', [fpoint9.y]));
  writeln(format('RATIO = %-5.3f', [fratio]));
  readln;

  if fpoint0.x = -1 then terminate;  if fpoint0.y = -1 then terminate;
  if fpoint1.x = -1 then terminate;  if fpoint1.y = -1 then terminate;
  if fpoint2.x = -1 then terminate;  if fpoint2.y = -1 then terminate;
  if fpoint3.x = -1 then terminate;  if fpoint3.y = -1 then terminate;
  if fpoint4.x = -1 then terminate;  if fpoint4.y = -1 then terminate;
  if fpoint5.x = -1 then terminate;  if fpoint5.y = -1 then terminate;
  if fpoint9.x = -1 then terminate;  if fpoint9.y = -1 then terminate;
  if fratio    = -1 then terminate;

  home.p.x := fpoint9.x;
  home.p.y := fpoint9.y;
  optimizexy(home);

  if not terminated then
  begin
    dotick(home, 1);

    findex := 0;
    if finlist.count > 0 then
    begin
      parse_line(finlist[findex], code);
      if (code.c ='G00 ') or (code.c ='G01 ') or
         (code.c ='G02 ') or (code.c ='G03 ') then
      begin
        xmin := code.x;
        xmax := code.x;
        ymin := code.y;
        ymax := code.y;
      end;
      inc(findex);
    end;

    while (findex < finlist.count) and (not terminated) do
    begin
      parse_line(finlist[findex], code);
      if (code.c ='G00 ') or (code.c ='G01 ') or
         (code.c ='G02 ') or (code.c ='G03 ') then
      begin
        xmin := min(xmin, code.x);
        xmax := max(xmax, code.x);
        ymin := min(ymin, code.y);
        ymax := max(ymax, code.y);
      end;
      inc(findex);
    end;
    offsetx := (fpoint1.x / 2) - ((xmax + xmin) / 2);
    offsety := (fpoint1.y / 2) - ((ymax + ymin) / 2);

    findex := 0;
    while (finlist.count > 0) and (not terminated) do
    begin
      parse_line(finlist[0], code);
      if (code.c ='G00 ') or (code.c ='G01 ') or
         (code.c ='G02 ') or (code.c ='G03 ') then
      begin
        code.x := code.x + offsetx;
        code.y := code.y + offsetx;
        encode(code);
      end;
      inc(findex);
    end;
    dotick(home, 1);
  end;
end;


// tvplotdriver //

constructor tvplotdriver.create;
begin
  inherited create;
  {$ifdef raspberrypi}
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
  fdelayms := 10;
  fenabled := false;
end;

destructor tvplotdriver.destroy;
begin
  vplotdriver := nil;
  inherited destroy;
end;

procedure  tvplotdriver.init(cnt0, cnt1, cntz: longint);
begin
  {$ifdef raspberrypi}
  fcount0  := cnt0;
  fcount1  := cnt1;
  fcountz  := cntz;
  {$endif}
end;

procedure tvplotdriver.move2(cnt0, cnt1, cntz: longint);
begin
  {$ifdef raspberrypi}
  move4(cnt0 - fcount0, cnt1 - fcount1, cntz - fcountz);
  {$endif}
end;

procedure tvplotdriver.largedisplacements(cnt0, cnt1: longint);
begin
  {$ifdef raspberrypi}
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
    digitalwrite(mot0_step, HIGH);  delay(fdelayms);
    digitalwrite(mot0_step,  LOW);  delay(fdelayms);
    dec(cnt0);
  end;
  // move step motor1
  cnt1 := abs(cnt1);
  while cnt1 > 0 do
  begin
    digitalwrite(mot1_step, HIGH);  delay(fdelayms);
    digitalwrite(mot1_step,  LOW);  delay(fdelayms);
    dec(cnt1);
  end;
  {$endif}
end;

procedure tvplotdriver.smalldisplacements(cnt0, cnt1: longint);
{$ifdef raspberrypi}
var
  i: longint;
{$endif}
begin
  {$ifdef raspberrypi}
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
      digitalwrite(mot0_step, HIGH);  delay(fdelayms);
      digitalwrite(mot0_step,  LOW);  delay(fdelayms);
    end;
    // move step motor1
    if vplotmatrix[cnt1, i] = 1 then
    begin
      digitalwrite(mot1_step, HIGH);  delay(fdelayms);
      digitalwrite(mot1_step,  LOW);  delay(fdelayms);
    end;
  end;
  {$endif}
end;

procedure tvplotdriver.move4(cnt0, cnt1, cntz: longint);
begin
  {$ifdef raspberrypi}
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

