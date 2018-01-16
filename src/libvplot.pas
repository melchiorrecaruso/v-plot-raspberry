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

type
  tvplotcoder = class(tthread)
  private
    fpx:      double;
    fpy:      double;
    fpz:      double;
    foffsetx: double;
    foffsety: double;
    fmot0:    longint;
    fmot1:    longint;
    fmotz:    longint;
    ffault:   longint;

    finlist:  tstringlist;
    finifile: tinifile;
    findex:   longint;
    fenabled: boolean;

    fontick: tthreadmethod;

    fratio:     double;
    fpoint0:    tvplotpoint;
    fpoint1:    tvplotpoint;
    fpoint2:    tvplotpoint;
    fpoint3:    tvplotpoint;
    fpoint4:    tvplotpoint;
    fpoint5:    tvplotpoint;
    fpoint9:    tvplotposition;


    fcurrpath:  array of tvplotposition;
    function  getcount: longint;
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
    property enabled: boolean       read fenabled write fenabled;
    property index:   longint       read findex;
    property count:   longint       read getcount;

    property px:      double        read fpx;
    property py:      double        read fpy;
    property pz:      double        read fpz;
    property mot0:    longint       read fmot0;
    property mot1:    longint       read fmot1;
    property motz:    longint       read fmotz;
    property offsetx: double        read foffsetx;
    property offsety: double        read foffsety;
    property height:  double        read fpoint1.y;
    property width:   double        read fpoint1.x;
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

function translatepoint(const cc, p: tvplotpoint): tvplotpoint; inline;
function rotatepoint(const p: tvplotpoint; const alpha: double): tvplotpoint; inline;
function distancebetween(const p0, p1: tvplotpoint): double; inline;
function linebetween(const p0, p1: tvplotpoint): tvplotline; inline;
function lineangle(var line: tvplotline): double; inline;
function intersectlines(const l0, l1: tvplotline): tvplotpoint; inline;

var
  vplotcoder:  tvplotcoder  = nil;
  vplotdriver: tvplotdriver = nil;

implementation

uses
  math, {$ifdef cpuarm} pca9685, wiringpi, {$endif} sysutils;

{$ifdef cpuarm}
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

// tvplotcoder //

constructor tvplotcoder.create(inlist: tstringlist; inifile: tinifile);
begin
  fenabled    := false;
  ffault      := 0;
  finlist     := inlist;
  finifile    := inifile;
  finifile.formatsettings.dateseparator := '.';
  // ---
  fratio      := finifile.readfloat('VPLOT v1.0', 'R'   ,  -1); if fratio    = -1 then ffault := -1;
  fpoint0.x   := finifile.readfloat('VPLOT v1.0', 'P00.X', -1); if fpoint0.x = -1 then ffault := -1;
  fpoint0.y   := finifile.readfloat('VPLOT v1.0', 'P00.Y', -1); if fpoint0.y = -1 then ffault := -1;
  fpoint1.x   := finifile.readfloat('VPLOT v1.0', 'P01.X', -1); if fpoint1.x = -1 then ffault := -1;
  fpoint1.y   := finifile.readfloat('VPLOT v1.0', 'P01.Y', -1); if fpoint1.y = -1 then ffault := -1;
  fpoint2.x   := finifile.readfloat('VPLOT v1.0', 'P02.X', -1); if fpoint2.x = -1 then ffault := -1;
  fpoint2.y   := finifile.readfloat('VPLOT v1.0', 'P02.Y', -1); if fpoint2.y = -1 then ffault := -1;
  foffsetx    := finifile.readfloat('VPLOT v1.0', 'D23.X', -1); if foffsetx  = -1 then ffault := -1;
  foffsety    := finifile.readfloat('VPLOT v1.0', 'D23.Y', -1); if foffsety  = -1 then ffault := -1;
  fpoint3.x   := fpoint2.x + foffsetx;
  fpoint3.y   := fpoint2.y + foffsety;
  foffsetx    := finifile.readfloat('VPLOT v1.0', 'D24.X', -1); if foffsetx  = -1 then ffault := -1;
  foffsety    := finifile.readfloat('VPLOT v1.0', 'D24.Y', -1); if foffsety  = -1 then ffault := -1;
  fpoint4.x   := fpoint2.x + foffsetx;
  fpoint4.y   := fpoint2.y + foffsety;
  foffsetx    := finifile.readfloat('VPLOT v1.0', 'D25.X', -1); if foffsetx  = -1 then ffault := -1;
  foffsety    := finifile.readfloat('VPLOT v1.0', 'D25.Y', -1); if foffsety  = -1 then ffault := -1;
  fpoint5.x   := fpoint2.x + foffsetx;
  fpoint5.y   := fpoint2.y + foffsety;
  fpoint9.p.x := -fpoint2.x;
  fpoint9.p.y := -fpoint2.y;
  fpoint2     := translatepoint(fpoint9.p, fpoint2);
  fpoint3     := translatepoint(fpoint9.p, fpoint3);
  fpoint4     := translatepoint(fpoint9.p, fpoint4);
  fpoint5     := translatepoint(fpoint9.p, fpoint5);
  fpoint9.p.x := -fpoint9.p.x;
  fpoint9.p.y := -fpoint9.p.y;
  {$ifdef debug}
  writeln('--- VPLOT v1.0 ---');
  writeln(format('P00.X = %-5.3f  P00.Y = %-5.3f', [fpoint0.x,   fpoint0.y]));
  writeln(format('P01.X = %-5.3f  P01.Y = %-5.3f', [fpoint1.x,   fpoint1.y]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [fpoint2.x,   fpoint2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [fpoint3.x,   fpoint3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [fpoint4.x,   fpoint4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [fpoint5.x,   fpoint5.y]));
  writeln(format('P09.X = %-5.3f  P09.Y = %-5.3f', [fpoint9.p.x, fpoint9.p.y]));
  writeln(format('R     = %-5.3f', [fratio]));
  {$endif}
  optimizexy  (fpoint9);
  // ---
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

    err := abs(tmp6.x - tmp5.x);
    if  err > 0.001 then
    begin
      if tmp6.x < tmp5.x then
        ang := ang - (err / 100)
      else
      if tmp6.x > tmp5.x then
        ang := ang + (err / 100);
    end else
      break;
  until false;

  position.m0 := round(distancebetween(fpoint0, tmp3) / fratio);
  position.m1 := round(distancebetween(fpoint1, tmp4) / fratio);
  {$ifdef debug}
  writeln('--- OPTIMIZE ---');
  writeln(format('alpha = %-5.3f', [radtodeg(ang)]));
  writeln(format('P02.X = %-5.3f  P02.Y = %-5.3f', [tmp2.x, tmp2.y]));
  writeln(format('P03.X = %-5.3f  P03.Y = %-5.3f', [tmp3.x, tmp3.y]));
  writeln(format('P04.X = %-5.3f  P04.Y = %-5.3f', [tmp4.x, tmp4.y]));
  writeln(format('P05.X = %-5.3f  P05.Y = %-5.3f', [tmp5.x, tmp5.y]));
  writeln(format('P06.X = %-5.3f  P06.Y = %-5.3f', [tmp6.x, tmp6.y]));
  writeln(format('D03   = %-5.3f', [distancebetween(fpoint0, tmp3)]));
  writeln(format('D14   = %-5.3f', [distancebetween(fpoint1, tmp4)]));
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
  if (ffault <> -1) then
  begin
    xmin   := fpoint1.x;
    xmax   := 0;
    ymin   := fpoint1.y;
    ymax   := 0;
    // ---
    findex := 0;
    while (findex < finlist.count) do
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
    foffsetx := (fpoint1.x / 2) - ((xmax + xmin) / 2);
    foffsety := (fpoint1.y / 2) - ((ymax + ymin) / 2);
    {$ifdef debug}
    writeln('--- CENTRE DRAWING ---');
    writeln('offset x = ', foffsetx:2:2);
    writeln('offset y = ', foffsety:2:2);
    {$endif}
    // ---
    findex := 0;
    dotick(fpoint9, 1);
    while (findex < finlist.count) and (not terminated) do
    begin
      if enabled then
      begin
        parse_line(finlist[findex], code);
        if (code.c ='G00 ') or (code.c ='G01 ') or
           (code.c ='G02 ') or (code.c ='G03 ') then encode(code);

        inc(findex);
      end else
        sleep(250);
    end;
    dotick(fpoint9, 1);
  end;
end;

function tvplotcoder.getcount: longint;
begin
  result := finlist.count;
end;

// tvplotdriver //

constructor tvplotdriver.create;
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
  sleep(10);

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

