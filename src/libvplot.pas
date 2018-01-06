{ Description: vPlot interface.

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
//{$define debug}

interface

uses
  classes, graphics, sysutils;

type
  tvplotpoint = record
    x: double;
    y: double;
  end;

  tvplotline = packed record
    a: double;
    b: double;
    c: double;
  end;

  tvplotposition = record
    m0: longint;
    m1: longint;
    mz: longint;
    p:  tvplotpoint;
  end;

  tvplotcode = record
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
    property delayms: longword read fdelayms write fdelayms;
    property fault:   longint  read ffault;
    property enabled: boolean  read fenabled write fenabled;
  end;

type
  tvplotcoder = class (tthread)
  private
    ffilename:   rawbytestring;
    fenabled:    boolean;
    foffsetx:    longint;
    foffsety:    longint;
    fmode:       longint;
    fondraw:     tthreadmethod;
    fondrawn:    tthreadmethod;
    fpath:       array of tvplotposition;
    fpoints:     array[0..6] of tvplotpoint;
    fpoint0:     tvplotpoint;
    fpoint1:     tvplotpoint;
    fposition:   tvplotposition;
    fgearratio:  double;
    procedure interpolate_line(const p0, p1: tvplotpoint);
    procedure interpolate_arc (const p0, p1, cc: tvplotpoint; clockwise: boolean);
    procedure optimizeposition(var position: tvplotposition);
    procedure draw(const code: tvplotcode);
  protected
    procedure execute; override;
  public
    constructor create;
    destructor destroy; override;
    procedure  drawn(var p0, p1: tvplotpoint);
  published
    property filename:   rawbytestring read ffilename   write ffilename;
    property ondraw:     tthreadmethod read fondraw     write fondraw;
    property ondrawn:    tthreadmethod read fondrawn    write fondrawn;
    property offsetx:    longint       read foffsetx    write foffsetx;
    property offsety:    longint       read foffsety    write foffsety;
    property enabled:    boolean       read fenabled    write fenabled;
    property mode:       longint       read fmode       write fmode;
  end;

var
  vplotcoder:  tvplotcoder  = nil;
  vplotdriver: tvplotdriver = nil;

implementation

uses
  inifiles, math, pca9685, wiringpi;

const
  mot0_step      = P38;
  mot0_dir       = P40;
  mot1_step      = P16;
  mot1_dir       = P18;

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

procedure intersectlines(const l0, l1: tvplotline; var p: tvplotpoint); inline;
begin
  if (l0.a * l1.b) <> (l0.b * l1.a) then
  begin
    p.x := (-l0.c * l1.b + l0.b * l1.c) / (l0.a * l1.b - l0.b * l1.a);
    p.y := (-l0.c - l0.a * p.x) / (l0.b);
  end else
    raise exception.create('Intersectlines routine exception');
end;

// tvplotcoder //

constructor tvplotcoder.create;
begin
  fmode    := 4;
  fenabled := false;
  foffsetx := 0;
  foffsety := 0;
  freeonterminate := true;
  inherited create(true);
end;

destructor tvplotcoder.destroy;
begin
  vplotcoder := nil;
  inherited destroy;
end;

procedure tvplotcoder.interpolate_line(const p0, p1: tvplotpoint);
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

procedure tvplotcoder.interpolate_arc(const p0, p1, cc: tvplotpoint; clockwise: boolean);
var
  line0: tvplotline;
  line1: tvplotline;
  angle0: double;
  angle1: double;
  sweep:  double;
  i, j: longint;
  tmp: array[0..2] of tvplotpoint;
begin
  tmp[0].x := p0.x - cc.x;
  tmp[0].y := p0.y - cc.y;
  tmp[1].x := p1.x - cc.x;
  tmp[1].y := p1.y - cc.y;
  tmp[2].x := 0;
  tmp[2].y := 0;

  line0  := linebetween(tmp[2], tmp[0]);
  line1  := linebetween(tmp[2], tmp[1]);
  angle0 := lineangle(line0);
  angle1 := lineangle(line1);
  sweep  := angle1 - angle0;

  if (clockwise) and (sweep >= 0) then
    sweep := sweep - (2 * pi)
  else
    if (not clockwise) and (sweep <= 0) then
      sweep := sweep + (2 * pi);

  i := max(1, round(abs(sweep) * distancebetween(tmp[2], tmp[0]) / 0.15));
  setlength(fpath, i + 1);
  for j := 0 to i do
  begin
    fpath[j].p := rotatepoint(tmp[0], (j * (sweep / i)));
    fpath[j].p := translatepoint(cc, fpath[j].p);
  end;
end;

procedure tvplotcoder.optimizeposition(var position: tvplotposition);
var
  alpha: double;
  error: double;
    tmp: array[0..8] of tvplotpoint;
  line0: tvplotline;
  line1: tvplotline;
  line2: tvplotline;
  F0, F1, L0,L1: double;
begin
  alpha := 0;
  repeat
    // absolute coordinates
    tmp[0] := fpoints[0];
    tmp[1] := fpoints[1];
    tmp[2] := translatepoint(position.p, rotatepoint(fpoints[2], alpha));
    tmp[3] := translatepoint(position.p, rotatepoint(fpoints[3], alpha));
    tmp[4] := translatepoint(position.p, rotatepoint(fpoints[4], alpha));
    tmp[5] := translatepoint(position.p, rotatepoint(fpoints[5], alpha));
    line0  := linebetween(tmp[0], tmp[3]);
    line1  := linebetween(tmp[1], tmp[4]);

    intersectlines(line0, line1, tmp[6]);

    error := abs(tmp[6].y - tmp[5].y);
    if  error > 0.001 then
    begin
      if tmp[6].y < tmp[5].y then
        alpha := alpha - (error / 100)
      else
      if tmp[6].y > tmp[5].y then
        alpha := alpha + (error / 100);
    end else
      break;

  until false;

writeln('OPTIMIZED');
writeln('alpha ', radtodeg(alpha):2:2);
writeln('2.x ', tmp[2].x:2:2);
writeln('2.y ', tmp[2].y:2:2);
writeln('3.x ', tmp[3].x:2:2);
writeln('3.y ', tmp[3].y:2:2);
writeln('4.x ', tmp[4].x:2:2);
writeln('4.y ', tmp[4].y:2:2);
writeln('5.x ', tmp[5].x:2:2);
writeln('5.y ', tmp[5].y:2:2);
writeln('6.x ', tmp[6].x:2:2);
writeln('6.y ', tmp[6].y:2:2);

L0 := distancebetween(tmp[0], tmp[3]);
L1 := distancebetween(tmp[1], tmp[4]);

writeln('03  ', L0:2:2);
writeln('14  ', L1:2:2);

//tmp[7].x := 0;
//tmp[7].y := tmp[3].y;
//line2 := linebetween(tmp[7], tmp[3]);
//intersectlines(line2, line1, tmp[8]);
//F0 := (1.030 * 9.81) * distancebetween(tmp[3], tmp[6]) / distancebetween(tmp[3], tmp[8]);
//F1 := (1.030 * 9.81) * distancebetween(tmp[6], tmp[8]) / distancebetween(tmp[3], tmp[8]);
//writeln('F0 ', F0:2:2);
//writeln('F1 ', F1:2:2);
//L0 := L0 / (F0 / (0.19635 * 2500) + 1);
//L1 := L1 / (F1 / (0.19635 * 2500) + 1);
//writeln('03**  ', L0:2:2);
//writeln('14**  ', L1:2:2);
//position.m0 := round(L0 / fratio);
//position.m1 := round(L1 / fratio);

  position.m0 := round(distancebetween(tmp[0], tmp[3]) / fgearratio);
  position.m1 := round(distancebetween(tmp[1], tmp[4]) / fgearratio);
end;

procedure tvplotcoder.draw(const code: tvplotcode);
var
  i, j: longint;
  p0, p1, cc: tvplotpoint;
begin
  if code.x < ((1 * fpoints[1].y) / 6) then exit;
  if code.x > ((5 * fpoints[1].y) / 6) then exit;
  if code.y < ((1 * fpoints[1].y) / 6) then exit;
  if code.y > ((5 * fpoints[1].y) / 6) then exit;

  setlength(fpath, 0);
  if (code.c ='G00 ') or (code.c = 'G01 ') then
  begin
    p0.x := fposition.p.x;
    p0.y := fposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    interpolate_line(p0, p1);
  end else
  if (code.c ='G02 ') or (code.c = 'G03 ') then
  begin
    p0.x := fposition.p.x;
    p0.y := fposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    cc.x := p0.x + code.i;
    cc.y := p0.y + code.j;
    interpolate_arc(p0, p1, cc, code.c = 'G02 ');
  end;

  j := length(fpath);
  if j > 0 then
  begin
    // move vplot
    for i := 0 to j - 1 do
      fpath[i].mz := round(code.z);

    for i := 0 to j - 1 do
    begin
      fpoint0 := fposition.p;
      fpoint1 := fpath[i].p;
      if assigned(fondraw) then
        synchronize(fondraw);

      optimizeposition(fpath[i]);
      with fpath[i] do
        vplotdriver. move2(m0, m1, mz);
      fposition := fpath[i];

      if (code.c <> 'G00 ') and (code.z < 0) then
        if assigned(fondrawn) then
          synchronize(fondrawn);
    end;
  end;
end;

procedure tvplotcoder.drawn(var p0, p1: tvplotpoint);
begin
  p0 := fpoint0;
  p1 := fpoint1;
end;

procedure tvplotcoder.execute;
const
  section = 'VPLOT layout';
var
  ini:  tinifile;
  code: tvplotcode;
  list: tstringlist;
begin
  // init coder
  ini := tinifile.create(changefileext(paramstr(0), '.ini'));
  fpoints[0].x := ini.readfloat(section, 'P0.X', -1);
  fpoints[0].y := ini.readfloat(section, 'P0.Y', -1);
  fpoints[1].x := ini.readfloat(section, 'P1.X', -1);
  fpoints[1].y := ini.readfloat(section, 'P1.Y', -1);
  fpoints[2].x := ini.readfloat(section, 'P2.X', -1);
  fpoints[2].y := ini.readfloat(section, 'P2.Y', -1);
  fpoints[3].x := ini.readfloat(section, 'P3.X', -1);
  fpoints[3].y := ini.readfloat(section, 'P3.Y', -1);
  fpoints[4].x := ini.readfloat(section, 'P4.X', -1);
  fpoints[4].y := ini.readfloat(section, 'P4.Y', -1);
  fpoints[5].x := ini.readfloat(section, 'P5.X', -1);
  fpoints[5].y := ini.readfloat(section, 'P5.Y', -1);
  fpoints[6].x := ini.readfloat(section, 'P6.X', -1);
  fpoints[6].y := ini.readfloat(section, 'P6.Y', -1);
  fgearratio   := ini.readfloat(section, 'R'   , -1);

  fgearratio := fgearratio / fmode;


  freeandnil(ini);
  {$ifdef debug}
  writeln('vPlot debug: load layout routine');
  writeln(format('PO.x = %-5.3f P1.x = %-5.3f', [fpoints[0].x, fpoints[1].x]));
  writeln(format('PO.y = %-5.3f P1.y = %-5.3f', [fpoints[0].y, fpoints[1].y]));
  writeln(format('P2.x = %-5.3f P3.x = %-5.3f P4.x = %-5.3f P5.x = %-5.3f', [fpoints[2].x, fpoints[3].x, fpoints[4].x, fpoints[5].x]));
  writeln(format('P2.y = %-5.3f P3.y = %-5.3f P4.y = %-5.3f P5.y = %-5.3f', [fpoints[2].y, fpoints[3].y, fpoints[4].y, fpoints[5].y]));
  writeln(format('P6.x = %-5.3f', [fpoints[6].x]));
  writeln(format('P6.y = %-5.3f', [fpoints[6].y]));
  writeln(format('R    = %-5.3f', [fgearratio]));

  {$endif}
  // ---
  fposition.p := fpoints[6];
  optimizeposition(fposition);
  with fposition do
    vplotdriver.init(m0, m1, 0);
  // load gcode
  list := tstringlist.create;
  list.loadfromfile(filename);
  // draw gcode
  while list.count > 0 do
  begin
    if fenabled then
    begin
      parse_line(list[0], code);
      if code.c <> '' then
      begin
        code.x := code.x + foffsetx;
        code.y := code.y + foffsetx;
        if (code.c ='G00 ') then draw(code) else
        if (code.c ='G01 ') then draw(code) else
        if (code.c ='G02 ') then draw(code) else
        if (code.c ='G03 ') then draw(code);
      end;
      list.delete(0);
      sleep(5);
    end else
    begin
      with fposition do
        vplotdriver.move2(m0, m1, 0);
      sleep(250);
    end;

    if terminated then break;
  end;
  // move to home
  fposition.p := fpoints[6];
  optimizeposition(fposition);
  with fposition do
    vplotdriver.move2(m0, m1, 0);
  freeandnil(list);
end;

// tvplotdriver //

constructor tvplotdriver.create;
begin
  inherited create;
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
  fcount0  := cnt0;
  fcount1  := cnt1;
  fcountz  := cntz;
end;

procedure tvplotdriver.move2(cnt0, cnt1, cntz: longint);
begin
  move4(cnt0 - fcount0, cnt1 - fcount1, cntz - fcountz);
end;

procedure tvplotdriver.largedisplacements(cnt0, cnt1: longint);
begin
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
end;

procedure tvplotdriver.smalldisplacements(cnt0, cnt1: longint);
var
  i: longint;
begin
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
end;

procedure tvplotdriver.move4(cnt0, cnt1, cntz: longint);
begin
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
end;

end.

