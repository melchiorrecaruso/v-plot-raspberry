{ Description: vPlot driver.

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

{$mode objfpc}{$h+}

interface

uses
  classes, graphics, sysutils;

type
  tvplotpoint = record
    x: double;
    y: double;
  end;

  tvplotline = record
    a: double;
    b: double;
    c: double;
  end;

  tvplotposition = record
    m0: longint;
    m1: longint;
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

type
  tvplotinterface = class
  private
    fgcode:     rawbytestring;
    fdelay:     longword;
    fpoint0:    tvplotpoint;
    fpoint1:    tvplotpoint;
    fsync1:     tthreadmethod;
    fsync2:     tthreadmethod;
    fsync3:     tthreadmethod;
    fsync4:     tthreadmethod;
    fpreview:   boolean;
    fsuspended: boolean;
  public
    property gcode:     rawbytestring read fgcode     write fgcode;
    property delay:     longword      read fdelay     write fdelay;
    property point0:    tvplotpoint   read fpoint0    write fpoint0;
    property point1:    tvplotpoint   read fpoint1    write fpoint1;
    property sync1:     tthreadmethod read fsync1     write fsync1;
    property sync2:     tthreadmethod read fsync2     write fsync2;
    property sync3:     tthreadmethod read fsync3     write fsync3;
    property sync4:     tthreadmethod read fsync4     write fsync4;
    property preview:   boolean       read fpreview   write fpreview;
    property suspended: boolean       read fsuspended write fsuspended;
  end;

type
  tvplotdriver = class (tthread)
  private
    fvplot:     array[0..6] of tvplotpoint;
    fvplotpath: array of tvplotposition;
    fvplotcode: tvplotcode;
    fvplotinterface: tvplotinterface;
    fcurrposition: tvplotposition;
    fvplotratio: double;

    procedure interpolate_line(const p0, p1: tvplotpoint);
    procedure interpolate_arc (const p0, p1, cc: tvplotpoint; clockwise: boolean);

    procedure optimize(var position: tvplotposition);
    procedure moveto(var nextposition: tvplotposition);
    procedure draw(const code: tvplotcode);
  protected
    procedure execute; override;
  public
    constructor create(vplotinterface: tvplotinterface);
    destructor destroy; override;
    procedure initialize;
    procedure move(motor, count: longint; cw: boolean);
  end;


var
  vplotinterface: tvplotinterface = nil;
  vplotdriver:    tvplotdriver    = nil;
  vplotinit:      boolean;
  vplotservo:     double;

implementation

uses
  inifiles, math, pca9685, wiringpi;

const
  vplotservo_maxvalue = 2.50;
  vplotservo_minvalue = 0.50;
  vplotservo_rstvalue = 1.50;
  vplotservo_incvalue = 0.10;
  vplotservo_freq     = 50;

  vplotmot0_step      = P38;
  vplotmot0_dir       = P40;
  vplotmot1_step      = P16;
  vplotmot1_dir       = P18;

  vplotmatrix : array [0..5, 0..8] of longint =
    ((0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 1, 0, 0, 0, 0),
     (0, 0, 1, 0, 0, 0, 1, 0, 0),
     (1, 0, 0, 0 ,1, 0, 0, 0, 1),
     (0, 1, 0, 1, 0, 1, 0, 1, 0),
     (1, 0, 1, 0, 1, 0, 1, 0, 1));

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

// vplotdriver //

constructor tvplotdriver.create(vplotinterface: tvplotinterface);
begin
  // initializa wiringPI library
  vplotinit := wiringPISetup <> -1;
  if vplotinit then
  begin
    pinMode(P11, OUTPUT);
    digitalwrite(P11, LOW);
    // inizialize pca9685 library
    vplotinit := pca9685Setup(PCA9685_PIN_BASE, PCA9685_ADDRESS, vplotservo_freq) <> -1;
    if vplotinit then
    begin
      pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_minvalue, vplotservo_freq));
      delay(2000);
      pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_maxvalue, vplotservo_freq));
      delay(2000);
      pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_rstvalue, vplotservo_freq));
      delay(2000);
      vplotservo := 0;
    end;
    digitalwrite(P11, HIGH);
    // initialize step motor1
    pinMode(vplotmot0_dir, OUTPUT);
    pinMode(vplotmot0_step, OUTPUT);
    digitalwrite(vplotmot0_dir, LOW);
    digitalwrite(vplotmot0_step, LOW);
    // initializa step motor2
    pinMode(vplotmot1_dir, OUTPUT);
    pinMode(vplotmot1_step, OUTPUT);
    digitalwrite(vplotmot1_dir, LOW);
    digitalwrite(vplotmot1_step, LOW);
  end;
  fvplotinterface := vplotinterface;
  freeonterminate := true;
  inherited create(false);
end;

destructor tvplotdriver.destroy;
begin
  fvplotinterface := nil;
  inherited destroy;
end;

procedure tvplotdriver.initialize;
const
  section1 = 'Anchors';
  section2 = 'Vplot';
  section3 = 'Gear ratio';
  section4 = 'Home';
         r = (pi * 25) / 800;
var
  ini: tinifile;
begin
  if not fileexists(changefileext(paramstr(0),'.ini')) then
  begin
    ini := tinifile.create(changefileext(paramstr(0),'.ini'));
    // absolute coordinates
    ini.writefloat(section1, '0.X',    0);
    ini.writefloat(section1, '0.Y',    0);
    ini.writefloat(section1, '1.X',    0);
    ini.writefloat(section1, '1.Y', 1500);
    ini.writefloat(section4, '6.X',  250);
    ini.writefloat(section4, '6.Y',  750);
    // relative coordinates
    ini.writefloat(section2, '2.X',    0);
    ini.writefloat(section2, '2.Y',    0);
    ini.writefloat(section2, '3.X',   30);
    ini.writefloat(section2, '3.Y',  -35);
    ini.writefloat(section2, '4.X',   30);
    ini.writefloat(section2, '4.Y',   35);
    ini.writefloat(section2, '5.X',   90);
    ini.writefloat(section2, '5.Y',    0);
    // ---
    ini.writefloat  (section3, 'R',    r);
    // ---
    freeandnil(ini);
  end;

  ini := tinifile.create(changefileext(paramstr(0),'.ini'));
  // absolute coordinates
  fvplot[0].x := ini.readfloat(section1, '0.X', -1);
  fvplot[0].y := ini.readfloat(section1, '0.Y', -1);
  fvplot[1].x := ini.readfloat(section1, '1.X', -1);
  fvplot[1].y := ini.readfloat(section1, '1.Y', -1);
  fvplot[6].x := ini.readfloat(section4, '6.X', -1);
  fvplot[6].y := ini.readfloat(section4, '6.Y', -1);
  // relative coordinates
  fvplot[2].x := ini.readfloat(section2, '2.X', -1);
  fvplot[2].y := ini.readfloat(section2, '2.Y', -1);
  fvplot[3].x := ini.readfloat(section2, '3.X', -1);
  fvplot[3].y := ini.readfloat(section2, '3.Y', -1);
  fvplot[4].x := ini.readfloat(section2, '4.X', -1);
  fvplot[4].y := ini.readfloat(section2, '4.Y', -1);
  fvplot[5].x := ini.readfloat(section2, '5.X', -1);
  fvplot[5].y := ini.readfloat(section2, '5.Y', -1);
  // ---
  fvplotratio := ini.readfloat(section3,   'R', -1);
  // ---
  freeandnil(ini);
  // ---
  fvplotcode.c := '';
  fvplotcode.x  := 0;
  fvplotcode.y  := 0;
  fvplotcode.z  := 0;
  fvplotcode.f  := 0;
  fvplotcode.e  := 0;
  fvplotcode.i  := 0;
  fvplotcode.j  := 0;
  fvplotcode.k  := 0;
  fvplotcode.r  := 0;
  // ---
  fcurrposition.p := fvplot[6];
  optimize(fcurrposition);

  fvplotinterface.fdelay := 50;
end;

procedure tvplotdriver.interpolate_line(const p0, p1: tvplotpoint);
var
  dx, dy: double;
  i, j: longint;
begin
  i := max(1, round(distancebetween(p0, p1) / 0.25));

  dx := (p1.x - p0.x) / i;
  dy := (p1.y - p0.y) / i;

  setlength(fvplotpath, i + 1);
  for j := 0 to i do
  begin
    fvplotpath[j].p.x := j * dx;
    fvplotpath[j].p.y := j * dy;
    fvplotpath[j].p   := translatepoint(p0, fvplotpath[j].p);
  end;
end;

procedure tvplotdriver.interpolate_arc(const p0, p1, cc: tvplotpoint; clockwise: boolean);
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

  i := max(1, round(abs(sweep) * distancebetween(tmp[2], tmp[0]) / 0.25));
  setlength(fvplotpath, i + 1);
  for j := 0 to i do
  begin
    fvplotpath[j].p := rotatepoint(tmp[0], (j * (sweep / i)));
    fvplotpath[j].p := translatepoint(cc, fvplotpath[j].p);
  end;
end;

procedure tvplotdriver.optimize(var position: tvplotposition);
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
    tmp[0] := fvplot[0];
    tmp[1] := fvplot[1];
    tmp[2] := translatepoint(position.p, rotatepoint(fvplot[2], alpha));
    tmp[3] := translatepoint(position.p, rotatepoint(fvplot[3], alpha));
    tmp[4] := translatepoint(position.p, rotatepoint(fvplot[4], alpha));
    tmp[5] := translatepoint(position.p, rotatepoint(fvplot[5], alpha));
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
//position.m0 := round(L0 / fvplotratio);
//position.m1 := round(L1 / fvplotratio);

position.m0 := round(distancebetween(tmp[0], tmp[3]) / fvplotratio);
position.m1 := round(distancebetween(tmp[1], tmp[4]) / fvplotratio);

writeln('pos.count0 ', position.m0);
writeln('pos.count1 ', position.m1);
end;

procedure tvplotdriver.moveto(var nextposition: tvplotposition);
var
       i: longint;
    inc0: longint;
    inc1: longint;
  count0: longint;
  count1: longint;
begin
  optimize(nextposition);

  count0 := nextposition.m0 - fcurrposition.m0;
  if count0 > 0 then
  begin
    digitalwrite(vplotmot0_dir, HIGH);
    inc0 := +1;
  end else
  begin
    digitalwrite(vplotmot0_dir, LOW);
    inc0 := -1;
  end;
  count0 := abs(count0);

  count1 := nextposition.m1 - fcurrposition.m1;
  if count1 > 0 then
  begin
    digitalwrite(vplotmot1_dir, LOW);
    inc1 := +1;
  end else
  begin
    digitalwrite(vplotmot1_dir, HIGH);
    inc1 := -1;
  end;
  count1 := abs(count1);

  assert(max(count0, count1) >= 0, 'min count error');
  assert(max(count0, count1) <= 5, 'max count error');

  if (count0 <> 0) or (count1 <> 0) then
  begin


    // move stepper
    for i := 0 to 8 do
    begin
      if vplotmatrix[count0, i] = 1 then
      begin
        digitalwrite(vplotmot0_step, HIGH);
        delay(vplotinterface.fdelay);
        digitalwrite(vplotmot0_step, LOW);
        delay(vplotinterface.fdelay);
        fcurrposition.m0 := fcurrposition.m0 + inc0;
      end;

      if vplotmatrix[count1, i] = 1 then
      begin
        digitalwrite(vplotmot1_step, HIGH);
        delay(vplotinterface.fdelay);
        digitalwrite(vplotmot1_step, LOW);
        delay(vplotinterface.fdelay);
        fcurrposition.m1 := fcurrposition.m1 + inc1;
      end;
    end;
    fcurrposition.p := nextposition.p;
  end;

  writeln('next.count0 ',  nextposition.m0);
  writeln('next.count1 ',  nextposition.m1);
  writeln('curr.count0 ', fcurrposition.m0);
  writeln('curr.count1 ', fcurrposition.m1);
end;

procedure tvplotdriver.move(motor, count: longint; cw: boolean);
begin
  if cw then
  begin
    if motor = 0 then
      digitalwrite(vplotmot0_dir, HIGH)
    else
      digitalwrite(vplotmot1_dir, LOW);
  end else
  begin
    if motor = 0 then
      digitalwrite(vplotmot0_dir, LOW)
    else
      digitalwrite(vplotmot1_dir, HIGH);
  end;

  while count > 0 do
  begin
    if motor = 0 then
      digitalwrite(vplotmot0_step, HIGH)
    else
      digitalwrite(vplotmot1_step, HIGH);
    delay(10);
    if motor = 0 then
      digitalwrite(vplotmot0_step, LOW)
    else
      digitalwrite(vplotmot1_step, LOW);
    delay(10);
    dec(count);
  end;
end;

procedure tvplotdriver.draw(const code: tvplotcode);
var
  i, j: longint;
  p0, p1, cc: tvplotpoint;
begin
  if code.x < ((1 * fvplot[1].y) / 6) then exit;
  if code.x > ((5 * fvplot[1].y) / 6) then exit;
  if code.y < ((1 * fvplot[1].y) / 6) then exit;
  if code.y > ((5 * fvplot[1].y) / 6) then exit;

  setlength(fvplotpath, 0);
  if (code.c ='G00 ') or (code.c = 'G01 ') then
  begin
    p0.x := fcurrposition.p.x;
    p0.y := fcurrposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    interpolate_line(p0, p1);
  end else
  if (code.c ='G02 ') or (code.c = 'G03 ') then
  begin
    p0.x := fcurrposition.p.x;
    p0.y := fcurrposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    cc.x := p0.x + code.i;
    cc.y := p0.y + code.j;
    interpolate_arc(p0, p1, cc, code.c = 'G02 ');
  end;

  j := length(fvplotpath);
  if j > 0 then
  begin
    // move servo
    if not fvplotinterface.fpreview then
      if vplotservo <> code.z then
      begin
        vplotservo := code.z;
        if vplotservo < 0 then
          pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_maxvalue, vplotservo_freq))
        else
          pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_rstvalue, vplotservo_freq));
        delay(1000);
      end;
    // move stepper
    for i := 0 to j - 1 do
    begin
      fvplotinterface.point0 := fcurrposition.p;
      fvplotinterface.point1 := fvplotpath[i].p;

      synchronize(fvplotinterface.fsync2);
      if not fvplotinterface.fpreview then
        moveto(fvplotpath[i]);
      if (fvplotcode.c <> 'G00 ') and (code.z < 0) then
        synchronize(fvplotinterface.fsync3);
    end;
  end;
end;

procedure tvplotdriver.execute;
begin
  repeat
    synchronize(fvplotinterface.fsync1);
    if not fvplotinterface.suspended then
    begin
      digitalwrite(P11, LOW);
      parse_line(fvplotinterface.gcode, fvplotcode);
      if fvplotcode.c <> '' then
      begin
        if (fvplotcode.c ='G00 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G01 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G02 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G03 ') then draw(fvplotcode);
      end;
      sleep(5);
      synchronize(fvplotinterface.fsync4);
    end else
    begin
      sleep(100);
      vplotservo := 0;
      pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_rstvalue, vplotservo_freq));
    end;
    digitalwrite(P11, HIGH);
  until terminated;
end;

end.

