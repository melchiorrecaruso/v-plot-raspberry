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
    m: array[0..1] of longint;
    p: tvplotpoint;
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
    fpoint1:    tvplotpoint;
    fpoint2:    tvplotpoint;
    fsync1:     tthreadmethod;
    fsync2:     tthreadmethod;
    fsync3:     tthreadmethod;
    fsync4:     tthreadmethod;
    fpreview:   boolean;
    fsuspended: boolean;
  public
    property gcode:     rawbytestring read fgcode     write fgcode;
    property point1:    tvplotpoint   read fpoint1    write fpoint1;
    property point2:    tvplotpoint   read fpoint2    write fpoint2;
    property sync1:     tthreadmethod read fsync1     write fsync1;
    property sync2:     tthreadmethod read fsync2     write fsync2;
    property sync3:     tthreadmethod read fsync3     write fsync3;
    property sync4:     tthreadmethod read fsync4     write fsync4;
    property preview:   boolean       read fpreview   write fpreview;
    property suspended: boolean       read fsuspended write fsuspended;
  end;

type
  tvplotstepper = class (tthread)
  private

  public

  end;

type
  tvplotdriver = class (tthread)
  private
    fvplot:     array[0..6] of tvplotpoint;
    fvplotpath: array of tvplotposition;
    fvplotcode: tvplotcode;
    fvplotinterface: tvplotinterface;
    fvplotposition: tvplotposition;
    fvplotratio: double;
    procedure interpolate_line(const p0, p1: tvplotpoint);
    procedure interpolate_arc (const p0, p1, cc: tvplotpoint; clockwise: boolean);

    procedure optimize(var position: tvplotposition);
    procedure moveto(var position: tvplotposition);
    procedure draw(const code: tvplotcode);
  protected
    procedure execute; override;
  public
    constructor create(vplotinterface: tvplotinterface);
    destructor destroy; override;
    procedure initialize;
  end;

var
  vplotinterface: tvplotinterface = nil;
  vplotdriver:    tvplotdriver    = nil;

implementation

uses
  inifiles, math;

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
    writeln('C0257');
end;

// vplotserver //

constructor tvplotdriver.create(vplotinterface: tvplotinterface);
begin
  fvplotinterface := vplotinterface;
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
    ini.writeinteger(section1, '0.X',    0);
    ini.writeinteger(section1, '0.Y',    0);
    ini.writeinteger(section1, '1.X',    0);
    ini.writeinteger(section1, '1.Y', 1500);
    ini.writeinteger(section4, '6.X',  250);
    ini.writeinteger(section4, '6.Y',  750);
    // relative coordinates
    ini.writeinteger(section2, '2.X',    0);
    ini.writeinteger(section2, '2.Y',    0);
    ini.writeinteger(section2, '3.X',   30);
    ini.writeinteger(section2, '3.Y',  -35);
    ini.writeinteger(section2, '4.X',   30);
    ini.writeinteger(section2, '4.Y',   35);
    ini.writeinteger(section2, '5.X',   90);
    ini.writeinteger(section2, '5.Y',    0);
    // ---
    ini.writefloat  (section3,   'R',    r);
    // ---
    freeandnil(ini);
  end;

  ini := tinifile.create(changefileext(paramstr(0),'.ini'));
  // absolute coordinates
  fvplot[0].x := ini.readinteger(section1, '0.X', -1);
  fvplot[0].y := ini.readinteger(section1, '0.Y', -1);
  fvplot[1].x := ini.readinteger(section1, '1.X', -1);
  fvplot[1].y := ini.readinteger(section1, '1.Y', -1);
  fvplot[6].x := ini.readinteger(section4, '6.X', -1);
  fvplot[6].y := ini.readinteger(section4, '6.Y', -1);
  // relative coordinates
  fvplot[2].x := ini.readinteger(section2, '2.X', -1);
  fvplot[2].y := ini.readinteger(section2, '2.Y', -1);
  fvplot[3].x := ini.readinteger(section2, '3.X', -1);
  fvplot[3].y := ini.readinteger(section2, '3.Y', -1);
  fvplot[4].x := ini.readinteger(section2, '4.X', -1);
  fvplot[4].y := ini.readinteger(section2, '4.Y', -1);
  fvplot[5].x := ini.readinteger(section2, '5.X', -1);
  fvplot[5].y := ini.readinteger(section2, '5.Y', -1);
  // ---
  fvplotratio := ini.readfloat(section3,     'R', -1);
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
  fvplotposition.p := fvplot[6];
  optimize(fvplotposition);
end;

procedure tvplotdriver.interpolate_line(const p0, p1: tvplotpoint);
var
  dx, dy: double;
  i, j: longint;
begin
  i := max(1, round(abs(distancebetween(p0, p1) / 0.25)));

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
    tmp: array[0..6] of tvplotpoint;
  line0: tvplotline;
  line1: tvplotline;
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

    (*
    writeln('[optimize]');
    writeln(   'R=', fvplotratio:2:6);
    writeln('ALFA=', radtodeg(alpha):2:6);
    writeln(' 0.X=', tmp[0].x:6:2);
    writeln(' 0.Y=', tmp[0].y:6:2);
    writeln(' 1.X=', tmp[1].x:6:2);
    writeln(' 1.Y=', tmp[1].y:6:2);
    writeln(' 2.X=', tmp[2].x:6:2);
    writeln(' 2.Y=', tmp[2].y:6:2);
    writeln(' 3.X=', tmp[3].x:6:2);
    writeln(' 3.Y=', tmp[3].y:6:2);
    writeln(' 4.X=', tmp[4].x:6:2);
    writeln(' 4.Y=', tmp[4].y:6:2);
    writeln(' 5.X=', tmp[5].x:6:2);
    writeln(' 5.Y=', tmp[5].y:6:2);

    writeln(' 6.X=', tmp[6].x:6:2);
    writeln(' 6.Y=', tmp[6].y:6:2);

    writeln('  03=', distancebetween(tmp[0], tmp[3]):6:2);
    writeln('  14=', distancebetween(tmp[1], tmp[4]):6:2);
    writeln;
    *)

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

  position.m[0] := round(distancebetween(tmp[0], tmp[3]) / fvplotratio);
  position.m[1] := round(distancebetween(tmp[1], tmp[4]) / fvplotratio);
end;

procedure tvplotdriver.moveto(var position: tvplotposition);
begin
  if not fvplotinterface.fpreview then
  begin
    optimize(position);
    (*
    writeln('[moveto]');
    writeln('DELTA STEPS M1 = ', ds1, '(', fvplotposition.m1, ')');
    writeln('DELTA STEPS M2 = ', ds2, '(', fvplotposition.m2, ')');
    writeln;
    *)
  end;
  fvplotposition := position;
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
    p0.x := fvplotposition.p.x;
    p0.y := fvplotposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    interpolate_line(p0, p1);
  end else
  if (code.c ='G02 ') or (code.c = 'G03 ') then
  begin
    p0.x := fvplotposition.p.x;
    p0.y := fvplotposition.p.y;
    p1.x := code.x;
    p1.y := code.y;
    cc.x := p0.x + code.i;
    cc.y := p0.y + code.j;
    interpolate_arc(p0, p1, cc, code.c = 'G02 ');
  end;

  j := length(fvplotpath);
  if j > 0 then
    for i := 0 to j - 1 do
    begin
      fvplotinterface.point1 := fvplotposition.p;
      fvplotinterface.point2 := fvplotpath[i].p;

      synchronize(fvplotinterface.fsync2);
      moveto(fvplotpath[i]);
      if (fvplotcode.c <> 'G00 ') and (code.z < 0) then
        synchronize(fvplotinterface.fsync3);
    end;
end;

procedure tvplotdriver.execute;
begin
  repeat
    synchronize(fvplotinterface.fsync1);
    if not fvplotinterface.suspended then
    begin
      parse_line(fvplotinterface.gcode, fvplotcode);
      if fvplotcode.c <> '' then
      begin
        if (fvplotcode.c ='G00 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G01 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G02 ') then draw(fvplotcode) else
        if (fvplotcode.c ='G03 ') then draw(fvplotcode);
      end;
      sleep(2);
      synchronize(fvplotinterface.fsync4);
    end else
      sleep(100);
  until false;
end;

end.

