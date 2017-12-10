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
  end;

type
  tvplotinterface = class
  private
    fcode:      rawbytestring;
    fpoint1:    tvplotpoint;
    fpoint2:    tvplotpoint;
    fsync1:     tthreadmethod;
    fsync2:     tthreadmethod;
    fsync3:     tthreadmethod;
    fsync4:     tthreadmethod;
    fsuspended: boolean;
  public
    property code:      rawbytestring read fcode      write fcode;
    property point1:    tvplotpoint   read fpoint1    write fpoint1;
    property point2:    tvplotpoint   read fpoint2    write fpoint2;
    property sync1:     tthreadmethod read fsync1     write fsync1;
    property sync2:     tthreadmethod read fsync2     write fsync2;
    property sync3:     tthreadmethod read fsync3     write fsync3;
    property sync4:     tthreadmethod read fsync4     write fsync4;
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
    fvplotinterface: tvplotinterface;
    fvplotposition: tvplotposition;
    fvplotratio: double;
    procedure linearinterpolation(const point: tvplotpoint);
    procedure optimize(var position: tvplotposition);
    procedure moveto(var position: tvplotposition);
    procedure draw(const x,y,z: double);
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

function parse_comment(const gcode: rawbytestring): rawbytestring;
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
end;

function parse_prefix(const prefix, gcode: rawbytestring): extended;
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
    result := strtofloat(s)
  else
    result := 0;
end;

procedure parse_g00(const gcode: rawbytestring; var vcode: tvplotcode);
begin
  vcode.c  := 'G00';
  vcode.x  := parse_prefix('X', gcode);
  vcode.y  := parse_prefix('Y', gcode);
  vcode.z  := parse_prefix('Z', gcode);
  vcode.f  := parse_prefix('F', gcode);
  vcode.e  := parse_prefix('E', gcode);
end;

procedure parse_g01(const gcode: rawbytestring; var vcode: tvplotcode);
begin
  vcode.c := 'G01';
  vcode.x  := parse_prefix('X', gcode);
  vcode.y  := parse_prefix('Y', gcode);
  vcode.z  := parse_prefix('Z', gcode);
  vcode.f  := parse_prefix('F', gcode);
  vcode.e  := parse_prefix('E', gcode);
end;

procedure parse_line(const gcode: rawbytestring; var vcode: tvplotcode);
begin
  vcode.c := '';
  vcode.x  := 0;
  vcode.y  := 0;
  vcode.z  := 0;
  vcode.f  := 0;
  vcode.e  := 0;
  if length(gcode) <> 0 then
  begin
    if pos('G0',  gcode) = 1 then parse_g00(gcode, vcode) else
    if pos('G1',  gcode) = 1 then parse_g01(gcode, vcode) else
    if pos('G00', gcode) = 1 then parse_g00(gcode, vcode) else
    if pos('G01', gcode) = 1 then parse_g01(gcode, vcode) else
    if pos('G21', gcode) = 1 then vcode.c := 'G21'        else
    if pos('G28', gcode) = 1 then vcode.c := 'G28';
  end;
end;

// routines //

function translatepoint(base, p: tvplotpoint): tvplotpoint; inline;
begin
  result.x := base.x + p.x;
  result.y := base.y + p.y;
end;

function rotatepoint(p: tvplotpoint; alfa: double): tvplotpoint; inline;
begin
  result.x := p.x * cos(alfa) - p.y * sin(alfa);
  result.y := p.x * sin(alfa) + p.y * cos(alfa);
end;

function distancebetween(p1, p2: tvplotpoint): double; inline;
begin
  result := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y));
end;

procedure linebetween(p1, p2: tvplotpoint; var m, q: double); inline;
begin
  m := (p2.y - p1.y) / (p2.x - p1.x);
  q :=  p2.y - (m * p2.x);
end;

function intersectlines(m1, q1, m2, q2: double): tvplotpoint; inline;
begin
  result.x := -((q2 - q1) / (m2 - m1));
  result.y := m2 * result.x + q2;
end;

// vplotserver //

constructor tvplotdriver.create(vplotinterface: tvplotinterface);
begin
  inherited create(false);
  fvplotinterface := vplotinterface;
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
  fvplotposition.p := fvplot[6];
  fvplotposition.p.x := 250;
  fvplotposition.p.y := 250;
  optimize(fvplotposition);
end;

procedure tvplotdriver.linearinterpolation(const point: tvplotpoint);
var
  dx: double;
  dy: double;
  i: longint;
  j: longint;
begin
  dx := point.x - fvplotposition.p.x;
  dy := point.y - fvplotposition.p.y;

  i   := 1;
  while (abs(dx / i) > 0.25) or
        (abs(dy / i) > 0.25) do inc(i);

  setlength(fvplotpath, i + 1);
  for j := 0 to i do
  begin
    fvplotpath[j].p.x := fvplotposition.p.x + (j * (dx / i));
    fvplotpath[j].p.y := fvplotposition.p.y + (j * (dy / i));
  end;
end;

procedure tvplotdriver.optimize(var position: tvplotposition);
var
    alfa: double;
   error: double;
     tmp: array[0..6] of tvplotpoint;
  m1, m2: double;
  q1, q2: double;
begin
  alfa := 0;
  repeat
    // absolute coordinates
    tmp[0] := fvplot[0];
    tmp[1] := fvplot[1];
    tmp[2] := translatepoint(position.p, rotatepoint(fvplot[2], alfa));
    tmp[3] := translatepoint(position.p, rotatepoint(fvplot[3], alfa));
    tmp[4] := translatepoint(position.p, rotatepoint(fvplot[4], alfa));
    tmp[5] := translatepoint(position.p, rotatepoint(fvplot[5], alfa));
    linebetween(tmp[0], tmp[3], m1, q1);
    linebetween(tmp[1], tmp[4], m2, q2);
    tmp[6] := intersectlines(m1, q1, m2, q2);

    error := abs(tmp[6].y - tmp[5].y);
    if  error > 0.001 then
    begin
      if tmp[6].y < tmp[5].y then
        alfa := alfa - (error / 100)
      else
      if tmp[6].y > tmp[5].y then
        alfa := alfa + (error / 100);
    end else
      break;
  until false;

  position.m[0] := round(distancebetween(tmp[0], tmp[3]) / fvplotratio);
  position.m[1] := round(distancebetween(tmp[1], tmp[4]) / fvplotratio);
  (*
  writeln('[optimize]');
  writeln(   'R=', fvplotratio:2:6);
  writeln('ALFA=', radtodeg(alfa):2:6);
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
end;

procedure tvplotdriver.moveto(var position: tvplotposition);
begin
  if position.p.x < ((1 * fvplot[1].y) / 6) then exit;
  if position.p.x > ((5 * fvplot[1].y) / 6) then exit;
  if position.p.y < ((1 * fvplot[1].y) / 6) then exit;
  if position.p.y > ((5 * fvplot[1].y) / 6) then exit;

  optimize(position);

  (*
  writeln('[moveto]');
  writeln('DELTA STEPS M1 = ', ds1, '(', fvplotposition.m1, ')');
  writeln('DELTA STEPS M2 = ', ds2, '(', fvplotposition.m2, ')');
  writeln;
  *)

  fvplotposition := position;
end;

procedure tvplotdriver.draw(const x, y, z: double);
var
  i, j: longint;
  p: tvplotpoint;
begin
  p.x := x;
  p.y := y;

  if (x > 0) and (y > 0) then
  begin
    linearinterpolation(p);
    i := length(fvplotpath);
    if i > 0 then
    begin
      fvplotinterface.point1 := fvplotpath[0].p;
      fvplotinterface.point2 := fvplotpath[i - 1].p;

      synchronize(fvplotinterface.fsync2);
      for j := 0 to i do
        moveto(fvplotpath[j]);

      if z < 0 then
        synchronize(fvplotinterface.fsync3);
    end;
  end;

end;

procedure tvplotdriver.execute;
var
  code: tvplotcode;
begin
  repeat
    synchronize(fvplotinterface.fsync1);
    if not fvplotinterface.suspended then
    begin
      parse_line(parse_comment(fvplotinterface.code), code);
      if (code.c ='G00') or (code.c = 'G0') then
        draw(code.x, code.y, code.z)
      else
      if (code.c ='G01') or (code.c = 'G1') then
        draw(code.x, code.y, code.z);
      synchronize(fvplotinterface.fsync4);
    end;
    sleep(100);
  until false;
end;

end.

