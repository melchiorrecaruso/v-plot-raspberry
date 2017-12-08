unit libvplot;

{$mode objfpc}{$h+}

interface

uses
  classes, graphics, sysutils;

type
  tvplotpoint = record
    x: double;
    y: double;
    z: double;
  end;

  tvplotposition = record
    m1: longword;
    m2: longword;
     p: tvplotpoint;
  end;

  tvplotline = record
    m: double;
    q: double;
  end;

type
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
    fpoint:     tvplotpoint;
    fsync1:      tthreadmethod;
    fsync2:      tthreadmethod;
    fsuspended: boolean;
  public
    property code:      rawbytestring read fcode      write fcode;
    property point:     tvplotpoint   read fpoint     write fpoint;
    property sync1:     tthreadmethod read fsync1     write fsync1;
    property sync2:     tthreadmethod read fsync2     write fsync2;
    property suspended: boolean       read fsuspended write fsuspended;
  end;

type
  tvplotdriver = class (tthread)
  private
    fvplot:          array [0..6] of tvplotpoint;
    fvplotinterface: tvplotinterface;
    fvplotposition:  tvplotposition;
    fvplotpath:      array of tvplotposition;
    fvplotratio:     double;
    procedure linearinterpolation(const point: tvplotpoint);
    procedure optimize(var position: tvplotposition);
    procedure moveto(const position: tvplotposition);
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
  inifiles;

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

function translatepoint(base, p: tvplotpoint): tvplotpoint;
begin
  result.x := base.x + p.x;
  result.y := base.y + p.y;
end;

function rotatepoint(p: tvplotpoint; alfa: double): tvplotpoint;
begin
  result.x := p.x * cos(alfa) - p.y * sin(alfa);
  result.y := p.x * sin(alfa) + p.y * cos(alfa);
end;

function linebetween(p1, p2: tvplotpoint): tvplotline;
begin
  result.m := (p2.y - p1.y) / (p2.x - p1.x);
  result.q :=  p2.y - (result.m * p2.x);
end;

function anglebetween(l1, l2: tvplotline): double;
begin
  result := arctan((l2.m - l1.m)/(1 + (l1.m * l2.m)));
end;

function distancebetween(p1, p2: tvplotpoint): double;
begin
  result := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y));
end;

function intersectlines(l1, l2: tvplotline): tvplotpoint;
begin
  result.x := (l2.q - l1.q) / (l1.m - l2.m);

  result.y := result.x * l2.m + l2.q;
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
    ini.writeinteger(section4,   'X',  250);
    ini.writeinteger(section4,   'Y',  750);
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
  fvplot[6].x := ini.readinteger(section4,   'X', -1);
  fvplot[6].y := ini.readinteger(section4,   'Y', -1);
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
  fvplotratio := ini.readfloat  (section3,   'R', -1);
  // ---
  freeandnil(ini);

  fvplotposition.p.x := fvplot[6].x;
  fvplotposition.p.y := fvplot[6].y;
  optimize(fvplotposition);
end;

procedure tvplotdriver.linearinterpolation(const point: tvplotpoint);
var
   dx: double;
   dy: double;
    i: longint;
  len: longint;
begin
  //writeln('[linearinterpolation]');

  dx := point.x - fvplotposition.p.x;
  dy := point.y - fvplotposition.p.y;

  (*
  writeln('dx ', dx:6:2);
  writeln('dy ', dy:6:2);
  *)
  len := 2;
  while true do
  begin
    if (abs(dy / len) < 0.2) then
    if (abs(dx / len) < 0.2) then break;
    inc(len, 2);
  end;

  setlength(fvplotpath, len);
  for i := 0 to len - 1 do
  begin
    fvplotpath[i].p.x := fvplotposition.p.x + i * (dx / len);
    fvplotpath[i].p.y := fvplotposition.p.y + i * (dy / len);
  end;

  //writeln('step count=', length(fvplotpath));
  //fvplotpath[0].m1 := fvplotposition.m1;
  //fvplotpath[0].m2 := fvplotposition.m2;
  for i := 0 to length(fvplotpath) - 1 do
  begin
    optimize(fvplotpath[i]);
  end;
end;

procedure tvplotdriver.optimize(var position: tvplotposition);
var
  alfa:   double;
  error:  double;
  lines:  array[0..1] of tvplotline;
  points: array[0..6] of tvplotpoint;
begin
  alfa := 0;
  repeat
    // absolute coordinates
    points[0] := fvplot[0];
    points[1] := fvplot[1];
    points[2] := translatepoint(position.p, rotatepoint(fvplot[2], alfa));
    points[3] := translatepoint(position.p, rotatepoint(fvplot[3], alfa));
    points[4] := translatepoint(position.p, rotatepoint(fvplot[4], alfa));
    points[5] := translatepoint(position.p, rotatepoint(fvplot[5], alfa));
    lines [0] := linebetween(points[0], points[3]);
    lines [1] := linebetween(points[1], points[4]);
    points[6] := intersectlines(lines[0], lines[1]);

    error := abs(points[6].y - points[5].y);
    if  error > 0.001 then
    begin
      if points[6].y < points[5].y then
        alfa := alfa - (error / 100)
      else
      if points[6].y > points[5].y then
        alfa := alfa + (error / 100);
    end;

  until error < 0.001;

  (*
  writeln('[optimize]');
  writeln(   'R=', fvplotratio:2:6);
  writeln('ALFA=', alfa:2:6);
  writeln(' 0.X=', points[0].x:6:2);
  writeln(' 0.Y=', points[0].y:6:2);
  writeln(' 1.X=', points[1].x:6:2);
  writeln(' 1.Y=', points[1].y:6:2);
  writeln(' 2.X=', points[2].x:6:2);
  writeln(' 2.Y=', points[2].y:6:2);
  writeln(' 3.X=', points[3].x:6:2);
  writeln(' 3.Y=', points[3].y:6:2);
  writeln(' 4.X=', points[4].x:6:2);
  writeln(' 4.Y=', points[4].y:6:2);
  writeln(' 5.X=', points[5].x:6:2);
  writeln(' 5.Y=', points[5].y:6:2);
  writeln(' 6.X=', points[6].x:6:2);
  writeln(' 6.Y=', points[6].y:6:2);

  writeln('Len-03=', distancebetween(points[0], points[3]):6:2);
  writeln('Len-14=', distancebetween(points[1], points[4]):6:2);
  writeln;
  *)

  position.m1 := round(distancebetween(points[0], points[3]) / fvplotratio);
  position.m2 := round(distancebetween(points[1], points[4]) / fvplotratio);
end;

procedure tvplotdriver.moveto(const position: tvplotposition);
var
  ds1: longint;
  ds2: longint;
begin
  if position.p.x < ((1 * fvplot[1].y) / 6) then exit;
  if position.p.x > ((5 * fvplot[1].y) / 6) then exit;
  if position.p.y < ((1 * fvplot[1].y) / 6) then exit;
  if position.p.y > ((5 * fvplot[1].y) / 6) then exit;

  ds1 := position.m1 - fvplotposition.m1;
  ds2 := position.m2 - fvplotposition.m2;

  (*
  writeln('[moveto]');
  writeln('DELTA STEPS M1 = ', ds1, '(', fvplotposition.m1, ')');
  writeln('DELTA STEPS M2 = ', ds2, '(', fvplotposition.m2, ')');
  writeln;
  *)

  fvplotposition := position;
  fvplotinterface.point := position.p;
  synchronize(fvplotinterface.fsync2);
end;

procedure tvplotdriver.execute;
var
  i:     longint;
  code:  tvplotcode;
  point: tvplotpoint;
begin
  synchronize(fvplotinterface.fsync1);
  while true do
  begin
    if not fvplotinterface.suspended then
    begin

      parse_line(parse_comment(fvplotinterface.code), code);
      if (code.c ='G00') or (code.c = 'G0') then
      begin
        // writeln('Moving...');

        point.x := code.x;
        point.y := code.y;
        point.z := code.z;
        linearinterpolation(point);
        for i := 0 to length(fvplotpath) - 1 do
          moveto(fvplotpath[i]);




      end else
      if (code.c ='G01') or (code.c = 'G1') then
      begin



      end;
      // writeln('skip "'+ fvplotinterface.code +'"');


    end;
    synchronize(fvplotinterface.fsync1);
  end;
end;

end.

