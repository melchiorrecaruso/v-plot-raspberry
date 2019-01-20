{
  Description: vPlot sketch class.

  Copyright (C) 2017-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit sketchyimage;

{$mode objfpc}

interface

uses
  classes, graphics, bgrabitmap, bgrasvg, fpimage, vpmath;


  procedure decodePNG(const filename: string; maxlinelength: longint; nibsize: single; linesize: longint; maxlinelen: longint);


implementation

uses
  math, sysutils;


type
  tlinedarknessmode = (linedarknessmodeavg, linedarknessmodeclear);

type
  tsketchyimage = class(tobject)
    retaincount:    longint;
    stype:          shortstring;
    fdata_in:       array of array of byte;
    fdata_out:      array of array of byte;
    scalefactor:    double;
    xcorrection:    double;
    ycorrection:    double;
    xoffset:        double;
    yoffset:        double;

    fbrightness_in:  longint;
    fbrightness_out: longint;
    fbrightness_avg: double;

    fheight:        longword;
    fwidth:         longword;
    fnibsize:       longint;
  protected
    function px(x: longint): longint;
    function py(y: longint): longint;


    function darknesshelperforline(
      const x1, y1, x2, y2: single; mode: tlinedarknessmode): single;

    function kernelvaluebyxy(x, y: longint; clear: boolean; kernelsize: longint): longint;

    function avgdarknessforline  (x1, y1, x2, y2: single): single;
    function cleardarknessforline(x1, y1, x2, y2: single): single;
    function bestpointofndestinationsfromxy2(x, y, radius: single): tvppoint;

  public
    constructor create(const filename: rawbytestring);
    destructor destroy; override;
    function getdarkpixel: tpoint;

    procedure save(const filename: rawbytestring);
    procedure savestate(const filename: rawbytestring);

  published


    property brightness_in:  longint read fbrightness_in;
    property brightness_out: longint read fbrightness_out;
    property brightness_avg: double  read fbrightness_avg;



    property nibsize: longint  read fnibsize write fnibsize;

    property height:  longword read fheight;
    property width:   longword read fwidth;


  end;

var
  gcounter: longint = 0;


constructor tsketchyimage.create(const filename: rawbytestring);
var
  x, y:   longint;
  pixval: longint;
  image:  tbgrabitmap;
begin
  inherited create;
  retaincount     := 1;
  stype           := 'sketchyimage';
  scalefactor     := 1.0;
  xcorrection     := 0.0;
  ycorrection     := 0.0;
  xoffset         := 0.0;
  yoffset         := 0.0;

  fbrightness_in  := 0;
  fbrightness_out := 0;
  fbrightness_avg := 255;
  fnibsize        := 1;

  image  := tbgrabitmap.create;
  try
    image.loadfromfile(filename);
  except;
  end;
  fheight := image.height;
  fwidth  := image.width;

  setlength(fdata_in,  fwidth, fheight);
  setlength(fdata_out, fwidth, fheight);
  for y := 0 to fheight - 1 do
    for x := 0 to fwidth - 1 do
    begin
      pixval := byte(image.canvas.colors[x, y].green);
      inc(fbrightness_in, pixval);
      fdata_out[x, y] := 255;
      fdata_in [x, y] := pixval;
    end;
  fbrightness_out := fwidth*fheight*255;
  fbrightness_avg := fbrightness_in div (fwidth*fheight);

  writeln('brightness_in  ', fbrightness_in);
  writeln('brightness_avg ', fbrightness_avg);
  writeln('brightness_out ', fbrightness_out);

  image.destroy;
end;

destructor tsketchyimage.destroy;
begin
  setlength(fdata_in,  0, 0);
  setlength(fdata_out, 0, 0);
  inherited destroy;
end;

function tsketchyimage.getdarkpixel: tpoint;
var
  x, y:    longint;
  darkest: longint;
  pixval:  longint;
begin
  darkest  := 255;
  result.x := 0;
  result.y := 0;

  for y := 0 to fheight -1 do
    for x := 0 to fwidth - 1 do
    begin
      pixval := fdata_in[x, y];
      if pixval < darkest then
      begin
        darkest  := pixval;
        result.x := x;
        result.y := y;
      end;
    end;
end;

function tsketchyimage.darknesshelperforline(
  const x1, y1, x2, y2: single; mode: tlinedarknessmode): single;
var

  i: longint;

  cx: longint;
  cy: longint;

  kernelsize: longint;

  xpol: longint;
  ypol: longint;


  xd:    single;
  yd:    single;
  slope: single;
  totdarkness: single;

  darkness: longint;
begin

  kernelsize := nibsize;
  if (kernelsize mod 2) = 0 then
    kernelsize := kernelsize - 1;

  xpol := longint((x1-x2)<0);
  ypol := longint((y1-y2)<0);

  if (xpol = 0) then xpol := -1;
  if (ypol = 0) then ypol := -1;

  xd := abs(x1-x2);
  yd := abs(y1-y2);
  slope := (x1-x2)/(y1-y2);
  totdarkness := 0;

  if (xd > yd) then
  begin
    i  := 0;
    cx := round(x1);
    cy := round(y1);
    while i < xd do
    begin
      if mode = linedarknessmodeavg then
      begin
        darkness := kernelvaluebyxy(trunc(cx), trunc(cy), false, kernelsize);
        totdarkness := totdarkness + darkness;
      end else
        kernelvaluebyxy(cx, cy, true, kernelsize);

      cx := cx + xpol;
      cy := round(y1 + i/slope*xpol);
      inc(i);
    end;
    result := totdarkness/xd;
  end else
  begin
    i  := 0;
    cx := round(x1);
    cy := round(y1);
    while i < yd do
    begin
      if mode = linedarknessmodeavg then
      begin
        darkness := kernelvaluebyxy(cx, cy, false, kernelsize);
        totdarkness := totdarkness + darkness;
      end else
        kernelvaluebyxy(cx, cy, true, kernelsize);

      cy := cy + ypol;
      cx := trunc(x1 + i*slope*ypol);
      inc(i);
    end;
    result := totdarkness/yd;
  end;
end;

function tsketchyimage.avgdarknessforline(x1, y1, x2, y2: single): single;
begin
  result := darknesshelperforline(x1, y1, x2, y2, linedarknessmodeavg);
end;

function tsketchyimage.cleardarknessforline(x1, y1, x2, y2: single): single;
begin
  result := darknesshelperforline(x1, y1, x2, y2, linedarknessmodeclear);
end;

function tsketchyimage.bestpointofndestinationsfromxy2(x, y, radius: single): tvppoint;
var
  i: longint;
  best:  single;
  bestx: single;
  besty: single;

  rx:  single;
  ry:  single;
  avg: single;

begin
  x := x * scalefactor;
  y := y * scalefactor;

  best  := 9999.0;
  bestx := 0;
  besty := 0;
  inc(gcounter);

  i := gcounter;
  while i < 360 + gcounter do
  begin
    rx := x + cos(degtorad(i)) * radius;
    ry := y + sin(degtorad(i)) * radius;

    if (rx < fwidth -1) and (rx > 0) and
       (ry < fheight-1) and (ry > 0) then
    begin
      avg := avgdarknessforline(x, y, rx, ry);
      if (avg < best) then
      begin
        best  := avg;
        bestx := rx;
        besty := ry;
      //if (avg < 100) then
      //  break;
      end;
    end;
    inc(i, 3);
  end;

  cleardarknessforline(x, y, bestx, besty);
  result.x := bestx/scalefactor;
  result.y := besty/scalefactor;
end;

function tsketchyimage.px(x: longint): longint;
begin
  if x < fwidth then
    result := x
  else
    result := 0;
end;

function tsketchyimage.py(y: longint): longint;
begin
  if y < fheight then
    result := y
  else
    result := 0;
end;

function tsketchyimage.kernelvaluebyxy(x, y: longint; clear: boolean; kernelsize: longint): longint;
var
  xt: longint;
  yt: longint;

  i: longint;
  limit: longint;
begin
  result := -1;
  if (x > fwidth -1) or
     (y > fheight-1) or
     (x < 0)        or
     (y < 0)        then exit;

  result := fdata_in[x, y];
  if (kernelsize > 1) then
  begin
    limit := (kernelsize - 1) div 2;
    for i := 0 to limit - 1 do
    begin
      inc(result, fdata_in[px(x+1+i), y]);
      inc(result, fdata_in[px(x-1-i), y]);
      inc(result, fdata_in[x, py(y+1+i)]);
      inc(result, fdata_in[x, py(y-1-i)]);
    end;
  end;

   if clear then
   begin
     if (kernelsize > 1) then
     begin
       limit := (kernelsize - 1) div 2;
       for i := 0 to limit - 1 do
       begin
         xt := px(x+1+i);
         inc(fbrightness_in, 255 - fdata_in [xt, y]);
         dec(fbrightness_out,      fdata_out[xt, y]);
         fdata_in [xt, y] := 255;
         fdata_out[xt, y] := 0;

         xt := px(x-1-i);
         inc(fbrightness_in, 255 - fdata_in [xt, y]);
         dec(fbrightness_out,      fdata_out[xt, y]);
         fdata_in [xt, y] := 255;
         fdata_out[xt, y] := 0;

         yt := py(y+1+i);
         inc(fbrightness_in, 255 - fdata_in [x, yt]);
         dec(fbrightness_out,      fdata_out[x, yt]);
         fdata_in [x, yt] := 255;
         fdata_out[x, yt] := 0;

         yt := py(y-1-i);
         inc(fbrightness_in, 255 - fdata_in [x, yt]);
         dec(fbrightness_out,      fdata_out[x, yt]);
         fdata_in [x, yt] := 255;
         fdata_out[x, yt] := 0;
       end
     end;

     inc(fbrightness_in, 255 - fdata_in [x, y]);
     dec(fbrightness_out,      fdata_out[x, y]);
     fdata_in [x, y] := 255;
     fdata_out[x, y] := 0;
   end;
end;

procedure tsketchyimage.savestate(const filename: rawbytestring);
var
  x, y:  longint;
  image: tbgrabitmap;
  color: tfpcolor;
begin
  image := tbgrabitmap.create(fwidth, fheight);
  for y := 0 to fheight - 1 do
    for x := 0 to fwidth - 1 do
    begin
      color       := image.canvas.colors[x, y];
      color.green := fdata_in[x, y];
      image.canvas.colors[x, y] := color;
    end;
  image.savetofile(filename);
  image.destroy;
end;

procedure tsketchyimage.save(const filename: rawbytestring);
var
  x, y:  longint;
  image: tbgrabitmap;
  color: tfpcolor;
begin
  image := tbgrabitmap.create(fwidth, fheight);
  for y := 0 to fheight - 1 do
    for x := 0 to fwidth - 1 do
    begin
      color       := image.canvas.colors[x, y];
      color.alpha := fdata_out[x, y];
      image.canvas.colors[x, y] := color;
    end;
  image.savetofile(filename);
  image.destroy;
end;

procedure decodePNG(const filename: string; maxlinelength: longint; nibsize: single; linesize: longint; maxlinelen: longint);
var
  x: single;
  y: single;
  r: longint;
  p: tvppoint;

  sketchy: tsketchyimage;
  avg: single;
  threshold : longint;
  brightness_out: longint;
  darkestpixel: tpoint;

  svg: tbgrasvg;

begin
  randomize;
  sketchy := tsketchyimage.create(filename);
  sketchy.nibsize := linesize;

  avg       := sketchy.brightness_avg;
  threshold := sketchy.brightness_in;

  if (avg < 128) then
    threshold := round(threshold * (128.0/avg));

  brightness_out := sketchy.brightness_out;
  darkestpixel   := sketchy.getdarkpixel;

  x := darkestpixel.x;
  y := darkestpixel.y;

  writeln('darkestpixel.x ', darkestpixel.x);
  writeln('darkestpixel.y ', darkestpixel.y);

  svg := tbgrasvg.create(sketchy.width, sketchy.height, cupixel);
  while brightness_out > threshold do
  begin
    r := 10 + random(maxlinelen - 10);
    p := sketchy.bestpointofndestinationsfromxy2(r,x,y);
    svg.content.appendline(x, y, p.x, p.y);
    brightness_out := sketchy.brightness_out;
    writeln('brightness =', brightness_out);

    x := p.x;
    y := p.y;
  end;
  sketchy.savestate(changefileext(filename, '.bmp'));
  svg.savetofile(changefileext(filename, '.svg'));
  sketchy.destroy;

  writeln('decodePNG - end');
end;


end.
