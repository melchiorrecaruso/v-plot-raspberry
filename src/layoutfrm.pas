{
  Description: vPlot layout form.

  Copyright (C) 2019 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit layoutfrm;

{$mode objfpc}

interface

uses
  buttons, classes, sysutils, forms, controls, graphics, dialogs, extctrls,
  math, spin, stdctrls, vpmath;

type
  { tlayoutform }

  tlayoutform = class(tform)
    bevel1:     tbevel;
    bitbtn1:    tbitbtn;
    distancelb: tlabel;
    distancese: tspinedit;
    Image: TImage;
    procedure drawbtnclick(sender: tobject);
  private
     m0: tvppoint;
     m1: tvppoint;
  public

  end;

var
     layoutform: tlayoutform;
        minload: vpfloat = 0.5;
        maxload: vpfloat = 1.5;
  minresolution: vpfloat = 1.4;
        offsety: longint = 0;

implementation

{$R *.lfm}

const
  gap1 = 1;
  gap2 = 2;
  gap6 = 6;

var
  bit: tbitmap;

{ tlayoutform }

function calc_pp(const a, b, c: vpfloat): tvppoint;
var
  alpha: vpfloat;
begin
     alpha := arccos((sqr(a)+sqr(c)-sqr(b))/(2*a*c));
  result.x := +a*cos(alpha);
  result.y := -a*sin(alpha);
end;

procedure calc_load(const m0, m1, p: tvppoint; out l0, l1: vpfloat);
var
  a0, a1: vpfloat;
       d: vpfloat;
begin
  a0 :=    angle(line_by_two_points(p, m0));
  a1 := pi-angle(line_by_two_points(p, m1));
  // calculate loads
   d := (cos(a0)*sin(a1)+sin(a0)*cos(a1));
  l0 := cos(a1)/d;
  l1 := cos(a0)/d;
end;

procedure draw(top, h, w: longint; const s: string; b: tbitmap);
var
  rect: trect;
begin
  rect.left   := (b.width div 2) - (w div 2);
  rect.right  := (b.width div 2) + (w div 2);
  rect.top    := top;
  rect.bottom := rect.top + h;

  b.canvas.pen.color   := clblack;
  b.canvas.brush.color := clblack;
  b.canvas.rectangle(rect);

  rect.left   := rect.left   + 1;
  rect.right  := rect.right  - 1;
  rect.top    := rect.top    + 1;
  rect.bottom := rect.bottom - 1;

  b.canvas.brush.color := clltgray;
  b.canvas.rectangle(rect);

  b.canvas.brush.color := clltgray;
  b.canvas.textout(rect.left +15, rect.bottom - b.canvas.textheight(s)-2, s);
end;

procedure tlayoutform.drawbtnclick(sender: tobject);
var
       a,b,c: vpfloat;

      d0, d1: vpfloat;

       x,  y: longint;
      dx, dy: longint;
           s: string;
      sx, sy: longint;


    ld0, ld1: vpfloat;
       p, pp: tvppoint;
        clrs: array[0..9] of tcolor;
        clr : tcolor;
begin
  distancese.enabled := false;
  // init colors array
  clrs[0] := rgbtocolor(175, 194, 250);
  clrs[1] := rgbtocolor(146, 171, 248);
  clrs[2] := rgbtocolor(116, 149, 245);
  clrs[3] := rgbtocolor( 86, 125, 243);
  clrs[4] := rgbtocolor( 56, 103, 241);
  clrs[5] := rgbtocolor( 26,  80, 240);
  clrs[6] := rgbtocolor( 15,  66, 219);
  clrs[7] := rgbtocolor( 13,  57, 189);
  clrs[8] := rgbtocolor( 11,  48, 159);
  clrs[9] := rgbtocolor(  9,  39, 130);
  // init motors position
  dx   :=       distancese.value;
  dy   := round(distancese.value*0.7);
  m0.x := 0;
  m0.y := dy;
  m1.x := dx+m0.x;
  m1.y := dy;
  // update bitmap
  bit := image.picture.bitmap;
  bit.beginupdate(true);
  bit.setsize(dx, dy);
  bit.canvas.font.bold   := true;
  bit.canvas.font.size   := 40;
  bit.canvas.pen.color   := clrs[0];
  bit.canvas.brush.color := clrs[0];
  bit.canvas.rectangle(0, 0, dx, dy);

  for x := round(m0.x+1) to (dx -1) do
    for y := (bit.canvas.textheight('X=')) to dy -(bit.canvas.textheight('X=')+gap2) do
    begin
      p.x := x;
      p.y := y;
      calc_load(m0, m1, p, ld0, ld1);
      if p.x < (dx div 2) then
      begin
        ld0 := ld1;
      end;
      clr := clwhite;

      if                          (ld0 <  minload-0.4) then clr := clrs[0] else
      if (ld0 >= minload-0.4) and (ld0 <  minload-0.3) then clr := clrs[1] else
      if (ld0 >= minload-0.3) and (ld0 <  minload-0.2) then clr := clrs[2] else
      if (ld0 >= minload-0.2) and (ld0 <  minload-0.1) then clr := clrs[3] else
      if (ld0 >= minload-0.1) and (ld0 <  minload    ) then clr := clrs[4] else
      if (ld0 >  maxload    ) and (ld0 <= maxload+0.1) then clr := clrs[5] else
      if (ld0 >  maxload+0.1) and (ld0 <= maxload+0.2) then clr := clrs[6] else
      if (ld0 >  maxload+0.2) and (ld0 <= maxload+0.3) then clr := clrs[7] else
      if (ld0 >  maxload+0.3) and (ld0 <= maxload+0.4) then clr := clrs[8] else
      if (ld0 >  maxload+0.4)                          then clr := clrs[9];

      if clr = clwhite then
      begin
        a := distance_between_two_points(m0,  p);
        b := distance_between_two_points(m1,  p);
        c := distance_between_two_points(m0, m1);

        pp   := calc_pp(a+1,b,c);
        pp.x := pp.x + m0.x;
        pp.y := pp.y + m0.y;
        d0   := distance_between_two_points(p, pp);

        pp   := calc_pp(a,b+1,c);
        pp.x := pp.x + m0.x;
        pp.y := pp.y + m0.y;
        d1   := distance_between_two_points(p, pp);

        if ((d0 > minresolution)      and (d0 <= minresolution +0.1)) and
           ((d1 > minresolution)      and (d1 <= minresolution +0.1)) then clr := rgbtocolor(223, 255, 0) else

        if ((d0 > minresolution +0.1) and (d0 <= minresolution +0.2)) and
           ((d1 > minresolution +0.1) and (d1 <= minresolution +0.2)) then clr := rgbtocolor(255, 159, 0) else

        if  (d0 > minresolution +0.2) and (d1 >  minresolution +0.2)  then clr := rgbtocolor(255, 159, 0);
      end;
      bit.canvas.pixels[x, dy -y] := clr;
    end;

  x := bit.width div 2;
  y := 1;
  while y < bit.height -1 do
  begin
    if bit.canvas.pixels[x, y] = clwhite then
    begin
      break;
    end;
    inc(y);
  end;
  inc(y, offsety);
  // draw sheets
  bit.canvas.font.size := 42;
  bit.canvas.font.bold := true;
  draw(y, 841, 1189, 'A0',bit);
  draw(y, 594,  841, 'A1',bit);
  draw(y, 420,  594, 'A2',bit);
  draw(y, 297,  420, 'A3',bit);
  // draw texts
  bit.canvas.brush.color := clrs[0];
  s  :=       ('X=0, Y=0'        ); sx := gap6;            sy := 0;     bit.canvas.textout(sx, sy, s);
  s  := format('Y=%d', [ y      ]); sx := gap6;            sy := y;     bit.canvas.textout(sx, sy, s);
  s  := format('Y=%d', [ y + 297]); sx := gap6;            sy := y+295; bit.canvas.textout(sx, sy, s);
  s  := format('Y=%d', [ y + 420]); sx := gap6;            sy := y+418; bit.canvas.textout(sx, sy, s);
  s  := format('Y=%d', [ y + 594]); sx := gap6;            sy := y+592; bit.canvas.textout(sx, sy, s);
  s  := format('Y=%d', [ y + 841]); sx := gap6;            sy := y+839; bit.canvas.textout(sx, sy, s);
  s  := format('X=%d', [dx div 2]); sx := gap6+(dx div 2); sy := 0;     bit.canvas.textout(sx, sy, s);

  s  := format('X=%d', [dx]);
  sx := dx-bit.canvas.textwidth(s)-gap6;
  sy := 0;
  bit.canvas.textout(sx, sy, s);

  s  := format('min-load=%f   max-load=%f   min-resolution=%f', [minload, maxload, minresolution]);
  sx := (dx-canvas.textwidth(s)) div 4;
  sy := dy-bit.canvas.textheight(s);
  bit.canvas.textout(sx, sy, s);
  // draw lines
  bit.canvas.pen.color := clblack;
  bit.canvas.rectangle(0, 0, dx,   gap2);
  bit.canvas.rectangle(0, y, dx, y+gap2);
  bit.canvas.rectangle(0, y+295, dx, y+297);
  bit.canvas.rectangle(0, y+418, dx, y+420);
  bit.canvas.rectangle(0, y+592, dx, y+594);
  bit.canvas.rectangle(0, y+839, dx, y+841);
  bit.canvas.rectangle((dx div 2)-gap1, 0, (dx div 2)+gap1, dy-(bit.canvas.textheight('X=')));
  bit.endupdate(false);
  //---
  distancese.enabled := true;
end;

end.

