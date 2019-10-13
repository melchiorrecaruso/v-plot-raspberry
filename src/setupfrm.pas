{
  Description: vPlot setup form.

  Copyright (C) 2017-2019 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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


unit setupfrm;

{$mode objfpc}

interface

uses
  classes, sysutils, forms, controls, graphics, extctrls, buttons,
  spin, stdctrls, grids, comctrls;

type

  { tsetupform }

  tsetupform = class(tform)
    clearbtn: TBitBtn;
    calculatebtn: tbitbtn;
    memo: TStringGrid;
    resultlb: TLabel;
    pagelb: tlabel;
    widthse: TFloatSpinEdit;
    heigthse: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    moto0lb: tlabel;
    moto0lb1: tlabel;
    homelb: tlabel;
    pnt1xlb: tlabel;
    pnt0ylb: tlabel;
    pnt0xlb: tlabel;
    pnt1ylb: tlabel;
    pnt0xse: tfloatspinedit;
    pnt1xse: tfloatspinedit;
    pnt0yse: tfloatspinedit;
    pnt1yse: tfloatspinedit;
    radius0lb: tlabel;
    radius1lb: tlabel;
    radius0se: tfloatspinedit;
    homexse: tfloatspinedit;
    homeyse: tfloatspinedit;
    homexlb: tlabel;
    homeylb: tlabel;
    radius1se: tfloatspinedit;
    ratio0lb: tlabel;
    ratio1lb: tlabel;
    ratio0se: tfloatspinedit;
    ratio1se: tfloatspinedit;
    procedure calculatebtnclick(sender: tobject);
    procedure clearbtnclick(sender: tobject);
    procedure clearmemo(Sender: TObject);
  private
  public
  end;

var
  setupform: tsetupform;

implementation

{$R *.lfm}

uses
  vpdriverthread, vpmath, vpsetting;

{ tsetupform }

procedure tsetupform.clearbtnclick(sender: tobject);
begin
  pnt0xse.value   := 0;
  pnt0yse.value   := 0;
  radius0se.value := 0;
  ratio0se.value  := 0;

  pnt1xse.value   := 0;
  pnt1yse.value   := 0;
  radius1se.value := 0;
  ratio1se.value  := 0;

  homexse.value   := 0;
  homeyse.value   := 0;

  clearmemo(sender);
end;

procedure tsetupform.clearmemo(sender: tobject);
begin
  memo.clean;
end;

procedure tsetupform.calculatebtnclick(sender: tobject);
const
  range    = 1500;
  rangeinc = 2;
var
  arrp:  array [0.. 9] of tvppoint;
  arrpr: array [1 ..9] of tvppoint;
  c:  tvppoint;
  t0: tvppoint;
  t1: tvppoint;
  p, pr:  tvppoint;

  l0,  li0: vpfloat;
  l1,  li1: vpfloat;
  lr0, lri0: vpfloat;
  lr1, lri1: vpfloat;
  dl0,  dl1: vpfloat;

     i: longint;
  x, y: longint;
begin
  // real machine setting
  t0.x := setting.point0.x + pnt0xse.value;
  t0.y := setting.point0.y + pnt0yse.value;
  t1.x := setting.point1.x + pnt1xse.value;
  t1.y := setting.point1.y + pnt1yse.value;

  // calc center point
  c.x := setting.point8.x;
  c.y := setting.point8.y+
         setting.yfactor*(height)+
         setting.yoffset;

  // init array of points
  arrp[0]   := setting.point8;
  arrp[1].x := c.x - widthse.value/2;
  arrp[1].y := c.y +heigthse.value/2;
  arrp[2].x := c.x;
  arrp[2].y := c.y +heigthse.value/2;
  arrp[3].x := c.x + widthse.value/2;
  arrp[3].y := c.y +heigthse.value/2;
  arrp[4].x := c.x - widthse.value/2;
  arrp[4].y := c.y;
  arrp[5].x := c.x;
  arrp[5].y := c.y;
  arrp[6].x := c.x + widthse.value/2;
  arrp[6].y := c.y;
  arrp[7].x := c.x - widthse.value/2;
  arrp[7].y := c.y -heigthse.value/2;
  arrp[8].x := c.x;
  arrp[8].y := c.y -heigthse.value/2;
  arrp[9].x := c.x + widthse.value/2;
  arrp[9].y := c.y -heigthse.value/2;

  // init array of result
  arrpr[1].x := -1; arrpr[1].y := -1;
  arrpr[2].x := -1; arrpr[2].y := -1;
  arrpr[3].x := -1; arrpr[3].y := -1;
  arrpr[4].x := -1; arrpr[4].y := -1;
  arrpr[5].x := -1; arrpr[5].y := -1;
  arrpr[6].x := -1; arrpr[6].y := -1;
  arrpr[7].x := -1; arrpr[7].y := -1;
  arrpr[8].x := -1; arrpr[8].y := -1;
  arrpr[9].x := -1; arrpr[9].y := -1;

  // run ...
  for i := 1 to 9 do
  begin
    // ideal machine
    calc_(arrp[0], l0,   l1);
    calc_(arrp[i], li0, li1);

    // real machine
    p.x := arrp[0].x + homexse.value;
    p.y := arrp[0].y + homeyse.value;
    lr0 := calc_l0(p, t0, setting.m0radius + radius0se.value);
    lr1 := calc_l1(p, t1, setting.m1radius + radius1se.value);
    lri0 := lr0 + (li0-l0)/setting.m0ratio*(setting.m0ratio+ratio0se.value);
    lri1 := lr1 + (li1-l1)/setting.m1ratio*(setting.m1ratio+ratio1se.value);

    y := -range;
    while y <= range do
    begin
      x  := -range;
      while x <= range do
      begin
        pr.x := arrp[i].x + x/100;
        pr.y := arrp[i].y + y/100;

        dl0 := abs(calc_l0(pr, t0, setting.m0radius + radius0se.value) - lri0);
        dl1 := abs(calc_l1(pr, t1, setting.m1radius + radius1se.value) - lri1);

        if (dl0 < 0.05) and (dl1 < 0.05) then
        begin
          arrpr[i] := pr;
        end;
        inc(x, rangeinc);
      end;
      inc(y, rangeinc);
    end;
  end;

  p := arrpr[5];
  for i := 1 to 9 do
  begin
    arrpr[i].x := arrpr[i].x - (p.x -arrp[5].x) + arrp[i].x - c.x;
    arrpr[i].y := arrpr[i].y - (p.y -arrp[5].y) + arrp[i].y - c.y;
  end;
  memo.cells[0, 0] := format('(X%3.1f, Y%3.1f)', [arrpr[1].x-arrp[1].x, arrpr[1].y-arrp[1].y]);
  memo.cells[1, 0] := format('(X%3.1f, Y%3.1f)', [arrpr[2].x-arrp[2].x, arrpr[2].y-arrp[2].y]);
  memo.cells[2, 0] := format('(X%3.1f, Y%3.1f)', [arrpr[3].x-arrp[3].x, arrpr[3].y-arrp[3].y]);

  memo.cells[0, 1] := format('(X%3.1f, Y%3.1f)', [arrpr[4].x-arrp[4].x, arrpr[4].y-arrp[4].y]);
  memo.cells[1, 1] := format('(X%3.1f, Y%3.1f)', [arrpr[5].x-arrp[5].x, arrpr[5].y-arrp[5].y]);
  memo.cells[2, 1] := format('(X%3.1f, Y%3.1f)', [arrpr[6].x-arrp[6].x, arrpr[6].y-arrp[6].y]);

  memo.cells[0, 2] := format('(X%3.1f, Y%3.1f)', [arrpr[7].x-arrp[7].x, arrpr[7].y-arrp[7].y]);
  memo.cells[1, 2] := format('(X%3.1f, Y%3.1f)', [arrpr[8].x-arrp[8].x, arrpr[8].y-arrp[8].y]);
  memo.cells[2, 2] := format('(X%3.1f, Y%3.1f)', [arrpr[9].x-arrp[9].x, arrpr[9].y-arrp[9].y]);
end;

end.

