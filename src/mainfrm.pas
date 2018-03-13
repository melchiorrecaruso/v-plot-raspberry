{
  Description: vPlot Main Form.

  Copyright (C) 2014-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, spin, vpcommon, vpcoder, vplayout, vpdriver,
  fpvectorial;

type
  { tmainform }

  tmainform = class(tform)
    aboutbtn: tbitbtn;
    aboutgb: tgroupbox;
    image: TImage;
    leftedit: tspinedit;
    rightedit: tspinedit;
    verticalcb: tcheckbox;
    redrawbtn: tbitbtn;
    formatcb: tcombobox;
    papersizegb: tgroupbox;
    label1: tlabel;
    label2: tlabel;
    heightl: tlabel;
    widthl: tlabel;
    formatl: tlabel;
    loadbtn: tbitbtn;
    offsetyse: tspinedit;
    offsetxse: tspinedit;
    heightse: tspinedit;
    widthse: tspinedit;
    startbtn: tbitbtn;
    stopbtn: tbitbtn;
    creativecontrolgb: tgroupbox;
    sethomebtn: tbitbtn;
    bordersbtn: tbitbtn;
    leftdownbtn: tbitbtn;
    rightdownbtn: tbitbtn;
    penupbtn: tbitbtn;
    pendownbtn: tbitbtn;
    leftupbtn: tbitbtn;
    rightupbtn: tbitbtn;
    gohomebtn: tbitbtn;
    manualdrivinggb: tgroupbox;
    drawingcontrolgb: tgroupbox;
    opendialog: topendialog;

    procedure bordersbtnclick(sender: tobject);
    procedure creativecontrolgbclick(sender: tobject);

    procedure formatcbchange(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure gohomebtnclick(sender: tobject);
    procedure imagemousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure imagemousemove(sender: tobject; shift: tshiftstate; x, y: integer
      );
    procedure imagemouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure leftdownbtnclick(sender: tobject);

    procedure leftupbtnclick(sender: tobject);
    procedure pendownbtnclick(sender: tobject);
    procedure penupbtnclick(sender: tobject);
    procedure reloadbtnclick(sender: tobject);
    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick(sender: tobject);
    procedure sethomebtnclick(sender: tobject);

    procedure loadbtnclick(sender: tobject);
    procedure playorstopbtnclick(sender: tobject);

    procedure verticalcbeditingdone(sender: tobject);
  private
   bitmap:    tbitmap;
    paths:    tvppaths;
      vec:    tvvectorialdocument;

   progress:  longint;
 mouseisdown: boolean;
   px: longint;
   py: longint;
    procedure onplotterstart;
    procedure onplotterstop;
    procedure onplottertick;
  end;


var
  mainform: Tmainform;


implementation

{$R *.lfm}

uses
  math, sysutils, dxfvectorialreader;

var
  layout:  tvplayout;
  plotter: tvplotter = nil;
  driver:  tvpdriver = nil;

// form events //

procedure tmainform.formcreate(sender: tobject);
begin
  manualdrivinggb  .enabled := true;
  creativecontrolgb.enabled := true;
  papersizegb      .enabled := false;
  drawingcontrolgb .enabled := false;
  // ---
  loadlayout(layout, changefileext(paramstr(0), '.ini'));
  driver    := tvpdriver.create(layout.mode);
  vec       := tvvectorialdocument.create;
  paths     := createvppaths(vec);
  bitmap    := tbitmap.create;
  // ---
  formatcbchange (nil);
  sethomebtnclick(nil);
  reloadbtnclick (nil);
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  gohomebtnclick(nil);
  // ---
  vec.destroy;
  paths.destroy;
  driver.destroy;
  bitmap.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(plotter) then
  begin
    closeaction := canone;
    plotter.plot := true;
    plotter.terminate;
  end else
    closeaction := cafree;
end;

// manual driving //

procedure tmainform.leftupbtnclick(Sender: TObject);
begin
  driver.enabled := true;
  driver.move4(-leftedit.value, 0);
end;

procedure tmainform.leftdownbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.move4(+leftedit.value, 0);
end;

procedure tmainform.pendownbtnclick(Sender: TObject);
begin
  driver.enabled := true;
  driver.pen := true;
end;

procedure tmainform.penupbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.pen := false;
end;

procedure tmainform.rightupbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.move4(0, -rightedit.value);
end;

procedure tmainform.rightdownbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.move4(0, +rightedit.value);
end;

procedure tmainform.sethomebtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.bordersbtnclick(sender: tobject);
begin




end;

procedure tmainform.creativecontrolgbclick(sender: TObject);
begin

end;

procedure tmainform.gohomebtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  driver.enabled := true;
  optimize(layout.p09, layout, m0, m1);
  driver.move2(m0, m1);
end;

procedure tmainform.imagemousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  if button = mbleft then
  begin
    mouseisdown := true;
    px := x;
    py := y;
  end;
end;

procedure tmainform.imagemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
var
  nleft: longint;
   ntop: longint;
begin
  if mouseisdown then
  begin
    nleft := image.left + (x - px);
    ntop  := image.top  + (y - py);

    image.left := nleft;
    image.top  := ntop;
  end;
end;

procedure tmainform.imagemouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  mouseisdown := false;
end;

// creativecontrol groupbox

procedure tmainform.loadbtnclick(sender: tobject);
begin
  opendialog.filter := 'DXF Files (*.dxf)|*.dxf';
  if opendialog.execute then
  begin
    manualdrivinggb  .enabled := false;
    creativecontrolgb.enabled := false;
    papersizegb      .enabled := false;
    drawingcontrolgb .enabled := false;

    freeandnil(paths);
    freeandnil(vec);
    vec := tvvectorialdocument.create;
    try
      vec.readfromfile(opendialog.filename,
        vec.getformatfromextension(opendialog.filename));
    finally
    end;
    paths := createvppaths(vec);

    manualdrivinggb  .enabled := true;
    creativecontrolgb.enabled := true;
    papersizegb      .enabled := true;
    drawingcontrolgb .enabled := true;

    formatcbchange(nil);
  end;
end;

procedure tmainform.reloadbtnclick(sender: tobject);
begin
  verticalcbeditingdone(sender);
  // ---
  driver.enabled := sender = startbtn;
  if sender <> nil then
  begin
    plotter         := tvplotter.create(paths);
    plotter.onstart := @onplotterstart;
    plotter.onstop  := @onplotterstop;
    plotter.ontick  := @onplottertick;
    plotter.start;
  end;
end;

procedure tmainform.formatcbchange(sender: tobject);
begin
  verticalcb.enabled := true;
  heightse  .enabled := false;
  widthse   .enabled := false;
  case formatcb.itemindex of
    0: begin heightse.value := 841; widthse .value := 1189; end;
    1: begin heightse.value := 594; widthse .value :=  841; end;
    2: begin heightse.value := 420; widthse .value :=  594; end;
    3: begin heightse.value := 297; widthse .value :=  420; end;
    4: begin heightse.value := 210; widthse .value :=  297; end;
    5: begin heightse.value := 148; widthse .value :=  210; end;
  else begin
         verticalcb.enabled := false;
         heightse  .enabled := true;
         widthse   .enabled := true;
       end;
  end;

  if verticalcb.enabled then
    verticalcbeditingdone(formatcb);
end;

procedure tmainform.verticalcbeditingdone(sender: tobject);
var
  amin, amax: longint;
begin
  amin := min(heightse.value, widthse.value);
  amax := max(heightse.value, widthse.value);
  if verticalcb.checked then
  begin
    heightse.value := amax;
    widthse .value := amin;
  end else
  begin
    heightse.value := amin;
    widthse .value := amax;
  end;
  // ---
  bitmap.canvas.pen.color   := clltgray;
  bitmap.canvas.brush.color := clltgray;
  bitmap.canvas.brush.style := bssolid;
  bitmap.setsize(
     widthse.value,
    heightse.value);
  bitmap.canvas.fillrect(0, 0,
     widthse.value,
    heightse.value);
  // ---
  image.canvas.pen.color   := clltgray;
  image.canvas.brush.color := clltgray;
  image.canvas.brush.style := bssolid;
  image.picture.bitmap.setsize(
     widthse.value,
    heightse.value);
  image.canvas.fillrect(0, 0,
     widthse.value,
    heightse.value);

  image.center            := true;
  image.proportional      := true;
  image.stretchinenabled  := false;
  image.stretchoutenabled := false;
  image.stretch           := false;
end;

procedure tmainform.playorstopbtnclick(sender: tobject);
begin
  if assigned(plotter) then
  begin
    if sender = stopbtn then
    begin
      plotter.plot := true;
      plotter.terminate;
    end else
    if sender = startbtn then
    begin
      plotter.plot := not plotter.plot;
      if plotter.plot then
        startbtn.caption := 'Pause'
      else
        startbtn.caption := 'Play';
    end;

  end else
  begin
    if sender = startbtn then
    begin
      reloadbtnclick(startbtn);
    end;
  end;
end;

procedure tmainform.onplotterstart;
begin
  startbtn.caption          := 'Pause';
  manualdrivinggb  .enabled := false;
  creativecontrolgb.enabled := false;
  papersizegb      .enabled := false;
  drawingcontrolgb .enabled := true;
  application.processmessages;

  progress := 0;
end;

procedure tmainform.onplotterstop;
begin
  caption                   := 'VPlot Driver';
  startbtn.caption          := 'Play';
  manualdrivinggb  .enabled := true;
  creativecontrolgb.enabled := true;
  papersizegb      .enabled := true;
  drawingcontrolgb .enabled := true;
  image.canvas.draw(0,0, bitmap);
  application.processmessages;

  plotter := nil;
end;

procedure tmainform.onplottertick;
var
   p: tvppoint;
  m0: longint;
  m1: longint;
begin
  if (abs(plotter.px) > ( widthse.value div 2)) then exit;
  if (abs(plotter.py) > (heightse.value div 2)) then exit;

  p.x := (widthse .value div 2) + offsetxse.value + (plotter.px);
  p.y := (heightse.value div 2) - offsetyse.value - (plotter.py);
  bitmap.canvas.pixels[trunc(p.x), trunc(p.y)] := clblack;

  if driver.enabled then
  begin
    p.x := layout.p08.x + offsetxse.value + (plotter.px);
    p.y := layout.p08.y + offsetyse.value + (plotter.py);
    optimize(p, layout, m0, m1);

    driver.move2(m0, m1);
  end;
  sleep(2);

  inc(progress);
  //if progress mod (25) = 0 then
  begin
    image.canvas.draw(0,0, bitmap);
    progress := 0;
  end;

  caption := inttostr(plotter.progress);
  application.processmessages;
end;



end.

