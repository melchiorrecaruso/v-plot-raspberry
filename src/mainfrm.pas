{
  Description: vPlot main form.

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

unit mainfrm;

{$mode objfpc}
{$i include.inc}

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, spin, vpcommon, vpcoder, vplayout, vpdriver,
  fpvectorial;

type
  { tmainform }

  tmainform = class(tform)
    image: timage;
    leftedit: tspinedit;
    mainmenu: tmainmenu;
    filemi: tmenuitem;
    line2mi: tmenuitem;
    killmi: tmenuitem;
    pagemi: tmenuitem;
    showpagesizepanelmi: TMenuItem;
    timer: TTimer;
    updatemi: TMenuItem;
    clearmi: TMenuItem;
    calibrationmi: TMenuItem;
    movebordersmi: TMenuItem;
    movebottonmi: TMenuItem;
    line4mi: TMenuItem;
    movetohomemi: TMenuItem;
    line3mi: TMenuItem;
    showcalibrationpanelmi: TMenuItem;
    line5mi: TMenuItem;
    skipsmallmi: tmenuitem;
    zerocentermi: tmenuitem;
    startmi: tmenuitem;
    stopmi: tmenuitem;
    infomi: tmenuitem;
    aboutmi: tmenuitem;
    openmi: tmenuitem;
    closemi: tmenuitem;
    line1mi: tmenuitem;
    exitmi: tmenuitem;
    plotmi: tmenuitem;
    rightedit: tspinedit;
    verticalcb: tcheckbox;
    formatcb: tcombobox;
    pagesizegb: tgroupbox;
    label1: tlabel;
    label2: tlabel;
    heightl: tlabel;
    widthl: tlabel;
    formatl: tlabel;
    offsetyse: tspinedit;
    offsetxse: tspinedit;
    heightse: tspinedit;
    widthse: tspinedit;
    leftdownbtn: tbitbtn;
    rightdownbtn: tbitbtn;
    penupbtn: tbitbtn;
    pendownbtn: tbitbtn;
    leftupbtn: tbitbtn;
    rightupbtn: tbitbtn;
    manualdrivinggb: tgroupbox;
    opendialog: topendialog;

    procedure aboutmiclick(sender: tobject);
    procedure clearallmiclick(sender: tobject);
    procedure bordersbtnclick(sender: tobject);
    procedure closemiclick(sender: tobject);
    procedure exitmiclick(sender: tobject);

    procedure formatcbchange(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure gohomebtnclick(sender: tobject);
    procedure heightseEditingDone(Sender: TObject);
    procedure imagemousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure imagemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure imagemouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure killmiclick(sender: tobject);
    procedure leftdownbtnclick(sender: tobject);

    procedure leftupbtnclick   (sender: tobject);
    procedure movebottonmiClick(Sender: TObject);

    procedure showtoolbarclick(sender: tobject);
    procedure skipsmallmiclick(sender: tobject);
    procedure startmiclick(sender: tobject);
    procedure pendownbtnclick  (sender: tobject);
    procedure penupbtnclick    (sender: tobject);

    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick  (sender: tobject);


    procedure openbtnclick(sender: tobject);

    procedure stopmiclick(sender: tobject);
    procedure timerTimer(Sender: TObject);
    procedure updatemiclick(sender: tobject);

    procedure verticalcbeditingdone(sender: tobject);
    procedure widthseEditingDone(Sender: TObject);
    procedure zerocentermiclick(sender: tobject);
  private
      bitmap: tbitmap;
       paths: tvppaths;
         vec: tvvectorialdocument;
     elapsed: longint;

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
  math, sysutils, dxfvectorialreader, aboutfrm;

var
  layout:  tvplayout;
  plotter: tvplotter = nil;
  driver:  tvpdriver = nil;

// FORM events

procedure tmainform.formcreate(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  manualdrivinggb.enabled := true;
  pagesizegb     .enabled := true;
  // ---
  loadlayout(layout, changefileext(paramstr(0), '.ini'));
  driver    := tvpdriver.create(layout.mode);
  vec       := tvvectorialdocument.create;
  paths     := createpaths(vec, zerocentermi.checked,
                                 skipsmallmi.checked);
  bitmap    := tbitmap.create;
  // ---
  showtoolbarclick(nil);
  formatcbchange (nil);
  // set home
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
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
    killmiclick(nil);
  end else
    closeaction := cafree;
end;

// MANUAL DRIVING groupbox

procedure tmainform.leftupbtnclick(Sender: TObject);
var
  m0: longint;
  m1: longint;
begin
  driver.enabled := true;
  driver.penoff  := true;
  driver.move4(-leftedit.value, 0);
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.leftdownbtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  driver.enabled := true;
  driver.penoff  := true;
  driver.move4(+leftedit.value, 0);
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.rightupbtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  driver.enabled := true;
  driver.penoff  := true;
  driver.move4(0, -rightedit.value);
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.rightdownbtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  driver.enabled := true;
  driver.penoff  := true;
  driver.move4(0, +rightedit.value);
  optimize(layout.p09, layout, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.pendownbtnclick(Sender: TObject);
begin
  driver.enabled := true;
  driver.penoff  := false;
  driver.pen     := true;
end;

procedure tmainform.penupbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.penoff  := false;
  driver.pen     := false;
end;

procedure tmainform.bordersbtnclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  formatcb.itemindex := formatcb.items.count - 1;
  heightse.value     := layout.p11.y;
   widthse.value     := layout.p11.x;
  formatcbchange(nil);
  gohomebtnclick(nil);
  paths.clear;
  paths.add(interpolate_line(layout.p09, layout.p13));
  paths.add(interpolate_line(layout.p13, layout.p10));
  paths.add(interpolate_line(layout.p10, layout.p11));
  paths.add(interpolate_line(layout.p11, layout.p12));
  paths.add(interpolate_line(layout.p12, layout.p13));
  paths.add(interpolate_line(layout.p13, layout.p09));
  paths.zerocenter;

  driver.enabled  := true;
  driver.penoff   := false;
  plotter         := tvplotter.create(paths);
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.movebottonmiclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  formatcb.itemindex := formatcb.items.count - 1;
  heightse.value     := layout.p11.y;
   widthse.value     := layout.p11.x;
  formatcbchange(nil);
  gohomebtnclick(nil);
  paths.clear;
  paths.add(interpolate_line(layout.p09, layout.p13));
  paths.add(interpolate_line(layout.p13, layout.p12));
  paths.add(interpolate_line(layout.p12, layout.p09));
  paths.zerocenter;

  driver.enabled  := true;
  driver.penoff   := false;
  plotter         := tvplotter.create(paths);
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.gohomebtnclick(sender: tobject);
var
  m0: longint;
  m1: longint;
begin
  if assigned(plotter) then exit;

  driver.enabled := true;
  driver.penoff  := true;
  optimize(layout.p09, layout, m0, m1);
  driver.move2(m0, m1);
end;

// IMAGE events

procedure tmainform.imagemousedown(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
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

    if nleft                >  width - 150 then nleft :=  width - 150;
    if  ntop                > height - 150 then  ntop := height - 150;
    if nleft + image. width <          150 then nleft :=          150 - image.width;
    if  ntop + image.height <          150 then  ntop :=          150 - image.height;

    image.left := nleft;
    image.top  := ntop;
  end;
end;

procedure tmainform.imagemouseup(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
begin
  mouseisdown := false;
end;

// FILE mainmenu

procedure tmainform.openbtnclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  opendialog.filter := 'dxf files (*.dxf)|*.dxf';
  if opendialog.execute then
  begin
    manualdrivinggb.enabled := false;
    pagesizegb    .enabled := false;

    freeandnil(paths);
    freeandnil(vec);
    vec := tvvectorialdocument.create;
    try
      vec.readfromfile(opendialog.filename,
        vec.getformatfromextension(opendialog.filename));
    except
      freeandnil(vec);
      vec := tvvectorialdocument.create;
    end;
    paths := createpaths(vec, zerocentermi.checked,
                               skipsmallmi.checked);

    manualdrivinggb.enabled := true;
    pagesizegb     .enabled := true;
  end;
end;

procedure tmainform.closemiclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  freeandnil(paths);
  freeandnil(vec);
  vec   := tvvectorialdocument.create;
  paths := createpaths(vec, zerocentermi.checked,
                             skipsmallmi.checked);
end;

procedure tmainform.exitmiclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  close;
end;

// VIEW mainmenu

procedure tmainform.showtoolbarclick(sender: tobject);
begin
  if sender = showcalibrationpanelmi then
  begin
    showcalibrationpanelmi.checked := not showcalibrationpanelmi.checked;
    manualdrivinggb       .visible :=     showcalibrationpanelmi.checked;
  end else
  if sender = showpagesizepanelmi then
  begin
    showpagesizepanelmi.checked := not showpagesizepanelmi.checked;
    pagesizegb         .visible :=     showpagesizepanelmi.checked;
  end;

  if manualdrivinggb.visible then
  begin
    manualdrivinggb.anchors := [aktop, akright];
    manualdrivinggb.top     := 10;
    if pagesizegb.visible then
    begin
      pagesizegb.anchors := [aktop, akright];
      pagesizegb.top     := manualdrivinggb.top    +
                            manualdrivinggb.height + 10;
    end;
  end else
  begin
    if pagesizegb.visible then
    begin
      pagesizegb.anchors := [aktop, akright];
      pagesizegb.top     := 10;
    end;
  end;
end;

// PREVIEW mainmenu

procedure tmainform.updatemiclick(sender: tobject);
begin
  startmiclick(nil);
end;

procedure tmainform.clearallmiclick(sender: tobject);
begin
  bitmap.canvas.pen  .color := clltgray;
  bitmap.canvas.brush.color := clltgray;
  bitmap.canvas.brush.style := bssolid;
  bitmap.setsize(
     widthse.value,
    heightse.value);
  bitmap.canvas.fillrect(0, 0,
     widthse.value,
    heightse.value);
  // ---
  image.canvas.pen  .color := clltgray;
  image.canvas.brush.color := clltgray;
  image.canvas.brush.style := bssolid;
  image.picture.bitmap.setsize(
     widthse.value,
    heightse.value);
  image.canvas.fillrect(0, 0,
     widthse.value,
    heightse.value);
  // ---
  image.align             := alnone;
  image.anchors           := [aktop, akleft, akright, akbottom];
  image.anchors           := [];
  image.center            := true;
  image.proportional      := false;
  image.stretchinenabled  := false;
  image.stretchoutenabled := false;
  image.stretch           := false;
end;

// PLOT mainmenu

procedure tmainform.startmiclick(sender: tobject);
begin
  if assigned(plotter) then
  begin
    plotter.plot  := true;
    timer.enabled := true;
  end else
  begin
    driver.enabled  := (sender = startmi);
    driver.penoff   := false;
    plotter         := tvplotter.create(paths);
    plotter.onstart := @onplotterstart;
    plotter.onstop  := @onplotterstop;
    plotter.ontick  := @onplottertick;
    plotter.start;
    elapsed := 1;
  end;
end;

procedure tmainform.stopmiclick(sender: tobject);
begin
  if assigned(plotter) then
  begin
    plotter.plot  := false;
    timer.enabled := false;
  end;
end;

procedure tmainform.killmiclick(sender: tobject);
begin
  if assigned(plotter) then
  begin
    plotter.plot := true;
    plotter.terminate;
  end;
end;

// OPTION menumenu

procedure tmainform.zerocentermiclick(sender: tobject);
begin
  zerocentermi.checked := not zerocentermi.checked;
  begin
    freeandnil(paths);
    paths := createpaths(vec, zerocentermi.checked,
                               skipsmallmi.checked);
  end;
end;

procedure tmainform.skipsmallmiclick(sender: tobject);
begin
  skipsmallmi.checked := not skipsmallmi.checked;
  begin
    freeandnil(paths);
    paths := createpaths(vec, zerocentermi.checked,
                               skipsmallmi.checked);
  end;
end;

// INFO mainmenu

procedure tmainform.aboutmiclick(sender: tobject);
var
  about: taboutform;
begin
  about := taboutform.create(nil);
  about.showmodal;
  freeandnil(about);
end;

// PAGE SIZE groupbox

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
  verticalcbeditingdone(formatcb);
end;

procedure tmainform.heightseeditingdone(sender: tobject);
begin
  formatcbchange(nil);
end;

procedure tmainform.widthseeditingdone(sender: tobject);
begin
  formatcbchange(nil);
end;

procedure tmainform.verticalcbeditingdone(sender: tobject);
var
  amin: longint;
  amax: longint;
begin
  if verticalcb.enabled then
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
  end;
  clearallmiclick(sender);
end;

// PLOTTER THREAD methods

procedure tmainform.onplotterstart;
begin
  openmi  .enabled := false;
  closemi .enabled := false;
  exitmi  .enabled := false;
  updatemi.enabled := false;
  clearmi .enabled := false;

  manualdrivinggb.enabled := false;
  pagesizegb     .enabled := false;
  timer          .enabled := true;
  application.processmessages;
end;

procedure tmainform.onplotterstop;
begin
  image.canvas.draw(0,0, bitmap);
  penupbtnclick(nil);
  plotter := nil;
  // ---
  openmi  .enabled := true;
  closemi .enabled := true;
  exitmi  .enabled := true;
  updatemi.enabled := true;
  clearmi .enabled := true;

  manualdrivinggb.enabled := true;
  pagesizegb     .enabled := true;
  timer          .enabled := false;
  application.processmessages;
end;

procedure tmainform.onplottertick;
var
         m0: longint;
         m1: longint;
          p: tvppoint;
  remaining: longint;
begin
  p.x := offsetxse.value + plotter.px;
  p.y := offsetyse.value + plotter.py;

  if abs(p.x) > ( widthse.value div 2) then exit;
  if abs(p.y) > (heightse.value div 2) then exit;

  bitmap.canvas.pixels[
    trunc(( widthse.value div 2) + offsetxse.value + plotter.px),
    trunc((heightse.value div 2) - offsetyse.value - plotter.py)] := clblack;

  if driver.enabled then
  begin
    p.x := layout.p08.x + p.x;
    p.y := layout.p08.y + p.y;
    optimize(p, layout, m0, m1);
    driver.move2(m0, m1);
  end;

  if plotter.index mod 24 = 0 then
  begin
    image.canvas.draw(0, 0, bitmap);
  end;

  if plotter.index mod $FF = 0 then
  begin
    remaining := (elapsed * (plotter.count - plotter.index)) div plotter.index;
    caption := format('Elapsed %u sec - Remaing %u sec', [elapsed, remaining]);
  end;
  application.processmessages;
end;

// TIMER events

procedure tmainform.timertimer(sender: tobject);
begin
  inc(elapsed);
end;

end.

