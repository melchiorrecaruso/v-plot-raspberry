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

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, spin, vpcommon, vpcoder, vpsetting, vpdriver,
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
    MenuItem1: TMenuItem;
    writeleftmi: TMenuItem;
    writerightmi: TMenuItem;
    line7mi: TMenuItem;
    line5mi: TMenuItem;
    layoutmi: TMenuItem;
    moveleftmi: TMenuItem;
    moverightmi: TMenuItem;
    movetopmi: TMenuItem;
    line6mi: TMenuItem;
    writebordersmi: TMenuItem;
    writetopmi: TMenuItem;
    writebottommi: TMenuItem;
    showpagesizepanelmi: TMenuItem;
    showcalibrationpanelmi: TMenuItem;
    timer: TTimer;
    updatemi: TMenuItem;
    clearmi: TMenuItem;
    calibrationmi: TMenuItem;
    movebordersmi: TMenuItem;
    movebottommi: TMenuItem;
    line4mi: TMenuItem;
    movetohomemi: TMenuItem;
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
    procedure layoutmiclick(sender: tobject);

    procedure movebordersmiclick(sender: tobject);
    procedure closemiclick(sender: tobject);
    procedure exitmiclick(sender: tobject);

    procedure formatcbchange(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure gohomebtnclick(sender: tobject);
    procedure heightseeditingdone(sender: tobject);
    procedure imagemousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure imagemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure imagemouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure killmiclick(sender: tobject);
    procedure leftdownbtnclick(sender: tobject);

    procedure leftupbtnclick   (sender: tobject);
    procedure moveleftmiClick(Sender: TObject);
    procedure moverightmiClick(Sender: TObject);
    procedure movetopmiClick(Sender: TObject);
    procedure movebottommiClick(sender: tobject);

    procedure showtoolbarclick(sender: tobject);

    procedure startmiclick(sender: tobject);
    procedure pendownbtnclick  (sender: tobject);
    procedure penupbtnclick    (sender: tobject);

    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick  (sender: tobject);


    procedure openbtnclick(sender: tobject);

    procedure stopmiclick(sender: tobject);
    procedure timertimer(sender: tobject);
    procedure updatemiclick(sender: tobject);

    procedure verticalcbeditingdone(sender: tobject);
    procedure widthseeditingdone(sender: tobject);

  private
      bitmap: tbitmap;
       paths: tvppaths;
         vec: tvvectorialdocument;
     elapsed: longint;

 mouseisdown: boolean;
          px: longint;
          py: longint;

    procedure lock1(value: boolean);
    procedure lock2(value: boolean);

    procedure onplotterstart;
    procedure onplotterstop;
    procedure onplottertick;
  end;


var
  mainform: tmainform;


implementation

{$R *.lfm}

uses
  math, sysutils, dxfvectorialreader, aboutfrm;


// FORM events

procedure tmainform.formcreate(sender: tobject);
var
  m0: longint;
  m1: longint;
  p : tvppoint;
begin
  // load setting
  setting := tvpsetting.create;
  setting.load(changefileext(paramstr(0), '.ini'));
  // create plotter driver
  driver := tvpdriver.create;
  driver.mode   := setting.mode;
  driver.delay0 := setting.delay0;
  driver.delay1 := setting.delay1;
  // create preview, vectorial file and paths
  bitmap := tbitmap.create;
  vec    := tvvectorialdocument.create;
  paths  := createpaths(vec);
  // update preview
  formatcbchange (nil);
  showtoolbarclick(nil);
  // show toolbars
  manualdrivinggb.enabled := true;
  pagesizegb     .enabled := true;
  // init wave
  wave := twave.create(setting.wavexmax,
                       setting.waveymax,
                       setting.wave);
  if enabledebug then
  begin
    p.x := -594.5;   p.y := +420.5; wave.update(p);
    p.x := +0.000;   p.y := +420.5; wave.update(p);
    p.x := +594.5;   p.y := +420.5; wave.update(p);
    p.x := -594.5;   p.y := +0.000; wave.update(p);
    p.x := +0.000;   p.y := +0.000; wave.update(p);
    p.x := +594.5;   p.y := +0.000; wave.update(p);
    p.x := -594.5;   p.y := -420.5; wave.update(p);
    p.x := +0.000;   p.y := -420.5; wave.update(p);
    p.x := +594.5;   p.y := -420.5; wave.update(p);
  end;
  // initialize driver
  optimize(setting.layout09, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  // move to base position
  gohomebtnclick(nil);
  // ---
  wave.destroy;
  driver.destroy;
  setting.destroy;
  paths.destroy;
  vec.destroy;
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

procedure tmainform.leftupbtnclick(sender: tobject);
var
  m0, dm0: longint;
  m1:      longint;
begin
  lock2(false);
  begin
    driver.enabled    := true;
    driver.penoff     := true;
    driver.clockwise0 := false;

    m0 := abs(leftedit.value);
    repeat
      dm0 :=  min(10, m0);
      driver.step(dm0, 0);
      dec (m0, dm0);
    until (m0 =  0);
    optimize(setting.layout09, m0, m1);
    driver.init(m0, m1);
  end;
  lock2(true);
end;

procedure tmainform.leftdownbtnclick(sender: tobject);
var
  m0, dm0: longint;
  m1:      longint;
begin
  lock2(false);
  begin
    driver.enabled    := true;
    driver.penoff     := true;
    driver.clockwise0 := true;

    m0 := abs(leftedit.value);
    repeat
      dm0 :=  min(10, m0);
      driver.step(dm0, 0);
      dec (m0, dm0);
    until (m0 =  0);
    optimize(setting.layout09, m0, m1);
    driver.init(m0, m1);
  end;
  lock2(true);
end;

procedure tmainform.rightupbtnclick(sender: tobject);
var
  m0:      longint;
  m1, dm1: longint;
begin
  lock2(false);
  begin
    driver.enabled    := true;
    driver.penoff     := true;
    driver.clockwise1 := true;

    m1 := abs(rightedit.value);
    repeat
      dm1 :=  min(10, m1);
      driver.step(0, dm1);
      dec (m1, dm1);
    until (m1 =  0);
    optimize(setting.layout09, m0, m1);
    driver.init(m0, m1);
  end;
  lock2(true);
end;

procedure tmainform.rightdownbtnclick(sender: tobject);
var
  m0:      longint;
  m1, dm1: longint;
begin
  lock2(false);
  begin
    driver.enabled    := true;
    driver.penoff     := true;
    driver.clockwise1 := false;

    m1 := abs(rightedit.value);
    repeat
      dm1 :=  min(10, m1);
      driver.step(0, dm1);
      dec (m1, dm1);
    until (m1 =  0);
    optimize(setting.layout09, m0, m1);
    driver.init(m0, m1);
  end;
  lock2(true);
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

procedure tmainform.movebordersmiclick(sender: tobject);
var
  p0:   tvppoint;
  p1:   tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // from middle bottom to left-bottom
  p0.x := + 0;
  p0.y := - (heightse.value / 2);
  p1.x := - (widthse .value / 2);
  p1.y := - (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));
  // form left-bottom to left-top
  p0   := p1;
  p1.x := - (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));
  // form left-top to right-top
  p0   := p1;
  p1.x := + (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));
  // from right-top to right-bottom
  p0   := p1;
  p1.x := + (widthse .value / 2);
  p1.y := - (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));
  // from right bottom to middle-bottom
  p0   := p1;
  p1.x := + 0;
  p1.y := - (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := sender = movebordersmi;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := true;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.movetopmiclick(Sender: TObject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // form left-top to right-top
  p0.x := - (widthse .value / 2);
  p0.y := + (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := sender = movetopmi;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := true;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.movebottommiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // form left-bottom to right-bottom
  p0.x := - (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := - (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := sender = movebottommi;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := true;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.moveleftmiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // form left-bottom to left-top
  p0.x := - (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := - (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := sender = moveleftmi;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := true;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.moverightmiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // form right-bottom to right-top
  p0.x := + (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := sender = moverightmi;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := true;
  plotter.start;
  elapsed := 1;
end;

procedure tmainform.layoutmiclick(Sender: TObject);
var
  m0: longint = 0;
  m1: longint = 0;
begin
  if assigned(plotter) then exit;

  gohomebtnclick(nil);
  // load configuration
  setting.clear;
  setting.load(changefileext(paramstr(0), '.ini'));
  // update plotter driver
  driver.mode   := setting.mode;
  driver.delay0 := setting.delay0;
  driver.delay1 := setting.delay1;

  optimize(setting.layout09, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.gohomebtnclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  if assigned(plotter) then exit;

  paths.clear;
  // form x-x to base
  p0.x := setting.layout09.x - setting.layout08.x;
  p0.y := setting.layout09.y - setting.layout08.y - (heightse.value / 2);
  p1   := p0;
  paths.add(interpolate_line(p0, p1));

  driver.enabled  := true;
  driver.penoff   := true;
  plotter         := tvplotter.create(paths);
  plotter.midx    := setting.layout08.x;
  plotter.midy    := setting.layout08.y + (heightse.value / 2);
  plotter.maxdx   := bitmap.width  div 2;
  plotter.maxdy   := bitmap.height div 2;
  plotter.offsetx := offsetxse.value;
  plotter.offsety := offsetyse.value;
  plotter.onstart := @onplotterstart;
  plotter.onstop  := @onplotterstop;
  plotter.ontick  := @onplottertick;
  plotter.check   := false;
  plotter.start;
  elapsed := 1;
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
    lock2(false);
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
    paths := createpaths(vec);
    lock2(true);
  end;
end;

procedure tmainform.closemiclick(sender: tobject);
begin
  if assigned(plotter) then exit;

  lock2(false);
  freeandnil(paths);
  freeandnil(vec);
  vec   := tvvectorialdocument.create;
  paths := createpaths(vec);
  clearallmiclick(sender);
  lock2(true);
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
  updatemi.checked := not updatemi.checked;
end;

procedure tmainform.clearallmiclick(sender: tobject);
begin
  if assigned(plotter) then exit;
  // ---
  caption := 'vPlotter 2.0';
  // ---
  bitmap.canvas.pen  .color := clltgray;
  bitmap.canvas.brush.color := clltgray;
  bitmap.canvas.brush.style := bssolid;
  bitmap.setsize(
     widthse.value + 2,
    heightse.value + 2);
  bitmap.canvas.fillrect(0, 0,
     widthse.value + 2,
    heightse.value + 2);
  // ---
  image.canvas.pen  .color := clltgray;
  image.canvas.brush.color := clltgray;
  image.canvas.brush.style := bssolid;
  image.picture.bitmap.setsize(
     widthse.value + 2,
    heightse.value + 2);
  image.canvas.fillrect(0, 0,
     widthse.value + 2,
    heightse.value + 2);
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
    driver.enabled  := not updatemi.checked;
    driver.penoff   := false;
    driver.pen      := false;
    plotter         := tvplotter.create(paths);
    plotter.midx    := setting.layout08.x;
    plotter.midy    := setting.layout08.y + (heightse.value / 2);
    plotter.maxdx   := bitmap.width  div 2;
    plotter.maxdy   := bitmap.height div 2;
    plotter.offsetx := offsetxse.value;
    plotter.offsety := offsetyse.value;
    plotter.onstart := @onplotterstart;
    plotter.onstop  := @onplotterstop;
    plotter.ontick  := @onplottertick;
    plotter.check   := true;
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
    driver.pen    := false;
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

  if (heightse.value > (2*setting.waveymax)) or
     (widthse .value > (2*setting.wavexmax)) then
  begin
    messagedlg('vPlotter Error', 'Selected page size is bigger than work area !', mterror, [mbok], 0);

    formatcb.itemindex := formatcb.items.count - 2;
    formatcbchange (nil);
  end;
  clearallmiclick(sender);
end;

// PLOTTER THREAD methods

procedure tmainform.lock1(value: boolean);
begin
  // main menu
  openmi        .enabled := value;
  closemi       .enabled := value;
  exitmi        .enabled := value;
  updatemi      .enabled := value;
  clearmi       .enabled := value;
  movebordersmi .enabled := value;
  movebottommi  .enabled := value;
  movetopmi     .enabled := value;
  moveleftmi    .enabled := value;
  moverightmi   .enabled := value;
  movetohomemi  .enabled := value;
  writebordersmi.enabled := value;
  writebottommi .enabled := value;
  writetopmi    .enabled := value;
  writeleftmi   .enabled := value;
  writerightmi  .enabled := value;
  layoutmi      .enabled := value;
  // calibration
  leftupbtn     .enabled := value;
  leftdownbtn   .enabled := value;
  leftedit      .enabled := value;
  rightupbtn    .enabled := value;
  rightdownbtn  .enabled := value;
  rightedit     .enabled := value;
  penupbtn      .enabled := value;
  pendownbtn    .enabled := value;
  // page format
  formatcb      .enabled := value;
  if formatcb.itemindex > 5 then
  begin
    heightse    .enabled := value;
    widthse     .enabled := value;
  end;
  offsetxse     .enabled := value;
  offsetyse     .enabled := value;
  verticalcb    .enabled := value;
end;

procedure tmainform.lock2(value: boolean);
begin
  // main menu
  openmi        .enabled := value;
  closemi       .enabled := value;
  exitmi        .enabled := value;
  updatemi      .enabled := value;
  clearmi       .enabled := value;
  startmi       .enabled := value;
  stopmi        .enabled := value;
  killmi        .enabled := value;
  movebordersmi .enabled := value;
  movebottommi  .enabled := value;
  movetopmi     .enabled := value;
  moveleftmi    .enabled := value;
  moverightmi   .enabled := value;
  movetohomemi  .enabled := value;
  writebordersmi.enabled := value;
  writebottommi .enabled := value;
  writetopmi    .enabled := value;
  writeleftmi   .enabled := value;
  writerightmi  .enabled := value;
  layoutmi      .enabled := value;
  // calibration
  leftupbtn     .enabled := value;
  leftdownbtn   .enabled := value;
  leftedit      .enabled := value;
  rightupbtn    .enabled := value;
  rightdownbtn  .enabled := value;
  rightedit     .enabled := value;
  penupbtn      .enabled := value;
  pendownbtn    .enabled := value;
  // page format
  formatcb      .enabled := value;
  if formatcb.itemindex > 5 then
  begin
    heightse    .enabled := value;
    widthse     .enabled := value;
  end;
  offsetxse     .enabled := value;
  offsetyse     .enabled := value;
  verticalcb    .enabled := value;
end;

procedure tmainform.onplotterstart;
begin
  lock1(false);
  timer.enabled := true;
  application.processmessages;
end;

procedure tmainform.onplotterstop;
begin
  timer.enabled := false;
  penupbtnclick(nil);
  plotter := nil;
  lock1(true);

  image.canvas.draw(0, 0, bitmap);
  caption := format('Elapsed %u sec', [elapsed]);
  application.processmessages;
end;

procedure tmainform.onplottertick;
begin
  if enabledebug then
  begin
    writeln(format('    TICK::P.X  = %12.5f', [plotter.point.x]));
    writeln(format('    TICK::P.Y  = %12.5f', [plotter.point.y]));
  end;
  // update preview
  bitmap.canvas.pixels[
    trunc(( widthse.value div 2) + plotter.point.x),
    trunc((heightse.value div 2) - plotter.point.y)] := clblack;
  // update progress bar
  if plotter.index mod $FFF = 0 then
  begin
    caption := format('Elapsed %u sec - Remaing %u sec', [elapsed,
      (elapsed * (plotter.count - plotter.index)) div plotter.index]);
    image.canvas.draw(0, 0, bitmap);
  end;
  application.processmessages;
end;

// TIMER events

procedure tmainform.timertimer(sender: tobject);
begin
  inc(elapsed);
end;

end.

