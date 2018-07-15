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
    reloadmi: TMenuItem;
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
    loadmi: tmenuitem;
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
    procedure reloadmiClick(Sender: TObject);

    procedure showtoolbarclick(sender: tobject);

    procedure startmiclick(sender: tobject);
    procedure pendownbtnclick  (sender: tobject);
    procedure penupbtnclick    (sender: tobject);

    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick  (sender: tobject);


    procedure openbtnclick(sender: tobject);

    procedure stopmiclick(sender: tobject);
    procedure timertimer(sender: tobject);

    procedure verticalcbeditingdone(sender: tobject);
    procedure widthseeditingdone(sender: tobject);

  private
      bitmap: tbitmap;
       paths: tvppaths;

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
  math, sysutils, dxfvectorialreader, aboutfrm, vpmath, vpwave;


// FORM events

procedure tmainform.formcreate(sender: tobject);
var
  m0: longint = 0;
  m1: longint = 0;
   p: tvppoint;
begin
  // load setting
  setting := tvpsetting.create;
  setting.load(changefileext(paramstr(0), '.ini'));
  // create plotter driver
  driver := tvpdriver.create;
  driver.mode   := setting.mode;
  driver.delaym := setting.delaym;
  driver.delayz := setting.delayz;
  // create preview and empty paths
  bitmap := tbitmap.create;
  paths  := tvppaths.create;
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
  wave.test;
  // initialize driver
  optimize_point(setting.layout09, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  gohomebtnclick(nil);
  wave.destroy;
  paths.destroy;
  bitmap.destroy;
  driver.destroy;
  setting.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(driverthread) then
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
    driver.enabled:= true;
    driver.penoff := true;

    m0 := abs(leftedit.value);
    repeat
      dm0 :=  min(10, m0);
      driver.step(dm0, 0);
      dec (m0, dm0);
    until (m0 =  0);
    optimize_point(setting.layout09, m0, m1);
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

    m0 := abs(leftedit.value);
    repeat
      dm0 :=  min(10, m0);
      driver.step(dm0, 0);
      dec (m0, dm0);
    until (m0 =  0);
    optimize_point(setting.layout09, m0, m1);
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

    m1 := abs(rightedit.value);
    repeat
      dm1 :=  min(10, m1);
      driver.step(0, dm1);
      dec (m1, dm1);
    until (m1 =  0);
    optimize_point(setting.layout09, m0, m1);
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

    m1 := abs(rightedit.value);
    repeat
      dm1 :=  min(10, m1);
      driver.step(0, dm1);
      dec (m1, dm1);
    until (m1 =  0);
    optimize_point(setting.layout09, m0, m1);
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
  p0: tvppoint;
  p1: tvppoint;
begin
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

  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  driver.enabled       := true;
  driver.penoff        := sender = movebordersmi;

  driverthread         := tvpdriverthread.create(paths);
  driverthread.onstart := @onplotterstart;
  driverthread.onstop  := @onplotterstop;
  driverthread.ontick  := @onplottertick;
  driverthread.start;
  elapsed := 1;
end;

procedure tmainform.movetopmiclick(Sender: TObject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  paths.clear;
  // form left-top to right-top
  p0.x := - (widthse .value / 2);
  p0.y := + (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  driver.enabled       := true;
  driver.penoff        := sender = movetopmi;

  driverthread         := tvpdriverthread.create(paths);
  driverthread.onstart := @onplotterstart;
  driverthread.onstop  := @onplotterstop;
  driverthread.ontick  := @onplottertick;
  driverthread.start;
  elapsed := 1;
end;

procedure tmainform.movebottommiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  paths.clear;
  // form left-bottom to right-bottom
  p0.x := - (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := - (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  driver.enabled       := true;
  driver.penoff        := sender = movebottommi;

  driverthread         := tvpdriverthread.create(paths);
  driverthread.onstart := @onplotterstart;
  driverthread.onstop  := @onplotterstop;
  driverthread.ontick  := @onplottertick;
  driverthread.start;
  elapsed := 1;
end;

procedure tmainform.moveleftmiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  paths.clear;
  // form left-bottom to left-top
  p0.x := - (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := - (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  driver.enabled       := true;
  driver.penoff        := sender = moveleftmi;

  driverthread         := tvpdriverthread.create(paths);
  driverthread.onstart := @onplotterstart;
  driverthread.onstop  := @onplotterstop;
  driverthread.ontick  := @onplottertick;
  driverthread.start;
  elapsed := 1;
end;

procedure tmainform.moverightmiclick(sender: tobject);
var
  p0: tvppoint;
  p1: tvppoint;
begin
  paths.clear;
  // form right-bottom to right-top
  p0.x := + (widthse .value / 2);
  p0.y := - (heightse.value / 2);
  p1.x := + (widthse .value / 2);
  p1.y := + (heightse.value / 2);
  paths.add(interpolate_line(p0, p1));

  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  driver.enabled       := true;
  driver.penoff        := sender = moverightmi;

  driverthread         := tvpdriverthread.create(paths);
  driverthread.onstart := @onplotterstart;
  driverthread.onstop  := @onplotterstop;
  driverthread.ontick  := @onplottertick;
  driverthread.start;
  elapsed := 1;
end;

procedure tmainform.layoutmiclick(Sender: TObject);
var
  m0: longint = 0;
  m1: longint = 0;
begin
  gohomebtnclick(nil);
  // load configuration
  setting.clear;
  setting.load(changefileext(paramstr(0), '.ini'));
  // update plotter driver
  driver.mode   := setting.mode;
  driver.delaym := setting.delaym;
  driver.delayz := setting.delayz;

  optimize_point(setting.layout09, m0, m1);
  driver.init(m0, m1);
end;

procedure tmainform.gohomebtnclick(sender: tobject);
var
  m0: longint = 0;
  m1: longint = 0;
begin
  lock2(false);
  begin
    driver.enabled := true;
    driver.penoff  := true;
    optimize_point(setting.layout09, m0, m1);
    driver.step(m0, m1);
  end;
  lock2(true);
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
var
  vec: tvvectorialdocument;
begin
  opendialog.filter := 'dxf files (*.dxf)|*.dxf';
  if opendialog.execute then
  begin
    lock2(false);
    // load file ...
    vec := tvvectorialdocument.create;
    try
      vec.readfromfile(opendialog.filename,
        vec.getformatfromextension(opendialog.filename));
    except
    end;
    load_paths(paths, vec);
    reloadmiclick(nil);
    freeandnil(vec);
  end;
end;

procedure tmainform.reloadmiclick(sender: tobject);
var
  i, j: longint;
  path: tvppath;
   pos: tvpposition;
begin
  lock2(false);
  optimize_paths(paths,
    offsetxse.value,
    offsetyse.value,
    setting.layout08.x,
    setting.layout08.y + (heightse.value / 2),
    heightse.value,
    widthse.value);

  // updtare preview ...
  for i := 0 to paths.count - 1 do
  begin
    path := paths.item[i];
    for j := 0 to path.count - 1 do
    begin
      pos := path.item[j];
      bitmap.canvas.pixels[
        trunc(( widthse.value div 2) + pos.p.x + offsetxse.value),
        trunc((heightse.value div 2) - pos.p.y - offsetyse.value)] := clblack;
    end;
  end;
  image.canvas.draw(0, 0, bitmap);
  lock2(true);
end;

procedure tmainform.closemiclick(sender: tobject);
begin
  lock2(false);
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

  if sender = closemi then
  begin
    paths.clear
  end else
    reloadmiclick(closemi);
  // ---
  lock2(true);
end;

procedure tmainform.exitmiclick(sender: tobject);
begin
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

// PLOT mainmenu

procedure tmainform.startmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.enabled := true;
    timer       .enabled := true;
  end else
  begin
    driver.enabled       := true;
    driver.penoff        := false;
    driver.pen           := false;

    driverthread         := tvpdriverthread.create(paths);
    timer       .enabled := true;
    driverthread.onstart := @onplotterstart;
    driverthread.onstop  := @onplotterstop;
    driverthread.ontick  := @onplottertick;
    driverthread.start;
    elapsed := 1;
  end;
end;

procedure tmainform.stopmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.enabled := false;
    timer       .enabled := false;
    driver      .pen     := false;
  end;
end;

procedure tmainform.killmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.enabled := true;
    driverthread.terminate;
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
  closemiclick(verticalcb);
end;

// PLOTTER THREAD methods

procedure tmainform.lock1(value: boolean);
begin
  // main menu
  loadmi        .enabled := value;
  reloadmi      .enabled := value;
  closemi       .enabled := value;
  exitmi        .enabled := value;
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
  application.processmessages;
end;

procedure tmainform.lock2(value: boolean);
begin
  // main menu
  loadmi        .enabled := value;
  reloadmi      .enabled := value;
  closemi       .enabled := value;
  exitmi        .enabled := value;
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
  application.processmessages;
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
  driverthread := nil;
  lock1(true);

  caption := format('Finished - %u sec', [elapsed]);
  application.processmessages;
end;

procedure tmainform.onplottertick;
begin
  if enabledebug then
  begin
    writeln(format('    TICK::P.X    = %12.5f', [driverthread.position.p.x]));
    writeln(format('    TICK::P.Y    = %12.5f', [driverthread.position.p.y]));
  end;

  if (elapsed mod 2) = 0 then
    caption := format('Running - %u sec', [elapsed]);
  application.processmessages;
end;

// TIMER events

procedure tmainform.timertimer(sender: tobject);
begin
  inc(elapsed);
end;

end.

