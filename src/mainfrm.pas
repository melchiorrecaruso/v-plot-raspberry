{
  Description: vPlot main form.

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

unit mainfrm;

{$mode objfpc}

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls, comctrls,
  buttons, menus, spin, vppaths, types,
  bgrabitmap, bgrabitmaptypes, bgravirtualscreen, bgragradientscanner;

type
  { tmainform }

  tmainform = class(tform)
    clientformbevel: TBevel;
    scalebevelleft: TBevel;
    offsetbevelleft: TBevel;
    calibrationbevelleft: TBevel;
    scalelabel: TLabel;
    scalebevel: TBevel;
    mainformbevel: TBevel;
    offsetbevel: TBevel;
    calibrationbevel: TBevel;
    calibrationclosebtn: TBitBtn;
    xoffsetlabel: TLabel;
    yoffsetlabel: TLabel;
    offsetupdatebtn: TBitBtn;
    scaleupdatebtn: TBitBtn;
    offsetpanel: TPanel;
    scaleclosebtn: TBitBtn;
    scaleedit: TFloatSpinEdit;
    leftdownbtn: TBitBtn;
    edit: TSpinEdit;
    leftupbtn: TBitBtn;
    calibrationpanel: TPanel;
    calibrationmi: TMenuItem;
    scalepanel: TPanel;
    pendownbtn: TBitBtn;
    penupbtn: TBitBtn;
    rightdownbtn: TBitBtn;
    rightupbtn: TBitBtn;
    offsetclosebtn: TSpeedButton;
    yoffsetedit: TSpinEdit;
    xoffsetedit: TSpinEdit;
    statuslabel: TLabel;
    screen: TBGRAVirtualScreen;
    divideselpm: tmenuitem;
    fitmi: TMenuItem;
    zoominmi: TMenuItem;
    zoomoutmi: TMenuItem;
    n9: TMenuItem;
    viewmi: TMenuItem;
    selattachedpm: tmenuitem;
    selbylayerpm: tmenuitem;
    hidebylayerpm: tmenuitem;
    mergesel: tmenuitem;
    n2pm: tmenuitem;
    invertselpm: tmenuitem;
    deselallpm: tmenuitem;
    deselbylayerpm: tmenuitem;
    hideselpm: tmenuitem;
    showallpm: tmenuitem;
    showbylayerpm: tmenuitem;
    inverthiddenpm: tmenuitem;
    hideallpm: tmenuitem;
    selallpm: tmenuitem;
    popup: tpopupmenu;
    mainmenu: tmainmenu;
    editmi: tmenuitem;
    a0mi: tmenuitem;
    a1mi: tmenuitem;
    a2mi: tmenuitem;
    a3mi: tmenuitem;
    a4mi: tmenuitem;
    a5mi: tmenuitem;
    horizontalmi: tmenuitem;
    movetohomemi: tmenuitem;
    verticalmi: tmenuitem;
    n7: tmenuitem;
    rotate90mi: tmenuitem;
    rotate180mi: tmenuitem;
    rotate270mi: tmenuitem;
    mirrorxmi: tmenuitem;
    mirrorymi: tmenuitem;
    n6: tmenuitem;
    killmi: tmenuitem;
    savedialog: tsavedialog;
    stopmi: tmenuitem;
    startmi: tmenuitem;
    aboutmi: tmenuitem;
    toolpathmi: tmenuitem;
    n4: tmenuitem;
    mirrormi: tmenuitem;
    scalemi: tmenuitem;
    offsetmi: tmenuitem;
    pagesizemi: tmenuitem;
    n3: tmenuitem;
    rotatemi: tmenuitem;
    clearmi: tmenuitem;
    exitmi: tmenuitem;
    loadmi: tmenuitem;
    savemi: tmenuitem;
    importmi: tmenuitem;
    n2: tmenuitem;
    n1: tmenuitem;
    helpmi: tmenuitem;
    miprinter: tmenuitem;
    filemi: tmenuitem;
    opendialog: topendialog;

    procedure formcreate           (sender: tobject);
    procedure formdestroy          (sender: tobject);
    procedure formclose            (sender: tobject; var closeaction: tcloseaction);
    // MAIN MENU::FILE
    procedure loadmiclick          (sender: tobject);
    procedure savemiclick          (sender: tobject);
    procedure clearmiclick         (sender: tobject);
    procedure importmiclick        (sender: tobject);
    procedure exitmiclick          (sender: tobject);
    // MAIN MENU::EDIT
    procedure rotate180miclick     (sender: tobject);
    procedure rotate270miclick     (sender: tobject);
    procedure rotate90miclick      (sender: tobject);
    procedure mirrorxmiclick       (sender: tobject);
    procedure mirrorymiclick       (sender: tobject);
    procedure scalemiclick         (sender: tobject);
    procedure a0miclick            (sender: tobject);
    procedure horizontalmiclick    (sender: tobject);
    procedure toolpathmiclick      (sender: tobject);
    // MAIN-MENU::VIEW
    procedure zoomoutmiclick       (sender: tobject);
    procedure zoominmiclick        (sender: tobject);
    procedure fitmiclick           (sender: tobject);
    // MAIN-MENU::PRINTER
    procedure startmiclick         (sender: tobject);
    procedure stopmiclick          (sender: tobject);
    procedure killmiclick          (sender: tobject);
    procedure movetohomemiclick    (sender: tobject);
    // MAIN-FORM::HELP
    procedure aboutmiclick         (sender: tobject);
    // POPUP-MENU
    procedure selallpmclick        (sender: tobject);
    procedure deselallpmclick      (sender: tobject);
    procedure divideselpmclick     (sender: tobject);
    procedure hideallpmclick       (sender: tobject);
    procedure hidebylayerpmclick   (sender: tobject);
    procedure hideselpmclick       (sender: tobject);
    procedure inverthiddenpmclick  (sender: tobject);
    procedure invertselpmclick     (sender: tobject);
    procedure mergeselclick        (sender: tobject);
    procedure selattachedpmclick   (sender: tobject);
    procedure selbylayerpmclick    (sender: tobject);
    procedure showallpmclick       (sender: tobject);
    procedure showbylayerpmclick   (sender: tobject);
    // virtual screen events
    procedure screenredraw    (sender: tobject; bitmap: tbgrabitmap);
    procedure imagemouseup    (sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure imagemousedown  (sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure imagemousemove  (sender: tobject; shift: tshiftstate; x, y: integer);
    procedure screenmousewheel(sender: tobject; shift: tshiftstate;
      wheeldelta: integer; mousepos: tpoint; var handled: boolean);
    // panels event
    procedure scaleupdatebtnclick  (sender: tobject);
    procedure scaleclosebtnclick   (sender: tobject);
    procedure offsetupdatebtnclick (sender: tobject);
    procedure leftupbtnclick       (sender: tobject);
    procedure penupbtnclick        (sender: tobject);
  private
         bit: tbgrabitmap;
 mouseisdown: boolean;
          px: longint;
          py: longint;

   pagewidth: longint;
  pageheight: longint;
       paths: tvppaths;
        zoom: single;

   starttime: tdatetime;
   tickcount: longint;

       movex: longint;
       movey: longint;
      locked: boolean;
    // ---
    procedure onplotterstart;
    procedure onplotterstop;
    procedure onplottertick;
    procedure lockinternal1(value: boolean);
    procedure lockinternal2(value: boolean);
  public
    procedure lock1;
    procedure lock2;
    procedure unlock1;
    procedure unlock2;
    procedure updatescreen;
  end;


var
  mainform: tmainform;


implementation

{$r *.lfm}

uses
  math, sysutils, importfrm, aboutfrm,
  vpdriver, vpdriverthread, vpsketcher, vpmath, vpsvgreader,
  vpdxfreader, vpsetting, vpwave;

// FORM EVENTS

procedure tmainform.formcreate(sender: tobject);
var
  wavemesh: twavemesh;
  mx: longint = 0;
  my: longint = 0;
begin
  // load setting
  setting := tvpsetting.create;
  setting.load(changefileext(paramstr(0), '.ini'));
  // create plotter driver
  driver        := tvpdriver.create;
  driver.xdelay := setting.xdelay;
  driver.ydelay := setting.ydelay;
  driver.zdelay := setting.zdelay;
  // init wave
  wavemesh[0] := setting.wave0;
  wavemesh[1] := setting.wave1;
  wavemesh[2] := setting.wave2;
  wavemesh[3] := setting.wave3;
  wavemesh[4] := setting.wave4;
  wavemesh[5] := setting.wave5;
  wavemesh[6] := setting.wave6;
  wavemesh[7] := setting.wave7;
  wavemesh[8] := setting.wave8;
  wave := twave.create(
    setting.wavedxmax,
    setting.wavedymax,
    wavemesh);
  wave.enabled := setting.waveoff = 1;
  wave.debug;
  // create preview and empty paths
  paths := tvppaths.create;
    bit := tbgrabitmap.create;
  // update virtual screen
  a0miclick(a3mi);
  // initialize driver
  optimize(setting.layout9, mx, my);
  driver.init(mx, my);
  // update panels
  scalepanel      .anchors := [akleft, akright, aktop];
  offsetpanel     .anchors := [akleft, akright, aktop];
  calibrationpanel.anchors := [akleft, akright, aktop];
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  movetohomemiclick(sender);
  // ---
  bit.destroy;
  paths.destroy;
  wave.destroy;
  driver.destroy;
  setting.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(driverthread) then
  begin
    messagedlg('vPlotter Error', 'There is an active process!', mterror, [mbok], 0);
    closeaction := canone;
  end else
    closeaction := cafree;
end;

// MAIN-MENU::FILE

procedure tmainform.loadmiclick(sender: tobject);
begin
  opendialog.filter := 'vplot files (*.vplot)|*.vplot';
  if opendialog.execute then
  begin
    caption := 'vPlotter - ' + opendialog.filename;

    lock2;
    paths.clear;
    paths.load(opendialog.filename);
    fitmiclick(sender);
    unlock2;
  end;
end;

procedure tmainform.savemiclick(sender: tobject);
begin
  savedialog.filter := 'vplot files (*.vplot)|*.vplot';
  if savedialog.execute then
  begin
    caption := 'vPlotter - ' + changefileext(savedialog.filename, '.vplot');

    lock2;
    paths.save(changefileext(savedialog.filename, '.vplot'));
    updatescreen;
    unlock2;
  end;
end;

procedure tmainform.clearmiclick(sender: tobject);
begin
  caption := 'vPlotter';

  lock2;
  paths.clear;
  fitmiclick(sender);
  unlock2;
end;

procedure tmainform.importmiclick(sender: tobject);
var
  sk: tvpsketcher;
begin
  opendialog.filter := 'Supported files (*.svg, *.dxf, *.png, *.bmp)|*.svg; *.dxf; *.png; *.bmp';
  if opendialog.execute then
  begin
    caption := 'vPlotter - ' + opendialog.filename;

    lock2;
    paths.clear;
    if (lowercase(extractfileext(opendialog.filename)) = '.dxf') or
       (lowercase(extractfileext(opendialog.filename)) = '.svg') then
    begin
      importform.imcb.itemindex := 0;
      importform.imcb .enabled  := false;
      importform.ipwse.enabled  := false;
      importform.iphse.enabled  := false;
      importform.pwse .enabled  := false;
      importform.phse .enabled  := false;
      importform.dsfse.enabled  := false;
      importform.otpcb.enabled  := true;
      importform.otpcb.checked  := true;
      if importform.showmodal = mrok then
      begin
        if (lowercase(extractfileext(opendialog.filename)) = '.dxf') then
          dxf2paths(opendialog.filename, paths)
        else
        if (lowercase(extractfileext(opendialog.filename)) = '.svg') then
          svg2paths(opendialog.filename, paths);

        if importform.otpcb.checked then
          paths.createtoolpath;
      end;
    end else
    if (lowercase(extractfileext(opendialog.filename)) = '.png') then
    begin
      importform.imcb.itemindex := 1;
      importform.imcb .enabled  := false;
      importform.ipwse.enabled  := true;
      importform.iphse.enabled  := true;
      importform.pwse .enabled  := true;
      importform.phse .enabled  := true;
      importform.dsfse.enabled  := true;
      importform.otpcb.enabled  := true;
      importform.otpcb.checked  := true;
      if importform.showmodal = mrok then
      begin
        bit.canvas.clear;
        bit.loadfromfile(opendialog.filename);
        sk := tvpsketcher.create(bit);
        sk.patternbw := importform.ipwse.value;
        sk.patternbh := importform.iphse.value;
        sk.patternw  := importform. pwse.value;
        sk.patternh  := importform. phse.value;
        sk.dotsize   := importform.dsfse.value;
        sk.run(paths);
        sk.destroy;

        if importform.otpcb.checked then
          paths.createtoolpath;
      end;
    end;
    fitmiclick(sender);
    unlock2;
  end;
end;

procedure tmainform.exitmiclick(sender: tobject);
begin
  close;
end;

// MAIN-MENU::EDIT

procedure tmainform.rotate90miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(90));
  updatescreen;
  unlock2;
end;

procedure tmainform.rotate180miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(180));
  updatescreen;
  unlock2;
end;

procedure tmainform.rotate270miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(270));
  updatescreen;
  unlock2;
end;

procedure tmainform.mirrorxmiclick(sender: tobject);
begin
  lock2;
  paths.mirror(true);
  updatescreen;
  unlock2;
end;

procedure tmainform.mirrorymiclick(sender: tobject);
begin
  lock2;
  paths.mirror(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.scalemiclick(sender: tobject);
begin
  scalepanel      .visible := sender = scalemi;
  calibrationpanel.visible := sender = calibrationmi;
  offsetpanel     .visible := sender = offsetmi;
end;

procedure tmainform.scaleclosebtnclick(sender: tobject);
begin
  scalepanel      .visible := false;
  calibrationpanel.visible := false;
  offsetpanel     .visible := false;
end;

procedure tmainform.scaleupdatebtnclick(sender: tobject);
begin
  lock2;
  paths.scale(scaleedit.value);
  updatescreen;
  unlock2;
end;

procedure tmainform.offsetupdatebtnclick(sender: tobject);
begin
  lock2;
  paths.offset(xoffsetedit.value,
               yoffsetedit.value);
  updatescreen;
  unlock2;
end;

procedure tmainform.a0miclick(sender: tobject);
var
  amin: longint = 297;
  amax: longint = 420;
begin
  a0mi.checked := (sender = a0mi);
  a1mi.checked := (sender = a1mi);
  a2mi.checked := (sender = a2mi);
  a3mi.checked := (sender = a3mi);
  a4mi.checked := (sender = a4mi);
  a5mi.checked := (sender = a5mi);

  if a0mi.checked then begin amin :=  841; amax := 1189; end else
  if a1mi.checked then begin amin :=  594; amax :=  841; end else
  if a2mi.checked then begin amin :=  420; amax :=  594; end else
  if a3mi.checked then begin amin :=  297; amax :=  420; end else
  if a4mi.checked then begin amin :=  210; amax :=  297; end else
  if a5mi.checked then begin amin :=  148; amax :=  210; end;

  if verticalmi.checked then
  begin
    pageheight := amax;
    pagewidth  := amin;
  end else
  begin
    pageheight := amin;
    pagewidth  := amax;
  end;         
  fitmiclick(nil);

  lock2;
  updatescreen;
  unlock2;
end;

procedure tmainform.horizontalmiclick(sender: tobject);
var
  amin: longint;
  amax: longint;
begin
  verticalmi  .checked := sender = verticalmi;
  horizontalmi.checked := sender = horizontalmi;

  amax := max(pagewidth, pageheight);
  amin := min(pagewidth, pageheight);
  if verticalmi.checked then
  begin
    pageheight := amax;
    pagewidth  := amin;
  end else
  begin
    pageheight := amin;
    pagewidth  := amax;
  end;

  lock2;
  updatescreen;
  unlock2;
end;

procedure tmainform.toolpathmiclick(sender: tobject);
begin
  lock2;
  paths.selectall(false);
  paths.createtoolpath;
  updatescreen;
  unlock2;
end;

// MAIN MENU::VIEW

procedure tmainform.zoominmiclick(sender: tobject);
var
  value: single;
begin
  value := max(min(zoom*1.25, 25.0), 0.5);

  if value <> zoom then
  begin
    zoom  := value;
    movex := movex + round((bit.width  -(pagewidth *zoom))*(movex)/bit.width );
    movey := movey + round((bit.height -(pageheight*zoom))*(movey)/bit.height);
    updatescreen;
  end;
end;

procedure tmainform.zoomoutmiclick(sender: tobject);
var
  value: single;
begin
  value := max(min(zoom/1.25, 25.0), 0.5);

  if value <> zoom then
  begin
    zoom  := value;
    movex := movex + round((bit.width  -(pagewidth *zoom))*(movex)/bit.width );
    movey := movey + round((bit.height -(pageheight*zoom))*(movey)/bit.height);
    updatescreen;
  end;
end;

procedure tmainform.fitmiclick(sender: tobject);
begin
  zoom  := 1.0;
  movex := (screen.width  - pagewidth ) div 2;
  movey := (screen.height - pageheight) div 2;
  updatescreen;
end;

// MAIN MENU::PRINT

procedure tmainform.startmiclick(sender: tobject);
begin
  driver.xoff := false;
  driver.yoff := false;
  driver.zoff := false;
  if assigned(driverthread) then
  begin
    driverthread.enabled := true;
  end else
  begin
    driverthread         := tvpdriverthread.create(paths);
    driverthread.xcenter := setting.layout8.x;
    driverthread.ycenter := setting.layout8.y+pageheight/2;
    driverthread.dxmax   := pagewidth /2+2;
    driverthread.dymax   := pageheight/2+2;
    driverthread.onstart := @onplotterstart;
    driverthread.onstop  := @onplotterstop;
    driverthread.ontick  := @onplottertick;
    driverthread.start;
  end;
  starttime := now;
  tickcount := 0
end;

procedure tmainform.stopmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.enabled := false;
  end;
  driver.zcount := setting.zmax;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;
end;

procedure tmainform.killmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.terminate;
    driverthread.enabled := true;
  end;
end;

procedure tmainform.movetohomemiclick(sender: tobject);
var
  mx: longint = 0;
  my: longint = 0;
begin
  driver.zcount := setting.zmax;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;

  optimize(setting.layout9, mx, my);
  driver.move(mx, my);
end;

// MAIN-MENU::HELP

procedure tmainform.aboutmiclick(sender: tobject);
begin
  aboutform.showmodal;
end;

// POPUP-MENU

procedure tmainform.selallpmclick(sender: tobject);
begin
  lock2;
  paths.selectall(true);
  updatescreen;
  unlock2;
end;

procedure tmainform.selbylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  lock2;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.selectlayer(path.layer);
  end;
  updatescreen;
  unlock2;
end;

procedure tmainform.invertselpmclick(sender: tobject);
begin
  lock2;
  paths.invertselected;
  updatescreen;
  unlock2;
end;

procedure tmainform.deselallpmclick(sender: tobject);
begin
  lock2;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.showallpmclick(sender: tobject);
begin
  lock2;
  paths.showall(true);
  updatescreen;
  unlock2;
end;

procedure tmainform.showbylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  lock2;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.showlayer(path.layer);
  end;
  updatescreen;
  unlock2;
end;

procedure tmainform.inverthiddenpmclick(sender: tobject);
begin
  lock2;
  paths.inverthidden;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hideallpmclick(sender: tobject);
begin
  lock2;
  paths.showall(false);
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hidebylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  lock2;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.hidelayer(path.layer);
  end;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hideselpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  lock2;
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      path.hidden := true;
  end;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.mergeselclick(sender: tobject);
begin
  lock2;
  paths.mergeselected;
  updatescreen;
  unlock2;
end;

procedure tmainform.selattachedpmclick(sender: tobject);
begin
  lock2;
  paths.selectattached;
  updatescreen;
  unlock2;
end;

procedure tmainform.divideselpmclick(sender: tobject);
begin
  lock2;
  paths.unmergeselected;
  updatescreen;
  unlock2;
end;

// MOUSE EVENTS

procedure tmainform.imagemousedown(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
var
   i, j: longint;
   path: tvppath;
  point: tvppoint;
begin
  if locked then exit;
  popup.autopopup:= true;
  // search path ...
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    for j := 0 to path.count -1 do
    begin
      point   := path.items[j]^;
      point.x := (bit.width  div 2) + point.x*zoom;
      point.y := (bit.height div 2) - point.y*zoom;

      if (abs(point.x + movex - x) < 3) and
         (abs(point.y + movey - y) < 3) then
        if path.hidden = false then
        begin
          if not (ssctrl in shift) then
          begin
            paths.selectall(false);
          end;
          path.selected   := button = mbleft;
          popup.autopopup := false;
        end;
    end;
  end;

  if popup.autopopup = false then
  begin
    updatescreen;
  end else
    if button = mbleft then
    begin
      mouseisdown := true;
      px := x - movex;
      py := y - movey;
    end;
end;

procedure tmainform.imagemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  if locked then exit;
  if mouseisdown then
  begin
    movex := x - px;
    movey := y - py;
    screen.redrawbitmap;
  end;
end;

procedure tmainform.imagemouseup(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
begin
  if locked then exit;
  mouseisdown := false;
end;

procedure tmainform.screenmousewheel(sender: tobject; shift: tshiftstate;
  wheeldelta: integer; mousepos: tpoint; var handled: boolean);
var
  value: single;
begin
  if locked then exit;

  locked := true;
  if wheeldelta > 0 then
    value := max(min(zoom*1.25, 25.0), 0.5)
  else
    value := max(min(zoom/1.25, 25.0), 0.5);

  if value <> zoom then
  begin
    zoom  := value;
    movex := movex + round((bit.width  -(pagewidth *zoom))*(mousepos.x-movex)/bit.width );
    movey := movey + round((bit.height -(pageheight*zoom))*(mousepos.y-movey)/bit.height);
    updatescreen;
  end;
  locked := false;
end;

// PANEL EVENTS

procedure tmainform.leftupbtnclick(sender: tobject);
var
  mx: longint = 0;
  my: longint = 0;
begin
  lock2;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;
  driver.zcount := setting.zmax;

  if sender = leftupbtn    then driver.xcount := driver.xcount - edit.value;
  if sender = leftdownbtn  then driver.xcount := driver.xcount + edit.value;
  if sender = rightupbtn   then driver.ycount := driver.ycount - edit.value;
  if sender = rightdownbtn then driver.ycount := driver.ycount + edit.value;

  optimize(setting.layout9, mx, my);
  driver.init(mx, my);
  unlock2;
end;

procedure tmainform.penupbtnclick(sender: tobject);
begin
   lock2;
  if sender = penupbtn then
    driver.zcount := setting.zmax
  else
  if sender = pendownbtn then
    driver.zcount := setting.zmin;
  unlock2;
end;

// SCREEN EVENTS

procedure tmainform.updatescreen;
var
 i, j: longint;
    k: longint = 0;
 path: tvppath;
   p1: tvppoint;
   p2: tvppoint;
begin
  bit.setsize(round(pagewidth *zoom),
              round(pageheight*zoom));
  bit.fillrect(0, 0, bit.width,   bit.height,   bgra(100, 100, 100), dmset);
  bit.fillrect(1, 1, bit.width-1, bit.height-1, bgra(255, 255, 255), dmset);
  // updtare preview ...
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if (path.enabled) and (path.count > 1) then
    begin
      p1    := path.items[0]^;
      p1.x  := (bit.width  div 2) + p1.x*zoom;
      p1.y  := (bit.height div 2) - p1.y*zoom;
      for j := 1 to path.count -1 do
      begin
        p2   := path.items[j]^;
        p2.x := (bit.width  div 2) + p2.x*zoom;
        p2.y := (bit.height div 2) - p2.y*zoom;
        if path.hidden = false then
        begin
          if path.selected then
          begin
            inc(k);
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(57, 255, 20), true, dmset)
          end else
          begin
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(  0,  0,  0), true, dmset);
          end;
        end;
        p1 := p2;
      end;
    end;
  end;

  if k > 0 then
    statuslabel.caption := 'Selected Items ' + inttostr(k)
  else
    statuslabel.caption := '';
  screen.redrawbitmap;
end;

procedure tmainform.screenredraw(sender: tobject; bitmap: tbgrabitmap);
begin
  bitmap.putimage(movex, movey, bit, dmset);
end;

// LOCK/UNLOCK ROUTINES

procedure tmainform.lockinternal1(value: boolean);
begin
  locked                := not value;
  // main menu::file
  loadmi       .enabled := value;
  savemi       .enabled := value;
  clearmi      .enabled := value;
  importmi     .enabled := value;
  // main menu::editmi
  rotatemi     .enabled := value;
  mirrormi     .enabled := value;
  scalemi      .enabled := value;
  offsetmi     .enabled := value;
  pagesizemi   .enabled := value;
  toolpathmi   .enabled := value;
  // main menu::view
  zoominmi     .enabled := value;
  zoomoutmi    .enabled := value;
  fitmi        .enabled := value;
  // main menu::printer
  startmi      .enabled := true;
  stopmi       .enabled := true;
  killmi       .enabled := true;
  calibrationmi.enabled := value;
  movetohomemi .enabled := value;
  // main menu::help
  aboutmi      .enabled := value;
  // virtual screen
  screen       .enabled := value;
  // popup menu
  if value = false then
    screen.popupmenu := nil
  else
    screen.popupmenu := popup;
  application  .processmessages;
end;

procedure tmainform.lockinternal2(value: boolean);
begin
  locked                := not value;
  // main menu::file
  loadmi       .enabled := value;
  savemi       .enabled := value;
  clearmi      .enabled := value;
  importmi     .enabled := value;
  // main menu::editmi
  rotatemi     .enabled := value;
  mirrormi     .enabled := value;
  scalemi      .enabled := value;
  offsetmi     .enabled := value;
  pagesizemi   .enabled := value;
  toolpathmi   .enabled := value;
  // main menu::view
  zoominmi     .enabled := value;
  zoomoutmi    .enabled := value;
  fitmi        .enabled := value;
  // main menu::printer
  startmi      .enabled := value;
  stopmi       .enabled := value;
  killmi       .enabled := value;
  calibrationmi.enabled := value;
  movetohomemi .enabled := value;
  // main menu::help
  aboutmi      .enabled := value;
  // virtual screen
  screen       .enabled := value;
  // popup menu
  if value = false then
    screen.popupmenu := nil
  else
    screen.popupmenu := popup;
  application  .processmessages;
end;

procedure tmainform.lock1;
begin
  lockinternal1(false);
end;

procedure tmainform.unlock1;
begin
  lockinternal1(true);
end;

procedure tmainform.lock2;
begin
  lockinternal2(false);
end;

procedure tmainform.unlock2;
begin
  lockinternal2(true);
end;

// PLOTTER THREAD EVENTS

procedure tmainform.onplotterstart;
begin
  scalemiclick(nil);

  lock1;
  statuslabel.caption := '';
  application.processmessages;
end;

procedure tmainform.onplotterstop;
begin
  driverthread  := nil;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;
  driver.zcount := setting.zmax;

  unlock1;
  statuslabel.caption := '';
  application.processmessages;
end;

procedure tmainform.onplottertick;
begin
  inc(tickcount);
  if (tickcount mod 800) = 0 then
  begin
    statuslabel.caption := 'Remaining Time ' +
      formatdatetime('hh:nn:ss', (now-starttime)/tickcount*driverthread.tick);
  end;
  application.processmessages;
end;

end.

