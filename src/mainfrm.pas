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
  buttons, menus, spin, vppaths, vpsetting, vpdriver, bgrabitmap,  types,
  bgrabitmaptypes, bgravirtualscreen, bgratransform, bgragradientscanner,
  bctypes, bgracanvas2d;

type
  { tmainform }

  tmainform = class(tform)
    Bevel1: TBevel;
    divideselpm: TMenuItem;
    selattachedpm: TMenuItem;
    selbylayerpm: TMenuItem;
    hidebylayerpm: TMenuItem;
    mergesel: TMenuItem;
    n2pm: TMenuItem;
    invertselpm: TMenuItem;
    deselallpm: TMenuItem;
    deselbylayerpm: TMenuItem;
    hideselpm: TMenuItem;
    showallpm: TMenuItem;
    showbylayerpm: TMenuItem;
    inverthiddenpm: TMenuItem;
    hideallpm: TMenuItem;
    selallpm: TMenuItem;
    popup: TPopupMenu;
    screen: TBGRAVirtualScreen;
    mainmenu: TMainMenu;
    Edit: TMenuItem;
    calibrationmi: TMenuItem;
    layoutmi: TMenuItem;
    a0mi: TMenuItem;
    a1mi: TMenuItem;
    a2mi: TMenuItem;
    a3mi: TMenuItem;
    a4mi: TMenuItem;
    a5mi: TMenuItem;
    horizontalmi: TMenuItem;
    movetohomemi: TMenuItem;
    N8: TMenuItem;
    statusbar: TStatusBar;
    verticalmi: TMenuItem;
    N7: TMenuItem;
    rotate90mi: TMenuItem;
    rotate180mi: TMenuItem;
    rotate270mi: TMenuItem;
    mirrorxmi: TMenuItem;
    mirrorymi: TMenuItem;
    N6: TMenuItem;
    killmi: TMenuItem;
    N5: TMenuItem;
    savedialog: TSaveDialog;
    stopmi: TMenuItem;
    startmi: TMenuItem;
    aboutmi: TMenuItem;
    createtoolpathmi: TMenuItem;
    N4: TMenuItem;
    mirrormi: TMenuItem;
    scalemi: TMenuItem;
    offsetmi: TMenuItem;
    pagesizemi: TMenuItem;
    N3: TMenuItem;
    mirotate: TMenuItem;
    clearmi: TMenuItem;
    exitmi: TMenuItem;
    loadmi: TMenuItem;
    savemi: TMenuItem;
    importmi: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    mihelp: TMenuItem;
    miprint: TMenuItem;
    mifile: TMenuItem;
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
    procedure offsetmiclick        (sender: tobject);
    procedure a0miclick            (sender: tobject);
    procedure horizontalmiclick    (sender: tobject);
    procedure layoutmiclick        (sender: tobject);
    // MAIN-MENU::PRINTER
    procedure startmiclick         (sender: tobject);
    procedure stopmiclick          (sender: tobject);
    procedure killmiclick          (sender: tobject);
    procedure calibrationmiclick   (sender: tobject);
    procedure movetohomemiclick    (sender: tobject);
    procedure createtoolpathmiclick(sender: tobject);
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
  private
         bit: tbgrabitmap;
       paths: tvppaths;
 mouseisdown: boolean;
          px: longint;
          py: longint;

   pagewidth: longint;
  pageheight: longint;
        zoom: single;

       movex: longint;
       movey: longint;
        lock: boolean;

    // ---
    procedure onplotterstart;
    procedure onplotterstop;
    procedure onplottertick;
  public
    procedure lock1(value: boolean);
    procedure lock2(value: boolean);
    procedure updatevirtualscreen;
  end;

var
  mainform: tmainform;

implementation

{$R *.lfm}

uses
  math, sysutils, aboutfrm, calibrationfrm, offsetfrm,
  scalefrm, vpmath, vpwave, sketchyimage;

// FORM EVENTS

procedure tmainform.formcreate(sender: tobject);
var
  mx: longint;
  my: longint;
begin
  // load setting
  setting := tvpsetting.create;
  setting.load(changefileext(paramstr(0), '.ini'));
  // create plotter driver
  driver         := tvpdriver.create;
  driver.xdelay  := setting.xdelay;
  driver.ydelay  := setting.ydelay;
  driver.zdelay  := setting.zdelay;
  // create preview and empty paths
    bit := tbgrabitmap.create;
  paths := tvppaths.create;
  // update virtual screen
  a0miclick(a3mi);
  // init wave
  wave := twave.create(
    setting.wavexmax,
    setting.waveymax,
    setting.wave);
  wave.enabled := false;
  wave.test;
  // initialize driver
  optimize(setting.layout09, mx, my);
  driver.init(mx, my);
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  movetohomemiclick(sender);
  // ---
  wave.destroy;
  paths.destroy;
  bit.destroy;
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

    lock2(false);
    paths.clear;
    paths.load(opendialog.filename);
    updatevirtualscreen;
  end;
end;

procedure tmainform.savemiclick(sender: tobject);
begin
  savedialog.filter := 'vplot files (*.vplot)|*.vplot';
  if savedialog.execute then
  begin
    caption := 'vPlotter - ' + changefileext(savedialog.filename, '.vplot');

    lock2(false);
    paths.save(changefileext(savedialog.filename, '.vplot'));
    lock2(true);
  end;
end;

procedure tmainform.clearmiclick(sender: tobject);
begin
  paths.clear;
  updatevirtualscreen;
end;

procedure tmainform.updatevirtualscreen;
var
 i, j: longint;
 path: tvppath;
   p1: tvppoint;
   p2: tvppoint;
    a: array of tpointf;
begin
  lock2(false);
  // ---
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
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(57, 255, 20), true, dmset)
          else
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(  0,  0,  0), true, dmset);
        end;
        p1 := p2;
      end;
    end;
  end;
  screen.redrawbitmap;
  lock2(true);
end;

procedure tmainform.importmiclick(sender: tobject);
begin
  opendialog.filter := 'Supported files (*.svg, *.dxf)|*.svg; *.dxf';
  if opendialog.execute then
  begin
    caption := 'vPlotter - ' + opendialog.filename;

    lock2(false);
    paths.clear;
    if lowercase(extractfileext(opendialog.filename)) = '.dxf' then
      dxf2paths(opendialog.filename, paths);

    if lowercase(extractfileext(opendialog.filename)) = '.svg' then
      svg2paths(opendialog.filename, paths);
    //decodePNG(opendialog.filename, 100, 1, 1, 100);
    paths.createtoolpath;
    updatevirtualscreen;
  end;
end;

procedure tmainform.exitmiclick(sender: tobject);
begin
  close;
end;

// MAIN-MENU::EDIT

procedure tmainform.rotate90miclick(sender: tobject);
begin
  paths.rotate(degtorad(90));
  updatevirtualscreen;
end;

procedure tmainform.rotate180miclick(sender: tobject);
begin
  paths.rotate(degtorad(180));
  updatevirtualscreen;
end;

procedure tmainform.rotate270miclick(sender: tobject);
begin
  paths.rotate(degtorad(270));
  updatevirtualscreen;
end;

procedure tmainform.mirrorxmiclick(sender: tobject);
begin
  paths.mirror(true);
  updatevirtualscreen;
end;

procedure tmainform.mirrorymiclick(sender: tobject);
begin
  paths.mirror(false);
  updatevirtualscreen;
end;

procedure tmainform.scalemiclick(sender: tobject);
var
  f: tscaleform;
begin
  f := tscaleform.create(nil);
  if f.showmodal = mrok then
  begin
    paths.scale(f.factoredit.value);
  end;
  f.destroy;
  updatevirtualscreen;
end;

procedure tmainform.offsetmiclick(sender: tobject);
var
  f: toffsetform;
begin
  f := toffsetform.create(nil);
  if f.showmodal = mrok then
  begin
    paths.offset(
      f.offsetxse.value,
      f.offsetyse.value);
  end;
  f.destroy;
  updatevirtualscreen;
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

  zoom  := 1.0;
  movex := (screen.width  - pagewidth ) div 2;
  movey := (screen.height - pageheight) div 2;
  updatevirtualscreen;
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
  updatevirtualscreen;
end;

procedure tmainform.createtoolpathmiclick(sender: tobject);
begin
  paths.selectall(false);
  paths.createtoolpath;
  updatevirtualscreen;
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
    driverthread.xcenter := setting.layout08.x;
    driverthread.ycenter := setting.layout08.y+pageheight/2;
    driverthread.xmax    := pagewidth /2 + 2;
    driverthread.ymax    := pageheight/2 + 2;
    driverthread.onstart := @onplotterstart;
    driverthread.onstop  := @onplotterstop;
    driverthread.ontick  := @onplottertick;
    driverthread.start;
  end;
end;

procedure tmainform.stopmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.enabled := false;
  end;
  driver.zcount := setting.zmax;
  driver.xoff   := true;
  driver.yoff   := true;
  driver.zoff   := true;
end;

procedure tmainform.killmiclick(sender: tobject);
begin
  if assigned(driverthread) then
  begin
    driverthread.terminate;
  end;
end;

procedure tmainform.calibrationmiclick(sender: tobject);
var
  f: tcalibrationform;
begin
  f := tcalibrationform.create(nil);
  f.showmodal;
  f.destroy;
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

  optimize(setting.layout09, mx, my);
  driver.move(mx, my);
end;

procedure tmainform.layoutmiclick(sender: tobject);
var
  mx: longint = 0;
  my: longint = 0;
begin
  movetohomemiclick(sender);
  // load configuration
  setting.clear;
  setting.load(changefileext(paramstr(0), '.ini'));
  // update plotter driver
  driver.xdelay := setting.xdelay;
  driver.ydelay := setting.ydelay;
  driver.zdelay := setting.zdelay;

  optimize(setting.layout09, mx, my);
  driver.init(mx, my);
end;

// MAIN-MENU::HELP

procedure tmainform.aboutmiclick(sender: tobject);
var
  about: taboutform;
begin
  about := taboutform.create(nil);
  about.showmodal;
  about.destroy;
end;

// POPUP-MENU

procedure tmainform.selallpmclick(sender: tobject);
begin
  paths.selectall(true);
  updatevirtualscreen;
end;

procedure tmainform.selbylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.selectlayer(path.layer);
  end;
  updatevirtualscreen;
end;

procedure tmainform.invertselpmclick(sender: tobject);
begin
  paths.invertselected;
  updatevirtualscreen;
end;

procedure tmainform.deselallpmclick(sender: tobject);
begin
  paths.selectall(false);
  updatevirtualscreen;
end;

procedure tmainform.showallpmclick(sender: tobject);
begin
  paths.showall(true);
  updatevirtualscreen;
end;

procedure tmainform.showbylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.showlayer(path.layer);
  end;
  updatevirtualscreen;
end;

procedure tmainform.inverthiddenpmclick(sender: tobject);
begin
  paths.inverthidden;
  paths.selectall(false);
  updatevirtualscreen;
end;

procedure tmainform.hideallpmclick(sender: tobject);
begin
  paths.showall(false);
  paths.selectall(false);
  updatevirtualscreen;
end;

procedure tmainform.hidebylayerpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      paths.hidelayer(path.layer);
  end;
  paths.selectall(false);
  updatevirtualscreen;
end;

procedure tmainform.hideselpmclick(sender: tobject);
var
     i: longint;
  path: tvppath;
begin
  for i := 0 to paths.count -1 do
  begin
    path := paths.items[i];
    if path.selected then
      path.hidden := true;
  end;
  paths.selectall(false);
  updatevirtualscreen;
end;

procedure tmainform.mergeselclick(sender: tobject);
begin
  paths.mergeselected;
  updatevirtualscreen;
end;

procedure tmainform.selattachedpmclick(sender: tobject);
begin
  paths.selectattached;
  updatevirtualscreen;
end;

procedure tmainform.divideselpmclick(sender: tobject);
begin
  paths.unmergeselected;
  updatevirtualscreen;
end;

// PREVIEW EVENTS

procedure tmainform.imagemousedown(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
var
   i, j: longint;
   path: tvppath;
  point: tvppoint;
begin
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
    updatevirtualscreen;
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
  mouseisdown := false;
end;

procedure tmainform.screenmousewheel(sender: tobject; shift: tshiftstate;
  wheeldelta: integer; mousepos: tpoint; var handled: boolean);
var
  value: single;
begin
  if lock = false then
  begin
    lock := true;
    if wheeldelta > 0 then
      value := max(min(zoom*1.5, 25.0), 0.5)
    else
      value := max(min(zoom/1.5, 25.0), 0.5);

    if value <> zoom then
    begin
      zoom  := value;
      movex := movex + round((bit.width  -(pagewidth *zoom))*(mousepos.x-movex)/bit.width );
      movey := movey + round((bit.height -(pageheight*zoom))*(mousepos.y-movey)/bit.height);
      updatevirtualscreen;
    end;
    lock := false;
  end;
end;

// LOAD EVENTS

procedure tmainform.screenredraw(sender: tobject; bitmap: tbgrabitmap);
begin
  bitmap.putimage(movex, movey, bit, dmset);
end;

// LOCK/UNLOCK EVENTS

procedure tmainform.lock1(value: boolean);
begin
  // main menu
  loadmi      .enabled := value;
//reloadbtn    .enabled := value;
  clearmi      .enabled := value;
  startmi      .enabled := true;
  stopmi       .enabled := true;
  killmi       .enabled := true;
  layoutmi     .enabled := value;
  aboutmi      .enabled := value;
  // calibration
  // leftupbtn    .enabled := value;
  // leftdownbtn  .enabled := value;
  // leftedit     .enabled := value;
  // rightupbtn   .enabled := value;
  // rightdownbtn .enabled := value;
  // rightedit    .enabled := value;
  // penupbtn     .enabled := value;
  // pendownbtn   .enabled := value;
  // gohomebtn    .enabled := value;
  // page format
  // formatcb     .enabled := value;
  // heightse     .enabled := value;
  // widthse      .enabled := value;
  // offsetxse    .enabled := value;
  // offsetyse    .enabled := value;
  // verticalcb   .enabled := value;
  application  .processmessages;
end;

procedure tmainform.lock2(value: boolean);
begin
  // main menu
  loadmi      .enabled := value;
  // reloadbtn    .enabled := value;
  clearmi      .enabled := value;
  startmi      .enabled := value;
  stopmi       .enabled := value;
  killmi       .enabled := value;
  layoutmi     .enabled := value;
  aboutmi      .enabled := value;
  // calibration
  // leftupbtn    .enabled := value;
  // leftdownbtn  .enabled := value;
  // leftedit     .enabled := value;
  // rightupbtn   .enabled := value;
  // rightdownbtn .enabled := value;
  // rightedit    .enabled := value;
  // penupbtn     .enabled := value;
  // pendownbtn   .enabled := value;
  // gohomebtn    .enabled := value;
  // page format
  // formatcb     .enabled := value;
  // heightse     .enabled := value;
  // widthse      .enabled := value;
  // offsetxse    .enabled := value;
  // offsetyse    .enabled := value;
  // verticalcb   .enabled := value;
  application  .processmessages;
end;

// PLOTTER THREAD EVENTS

procedure tmainform.onplotterstart;
begin
  lock1(false);
  application.processmessages;
end;

procedure tmainform.onplotterstop;
begin
  driverthread   := nil;
  driver.xoff    := false;
  driver.yoff    := false;
  driver.zoff    := false;
  driver.zcount  := setting.zmax;

  lock1(true);
  application.processmessages;
end;

procedure tmainform.onplottertick;
begin


  application.processmessages;
end;

end.

