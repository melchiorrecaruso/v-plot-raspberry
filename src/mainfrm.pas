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
  bgrabitmap, bgrabitmaptypes, bgragradientscanner,  bgravirtualscreen,
  buttons, classes, comctrls, controls, dialogs, extctrls, forms, graphics,
  menus, spin, stdctrls, vppaths, vpmath;

type
  { tmainform }

  tmainform = class(tform)
    clientformbevel: tbevel;
    propertiesmi: TMenuItem;
    N10: TMenuItem;
    zoom3000mi: TMenuItem;
    zoom1000mi: TMenuItem;
    zoom1500mi: TMenuItem;
    zoom2000mi: TMenuItem;
    previewmi: TMenuItem;
    N8: TMenuItem;
    sb010mi: TMenuItem;
    sf025mi: TMenuItem;
    sf050mi: TMenuItem;
    sb050mi: TMenuItem;
    sf100mi: TMenuItem;
    sb100mi: TMenuItem;
    mtfsmi: TMenuItem;
    mtlsmi: TMenuItem;
    sb025mi: TMenuItem;
    N5: TMenuItem;
    sf001mi: TMenuItem;
    sb001mi: TMenuItem;
    sf005mi: TMenuItem;
    sb005mi: TMenuItem;
    sf010mi: TMenuItem;
    move2originmi: TMenuItem;
    zoom50mi: tmenuitem;
    zoom75mi: tmenuitem;
    zoom100mi: tmenuitem;
    zoom125mi: tmenuitem;
    zoom150mi: tmenuitem;
    zoom175mi: tmenuitem;
    zoom200mi: tmenuitem;
    zoom250mi: tmenuitem;
    zoom800mi: tmenuitem;
    zoom300mi: tmenuitem;
    zoom350mi: tmenuitem;
    zoom400mi: tmenuitem;
    zoom600mi: tmenuitem;
    scalebevelleft: tbevel;
    offsetbevelleft: tbevel;
    calibrationbevelleft: tbevel;
    scalelabel: tlabel;
    scalebevel: tbevel;
    mainformbevel: tbevel;
    offsetbevel: tbevel;
    calibrationbevel: tbevel;
    calibrationclosebtn: tbitbtn;
    xoffsetlabel: tlabel;
    yoffsetlabel: tlabel;
    offsetupdatebtn: tbitbtn;
    scaleupdatebtn: tbitbtn;
    offsetpanel: tpanel;
    scaleclosebtn: tbitbtn;
    scaleedit: tfloatspinedit;
    leftdownbtn: tbitbtn;
    edit: tspinedit;
    leftupbtn: tbitbtn;
    calibrationpanel: tpanel;
    calibrationmi: tmenuitem;
    scalepanel: tpanel;
    pendownbtn: tbitbtn;
    penupbtn: tbitbtn;
    rightdownbtn: tbitbtn;
    rightupbtn: tbitbtn;
    offsetclosebtn: tspeedbutton;
    yoffsetedit: tspinedit;
    xoffsetedit: tspinedit;
    statuslabel: tlabel;
    screen: tbgravirtualscreen;
    divideselpm: tmenuitem;
    fitmi: tmenuitem;
    n9: tmenuitem;
    viewmi: tmenuitem;
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
    procedure move2originmiclick   (sender: tobject);
    procedure propertiesmiclick    (sender: tobject);
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
    procedure sf001miclick         (sender: tobject);
    procedure toolpathmiclick      (sender: tobject);
    // MAIN-MENU::VIEW
    procedure zoommiclick          (sender: tobject);
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
    // panels event
    procedure previewspinediteditingdone (sender: tobject);
    procedure scaleupdatebtnclick        (sender: tobject);
    procedure scaleclosebtnclick         (sender: tobject);
    procedure offsetupdatebtnclick       (sender: tobject);
    procedure leftupbtnclick             (sender: tobject);
    procedure penupbtnclick              (sender: tobject);
  private
         bit: tbgrabitmap;
 mouseisdown: boolean;
          px: longint;
          py: longint;

        page: tvpelementlist;
   pagecount: longint;
   pagewidth: longint;
  pageheight: longint;
        path: tvppath;
        zoom: vpfloat;

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

{$R *.lfm}

uses
  math, sysutils, importfrm, aboutfrm, propertiesfrm, setupfrm, vpdriver,
  vpdriverthread, vpsketcher, vpsvgreader, vpdxfreader, vpsetting, vpwave;

// FORM EVENTS

procedure tmainform.formcreate(sender: tobject);
var
  wavemesh: twavemesh;
  mx: longint = 0;
  my: longint = 0;
begin
  // enable debug functions
  previewmi .visible := enabledebug;
  n4        .visible := enabledebug;
  toolpathmi.visible := enabledebug;
  // load setting
  setting := tvpsetting.create;
  setting.load(changefileext(paramstr(0), '.ini'));
  // create plotter driver
  driver        := tvpdriver.create;
  driver.xdelay := setting.m0delay;
  driver.ydelay := setting.m1delay;
  driver.zdelay := setting.mzdelay;
  // init space wave
  wavemesh[0] := setting.spacewave0;
  wavemesh[1] := setting.spacewave1;
  wavemesh[2] := setting.spacewave2;
  wavemesh[3] := setting.spacewave3;
  wavemesh[4] := setting.spacewave4;
  wavemesh[5] := setting.spacewave5;
  wavemesh[6] := setting.spacewave6;
  wavemesh[7] := setting.spacewave7;
  wavemesh[8] := setting.spacewave8;
  spacewave := tspacewave.create(
    setting.spacewavedxmax,
    setting.spacewavedymax,
    wavemesh);
  spacewave.enabled := setting.spacewaveoff = 1;
  spacewave.debug;
  // create preview and empty path
   bit := tbgrabitmap.create;
  page := tvpelementlist.create;
  path := tvppath.create;
  // update virtual screen
  a0miclick(a3mi);
  // initialize driver
  calc_(setting.point8, mx, my);
  driver.init(mx, my);
  // update panels
  scalepanel      .anchors := [akleft, akright, aktop];
  offsetpanel     .anchors := [akleft, akright, aktop];
  calibrationpanel.anchors := [akleft, akright, aktop];
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  movetohomemiclick(sender);
  //
  bit.destroy;
  page.destroy;
  path.destroy;
  //
  spacewave.destroy;
  setting.destroy;
  driver.destroy;
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
    page.clear;
    page.load(opendialog.filename);
    page.interpolate(0.5);
    pagecount := page.count;
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
    page.save(changefileext(savedialog.filename, '.vplot'));
    unlock2;
  end;
end;

procedure tmainform.clearmiclick(sender: tobject);
begin
  lock2;
  path.clear;
  page.clear;
  page.interpolate(0.5);
  pagecount := page.count;
  fitmiclick(sender);
  unlock2;

  caption := 'vPlotter';
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
    if (lowercase(extractfileext(opendialog.filename)) = '.dxf') then
    begin
      dxf2paths(opendialog.filename, page);
      page.interpolate(0.5);
      page.createtoolpath;
    end else
    if (lowercase(extractfileext(opendialog.filename)) = '.svg') then
    begin
      svg2paths(opendialog.filename, page);
      page.interpolate(0.5);
      page.createtoolpath;
    end else
    if (lowercase(extractfileext(opendialog.filename)) = '.bmp') or
       (lowercase(extractfileext(opendialog.filename)) = '.png') then
    begin
      importform.imcb.itemindex := 0;
      importform.imcb .enabled  := true;
      importform.ipwse.enabled  := true;
      importform.pwse .enabled  := true;
      importform.dsfse.enabled  := true;
      if importform.showmodal = mrok then
      begin
        page.clear;
        bit.canvas.clear;
        bit.loadfromfile(opendialog.filename);
        case (importform.imcb.itemindex + 1) of
          1: sk := tvpsketchersquare.create(bit);
          2: sk := tvpsketcherroundedsquare.create(bit);
          3: sk := tvpsketchertriangular.create(bit);
        else sk := tvpsketchersquare.create(bit);
        end;
        sk.patternbw := importform.ipwse.value;
        sk.patternbh := importform.ipwse.value;
        sk.patternw  := importform. pwse.value;
        sk.patternh  := importform. pwse.value;
        sk.dotsize   := importform.dsfse.value;
        sk.update(page);
        sk.destroy;
      end;
    end;
    pagecount := page.count;
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
  page.rotate(degtorad(90));
  updatescreen;
  unlock2;
end;

procedure tmainform.rotate180miclick(sender: tobject);
begin
  lock2;
  page.rotate(degtorad(180));
  updatescreen;
  unlock2;
end;

procedure tmainform.rotate270miclick(sender: tobject);
begin
  lock2;
  page.rotate(degtorad(270));
  updatescreen;
  unlock2;
end;

procedure tmainform.mirrorxmiclick(sender: tobject);
begin
  lock2;
  page.mirrorx;
  updatescreen;
  unlock2;
end;

procedure tmainform.mirrorymiclick(sender: tobject);
begin
  lock2;
  page.mirrory;
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
  page.scale(scaleedit.value);
  updatescreen;
  unlock2;
end;

procedure tmainform.offsetupdatebtnclick(sender: tobject);
begin
  lock2;
  page.move(
    xoffsetedit.value,
    yoffsetedit.value);
  updatescreen;
  unlock2;
end;

procedure tmainform.move2originmiclick(sender: tobject);
begin
  lock2;
  page.movetoorigin;
  updatescreen;
  unlock2;
end;

procedure tmainform.propertiesmiclick(sender: tobject);
begin
  lock2;
  path.update(page,
    pagewidth /2+1,
    pageheight/2+1);

  propertiesform.listbox.clear;
  propertiesform.listbox.additem(format('Points : %12.0u   ', [path.count     ]), nil);
  propertiesform.listbox.additem(format('Length : %12.0f mm', [path.pathlength]), nil);
  propertiesform.listbox.additem(format('Raises : %12.0u   ', [path.pathraises]), nil);
  propertiesform.listbox.additem(format('Vertex : %12.0u   ', [path.pathvertex]), nil);
  propertiesform.listbox.clearselection;

  propertiesform.showmodal;
  unlock2;
end;

procedure tmainform.a0miclick(sender: tobject);
var
  amin: longint = 297;
  amax: longint = 420;
begin
  lock2;
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
  unlock2;
end;

procedure tmainform.horizontalmiclick(sender: tobject);
var
  amin: longint;
  amax: longint;
begin
  lock2;
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
  updatescreen;
  unlock2;
end;

procedure tmainform.toolpathmiclick(sender: tobject);
begin
  lock2;
  page.select(false);
  page.createtoolpath;
  updatescreen;
  unlock2;
end;

// MAIN MENU::VIEW

procedure tmainform.zoommiclick(sender: tobject);
var
  i: longint;
begin
  if sender = zoom50mi   then zoom :=  0.50;
  if sender = zoom75mi   then zoom :=  0.75;
  if sender = zoom100mi  then zoom :=  1.00;
  if sender = zoom125mi  then zoom :=  1.25;
  if sender = zoom150mi  then zoom :=  1.50;
  if sender = zoom175mi  then zoom :=  1.75;
  if sender = zoom200mi  then zoom :=  2.00;
  if sender = zoom250mi  then zoom :=  2.50;
  if sender = zoom300mi  then zoom :=  3.00;
  if sender = zoom350mi  then zoom :=  3.50;
  if sender = zoom400mi  then zoom :=  4.00;
  if sender = zoom600mi  then zoom :=  6.00;
  if sender = zoom800mi  then zoom :=  8.00;
  if sender = zoom1000mi then zoom := 10.00;
  if sender = zoom1500mi then zoom := 15.00;
  if sender = zoom2000mi then zoom := 20.00;
  if sender = zoom3000mi then zoom := 30.00;

  for i := 0 to viewmi.count - 1 do
  begin
    viewmi.items[i].checked := viewmi.items[i] = sender;
  end;

  movex := (screen.width  - round(pagewidth *zoom)) div 2;
  movey := (screen.height - round(pageheight*zoom)) div 2;
  updatescreen;
end;

procedure tmainform.fitmiclick(sender: tobject);
begin
  zoommiclick(zoom100mi);
end;

// MAIN MENU::PREVIEW

procedure tmainform.sf001miClick(sender: tobject);
begin
  lock2;
  if sender = sf001mi then inc(pagecount,   1) else
  if sender = sb001mi then dec(pagecount,   1) else
  if sender = sf005mi then inc(pagecount,   5) else
  if sender = sb005mi then dec(pagecount,   5) else
  if sender = sf010mi then inc(pagecount,  10) else
  if sender = sb010mi then dec(pagecount,  10) else
  if sender = sf025mi then inc(pagecount,  25) else
  if sender = sb025mi then dec(pagecount,  25) else
  if sender = sf050mi then inc(pagecount,  50) else
  if sender = sb050mi then dec(pagecount,  50) else
  if sender = sf100mi then inc(pagecount, 100) else
  if sender = sb100mi then dec(pagecount, 100) else
  if sender =  mtfsmi then pagecount := 1      else
    pagecount := page.count;

  pagecount := max(0, min(pagecount, page.count));
  updatescreen;
  unlock2;
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
    driverthread         := tvpdriverthread.create(path);
    driverthread.xcenter := setting.point8.x;
    driverthread.ycenter := setting.point8.y+
                            setting.yfactor*(pageheight)+
                            setting.yoffset;

    driverthread.onstart := @onplotterstart;
    driverthread.onstop  := @onplotterstop;
    driverthread.ontick  := @onplottertick;
    path.update(page,
      pagewidth /2+1,
      pageheight/2+1);
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
  driver.zcount := setting.mzmax;
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
  driver.zcount := setting.mzmax;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;

  calc_(setting.point8, mx, my);
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
  page.select(true);
  updatescreen;
  unlock2;
end;

procedure tmainform.selbylayerpmclick(sender: tobject);
var
     i: longint;
  elem: tvpelement;
begin
  lock2;
  for i := 0 to page.count -1 do
  begin
    elem := page.items[i];
    if elem.selected then
      page.select(true, elem.layer);
  end;
  updatescreen;
  unlock2;
end;

procedure tmainform.invertselpmclick(sender: tobject);
begin
  lock2;
  page.invertselected;
  updatescreen;
  unlock2;
end;

procedure tmainform.deselallpmclick(sender: tobject);
begin
  lock2;
  page.select(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.showallpmclick(sender: tobject);
begin
  lock2;
  page.hide(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.showbylayerpmclick(sender: tobject);
var
     i: longint;
  elem: tvpelement;
begin
  lock2;
  for i := 0 to page.count -1 do
  begin
    elem := page.items[i];
    if elem.selected then
      page.hide(false, elem.layer);
  end;
  updatescreen;
  unlock2;
end;

procedure tmainform.inverthiddenpmclick(sender: tobject);
begin
  lock2;
  page.inverthidden;
  page.select(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hideallpmclick(sender: tobject);
begin
  lock2;
  page.hide(true);
  page.select(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hidebylayerpmclick(sender: tobject);
var
     i: longint;
  elem: tvpelement;
begin
  lock2;
  for i := 0 to page.count -1 do
  begin
    elem := page.items[i];
    if elem.selected then
      page.hide(true, elem.layer);
  end;
  page.select(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.hideselpmclick(sender: tobject);
var
     i: longint;
  elem: tvpelement;
begin
  lock2;
  for i := 0 to page.count -1 do
  begin
    elem := page.items[i];
    if elem.selected then
      elem.hidden := true;
  end;
  page.select(false);
  updatescreen;
  unlock2;
end;

procedure tmainform.mergeselclick(sender: tobject);
begin
  lock2;
  page.mergeselected;
  updatescreen;
  unlock2;
end;

procedure tmainform.selattachedpmclick(sender: tobject);
begin
  lock2;
  page.selectattached;
  updatescreen;
  unlock2;
end;

procedure tmainform.divideselpmclick(sender: tobject);
begin
  lock2;
  page.unmergeselected;
  updatescreen;
  unlock2;
end;

// MOUSE EVENTS

procedure tmainform.imagemousedown(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
var
   i, j: longint;
   elem: tvpelement;
  point: tvppoint;
begin
  if locked then exit;
  popup.autopopup := true;

  if (ssctrl in shift) then
  begin
    popup.autopopup := false;
    // search path ...
    for i := 0 to page.count -1 do
    begin
      elem := page.items[i];
      for j := 0 to elem.count -1 do
      begin
        point   := elem.items[j]^;
        point.x := (bit.width  div 2) + point.x*zoom;
        point.y := (bit.height div 2) - point.y*zoom;

        if (abs(point.x + movex - x)<2) and
           (abs(point.y + movey - y)<2) then
          if elem.hidden = false then
          begin
            elem.selected := button = mbleft;
          end;
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
  if locked then
  begin
    exit;
  end;
  mouseisdown := false;
end;

// PANEL EVENTS

procedure tmainform.previewspinediteditingdone(sender: tobject);
begin
  lock2;
  updatescreen;
  unlock2;
end;

procedure tmainform.leftupbtnclick(sender: tobject);
var
  mx: longint = 0;
  my: longint = 0;
begin
  lock2;
  driver.xoff   := false;
  driver.yoff   := false;
  driver.zoff   := false;
  driver.zcount := setting.mzmax;

  if sender = leftupbtn    then driver.xcount := driver.xcount - edit.value;
  if sender = leftdownbtn  then driver.xcount := driver.xcount + edit.value;
  if sender = rightupbtn   then driver.ycount := driver.ycount - edit.value;
  if sender = rightdownbtn then driver.ycount := driver.ycount + edit.value;

  calc_(setting.point8, mx, my);
  driver.init(mx, my);
  unlock2;
end;

procedure tmainform.penupbtnclick(sender: tobject);
begin
  lock2;
  if sender = penupbtn then
    driver.zcount := setting.mzmax
  else
  if sender = pendownbtn then
    driver.zcount := setting.mzmin;
  unlock2;
end;

// SCREEN EVENTS

procedure tmainform.updatescreen;
var
  i, j: longint;
     k: longint = 0;
  elem: tvpelement;
    p1: tvppoint;
    p2: tvppoint;
begin
  bit.setsize(
    round(pagewidth *zoom),
    round(pageheight*zoom));
  bit.fillrect(0, 0, bit.width,   bit.height,   bgra(255,   0,   0), dmset);
  bit.fillrect(1, 1, bit.width-1, bit.height-1, bgra(255, 255, 255), dmset);
  // updtare preview ...
  for i := 0 to min(pagecount, page.count) -1 do
  begin
    elem := page.items[i];
    if (elem.hidden = false) then
    begin

      p1    := elem.items[0]^;
      p1.x  := (bit.width  div 2) + p1.x*zoom;
      p1.y  := (bit.height div 2) - p1.y*zoom;

      for j := 1 to elem.count -1 do
      begin
        p2   := elem.items[j]^;
        p2.x := (bit.width  div 2) + p2.x*zoom;
        p2.y := (bit.height div 2) - p2.y*zoom;
        if elem.hidden = false then
        begin
          if elem.selected then
          begin
            inc(k);
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(57, 255, 20), true, dmset);
          end else
          if enabledebug and (j = elem.count - 1) then
          begin
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(57, 255, 20), true, dmset);
          end else
          begin
            bit.drawline(
              round(p1.x), round(p1.y),
              round(p2.x), round(p2.y),
              bgra(0, 0, 0), true, dmset);
          end
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
  lockinternal2(value);
  // override main menu::printer
  startmi.enabled := true;
  stopmi .enabled := true;
  killmi .enabled := true;
end;

procedure tmainform.lockinternal2(value: boolean);
begin
  locked                := not value;
  // main menu::file
  loadmi       .enabled := value;
  savemi       .enabled := value;
  clearmi      .enabled := value;
  importmi     .enabled := value;
  propertiesmi .enabled := value;
  // main menu::editmi
  rotatemi     .enabled := value;
  mirrormi     .enabled := value;
  scalemi      .enabled := value;
  offsetmi     .enabled := value;
  move2originmi.enabled := value;
  pagesizemi   .enabled := value;
  toolpathmi   .enabled := value;
  // main menu::view
  zoom50mi     .enabled := value;
  zoom75mi     .enabled := value;
  zoom100mi    .enabled := value;
  zoom125mi    .enabled := value;
  zoom150mi    .enabled := value;
  zoom175mi    .enabled := value;
  zoom200mi    .enabled := value;
  zoom250mi    .enabled := value;
  zoom300mi    .enabled := value;
  zoom350mi    .enabled := value;
  zoom400mi    .enabled := value;
  zoom600mi    .enabled := value;
  zoom800mi    .enabled := value;
  zoom1000mi   .enabled := value;
  zoom1500mi   .enabled := value;
  zoom2000mi   .enabled := value;
  zoom3000mi   .enabled := value;
  fitmi        .enabled := value;
  // main menu::preview
  sf001mi      .enabled := value;
  sb001mi      .enabled := value;
  sf005mi      .enabled := value;
  sb005mi      .enabled := value;
  sf010mi      .enabled := value;
  sb010mi      .enabled := value;
  sf025mi      .enabled := value;
  sb025mi      .enabled := value;
  sf050mi      .enabled := value;
  sb050mi      .enabled := value;
  sf100mi      .enabled := value;
  sb100mi      .enabled := value;
   mtfsmi      .enabled := value;
   mtlsmi      .enabled := value;
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
  driver.zcount := setting.mzmax;

  unlock1;
  statuslabel.caption := '';
  application.processmessages;
end;

procedure tmainform.onplottertick;
begin
  inc(tickcount);
  if (tickcount mod 800) = 0 then
  begin
    //statuslabel.caption := 'Remaining Time ' +
    //  formatdatetime('hh:nn:ss', (now-starttime)/tickcount*driverthread.tick);
  end;
  application.processmessages;
end;

end.

