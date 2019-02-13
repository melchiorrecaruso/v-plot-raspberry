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

unit clientfrm;

{$mode objfpc}

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls, comctrls,
  buttons, menus, spin, vppaths, vpsetting, bgrabitmap,  types,                                         
  bgrabitmaptypes, bgravirtualscreen, lnetcomponents, bgragradientscanner, sha1, lnet, zstream;

type
  { Tclientform }

  Tclientform = class(tform)
    printmi: TMenuItem;
    scalebevelleft: TBevel;
    offsetbevelleft: TBevel;
    calibrationbevelleft: TBevel;
    scalelabel: TLabel;
    scalebevel: TBevel;
    clientformbevel: TBevel;
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
    ltcp: TLTCPComponent;
    calibrationpanel: TPanel;
    disconnectmi: TMenuItem;
    calibrationmi: TMenuItem;
    n10: TMenuItem;
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
    connectmi: tmenuitem;
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


    procedure disconnectmiClick(Sender: TObject);
    procedure formcreate           (sender: tobject);
    procedure formdestroy          (sender: tobject);
    procedure leftupbtnClick(Sender: TObject);
    // MAIN MENU::FILE
    procedure loadmiclick          (sender: tobject);
    procedure ltcpCanSend(aSocket: TLSocket);
    procedure ltcpConnect(aSocket: TLSocket);
    procedure ltcpDisconnect(aSocket: TLSocket);
    procedure ltcpError(const msg: string; aSocket: TLSocket);
    procedure ltcpReceive(aSocket: TLSocket);
    procedure printmiClick(Sender: TObject);
    procedure offsetupdatebtnClick(Sender: TObject);
    procedure penupbtnClick(Sender: TObject);
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
    procedure scaleupdatebtnClick(Sender: TObject);
    procedure scaleclosebtnClick(Sender: TObject);
    procedure scalemiclick         (sender: tobject);

    procedure a0miclick            (sender: tobject);
    procedure horizontalmiclick    (sender: tobject);
    // MAIN-MENU::VIEW
    procedure zoomoutmiclick       (sender: tobject);
    procedure zoominmiclick        (sender: tobject);
    procedure fitmiclick           (sender: tobject);
    // MAIN-MENU::PRINTER
    procedure connectmiclick       (sender: tobject);
    procedure startmiclick         (sender: tobject);
    procedure stopmiclick          (sender: tobject);
    procedure killmiclick          (sender: tobject);

    procedure movetohomemiclick    (sender: tobject);
    procedure toolpathmiclick      (sender: tobject);
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
 mouseisdown: boolean;
          px: longint;
          py: longint;

   pagewidth: longint;
  pageheight: longint;
       paths: tvppaths;
        zoom: single;


      buffer: ansistring;
    bufindex: longint;

       movex: longint;
       movey: longint;
      locked: boolean;

    // ---
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
  clientform: Tclientform;


implementation

{$R *.lfm}

uses
  math, sysutils, aboutfrm,
  vpmath, vpsvgreader, vpdxfreader, vpwave, sketchyimage;

// FORM EVENTS

procedure tclientform.formcreate(sender: tobject);
var
  wavemesh: twavemesh;
begin
  // buffer
  buffer := '';
  // load setting
  setting := tvpsetting.create;
  setting.load(extractfilepath(paramstr(0)) + 'vplot.ini');
  // create preview and empty paths
    bit := tbgrabitmap.create;
  paths := tvppaths.create;
  // update virtual screen
  a0miclick(a3mi);
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
    setting.wavexmax,
    setting.waveymax,
    wavemesh);
  wave.enabled := false;
  wave.test;
  // update panels
  scalepanel      .anchors := [akleft, akright, aktop];
  offsetpanel     .anchors := [akleft, akright, aktop];
  calibrationpanel.anchors := [akleft, akright, aktop];
end;

procedure tclientform.formdestroy(sender: tobject);
begin
  if ltcp.connected then
    ltcp.disconnect(true);
  wave.destroy;
  paths.destroy;
  bit.destroy;
  setting.destroy;
end;

procedure tclientform.leftupbtnclick(sender: tobject);
var
  mx: longint = 0;
  my: longint = 0;
   s: string;
begin
  if sender = leftupbtn    then mx := - edit.value;
  if sender = leftdownbtn  then mx := + edit.value;
  if sender = rightupbtn   then my := - edit.value;
  if sender = rightdownbtn then my := + edit.value;

  if ltcp.connected then
  begin
    //buffer.clear;
    //buffer.add(format('MOVED X%d Y%d Z%d', [mx, my, $F]));
    //buffer.add(format('INIT  X%d Y%d Z%d', [mx, my, $F]));

    //s := sha1print(sha1string(buffer.text));
    //buffer.add(format('SHA1%s', [s]));
    ltcp.sendmessage('SEND');
  end else
    messagedlg('Error', 'Server is disconnected ', mterror, [mbok], 0);
end;

procedure tclientform.penupbtnclick(sender: tobject);
var
  mz: longint = 0;
   s: string;
begin
  if sender = penupbtn   then mz := + $F;
  if sender = pendownbtn then mz := - $F;

  if ltcp.connected then
  begin
    //buffer.clear;
    //buffer.add(format('MOVED X%d Y%d Z%d', [ 0,  0, mz]));

    //s := sha1print(sha1string(buffer.text));
    //buffer.add(format('SHA1%s', [s]));
    ltcp.sendmessage('SEND');
  end else
    messagedlg('Error', 'Server is disconnected ', mterror, [mbok], 0);
end;

// MAIN-MENU::FILE

procedure tclientform.loadmiclick(sender: tobject);
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

procedure tclientform.savemiclick(sender: tobject);
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

procedure tclientform.clearmiclick(sender: tobject);
begin
  caption := 'vPlotter';

  lock2;
  paths.clear;
  fitmiclick(sender);
  unlock2;
end;

procedure tclientform.importmiclick(sender: tobject);
begin
  opendialog.filter := 'Supported files (*.svg, *.dxf)|*.svg; *.dxf';
  if opendialog.execute then
  begin
    caption := 'vPlotter - ' + opendialog.filename;

    lock2;
    paths.clear;
    if lowercase(extractfileext(opendialog.filename)) = '.dxf' then
      dxf2paths(opendialog.filename, paths)
    else
      if lowercase(extractfileext(opendialog.filename)) = '.svg' then
        svg2paths(opendialog.filename, paths);
    //decodePNG(opendialog.filename, 100, 1, 1, 100);
    paths.createtoolpath;
    fitmiclick(sender);
    unlock2;
  end;
end;

procedure tclientform.exitmiclick(sender: tobject);
begin
  close;
end;

// MAIN-MENU::EDIT

procedure Tclientform.rotate90miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(90));
  updatescreen;
  unlock2;
end;

procedure Tclientform.rotate180miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(180));
  updatescreen;
  unlock2;
end;

procedure Tclientform.rotate270miclick(sender: tobject);
begin
  lock2;
  paths.rotate(degtorad(270));
  updatescreen;
  unlock2;
end;

procedure Tclientform.mirrorxmiclick(sender: tobject);
begin
  lock2;
  paths.mirror(true);
  updatescreen;
  unlock2;
end;

procedure Tclientform.mirrorymiclick(sender: tobject);
begin
  lock2;
  paths.mirror(false);
  updatescreen;
  unlock2;
end;

procedure tclientform.scalemiclick(sender: tobject);
begin
  scalepanel      .visible := sender = scalemi;
  calibrationpanel.visible := sender = calibrationmi;
  offsetpanel     .visible := sender = offsetmi;
end;

procedure tclientform.scaleclosebtnclick(sender: tobject);
begin
  scalepanel      .visible := false;
  calibrationpanel.visible := false;
  offsetpanel     .visible := false;
end;

procedure tclientform.scaleupdatebtnclick(sender: tobject);
begin
  lock2;
  paths.scale(scaleedit.value);
  updatescreen;
  unlock2;
end;

procedure tclientform.offsetupdatebtnclick(sender: tobject);
begin
  lock2;
  paths.offset(xoffsetedit.value,
               yoffsetedit.value);
  updatescreen;
  unlock2;
end;

procedure tclientform.a0miclick(sender: tobject);
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

procedure tclientform.horizontalmiclick(sender: tobject);
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

procedure tclientform.toolpathmiclick(sender: tobject);
begin
  lock2;
  paths.selectall(false);
  paths.createtoolpath;
  updatescreen;
  unlock2;
end;

// MAIN MENU::VIEW

procedure tclientform.zoominmiclick(sender: tobject);
var
  value: single;
begin
  value := max(min(zoom*1.5, 25.0), 0.5);

  if value <> zoom then
  begin
    zoom  := value;
    movex := movex + round((bit.width  -(pagewidth *zoom))*(movex)/bit.width );
    movey := movey + round((bit.height -(pageheight*zoom))*(movey)/bit.height);
    updatescreen;
  end;
end;

procedure tclientform.zoomoutmiclick(sender: tobject);
var
  value: single;
begin
  value := max(min(zoom/1.5, 25.0), 0.5);

  if value <> zoom then
  begin
    zoom  := value;
    movex := movex + round((bit.width  -(pagewidth *zoom))*(movex)/bit.width );
    movey := movey + round((bit.height -(pageheight*zoom))*(movey)/bit.height);
    updatescreen;
  end;
end;

procedure tclientform.fitmiclick(sender: tobject);
begin
  zoom  := 1.0;
  movex := (screen.width  - pagewidth ) div 2;
  movey := (screen.height - pageheight) div 2;
  updatescreen;
end;

// MAIN MENU::PRINT

procedure tclientform.connectmiclick(sender: tobject);
begin
  ltcp.connect(setting.ip, setting.port);
end;

procedure tclientform.disconnectmiclick(sender: tobject);
begin
  scalemiclick(sender);
  ltcp.disconnect(false);
end;

procedure Tclientform.printmiclick(sender: tobject);
begin
  lock2;
  if ltcp.connected then
  begin
    buffer := '';
    optimize2(paths,
      setting.layout8.x,
      setting.layout8.y,
      setting.wavexmax,
      setting.waveymax,
      buffer);

    writeln(length(buffer));

    bufindex := 1;
    //ltcp.sendmessage('SEND');
    ltcpcansend(nil);
  end;
  unlock2;
end;

procedure tclientform.startmiclick(sender: tobject);
begin
  if ltcp.connected then
    ltcp.sendmessage('START');
end;

procedure tclientform.stopmiclick(sender: tobject);
begin
  if ltcp.connected then
    ltcp.sendmessage('STOP');
end;

procedure tclientform.killmiclick(sender: tobject);
begin
  if ltcp.connected then
    ltcp.sendmessage('KILL');
end;

procedure Tclientform.movetohomemiclick(sender: tobject);
begin
  if ltcp.connected then
    ltcp.sendmessage('HOME');
end;

// MAIN-MENU::HELP

procedure Tclientform.aboutmiclick(sender: tobject);
var
  about: taboutform;
begin
  about := taboutform.create(nil);
  about.showmodal;
  about.destroy;
end;

// POPUP-MENU

procedure Tclientform.selallpmclick(sender: tobject);
begin
  lock2;
  paths.selectall(true);
  updatescreen;
  unlock2;
end;

procedure Tclientform.selbylayerpmclick(sender: tobject);
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

procedure Tclientform.invertselpmclick(sender: tobject);
begin
  lock2;
  paths.invertselected;
  updatescreen;
  unlock2;
end;

procedure Tclientform.deselallpmclick(sender: tobject);
begin
  lock2;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure Tclientform.showallpmclick(sender: tobject);
begin
  lock2;
  paths.showall(true);
  updatescreen;
  unlock2;
end;

procedure Tclientform.showbylayerpmclick(sender: tobject);
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

procedure Tclientform.inverthiddenpmclick(sender: tobject);
begin
  lock2;
  paths.inverthidden;
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure Tclientform.hideallpmclick(sender: tobject);
begin
  lock2;
  paths.showall(false);
  paths.selectall(false);
  updatescreen;
  unlock2;
end;

procedure Tclientform.hidebylayerpmclick(sender: tobject);
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

procedure Tclientform.hideselpmclick(sender: tobject);
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

procedure Tclientform.mergeselclick(sender: tobject);
begin
  lock2;
  paths.mergeselected;
  updatescreen;
  unlock2;
end;

procedure Tclientform.selattachedpmclick(sender: tobject);
begin
  lock2;
  paths.selectattached;
  updatescreen;
  unlock2;
end;

procedure Tclientform.divideselpmclick(sender: tobject);
begin
  lock2;
  paths.unmergeselected;
  updatescreen;
  unlock2;
end;

// MOUSE EVENTS

procedure tclientform.imagemousedown(sender: tobject;
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

procedure Tclientform.imagemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  if locked = false then
    if mouseisdown then
    begin
      movex := x - px;
      movey := y - py;
      screen.redrawbitmap;
    end;
end;

procedure Tclientform.imagemouseup(sender: tobject;
  button: tmousebutton; shift: tshiftstate; x, y: integer);
begin
  if locked = false then
  begin
    mouseisdown := false;
  end;
end;

procedure Tclientform.screenmousewheel(sender: tobject; shift: tshiftstate;
  wheeldelta: integer; mousepos: tpoint; var handled: boolean);
var
  value: single;
begin
  if locked = false then
  begin
    locked := true;
    if wheeldelta > 0 then
      value := max(min(zoom*1.5, 25.0), 0.5)
    else
      value := max(min(zoom/1.5, 25.0), 0.5);

    if value <> zoom then
    begin
      zoom  := value;
      movex := movex + round((bit.width  -(pagewidth *zoom))*(mousepos.x-movex)/bit.width );
      movey := movey + round((bit.height -(pageheight*zoom))*(mousepos.y-movey)/bit.height);
      updatescreen;
    end;
    locked := false;
  end;
end;

// SCREEN EVENTS

procedure Tclientform.updatescreen;
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

procedure Tclientform.screenredraw(sender: tobject; bitmap: tbgrabitmap);
begin
  bitmap.putimage(movex, movey, bit, dmset);
end;

// LOCK/UNLOCK ROUTINES

procedure Tclientform.lockinternal1(value: boolean);
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
//connectmi    .enabled := value;
//disconnectmi .enabled := value;
//startmi      .enabled := true;
//stopmi       .enabled := true;
//killmi       .enabled := true;
//calibrationmi.enabled := value;
//movetohomemi .enabled := value;
  // main menu::help
  aboutmi      .enabled := value;
  // popup menu
  if value = false then
    screen.popupmenu := nil
  else
    screen.popupmenu := popup;
  application  .processmessages;
end;

procedure Tclientform.lockinternal2(value: boolean);
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
//connectmi    .enabled := value;
//disconnectmi .enabled := value;
//startmi      .enabled := value;
//stopmi       .enabled := value;
//killmi       .enabled := value;
//calibrationmi.enabled := value;
//movetohomemi .enabled := value;
  // main menu::help
  aboutmi      .enabled := value;
  // popup menu
  if value = false then
    screen.popupmenu := nil
  else
    screen.popupmenu := popup;
  application  .processmessages;
end;

procedure Tclientform.lock1;
begin
  lockinternal1(false);
end;

procedure Tclientform.unlock1;
begin
  lockinternal1(true);
end;

procedure Tclientform.lock2;
begin
  lockinternal1(false);
end;

procedure Tclientform.unlock2;
begin
  lockinternal1(true);
end;

// LTCP EVENTS

procedure Tclientform.ltcpconnect(asocket: tlsocket);
begin
  connectmi    .enabled := false;
  disconnectmi .enabled := true;
  startmi      .enabled := true;
  stopmi       .enabled := true;
  killmi       .enabled := true;
  calibrationmi.enabled := true;
  movetohomemi .enabled := true;
end;

procedure tclientform.ltcpdisconnect(asocket: tlsocket);
begin
  connectmi    .enabled := true;
  disconnectmi .enabled := false;
  startmi      .enabled := false;
  stopmi       .enabled := false;
  killmi       .enabled := false;
  calibrationmi.enabled := false;
  movetohomemi .enabled := false;
end;

procedure tclientform.ltcperror(const msg: string; asocket: tlsocket);
begin
  messagedlg('Server Error', msg , mterror, [mbok], 0);
end;

procedure tclientform.ltcpreceive(asocket: tlsocket);
var
  m: ansistring;
begin
  if ltcp.getmessage(m) > 0 then
  begin
    (*
    if buffer.count > 0 then
    begin
      m := buffer[0];
      buffer.delete(0);
      ltcp.sendmessage(m);
    end;
    *)
  end;
end;

procedure tclientform.ltcpcansend(asocket: tlsocket);
var
  sent: longint;

begin
  writeln('ltcpcansend');
  writeln(bufindex, ' ', length(buffer));

  if bufindex <= length(buffer) then
  begin
    writeln(bufindex, ' ', length(buffer));
    repeat
      sent := ltcp.send(buffer[bufindex], min(length(buffer)-bufindex + 1, $ff));
      inc(bufindex, sent);


    until (bufindex > length(buffer)) or (sent = 0);
  end;

end;

end.

