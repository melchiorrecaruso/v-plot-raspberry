
{ Description: vPlot Main Form.

  Copyright (C) 2014-2017 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  comctrls, buttons, menus, inifiles, libvplot, lnet, lNetComponents;

type
  { tmainform }

  tmainform = class(tform)
    aboutbtn: TBitBtn;
    loadbtn: TBitBtn;
    startbtn: TBitBtn;
    pausebtn: TBitBtn;
    stopbtn: TBitBtn;
    reloadbtn: TBitBtn;
    creativecontrolgb: TGroupBox;
    sethomebtn: TBitBtn;
    bordersbtn: TBitBtn;
    leftdownbtn: TBitBtn;
    rightdownbtn: TBitBtn;
    penupbtn: TBitBtn;
    pendownbtn: TBitBtn;
    leftupbtn: TBitBtn;
    rightupbtn: TBitBtn;
    gohomebtn: TBitBtn;
    leftedit: TEdit;
    rightedit: TEdit;
    connectgb: TGroupBox;
    manualdrivinggb: TGroupBox;
    drawingcontrolgb: TGroupBox;
    leftrightbtn: TUpDown;
    tpc: TLTCPComponent;
    panbtn: TBitBtn;
    previewimage: TImage;
    opendialog: topendialog;
    conbtn: TSpeedButton;
    timer: ttimer;
    updownbtn: TUpDown;

    procedure conbtnClick(Sender: TObject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure leftrightbtnclick(sender: tobject; button: tudbtntype);
    procedure sethomebtnClick(Sender: TObject);

    procedure updownbtnclick(sender: tobject; button: tudbtntype);
    procedure panbtnclick(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playorstopbtnclick(sender: tobject);
    procedure morebtnclick(sender: tobject);
    procedure timertimer(sender: tobject);
  private
    bmp:      tbitmap;
    inlist:   tstringlist;
    inifile:  tinifile;
    procedure ontick;
  end;


var
  mainform: Tmainform;
  x:        longword;

implementation

{$R *.lfm}

uses
  initfrm, math, sysutils;

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  conbtn      .enabled := true;
  leftupbtn   .enabled := false;
  penupbtn    .enabled := false;
  rightupbtn  .enabled := false;
  leftedit    .enabled := false;
  sethomebtn  .enabled := false;
  rightedit   .enabled := false;
  leftdownbtn .enabled := false;
  pendownbtn  .enabled := false;
  rightdownbtn.enabled := false;
  bordersbtn  .enabled := false;
  gohomebtn   .enabled := false;
  loadbtn     .enabled := false;
  reloadbtn   .enabled := false;
  startbtn    .enabled := false;
  pausebtn    .enabled := false;
  stopbtn     .enabled := false;
  aboutbtn    .enabled := true;
  leftrightbtn.enabled := false;
  updownbtn   .enabled := false;
  panbtn      .enabled := false;
  // ---
  bmp     := tbitmap.create;
  inlist  := tstringlist.create;
  inifile := tinifile.create(changefileext(paramstr(0), '.ini'));
  // ---
  conbtnclick(conbtn);




  loadsetup(inifile, vplotsetup);
  vplothome.p.x := vplotsetup.point9.x;
  vplothome.p.y := vplotsetup.point9.y;
  optimize(vplothome, vplotsetup);

  vplotdriver := tvplotdriver.create(vplotsetup.mode);



  vplotdriver.init(vplothome.m0, vplothome.m1, 1);
end;

procedure tmainform.conbtnclick(sender: tobject);
begin
  if conbtn.caption = 'Connect' then
  begin
    tpc.connect('192.168.1.87', 4500);
    if tpc.connected then
      conbtn.caption := 'Disconnect'
  end else
  begin
    if tpc.connected then;
      tpc.disconnect;
    conbtn.caption := 'Connect';
  end;
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  bmp.destroy;
  inlist.destroy;
  inifile.destroy;
  vplotdriver.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(vplotcoder) then
    playorstopbtnclick(stopbtn);

  if assigned(vplotcoder) then
    closeaction := canone
  else
    closeaction := cafree;
end;

procedure tmainform.leftrightbtnclick(sender: tobject; button: tudbtntype);
begin
  if button = btprev then
    previewimage.left := min(2, previewimage.left + 100)
  else
    previewimage.left := max(mainform.width -
                             previewimage.width - 2,
                             previewimage.left  - 100);
end;

procedure tmainform.sethomebtnClick(Sender: TObject);
begin

end;

procedure tmainform.updownbtnclick(sender: tobject; button: tudbtntype);
begin
  if button = btnext then
    previewimage.top := min(2, previewimage.top + 100)
  else
    previewimage.top := max(mainform.height -
                            previewimage.height - 2,
                            previewimage.top    - 100);
end;

procedure tmainform.panbtnclick(sender: tobject);
begin
  previewimage.top  := (mainform.height - previewimage.height) div 2;
  previewimage.left := (mainform.width  - previewimage.width)  div 2;
end;

procedure tmainform.loadbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    loadbtn .enabled := false;
    startbtn .enabled := true;
    pausebtn.enabled := false;
    stopbtn .enabled := false;
    aboutbtn .enabled := true;
    // ---
    inlist.clear;
    inlist.loadfromfile(opendialog.filename);
    // ---
    vplotcoder         := tvplotcoder.create(inlist);
    vplotcoder.ontick  := @ontick;
    vplotcoder.enabled := false;
    // ---
    bmp.setsize(
      round(vplotsetup.point1.x),
      round(vplotsetup.point1.y));
    bmp.canvas.pen.color   := clwhite;
    bmp.canvas.brush.color := clwhite;
    bmp.canvas.brush.style := bssolid;
    bmp.canvas.fillrect(0,0,
      round(vplotsetup.point1.x),
      round(vplotsetup.point1.y));

    previewimage.picture.bitmap.setsize(
      round(vplotsetup.point1.x),
      round(vplotsetup.point1.y));
    previewimage.canvas.pen.color   := clwhite;
    previewimage.canvas.brush.color := clwhite;
    previewimage.canvas.brush.style := bssolid;
    previewimage.canvas.rectangle(0, 0,
      round(vplotsetup.point1.x),
      round(vplotsetup.point1.y));

    previewimage.anchors := [aktop, akleft, akright, akbottom];
    previewimage.anchors := [];

    previewimage.stretchinenabled  := false;
    previewimage.stretchoutenabled := false;
    previewimage.stretch           := false;
    panbtnclick(panbtn);
    // ---
    vplotcoder.start;
    // ---
    x := 0;
  end;
end;

procedure tmainform.playorstopbtnclick(sender: tobject);
begin
  if assigned(vplotcoder) then
  begin
    if sender = startbtn then
      vplotcoder.enabled := true
    else
    if sender = pausebtn then
      vplotcoder.enabled := false
    else
    if sender = stopbtn then
    begin
      vplotcoder.terminate;
      vplotcoder.enabled := true;
    end;

    timer   .enabled :=     vplotcoder.enabled;
    startbtn .enabled := not vplotcoder.enabled;
    pausebtn.enabled :=     vplotcoder.enabled;
    stopbtn .enabled :=     vplotcoder.enabled;
    aboutbtn .enabled := false;
  end;
end;

procedure tmainform.morebtnclick(sender: tobject);
begin

  initform.showmodal;

  with vplotcoder do
    vplotdriver.init(vplothome.m0, vplothome.m1, 1);
end;

procedure Tmainform.ontick;
begin
  if vplotcoder.pz < 0 then
    bmp.canvas.pixels[
      round(                      vplotcoder.px),
      round(vplotsetup.point1.y - vplotcoder.py)] := clblack
  else
    bmp.canvas.pixels[
      round(                      vplotcoder.px),
      round(vplotsetup.point1.y - vplotcoder.py)] := clred;



  with vplotcoder do
    vplotdriver.move2(mot0, mot1, motz);
  sleep(5);
  application.processmessages;
end;

procedure tmainform.timertimer(Sender: TObject);
begin
  inc(x, timer.interval);
  previewimage.canvas.copyrect(
    previewimage.canvas.cliprect,
    bmp.canvas,
    bmp.canvas.cliprect);
  previewimage.invalidate;
  if assigned(vplotcoder) then
  begin
    caption := format('VPlot Driver - Drawing %u%% - Time elapsed %u secs',
      [(100 * vplotcoder.listindex) div vplotcoder.listcount, x div 1000]);
  end else
  begin
    playorstopbtnclick(stopbtn);
    timer   .enabled := false;
    loadbtn .enabled := true;
    startbtn .enabled := false;
    pausebtn.enabled := false;
    stopbtn .enabled := false;
    aboutbtn .enabled := true;
    caption := format('VPlot Driver - Time elapsed %u secs', [x div 1000]);
  end;
end;

end.

