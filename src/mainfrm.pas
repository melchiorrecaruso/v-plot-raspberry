
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
  comctrls, buttons, menus, inifiles, libvplot;

type
  { tmainform }

  tmainform = class(tform)
    leftrightbtn: TUpDown;
    loadbtn: TBitBtn;
    morebtn: TBitBtn;
    panbtn: TBitBtn;
    pausebtn: TBitBtn;
    playbtn: TBitBtn;
    previewimage: TImage;
    opendialog: topendialog;
    prvwbtn: TCheckBox;
    stopbtn: TBitBtn;
    timer: ttimer;
    updownbtn: TUpDown;

    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure leftrightbtnclick(sender: tobject; button: tudbtntype);

    procedure updownbtnclick(sender: tobject; button: tudbtntype);
    procedure panbtnclick(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playorstopbtnclick(sender: tobject);
    procedure morebtnclick(sender: tobject);
    procedure timertimer(sender: tobject);
  private
    bmp:     tbitmap;
    inlist:  tstringlist;
    inifile: tinifile;
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
  loadbtn .enabled := true;
  playbtn .enabled := false;
  pausebtn.enabled := false;
  stopbtn .enabled := false;
  morebtn .enabled := true;
  // ---
  bmp         := tbitmap.create;
  inlist      := tstringlist.create;
  inifile     := tinifile.create(changefileext(paramstr(0), '.ini'));
  vplotdriver := tvplotdriver.create(2);
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
    playbtn .enabled := true;
    pausebtn.enabled := false;
    stopbtn .enabled := false;
    morebtn .enabled := true;
    // ---
    inlist.clear;
    inlist.loadfromfile(opendialog.filename);
    // ---
    vplotcoder         := tvplotcoder.create(inlist, inifile);
    vplotcoder.ontick  := @ontick;
    vplotcoder.enabled := false;
    // ---
    bmp.setsize(
      round(vplotcoder.width),
      round(vplotcoder.height));
    bmp.canvas.pen.color   := clwhite;
    bmp.canvas.brush.color := clwhite;
    bmp.canvas.brush.style := bssolid;
    bmp.canvas.fillrect(0,0,
      round(vplotcoder.width),
      round(vplotcoder.height));

    previewimage.picture.bitmap.setsize(
      round(vplotcoder.width),
      round(vplotcoder.height));
    previewimage.canvas.pen.color   := clwhite;
    previewimage.canvas.brush.color := clwhite;
    previewimage.canvas.brush.style := bssolid;
    previewimage.canvas.rectangle(0, 0,
      round(vplotcoder.width),
      round(vplotcoder.height));

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
    if sender = playbtn then
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
    playbtn .enabled := not vplotcoder.enabled;
    pausebtn.enabled :=     vplotcoder.enabled;
    stopbtn .enabled :=     vplotcoder.enabled;
    prvwbtn .enabled := not vplotcoder.enabled;
    morebtn .enabled := false;
  end;
end;

procedure tmainform.morebtnclick(sender: tobject);
var
  c0, c1: longint;
begin
  initform.showmodal;
  vplotcoder.gethome(c0, c1);
  vplotdriver.init  (c0, c1, 1);
end;

procedure Tmainform.ontick;
begin
  if vplotcoder.pz < 0 then
    bmp.canvas.pixels[
      round(vplotcoder.px),
      round(vplotcoder.height - vplotcoder.py)] := clblack
  else
    bmp.canvas.pixels[
      round(vplotcoder.px),
      round(vplotcoder.height - vplotcoder.py)] := clred;

  vplotdriver.enabled := not prvwbtn.checked;
  with vplotcoder do
    vplotdriver.move2(mot0, mot1, motz);
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
      [(100 * vplotcoder.index) div vplotcoder.count, x div 1000]);
  end else
  begin
    playorstopbtnclick(stopbtn);
    timer   .enabled := false;
    loadbtn .enabled := true;
    playbtn .enabled := false;
    pausebtn.enabled := false;
    stopbtn .enabled := false;
    morebtn .enabled := true;
    caption := format('VPlot Driver - Time elapsed %u secs', [x div 1000]);
  end;
end;

end.

