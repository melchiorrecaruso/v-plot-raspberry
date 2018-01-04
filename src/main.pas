
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

unit main;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus;

type

  { tmainform }

  tmainform = class(tform)
    bevel2: tbevel;
    prvwbtn: TCheckBox;
    panbtn: tbitbtn;
    previewimage: timage;
    loadbtn: tbitbtn;
    morebtn: tbitbtn;
    btnpanel: tpanel;
    previewpanel: tpanel;
    playbtn: tbitbtn;
    progressbar: tprogressbar;
    opendialog: topendialog;
    stopbtn: tbitbtn;
    timer: ttimer;
    updownbtn: tupdown;
    leftrightbtn: tupdown;

    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure leftrightbtnclick(sender: tobject; button: tudbtntype);

    procedure updownbtnclick(sender: tobject; button: tudbtntype);
    procedure panbtnclick(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playorstopbtnclick(sender: tobject);
    procedure prvwbtnchange(sender: tobject);
    procedure morebtnclick(sender: tobject);
    procedure timertimer(sender: tobject);
  private
    procedure ondraw;
    procedure ondrawn;
  end;


var
  mainform: Tmainform;
  x:        longword;

implementation

{$R main.lfm}

uses
  initform, libvplot, math;

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  loadbtn.enabled := true;
  playbtn.enabled := false;
  stopbtn.enabled := false;
  morebtn.enabled := true;
  // ---
  vplotdriver     := tvplotdriver.create;
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  vplotdriver.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(vplotcoder) then
    vplotcoder.terminate;

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
    previewimage.left := max(previewpanel.width -
                             previewimage.width - 2,
                             previewimage.left  - 100);
end;

procedure tmainform.updownbtnclick(sender: tobject; button: tudbtntype);
begin
  if button = btnext then
    previewimage.top := min(2, previewimage.top + 100)
  else
    previewimage.top := max(previewpanel.height -
                            previewimage.height - 2,
                            previewimage.top    - 100);
end;

procedure tmainform.panbtnclick(sender: tobject);
begin
  previewimage.top  := (previewpanel.height - previewimage.height) div 2;
  previewimage.left := (previewpanel.width  - previewimage.width)  div 2;
end;

procedure tmainform.loadbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    previewimage.picture.bitmap.setsize(775,775);
    previewimage.canvas.pen.color   := clwhite;
    previewimage.canvas.brush.color := clwhite;
    previewimage.canvas.brush.style := bssolid;
    previewimage.canvas.rectangle(0, 0, 775, 775);

    previewimage.anchors := [aktop, akleft, akright, akbottom];
    previewimage.anchors := [];

    previewimage.stretchinenabled  := false;
    previewimage.stretchoutenabled := false;
    previewimage.stretch           := false;

    // progressbar.min      := 0;
    // progressbar.max      := vplotlist.count;
    // progressbar.position := 0;
    // progressbar.smooth   := true;
    // ---
    playbtn.enabled := true;
    stopbtn.enabled := false;
    // ---
    vplotcoder          := tvplotcoder.create;
    vplotcoder.ondraw   := @ondraw;
    vplotcoder.ondrawn  := @ondrawn;
    vplotcoder.filename := opendialog.filename;
    vplotcoder.enabled  := false;
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
    if sender = stopbtn then
      vplotcoder.enabled := false;
    // ---
    timer  .enabled :=     vplotcoder.enabled;
    playbtn.enabled := not vplotcoder.enabled;
    stopbtn.enabled :=     vplotcoder.enabled;
    prvwbtn.enabled := not vplotcoder.enabled;
  end;
end;

procedure tmainform.prvwbtnchange(sender: tobject);
begin
  vplotdriver.enabled := not prvwbtn.checked;
end;

procedure tmainform.morebtnclick(sender: tobject);
begin
  form1.showmodal;
end;

procedure Tmainform.ondraw;
var
  p0, p1: tvplotpoint;
begin
  if assigned(vplotcoder) then
  begin
    previewimage.canvas.pen.color   := clred;
    previewimage.canvas.brush.color := clred;
    previewimage.canvas.brush.style := bssolid;

    vplotcoder.drawn(p0, p1);
    previewimage.canvas.line(
      round(p1.x), round(p1.y),
      round(p0.x), round(p0.y));
  end;
end;

procedure Tmainform.ondrawn;
var
  p0, p1: tvplotpoint;
begin
  if assigned(vplotcoder) then
  begin
    previewimage.canvas.pen.color   := clblack;
    previewimage.canvas.brush.color := clblack;
    previewimage.canvas.brush.style := bssolid;

    vplotcoder.drawn(p0, p1);
    previewimage.canvas.line(
      round(p1.x), round(p1.y),
      round(p0.x), round(p0.y));
  end;
end;

procedure tmainform.timertimer(Sender: TObject);
begin
  inc(x);
  caption := 'VPlot - Time elapsed ' + inttostr(x) + ' secs';
end;





end.

