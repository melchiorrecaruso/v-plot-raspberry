
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
    bevel1: tmenuitem;
    aboutvplot: tmenuitem;
    bevel2: tbevel;
    panbtn: tbitbtn;
    previewimage: timage;
    loadbtn: tbitbtn;
    morebtn: tbitbtn;
    btnpanel: tpanel;
    previewpanel: tpanel;
    playbtn: tbitbtn;
    progressbar: tprogressbar;
    showviewform: tmenuitem;
    moremenu: tpopupmenu;
    opendialog: topendialog;
    stopbtn: tbitbtn;
    timer: ttimer;
    trackbar: ttrackbar;
    updownbtn: tupdown;
    leftrightbtn: tupdown;
    procedure panbtnclick(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playbtnclick(sender: tobject);
    procedure stopbtnclick(sender: tobject);
    procedure morebtnclick(sender: tobject);
    procedure timertimer(sender: tobject);
    procedure updownbtnclick(sender: tobject; button: tudbtntype);
    procedure leftrightbtnclick(sender: tobject; button: tudbtntype);
  private
    vplotlist :tstringlist;
    vplotlistindex: longint;

    procedure formsync1;
    procedure formsync2;
    procedure formsync3;
    procedure formsync4;
  public

  end;


var
  mainform: Tmainform;
  x:        longword;

implementation

{$R main.lfm}

uses
  libvplot, math;

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  loadbtn.enabled := true;
  playbtn.enabled := false;
  stopbtn.enabled := false;
  morebtn.enabled := true;

  vplotinterface := tvplotinterface.create;
  vplotinterface.gcode     := '';
  vplotinterface.sync1     := @formsync1;
  vplotinterface.sync2     := @formsync2;
  vplotinterface.sync3     := @formsync3;
  vplotinterface.sync4     := @formsync4;
  vplotinterface.preview   := true;
  vplotinterface.suspended := true;

  vplotdriver := tvplotdriver.create(vplotinterface);
  vplotlist   := tstringlist.create;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if vplotinterface.suspended = false then
    closeaction := canone;
end;

procedure tmainform.panbtnClick(Sender: TObject);
begin
  previewimage.top  := (previewpanel.height - previewimage.height) div 2;
  previewimage.left := (previewpanel.width  - previewimage.width)  div 2;
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  vplotdriver.terminate;
  sleep(100);

  vplotinterface.destroy;
  vplotlist.destroy;
end;

procedure tmainform.formsync1;
begin
  vplotinterface.gcode := '';
  if not vplotinterface.suspended then
  begin
    vplotinterface.gcode := vplotlist[vplotlistindex];
    progressbar.stepit;
  end;
  application.processmessages;
end;

procedure Tmainform.formsync2;
begin
  previewimage.canvas.pen.color   := clred;
  previewimage.canvas.brush.color := clred;
  previewimage.canvas.brush.style := bssolid;
  with vplotinterface do
    previewimage.canvas.line(
      round(point1.x),
      round(point1.y),
      round(point2.x),
      round(point2.y));
end;

procedure Tmainform.formsync3;
begin
  previewimage.canvas.pen.color   := clblack;
  previewimage.canvas.brush.color := clblack;
  previewimage.canvas.brush.style := bssolid;
  with vplotinterface do
    previewimage.canvas.line(
      round(point1.x),
      round(point1.y),
      round(point2.x),
      round(point2.y));
end;

procedure Tmainform.formsync4;
begin
  if vplotlistindex = vplotlist.count - 1 then
  begin
    stopbtn.click;
  end else
    inc(vplotlistindex);
end;

procedure tmainform.playbtnclick(sender: tobject);
begin
  if vplotlist.count > 0 then
    vplotinterface.suspended := false;

  timer  .enabled := not vplotinterface.suspended;
  playbtn.enabled :=     vplotinterface.suspended;
  stopbtn.enabled := not vplotinterface.suspended;
end;

procedure tmainform.stopbtnclick(sender: tobject);
begin
  vplotinterface.suspended := true;

  timer.enabled   := not vplotinterface.suspended;
  playbtn.enabled :=     vplotinterface.suspended;
  stopbtn.enabled := not vplotinterface.suspended;
end;

procedure tmainform.morebtnclick(sender: tobject);
begin
  moremenu.popup(left + morebtn.left, top + 28);
end;

procedure tmainform.timertimer(Sender: TObject);
begin
  inc(x);
  caption := 'VPlot - Time elapsed ' + inttostr(x) + ' secs';
end;

procedure tmainform.updownbtnClick(sender: tobject; button: tudbtntype);
begin
  if button = btnext then
    previewimage.top := min(2, previewimage.top + 100)
  else
    previewimage.top := max(previewpanel.height -
                            previewimage.height - 2,
                            previewimage.top    - 100);
end;

procedure tmainform.leftrightbtnClick(sender: tobject; button: tudbtntype);
begin
  if button = btprev then
    previewimage.left := min(2, previewimage.left + 100)
  else
    previewimage.left := max(previewpanel.width -
                             previewimage.width - 2,
                             previewimage.left  - 100);
end;

procedure tmainform.loadbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    vplotlist.clear;
    vplotlist.loadfromfile(opendialog.filename);
    vplotlistindex := 0;
    // ---
    previewimage.picture.bitmap.setsize(1500,1500);
    previewimage.canvas.pen.color   := clwhite;
    previewimage.canvas.brush.color := clwhite;
    previewimage.canvas.brush.style := bssolid;
    previewimage.canvas.rectangle(0, 0, 1500, 1500);

    previewimage.anchors := [aktop, akleft, akright, akbottom];
    previewimage.anchors := [];

    previewimage.stretchinenabled  := false;
    previewimage.stretchoutenabled := false;
    previewimage.stretch           := false;

    progressbar.min      := 0;
    progressbar.max      := vplotlist.count;
    progressbar.position := 0;
    progressbar.smooth   := true;
    // ---
    vplotdriver.initialize;
    playbtn.enabled := true;

    x := 0;
  end;
end;

end.

