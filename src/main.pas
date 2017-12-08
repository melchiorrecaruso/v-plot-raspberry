unit main;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, dividerbevel;

type

  { tform1 }

  tform1 = class(tform)
    bevel: tbevel;
    bitbtn1: tbitbtn;
    openbitbtn: tbitbtn;
    startbitbtn: tbitbtn;
    stopbitbtn: tbitbtn;
    gcodelist: tlistbox;
    opendialog: topendialog;
    procedure bevel1changebounds(sender: tobject);
    procedure bitbtn1click(sender: tobject);
    procedure startbitbtnclick(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure openbitbtnclick(sender: tobject);
    procedure coolbar1change(sender: tobject);
    procedure stopbitbtnclick(sender: tobject);
  private
    procedure formsync1;
    procedure formsync2;
  public

  end;

var
  form1: tform1;


implementation

uses
  libvplot,
  view;

{$r *.lfm}

{ tform1 }

procedure tform1.formcreate(sender: tobject);
begin
  vplotinterface := tvplotinterface.create;
  vplotinterface.code      := '';
  vplotinterface.sync1     := @formsync1;
  vplotinterface.sync2     := @formsync2;
  vplotinterface.suspended := true;

  vplotdriver := tvplotdriver.create(vplotinterface);
end;

procedure tform1.formdestroy(sender: tobject);
begin
  vplotinterface.suspended := true;
  vplotinterface.destroy;
end;

procedure tform1.formsync1;
begin
  vplotinterface.code := '';
  if not vplotinterface.suspended then
  begin
    vplotinterface.code := gcodelist.items[gcodelist.itemindex];
    if gcodelist.itemindex = gcodelist.count - 1 then
      stopbitbtn.click
    else
      gcodelist.itemindex := gcodelist.itemindex + 1;
  end;
end;

procedure tform1.formsync2;
begin
  with vplotinterface do
  begin
    viewfrm.image.canvas.brush.color:= clblack;
    viewfrm.image.canvas.rectangle(
      round(point.y),
      round(point.x),
      round(point.y + 1),
      round(point.x + 1));
  end;
end;

procedure tform1.coolbar1change(sender: tobject);
begin

end;

procedure tform1.stopbitbtnclick(sender: tobject);
begin
  vplotinterface.suspended := true;
end;

procedure tform1.bevel1changebounds(sender: tobject);
begin

end;

procedure tform1.bitbtn1click(sender: tobject);
begin
  vplotdriver.initialize;
end;

procedure tform1.startbitbtnclick(sender: tobject);
begin
  viewfrm.show;
  if gcodelist.itemindex > -1 then
    vplotinterface.suspended := false;
end;

procedure tform1.openbitbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    gcodelist.clear;
    gcodelist.items.loadfromfile(opendialog.filename);
    if gcodelist.count > 0 then
      gcodelist.itemindex := 0;
  end;
end;

end.

