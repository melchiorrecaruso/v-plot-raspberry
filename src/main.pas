unit main;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, dividerbevel;

type

  { TMainForm }

  TMainForm = class(tform)
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
  MainForm: TMainForm;


implementation

uses
  libvplot,
  view;

{$r *.lfm}

{ TMainForm }

procedure TMainForm.formcreate(sender: tobject);
begin
  vplotinterface := tvplotinterface.create;
  vplotinterface.code      := '';
  vplotinterface.sync1     := @formsync1;
  vplotinterface.sync2     := @formsync2;
  vplotinterface.suspended := true;

  vplotdriver := tvplotdriver.create(vplotinterface);
end;

procedure TMainForm.formdestroy(sender: tobject);
begin
  vplotinterface.suspended := true;
  vplotinterface.destroy;
end;

procedure TMainForm.formsync1;
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

procedure TMainForm.formsync2;
begin
  with vplotinterface do
  begin
    viewform.image.canvas.brush.color:= clblack;
    viewform.image.canvas.rectangle(
      round(point.y),
      round(point.x),
      round(point.y + 1),
      round(point.x + 1));
  end;
end;

procedure TMainForm.coolbar1change(sender: tobject);
begin

end;

procedure TMainForm.stopbitbtnclick(sender: tobject);
begin
  vplotinterface.suspended := true;
end;

procedure TMainForm.bevel1changebounds(sender: tobject);
begin

end;

procedure TMainForm.bitbtn1click(sender: tobject);
begin
  vplotdriver.initialize;
end;

procedure TMainForm.startbitbtnclick(sender: tobject);
begin
  viewform.top  := top;
  viewform.left := left + width + 5;

  viewform.show;
  if gcodelist.itemindex > -1 then
    vplotinterface.suspended := false;
end;

procedure TMainForm.openbitbtnclick(sender: tobject);
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

