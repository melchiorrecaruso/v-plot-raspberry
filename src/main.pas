unit main;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus;

type

  { tmainform }

  tmainform = class(tform)
    bevel0: tbevel;
    bevel1: TMenuItem;
    aboutvplot: TMenuItem;
    showviewform: TMenuItem;
    morebtn: tbitbtn;
    loadbtn: tbitbtn;
    playbtn: tbitbtn;
    moremenu: TPopupMenu;
    stopbtn: tbitbtn;
    gcodelist: tlistbox;
    opendialog: topendialog;
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playbtnclick(sender: tobject);
    procedure showviewformClick(Sender: TObject);
    procedure stopbtnclick(sender: tobject);
    procedure morebtnclick(sender: tobject);
  private
    procedure formsync1;
    procedure formsync2;
  public

  end;

var
  mainform: Tmainform;


implementation

uses
  libvplot, view;

{$R *.lfm}

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  playbtn.enabled := false;
  stopbtn .enabled := false;

  vplotinterface := tvplotinterface.create;
  vplotinterface.code      := '';
  vplotinterface.sync1     := @formsync1;
  vplotinterface.sync2     := @formsync2;
  vplotinterface.suspended := true;

  vplotdriver := tvplotdriver.create(vplotinterface);
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  vplotinterface.suspended := true;
  vplotinterface.destroy;
end;

procedure tmainform.formsync1;
begin
  vplotinterface.code := '';
  if not vplotinterface.suspended then
  begin
    vplotinterface.code := gcodelist.items[gcodelist.itemindex];
    if gcodelist.itemindex = gcodelist.count - 1 then
      stopbtn.click
    else
      gcodelist.itemindex := gcodelist.itemindex + 1;
  end;
end;

procedure Tmainform.formsync2;
begin
  with vplotinterface do
  begin
    viewform.image.canvas.pen.color   := clblack;
    viewform.image.canvas.brush.color := clblack;
    viewform.image.canvas.brush.style := bssolid;
    viewform.image.canvas.rectangle(
      round(point.y),
      round(point.x),
      round(point.y + 2),
      round(point.x + 2));
  end;
end;

procedure tmainform.stopbtnclick(sender: tobject);
begin
  vplotinterface.suspended := true;

  playbtn.enabled :=     vplotinterface.suspended;
  stopbtn .enabled := not vplotinterface.suspended;
end;

procedure tmainform.morebtnclick(sender: tobject);
var
  x, y: longint;
begin
  x := left + morebtn.left;
  y := top  + bevel0.top + 28;

  moremenu.popup(x, y);


end;

procedure tmainform.playbtnclick(sender: tobject);
begin
  if gcodelist.itemindex > -1 then
    vplotinterface.suspended := false;

  playbtn.enabled :=     vplotinterface.suspended;
  stopbtn .enabled := not vplotinterface.suspended;
end;

procedure tmainform.showviewformClick(Sender: TObject);
begin
  viewform.show;
end;

procedure tmainform.loadbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    gcodelist.clear;
    gcodelist.items.loadfromfile(opendialog.filename);
    if gcodelist.count > 0 then
      gcodelist.itemindex := 0;
    playbtn.enabled := true;

    vplotdriver.initialize;
    viewform.clear;
  end;
end;

end.

