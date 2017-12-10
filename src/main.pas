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
    bevel2: TBevel;
    label1: TLabel;
    showviewform: TMenuItem;
    morebtn: tbitbtn;
    loadbtn: tbitbtn;
    playbtn: tbitbtn;
    moremenu: TPopupMenu;
    stopbtn: tbitbtn;
    gcodelist: tlistbox;
    opendialog: topendialog;
    timer: TTimer;
    TrackBar1: TTrackBar;
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure loadbtnclick(sender: tobject);
    procedure playbtnclick(sender: tobject);
    procedure showviewformClick(Sender: TObject);
    procedure stopbtnclick(sender: tobject);
    procedure morebtnclick(sender: tobject);
    procedure timerTimer(Sender: TObject);
  private
    procedure formsync1;
    procedure formsync2;
    procedure formsync3;
    procedure formsync4;
  public

  end;

var
  x:        longword;
  mainform: Tmainform;

implementation

uses
  libvplot, view;

{$R *.lfm}

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  playbtn.enabled := false;
  stopbtn.enabled := false;

  vplotinterface := tvplotinterface.create;
  vplotinterface.code      := '';
  vplotinterface.sync1     := @formsync1;
  vplotinterface.sync2     := @formsync2;
  vplotinterface.sync3     := @formsync3;
  vplotinterface.sync4     := @formsync4;
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
  sleep(trackbar1.Position);
  vplotinterface.code := '';
  if not vplotinterface.suspended then
  begin
    vplotinterface.code := gcodelist.items[gcodelist.itemindex];
  end;
end;

procedure Tmainform.formsync2;
begin
  with vplotinterface do
  begin
    viewform.image.canvas.pen.color   := clred;
    viewform.image.canvas.brush.color := clred;
    viewform.image.canvas.brush.style := bssolid;
    viewform.image.canvas.Line(
      round(point1.y),
      round(point1.x),
      round(point2.y),
      round(point2.x));
  end;
end;

procedure Tmainform.formsync3;
begin
  with vplotinterface do
  begin
    viewform.image.canvas.pen.color   := clblack;
    viewform.image.canvas.brush.color := clblack;
    viewform.image.canvas.brush.style := bssolid;
    viewform.image.canvas.Line(
      round(point1.y),
      round(point1.x),
      round(point2.y),
      round(point2.x));
  end;
end;

procedure Tmainform.formsync4;
begin
  if gcodelist.itemindex = gcodelist.count - 1 then
  begin
    stopbtn.click;
  end else
    gcodelist.itemindex := gcodelist.itemindex + 1;
end;


procedure tmainform.playbtnclick(sender: tobject);
begin
  if gcodelist.itemindex > -1 then
  begin
    vplotinterface.suspended := false;
  end;

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
  moremenu.popup(left + morebtn.left, top + bevel0.top + 28);
end;

procedure tmainform.timerTimer(Sender: TObject);
begin
  inc(x);
  label1.caption := 'Time elapsed ' + inttostr(x) + ' secs';
end;



procedure tmainform.showviewformClick(Sender: TObject);
begin
  viewform.top    := top;
  viewform.left   := left + width + 16;
  viewform.height := height;
  viewform.width  := width;
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
    x := 0;
  end;
end;

end.

