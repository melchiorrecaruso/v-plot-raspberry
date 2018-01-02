unit initform;

{$mode objfpc}

interface

uses
  classes, sysutils, lresources, forms, controls, graphics, dialogs, buttons,
  spin, stdctrls;

type

  { tform1 }

  tform1 = class(tform)
    dxdownbtn: tbitbtn;
    m1groupbox1: tgroupbox;
    dxupspinedit: tspinedit;
    dxdownspinedit: tspinedit;
    sxupbtn: tbitbtn;
    sxdownbtn: tbitbtn;
    onbtn: tbitbtn;
    m1groupbox: tgroupbox;
    sxupspinedit: tspinedit;
    sxdownspinedit: tspinedit;
    dxupbtn: tbitbtn;
    procedure dxupbtnclick(sender: tobject);
    procedure sxdownbtnclick(sender: tobject);
    procedure dxdownbtnclick(sender: tobject);
    procedure sxupbtnclick(sender: tobject);
  private

  public

  end;

var
  form1: tform1;

implementation

uses
  libvplot;

{ tform1 }

procedure tform1.sxupbtnclick(sender: tobject);
begin
  vplotdriver.move(0, sxupspinedit.value, false);
end;

procedure tform1.sxdownbtnclick(sender: tobject);
begin
  vplotdriver.move(0, sxdownspinedit.value, true);
end;

procedure tform1.dxupbtnclick(sender: tobject);
begin
  vplotdriver.move(1, dxupspinedit.value, false);
end;

procedure tform1.dxdownbtnclick(sender: tobject);
begin
  vplotdriver.move(1, dxdownspinedit.value, true);
end;


initialization

  {$I initform.lrs}

end.

