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
    m1gb: tgroupbox;
    dxupspinedit: tspinedit;
    dxdownspinedit: tspinedit;
    sxupbtn: tbitbtn;
    sxdownbtn: tbitbtn;
    okbtn: tbitbtn;
    m0gb: tgroupbox;
    sxupspinedit: tspinedit;
    sxdownspinedit: tspinedit;
    dxupbtn: tbitbtn;
    procedure updatebtn(value: boolean);
    procedure btnclick(sender: tobject);
  private
  public
  end;

var
  form1: tform1;

implementation

uses
  libvplot;

{ tform1 }

procedure tform1.updatebtn(value: boolean);
begin
  sxupbtn  .enabled := value;
  sxdownbtn.enabled := value;
  dxupbtn  .enabled := value;
  dxdownbtn.enabled := value;
end;

procedure tform1.btnclick(sender: tobject);
begin
  updatebtn(false);
  if sender = sxupbtn   then vplotdriver.move4(-sxupspinedit  .value, 0, 0) else
  if sender = sxdownbtn then vplotdriver.move4(+sxdownspinedit.value, 0, 0) else
  if sender = dxupbtn   then vplotdriver.move4(0, -dxupspinedit  .value, 0) else
  if sender = dxdownbtn then vplotdriver.move4(0, +dxdownspinedit.value, 0);
  updatebtn(true);
end;

initialization

  {$I initform.lrs}

end.

