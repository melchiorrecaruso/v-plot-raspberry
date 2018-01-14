unit initfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, lresources, forms, controls, graphics, buttons, spin, stdctrls;

type
  { tinitform }

  tinitform = class(tform)
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
  initform: tinitform;

implementation

{$R *.lfm}

uses
  libvplot;

{ tinitform }

procedure tinitform.updatebtn(value: boolean);
begin
  sxupbtn  .enabled := value;
  sxdownbtn.enabled := value;
  dxupbtn  .enabled := value;
  dxdownbtn.enabled := value;

  sxupspinedit  .enabled:= value;
  sxdownspinedit.enabled:= value;
  dxupspinedit  .enabled:= value;
  dxdownspinedit.enabled:= value;
end;

procedure tinitform.btnclick(sender: tobject);
begin
  updatebtn(false);
  if sender = sxupbtn   then vplotdriver.move4(-sxupspinedit  .value, 0, 0) else
  if sender = sxdownbtn then vplotdriver.move4(+sxdownspinedit.value, 0, 0) else
  if sender = dxupbtn   then vplotdriver.move4(0, -dxupspinedit  .value, 0) else
  if sender = dxdownbtn then vplotdriver.move4(0, +dxdownspinedit.value, 0);
  updatebtn(true);
end;

end.

