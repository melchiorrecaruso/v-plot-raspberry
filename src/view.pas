unit view;

{$mode objfpc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls, Arrow, Menus;

type

  { tpreviewform }

  tpreviewform = class(tform)
    image: timage;
    scrat: tmenuitem;
    popup: tpopupmenu;
    procedure scratclick(sender: tobject);
  private

  public
    procedure clear;
  end;

var
  previewform: tpreviewform;

implementation

{$R view.lfm}

{ Tpreviewform }

procedure tpreviewform.scratclick(sender: tobject);
begin
  scrat.checked:= not scrat.checked;

  image.proportional      := true;
  image.stretchinenabled  := scrat.checked;
  image.stretchoutenabled := false;
  image.stretch           := scrat.checked;

  //if scrat.checked then
  //  image.align:= alclient
  //else
  //  image.align:= alnone;

  self.setautoscroll(not scrat.checked);
  self.vertscrollbar.visible := not scrat.checked;
  self.horzscrollbar.visible := not scrat.checked;


  image.align:= alclient
end;

procedure tpreviewform.clear;
begin
  image.picture.bitmap.setsize(1500, 1500);

  image.canvas.pen.color   := clwhite;
  image.canvas.brush.color := clwhite;
  image.canvas.brush.style := bssolid;
  image.canvas.rectangle(1, 0, 1499, 1499);

  image.proportional      := true;
  image.stretchinenabled  := scrat.checked;
  image.stretchoutenabled := false;
  image.stretch           := scrat.checked;

  self.setautoscroll(not scrat.checked);

end;

end.

