unit view;

{$mode objfpc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, extctrls;

type

  { tviewform }

  tviewform = class(tform)
    image: timage;
  private

  public
    procedure clear;
  end;

var
  viewform: tviewform;

implementation

{$R *.lfm}

{ tviewform }

procedure tviewform.clear;
begin
  image.picture.bitmap.setsize(1500, 1500);

  image.canvas.pen.color   := clwhite;
  image.canvas.brush.color := clwhite;
  image.canvas.brush.style := bssolid;
  image.canvas.rectangle(0, 0, 1499, 1499);

  image.proportional      := true;
  image.stretchinenabled  := true;
  image.stretchoutenabled := false;
  image.stretch           := true;
end;

end.

