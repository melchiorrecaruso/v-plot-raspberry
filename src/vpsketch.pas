unit vpsketch;

{$mode objfpc}
{$i include.inc}

interface

uses
  classes,
  graphics,
  sysutils;

  procedure crosshatch(b: tbitmap);

implementation






  procedure crosshatch(b: tbitmap);
  var
    a: array of tbitmap;
    i: longint;

  begin
    setlength(a, 1);
    for i := 0 to length(a) - 1 do
    begin
      a[i] := tbitmap.create;
      a[i].setsize(b.width, b.height);
      a[i].canvas.clear;




    end;












    for i := 0 to length(a) - 1 do
      freeandnil(a[i]);
    setlength(a, 0);
  end;

end.

