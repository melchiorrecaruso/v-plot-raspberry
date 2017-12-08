program vplot;

{$mode objfpc}{$h+}

uses
  {$ifdef unix} cthreads, {$endif} interfaces, forms, lazcontrols, main, view;

{$r *.res}

begin
  application.title := 'VPlot Driver';
  requirederivedformresource := true;
  application.initialize;
  application.createform(tform1, form1);
  application.createform(tviewfrm, viewfrm);
  application.run;
end.

