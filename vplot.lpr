program vplot;

{$mode objfpc}{$h+}

uses
  {$ifdef unix} cthreads, {$endif} interfaces, forms, lazcontrols, main, view;

{$r *.res}

begin
  application.title := 'VPlot Driver';
  requirederivedformresource := true;
  application.initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(Tviewform, viewform);
  application.run;
end.

