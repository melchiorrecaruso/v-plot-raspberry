program vplot;

{$mode objfpc}{$h+}

uses
  {$ifdef unix} cthreads, {$endif} interfaces, forms, lazcontrols, main, view;

{$R *.res}

begin
  application.title := 'VPlot Driver';
  requirederivedformresource := true;
  application.initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.CreateForm(Tviewform, viewform);
  application.run;
end.

