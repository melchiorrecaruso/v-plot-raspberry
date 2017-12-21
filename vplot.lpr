program vplot;

{$mode objfpc}{$h+}

uses
  {$ifdef unix} cthreads, {$endif} interfaces, forms, lazcontrols, main, view;

{$R *.res}

begin
  application.title := 'VPlot Driver';
  requirederivedformresource := true;
  application.initialize;
  application.createform(tmainform, mainform);
  Application.CreateForm(Tpreviewform, previewform);
  application.run;
end.

