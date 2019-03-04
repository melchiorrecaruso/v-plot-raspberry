{
  Description: vPlot sketcher form.

  Copyright (C) 2017-2019 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit sketcherfrm;

{$mode objfpc}{$H+}

interface

uses
  sysutils, forms, controls, graphics, dialogs, spin, editbtn, buttons,
  StdCtrls, ExtCtrls;

type

  { tsketcherform }

  tsketcherform = class(tform)
    Bevel1: TBevel;
    otpcb: TCheckBox;
    okbtn: tbitbtn;
    imcb: TComboBox;
    dsfse: tfloatspinedit;
    importmethodl: TLabel;
    imagepatternwidthl: TLabel;
    imagepatternheightl: TLabel;
    patternwidthl: TLabel;
    patternheightl: TLabel;
    dotsizel: TLabel;
    ipwse: tspinedit;
    iphse: tspinedit;
    pwse: tspinedit;
    phse: tspinedit;
  private

  public

  end;

var
  sketcherform: tsketcherform;

implementation

{$R *.lfm}

uses
  bgrabitmap, vpsketcher, vppaths;

end.

