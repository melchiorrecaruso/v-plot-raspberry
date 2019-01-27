{
  Description: vPlot calibration form.

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

unit calibrationfrm;

{$mode objfpc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, buttons, extctrls,
  comctrls, stdctrls, spin;

type

  { tcalibrationform }

  tcalibrationform = class(tform)
    pendownbtn: tbitbtn;
    penupbtn: tbitbtn;
    leftdownbtn: tbitbtn;
    leftedit: tspinedit;
    leftupbtn: tbitbtn;
    rightdownbtn: tbitbtn;
    rightedit: tspinedit;
    rightupbtn: tbitbtn;
    procedure leftdownbtnclick (sender: tobject);
    procedure leftupbtnclick   (sender: tobject);
    procedure pendownbtnclick  (sender: tobject);
    procedure penupbtnclick    (sender: tobject);
    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick  (sender: tobject);
  private
    procedure lock;
    procedure unlock;
  public
  end;

var
  calibrationform: tcalibrationform;

implementation

{$R *.lfm}

uses
  vpdriver, vpsetting;

{ tcalibrationform }

procedure tcalibrationform.leftupbtnclick(sender: tobject);
var
  m0: longint = 0;
  m1: longint = 0;
begin
  lock;
  driver.enabled := true;
  driver.zoff    := false;
  driver.pen     := false;
  driver.zoff    := true;

  if sender = leftupbtn    then driver.count0 := driver.count0 - leftedit .value;
  if sender = leftdownbtn  then driver.count0 := driver.count0 + leftedit .value;
  if sender = rightupbtn   then driver.count1 := driver.count1 - rightedit.value;
  if sender = rightdownbtn then driver.count1 := driver.count1 + rightedit.value;

  optimize(setting.layout09, m0, m1);
  driver.init(m0, m1);
  unlock;
end;

procedure tcalibrationform.leftdownbtnclick(sender: tobject);
begin
  leftupbtnclick(sender);
end;

procedure tcalibrationform.rightupbtnclick(sender: tobject);
begin
  leftupbtnclick(sender);
end;

procedure tcalibrationform.rightdownbtnclick(sender: tobject);
begin
  leftupbtnclick(sender);
end;

procedure tcalibrationform.pendownbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.zoff    := false;
  driver.pen     := true;
end;

procedure tcalibrationform.penupbtnclick(sender: tobject);
begin
  driver.enabled := true;
  driver.zoff    := false;
  driver.pen     := false;
end;

procedure tcalibrationform.lock;
begin
  enabled := false;
end;

procedure tcalibrationform.unlock;
begin
  enabled := true;
end;

end.

