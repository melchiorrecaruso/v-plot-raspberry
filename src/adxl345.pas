{ Description: ADXL345 library.

  Copyright (C) 2019 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
unit adxl345;

{$mode objfpc}

interface

uses
  wiringPi;

const
  adxl345_addr = $53;

type
  adxl345_rec = packed record
    x: longint;
    y: longint;
    z: longint;
  end;

function adxl345setup: longint;
function adxl345_read(fd: longint): adxl345_rec;


implementation

procedure adxl345_init(fd: longint);
begin
  wiringpii2cwritereg8(fd, $31, $0b);
  wiringpii2cwritereg8(fd, $2d, $08);
//wiringpii2cwritereg8(fd, $2e, $00);
  wiringpii2cwritereg8(fd, $1e, $00);
  wiringpii2cwritereg8(fd, $1f, $00);

  wiringpii2cwritereg8(fd, $20, $00);
  wiringpii2cwritereg8(fd, $21, $00);
  wiringpii2cwritereg8(fd, $22, $00);
  wiringpii2cwritereg8(fd, $23, $00);

  wiringpii2cwritereg8(fd, $24, $01);
  wiringpii2cwritereg8(fd, $25, $0f);
  wiringpii2cwritereg8(fd, $26, $2b);
  wiringpii2cwritereg8(fd, $27, $00);

  wiringpii2cwritereg8(fd, $28, $09);
  wiringpii2cwritereg8(fd, $29, $ff);
  wiringpii2cwritereg8(fd, $2a, $80);
  wiringpii2cwritereg8(fd, $2c, $0a);
  wiringpii2cwritereg8(fd, $2f, $00);
  wiringpii2cwritereg8(fd, $38, $9f);
end;

function adxl345_read(fd: longint): adxl345_rec;
var
  x0, y0, z0: byte;
  x1, y1, z1: byte;
begin
  x0 := $ff - wiringpii2creadreg8(fd, $32);
  x1 := $ff - wiringpii2creadreg8(fd, $33);
  y0 := $ff - wiringpii2creadreg8(fd, $34);
  y1 := $ff - wiringpii2creadreg8(fd, $35);
  z0 := $ff - wiringpii2creadreg8(fd, $36);
  z1 := $ff - wiringpii2creadreg8(fd, $37);

  result.x := longint(x1 << 8) + longint(x0);
  result.y := longint(y1 << 8) + longint(y0);
  result.z := longint(z1 << 8) + longint(z0);
end;

function adxl345setup: longint;
var
  fd: longint;
begin
  result := -1;
  fd := wiringpii2csetup(adxl345_addr);

  if fd <> -1 then
  begin
    adxl345_init(fd);
    result := fd;
  end;
end;

end.



