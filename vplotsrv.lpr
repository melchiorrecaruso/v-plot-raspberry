{
  Description: vPlot server library.

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

program vplotsrv;

{$mode objfpc}

uses
 cthreads, classes, crt, lnet, lnetbase, sha1, sysutils, vpsrvdriver, vpsrvsetting;

type

{ tvplotthread }

  tvplotthread = class(tthread)
  private
    fenabled: boolean;
    flist:    tstringlist;
  protected
    procedure execute override;
  public
    constructor create(list: tstringlist);
    destructor destroy; override;
  public
    property enabled: boolean read fenabled write fenabled;

  end;

{ tvploserver }

  tvplotserver = class
  private
    fcon: tltcp;
    flst: tstringlist;
    fvth: tvplotthread;
  procedure oner(const msg: ansistring; asocket: tlsocket);
    procedure onac(asocket: tlsocket);
    procedure onre(asocket: tlsocket);
    procedure onds(asocket: tlsocket);
  public
    constructor create;
    destructor destroy; override;
    procedure run;
  end;

// parse_prefix

procedure parse_prefix(const prefix, code: ansistring; var value: ansistring);
var
  i: longint;
  s: ansistring = '';
begin
  i := pos(prefix, code);
  if i > 0 then
  begin
    while (i <= length(code)) and (code[i] <> ' ') do
    begin
      s := s + code[i];
      inc(i);
    end;
    delete(s, 1, 1);
  end;
  value := s;
end;

function parse_prefix(const prefix, code: ansistring; var value: longint): boolean;
var
  s: ansistring = '';
begin
  parse_prefix(prefix, code, s);

  result := length(s) > 0;
  if result then
  begin
    result := trystrtoint(s, value);
  end;
end;

// tvplotserver

procedure tvplotserver.oner(const msg: ansistring; asocket: tlsocket);
begin
  writeln(msg);
end;

procedure tvplotserver.onac(asocket: tlsocket);
begin
  writeln('connection accepted from ', asocket.peeraddress);
end;

procedure tvplotserver.onre(asocket: tlsocket);
var
  m: ansistring;
  s: ansistring;
begin
  if asocket.getmessage(m) > 0 then
  begin

    if assigned(fvth) then
    begin

      if m = 'START' then
      begin
        fvth.enabled := true;
      end else
      if m = 'STOP' then
      begin
        fvth.enabled := false;
      end else
      if m = 'KILL' then
      begin
        fvth.terminate;
        fvth.enabled := true;
      end else
      if m = 'INFO' then
      begin
        writeln('INFO');
      end;

    end else
    begin

      if m = 'SEND' then
      begin
        flst.clear;
      end else
      if pos('SHA1', m) = 1 then
      begin
        writeln(m);
        parse_prefix('SHA1', m, s);


        writeln(s);



        if s = sha1print(sha1string(flst.text)) then
        begin
          fvth := tvplotthread.create(flst);
          fvth.start;
        end;
      end else
      begin
        flst.add(m);
      end;

    end;

    fcon.iterreset;
    while fcon.iternext do
    begin
      fcon.sendmessage('NEXT', fcon.iterator);
    end;
  end;
end;

procedure tvplotserver.onds(asocket: tlsocket);
begin
  writeln('lost connection');
end;

constructor tvplotserver.create;
begin
  fcon := tltcp.create(nil);
  fcon.onerror      := @oner;
  fcon.onreceive    := @onre;
  fcon.ondisconnect := @onds;
  fcon.onaccept     := @onac;
  fcon.timeout      := 100;
  fcon.reuseaddress := true;
  flst   := tstringlist.create;
  fvth   := nil;
end;

destructor tvplotserver.destroy;
begin
  flst.free;
  fcon.free;
  inherited destroy;
end;

procedure tvplotserver.run;
var
  quit: boolean;
begin
  quit := false;
 
  if fcon.listen(srvsetting.port) then
  begin
    writeln('vplotsrv running!');
    writeln('press ''escape'' to quit, ''r'' to restart');
    repeat
      fcon.callaction;
      if keypressed then
        case readkey of
          #27: quit := true;
          'r': begin
                 writeln('restarting...');
                 fcon.disconnect;
                 fcon.listen(srvsetting.port);
                 quit := false;
               end;
        end;
    until quit;
  end;
end;

// tvplotthread

constructor tvplotthread.create(list: tstringlist);
begin
  flist := list;
  inherited create(true);
end;

destructor tvplotthread.destroy;
begin
  flist := nil;
  inherited destroy;
end;

procedure tvplotthread.execute;
var
  i: longint;
  s: ansistring;
  x: longint = 0;
  y: longint = 0;
  z: longint = 0;
begin
  writeln('');

  for i := 0 to flist.count -1 do
  begin
    s := flist[i];
    writeln(s);

    (*
    if pos('INIT ', s) = 1 then
    begin
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) then
      begin
        driver.init(x, y);
      end;

    end else
    if pos('MOVE ', s) = 1 then
    begin
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) and
         parse_prefix('Z', s, z) then
      begin
        if z < 0 then
          driver.zcount := srvsetting.zmin
        else
          driver.zcount := srvsetting.zmax;
        driver.move(x, y);
      end;

    end else
    if pos('MOVED ', s) = 1 then
    begin
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) and
         parse_prefix('Z', s, z) then
      begin
        x := x + driver.xcount;
        y := y + driver.ycount;
        z := z + driver.zcount;

        if z < 0 then
          driver.zcount := srvsetting.zmin
        else
          driver.zcount := srvsetting.zmax;
        driver.move(x, y);
      end;
    end;
    *)

  end;
end;

// MAIN BLOCK

var
  vploserver: tvplotserver;

begin
  vploserver := tvplotserver.create;
  vploserver.run;
  vploserver.free;
end.

