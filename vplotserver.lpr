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

program vplotserver;

{$mode objfpc}

uses
 cthreads, cmem, classes, crt, lnet, lnetbase, sha1, sysutils, vpsrvdriver, vpsetting;

type

{ tvplotthread }

  tvplotthread = class(tthread)
  private
    fenabled: boolean;
    flist:    tstringlist;
    fonstart: tthreadmethod;
    fonstop:  tthreadmethod;
  protected
    procedure execute override;
  public
    constructor create(list: tstringlist);
    destructor destroy; override;
  public
    property enabled: boolean       read fenabled write fenabled;
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop:  tthreadmethod read fonstop  write fonstop;
  end;

{ tvploserver }

  tvplotserver = class
  private
    fconn: tltcp;
    flist: tstringlist;
    fthrd: tvplotthread;
    procedure oner(const msg: ansistring; asocket: tlsocket);
    procedure onac(asocket: tlsocket);
    procedure onre(asocket: tlsocket);
    procedure onds(asocket: tlsocket);
    procedure dostart;
    procedure dostop;
  public
    constructor create;
    destructor destroy; override;
    procedure start;
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
    i := i + length(prefix) - 1;
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

    if assigned(fthrd) then
    begin
      writeln('LOCKED');

      if m = 'START' then
      begin
        fthrd.enabled := true;
      end else
      if m = 'STOP' then
      begin
        fthrd.enabled := false;
      end else
      if m = 'KILL' then
      begin
        fthrd.terminate;
        fthrd.enabled := true;
      end else
      if m = 'INFO' then
      begin
        writeln('INFO');
      end;

    end else
    begin
      writeln('UNLOCKED');

      if m = 'SEND' then
      begin
        flist.clear;
      end else
      if pos('SHA1', m) = 1 then
      begin
        parse_prefix('SHA1', m, s);
        if s = sha1print(sha1string(flist.text)) then
        begin
          fthrd := tvplotthread.create(flist);
          fthrd.onstart := @dostart;
          fthrd.onstop  := @dostop;
          fthrd.start;
        end;
      end else
      begin
        flist.add(m);
      end;

    end;

    fconn.iterreset;
    while fconn.iternext do
    begin
      if assigned(fthrd) then
        fconn.sendmessage('LOCKED', fconn.iterator)
      else
        fconn.sendmessage('UNLOCKED',   fconn.iterator);
    end;
  end;

end;

procedure tvplotserver.onds(asocket: tlsocket);
begin
  writeln('lost connection');
end;

procedure tvplotserver.dostart;
begin
  writeln('start thread');
end;

procedure tvplotserver.dostop;
begin
  writeln('stop thread');
  flist.clear;
  fthrd := nil;
end;

constructor tvplotserver.create;
begin
  inherited create;
  fconn := tltcp.create(nil);
  fconn.onerror      := @oner;
  fconn.onreceive    := @onre;
  fconn.ondisconnect := @onds;
  fconn.onaccept     := @onac;
  fconn.timeout      := 100;
  fconn.reuseaddress := true;
  flist := tstringlist.create;
  fthrd := nil;
end;

destructor tvplotserver.destroy;
begin
  flist.destroy;
  fconn.destroy;
  inherited destroy;
end;

procedure tvplotserver.start;
var
  quit: boolean;
begin
  quit := false;
 
  if fconn.listen(setting.port) then
  begin
    writeln('vplotsrv2.0 running!');
    writeln('press ''escape'' to quit, ''r'' to restart');
    repeat
      fconn.callaction;
      if keypressed then
        case readkey of
          #27: quit := true;
          'r': begin
                 writeln('restarting...');
                 fconn.disconnect;
                 fconn.listen(setting.port);
                 quit := false;
               end;
        end;

      sleep(100);
    until quit;
  end;
end;

// tvplotthread

constructor tvplotthread.create(list: tstringlist);
begin
  flist    := list;
  fonstart := nil;
  fonstop  := nil;

  freeonterminate := true;
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
  if assigned(fonstart) then fonstart;

  for i := 0 to flist.count -1 do
  begin
    s := flist[i];

    if pos('INIT ', s) = 1 then
    begin
      writeln(s);
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) then
      begin
        driver.init(x, y);
      end;

    end else
    if pos('MOVE ', s) = 1 then
    begin
      writeln(s);
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) and
         parse_prefix('Z', s, z) then
      begin
        if z < 0 then
          driver.zcount := setting.zmin
        else
          driver.zcount := setting.zmax;
        driver.move(x, y);
      end;

    end else
    if pos('MOVED ', s) = 1 then
    begin
      if parse_prefix('X', s, x) and
         parse_prefix('Y', s, y) and
         parse_prefix('Z', s, z) then
      begin
        writeln(s);

        x := x + driver.xcount;
        y := y + driver.ycount;
      //z := z + srvdriver.zcount;
        if z < 0 then
          driver.zcount := setting.zmin
        else
          driver.zcount := setting.zmax;
        driver.move(x, y);
      end;
    end;
  end;

  if assigned(fonstop) then fonstop;
end;

// MAIN BLOCK

var
  vploserver: tvplotserver;

begin
  driver     := tvpdriver.create;
  setting    := tvpsetting.create;
  setting.load(extractfilepath(paramstr(0) + 'vplot.ini'));
  begin
    vploserver := tvplotserver.create;
    vploserver.start;
    vploserver.destroy;
  end;
  setting.destroy;
  driver.destroy;
end.

