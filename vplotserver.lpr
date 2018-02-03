program vplotserver;

{$mode objfpc}{$H+}

uses
  classes, crt, libvplot, lnet, sysutils;

type
  { tvplotserver }

  tvplotserver = class
  private
    fcon:   tltcp;
    fdrv:   tvpdriver;
    ffault: longint;
    procedure oner(const msg: string; asocket: tlsocket);
    procedure onac(asocket: tlsocket);
    procedure onre(asocket: tlsocket);
    procedure onds(asocket: tlsocket);
  public
    constructor create;
    destructor destroy; override;
    procedure run;
  end;

procedure tvplotserver.oner(const msg: string; asocket: tlsocket);
begin
  writeln(msg);
end;

procedure tvplotserver.onac(asocket: tlsocket);
begin
  writeln('connection accepted from ', asocket.peeraddress);
end;

procedure tvplotserver.onre(asocket: tlsocket);
var
  buf: array[0..3] of longint;
  err: longint;
begin
  err := fcon.get(buf, sizeof(buf), asocket);

  if err = sizeof(buf) then
  begin
    if buf[0] = 0 then
    begin
      writeln('INIT');
      fdrv.init (buf[1] , buf[2] , buf[3]);
    end else
    if buf[0] = 2 then
    begin
      writeln('MOVE2');
      fdrv.move2(buf[1] , buf[2] , buf[3]);
    end else
    if buf[0] = 4 then
    begin
      writeln('MOVE4');
      fdrv.move4(buf[1] , buf[2] , buf[3]);
    end;
  end;

  fcon.iterreset;
  while fcon.iternext do
    fcon.send(err, sizeof(longint), fcon.iterator);
end;

procedure tvplotserver.onds(asocket: tlsocket);
begin
  writeln('Lost connection');
end;

constructor tvplotserver.create;
begin
  fdrv := tvpdriver.create(2);
  fcon := tltcp.create(nil);
  fcon.onerror      := @oner;
  fcon.onreceive    := @onre;
  fcon.ondisconnect := @onds;
  fcon.onaccept     := @onac;
  fcon.timeout      := 100;
  fcon.reuseaddress := true;
  ffault            := 0;
end;

destructor tvplotserver.destroy;
begin
  fcon.free;
  fdrv.free;
  inherited destroy;
end;

procedure tvplotserver.run;
var
  quit: boolean;
  port: word;
begin

  if paramcount > 0 then
  begin
    try
      port := word(strtoint(paramstr(1)));
    except
      on e: exception do begin
        writeln(e.message);
        halt;
      end;
    end;
    quit := false;

    if fcon.listen(port) then
    begin
      writeln('server running!');
      writeln('press ''escape'' to quit, ''r'' to restart');
      repeat
        fcon.callaction;
        if keypressed then
          case readkey of
            #27: quit := true;
            'r': begin
                   writeln('restarting...');
                   fcon.disconnect;
                   fcon.listen(port);
                   quit := false;
                 end;
          end;
      until quit;

    end;
  end else
    writeln('usage: ', paramstr(0), ' <port>');
end;

var
  srv: tvplotserver;

begin
  srv := tvplotserver.create;
  srv.run;
  srv.free;
end.
.

