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

procedure parser(const prefix, code: rawbytestring; var value: longint);
var
  i: longint;
  s: rawbytestring = '';
begin
  i := pos(prefix, code);
  if i > 0 then
  begin
    while (i < length(code)) and (code[i] <> ' ') do
    begin
      s := s + code[i];
      inc(i);
    end;
    for i := 0 to length(prefix) -1 do
      delete(s, 1, 1);
  end;

  if length(s) > 0 then
    value := strtoint(s);
end;

procedure tvplotserver.onre(asocket: tlsocket);
var
  a: longint;
  b: longint;
  c: longint;
  s: string;
begin
  if asocket.getmessage(s) > 0 then
  begin
    // writeln(s);

    a := -1;
    b := -1;
    c := -1;
    if (pos('MOVE4 ', s) = 1) or
       (pos('MOVE2 ', s) = 1) or
       (pos('INIT ' , s) = 1) then
    begin
      parser('A', s, a);
      parser('B', s, b);
      parser('C', s, c);

      if (a = -1) or
         (b = -1) or
         (c = -1) then ffault := -1;

      if ffault = 0 then
      begin
        if (pos('MOVE4 ', s) = 1) then fdrv.move4(a, b, c) else
        if (pos('MOVE2 ', s) = 1) then fdrv.move2(a, b, c) else
        if (pos('INIT ' , s) = 1) then fdrv.init (a, b, c);
        ffault := fdrv.fault;
      end

    end else
    if pos('DELAY ',  s) = 1 then
    begin
      parser('A', s, a);
      if (a = -1) then
        ffault := -1
      else
        fdrv.delayms := a;
    end else
      ffault := -1;

    //fcon.iterreset;
    //while fcon.iternext do
    //  a := fcon.sendmessage(inttostr(ffault), fcon.iterator);
  end;
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

