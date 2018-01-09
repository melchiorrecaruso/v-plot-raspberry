program vplotserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  classes,
  sysutils,
  sockets,
  fpasync,
  fpsock,
  pca9685,
  wiringpi;

type
  tclienthandlerthread = class(tthread)
  private
    fclientstream: tsocketstream;
  public
    constructor create(aclientstream: tsocketstream);
    procedure execute; override;
  end;

type
  tvplotserver = class(ttcpserver)
  private
    procedure onconnectclient(sender: tconnectionbasedsocket; astream: tsocketstream);
  public
    constructor create(aowner: tcomponent); override;
  end;

// implementation

function addrtostring(addr: tsockaddr): rawbytestring;
begin
  result := netaddrtostr(addr.sin_addr) + ':' + inttostr(addr.sin_port);
end;

{ tclienthandlerthread }

constructor tclienthandlerthread.create(aclientstream: tsocketstream);
begin
  inherited create(false);
  freeonterminate := true;
  fclientstream   := aclientstream;
end;

procedure tclienthandlerthread.execute;
var
  done: boolean;
begin
  done := false;
  repeat
    try

      case fclientstream.readbyte of
      //    0:
      //    1:
      //    2:
      //    3:
        255: writeln(addrtostring(fclientstream.peeraddress)
               + ': ' + fclientstream.readansistring);
      end;



      fclientstream.readbyte;
      fclientstream.readbyte;
      fclientstream.readbyte;
      fclientstream.readbyte;
      fclientstream.readbyte;
      fclientstream.readbyte;


    except
      on e: estreamerror do
      begin
        done := true;
      end;
    end;
  until done;
  writeln(addrtostring(fclientstream.peeraddress) + ' disconnected');
end;

{ tvplotserver }

procedure tvplotserver.onconnectclient(sender: tconnectionbasedsocket; astream: tsocketstream);
begin
  writeln('incoming connection from ' + addrtostring(astream.peeraddress));
  tclienthandlerthread.create(astream);
end;

constructor tvplotserver.create(aowner: tcomponent);
begin
  inherited create(aowner);
  onconnect := @onconnectclient;


end;


const
  vplotservo_maxvalue = 2.50;
  vplotservo_minvalue = 0.50;
  vplotservo_rstvalue = 1.50;
  vplotservo_incvalue = 0.10;
  vplotservo_freq     = 50;

  vplotmot0_step      = P38;
  vplotmot0_dir       = P40;
  vplotmot1_step      = P16;
  vplotmot1_dir       = P18;

  vplotmatrix : array [0..5, 0..8] of longint =
    ((0, 0, 0, 0, 0, 0, 0, 0, 0),
     (0, 0, 0, 0, 1, 0, 0, 0, 0),
     (0, 0, 1, 0, 0, 0, 1, 0, 0),
     (1, 0, 0, 0 ,1, 0, 0, 0, 1),
     (0, 1, 0, 1, 0, 1, 0, 1, 0),
     (1, 0, 1, 0, 1, 0, 1, 0, 1));




{ main block }

var
  servereventloop: teventloop;
  servovalue:      double;

begin
  // init wiringpi library
  if wiringpisetup = -1 then halt;

  // init pca9685 library
  if pca9685setup(
     pca9685_pin_base,
     pca9685_address, vplotservo_freq) = -1 then halt;

  pinmode(P11, OUTPUT);
  digitalwrite(P11, LOW);



  servovalue := 0;
  digitalwrite(P11, HIGH);

  // init servo
  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_minvalue, vplotservo_freq));
  delay(2000);
  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_maxvalue, vplotservo_freq));
  delay(2000);
  pwmWrite(PCA9685_PIN_BASE + 0, calcTicks(vplotservo_rstvalue, vplotservo_freq));
  delay(2000);
  // init step motor0
  pinmode(vplotmot0_dir, OUTPUT);
  pinmode(vplotmot0_step, OUTPUT);
  digitalwrite(vplotmot0_dir, LOW);
  digitalwrite(vplotmot0_step, LOW);
  // init step motor1
  pinmode(vplotmot1_dir, OUTPUT);
  pinmode(vplotmot1_step, OUTPUT);
  digitalwrite(vplotmot1_dir, LOW);
  digitalwrite(vplotmot1_step, LOW);

  // listening ...
  servereventloop := teventloop.create;
  with tvplotserver.create(nil) do
  begin
    eventloop := servereventloop;
    port := 12000;
    writeln('serving...');
    active := true;
    eventloop.run;
  end;
  servereventloop.free;
end.

