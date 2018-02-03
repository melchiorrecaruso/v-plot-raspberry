
{ Description: vPlot Main Form.

  Copyright (C) 2014-2018 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, forms, controls, graphics, dialogs, extctrls, stdctrls,
  comctrls, buttons, menus, spin, inifiles, libvplot, lnet, lnetcomponents;

type
  { tmainform }

  tmainform = class(tform)
    aboutbtn: TBitBtn;
    verticalcb: TCheckBox;
    redrawbtn: TBitBtn;
    formatcb: TComboBox;
    papersizegb: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    heightl: TLabel;
    widthl: TLabel;
    formatl: TLabel;
    loadbtn: TBitBtn;
    offsetyse: TSpinEdit;
    offsetxse: TSpinEdit;
    heightse: TSpinEdit;
    widthse: TSpinEdit;
    startbtn: TBitBtn;
    pausebtn: TBitBtn;
    stopbtn: TBitBtn;
    creativecontrolgb: TGroupBox;
    sethomebtn: TBitBtn;
    bordersbtn: TBitBtn;
    leftdownbtn: TBitBtn;
    rightdownbtn: TBitBtn;
    penupbtn: TBitBtn;
    pendownbtn: TBitBtn;
    leftupbtn: TBitBtn;
    rightupbtn: TBitBtn;
    gohomebtn: TBitBtn;
    leftedit: TEdit;
    rightedit: TEdit;
    connectgb: TGroupBox;
    manualdrivinggb: TGroupBox;
    drawingcontrolgb: TGroupBox;
    tpc: TLTCPComponent;
    previewimage: TImage;
    opendialog: topendialog;
    conbtn: TSpeedButton;

    procedure bordersbtnclick(sender: tobject);
    procedure conbtnclick(sender: tobject);
    procedure formatcbchange(sender: tobject);
    procedure formcreate(sender: tobject);
    procedure formdestroy(sender: tobject);
    procedure formclose(sender: tobject; var closeaction: tcloseaction);
    procedure gohomebtnclick(sender: tobject);
    procedure leftdownbtnclick(sender: tobject);

    procedure leftupbtnclick(sender: tobject);
    procedure pendownbtnclick(sender: tobject);
    procedure penupbtnclick(sender: tobject);
    procedure reloadbtnclick(sender: tobject);
    procedure rightdownbtnclick(sender: tobject);
    procedure rightupbtnclick(sender: tobject);
    procedure sethomebtnclick(sender: tobject);
    procedure startbtnclick(sender: tobject);

    procedure tpcconnect(asocket: tlsocket);
    procedure tpcdisconnect(asocket: tlsocket);
    procedure tpcerror(const msg: string; asocket: tlsocket);



    procedure loadbtnclick(sender: tobject);
    procedure playorstopbtnclick(sender: tobject);
    procedure tpcReceive(aSocket: TLSocket);
    procedure verticalcbeditingdone(sender: tobject);
  private
    strm: tmemorystream;
    ini:   tinifile;
    image: tbitmap;
    list: tstringlist;
    procedure onstart;
    procedure onstop;
    procedure ontick;
  end;


var
  mainform: Tmainform;
  x:        longword;

  serverisbusy: boolean;

implementation

{$R *.lfm}

uses
  math, sysutils;

{ tmainform }

procedure tmainform.formcreate(sender: tobject);
begin
  conbtn           .caption := 'Connect';
  connectgb        .enabled := true;
  manualdrivinggb  .enabled := false;
  creativecontrolgb.enabled := true;
  papersizegb      .enabled := false;
  drawingcontrolgb .enabled := false;
  // ---
  image := tbitmap.create;
  list  := tstringlist.create;
  ini := tinifile.create(changefileext(paramstr(0), '.ini'));
  loadlayout(ini, vplayout);
  strm := tmemorystream.create;


  reloadbtnclick(nil);
  conbtnclick(nil);
end;

procedure tmainform.conbtnclick(sender: tobject);
begin
  if tpc.connected then
    tpc.disconnect
  else
    tpc.connect(ini.readstring ('Server', 'Address', '192.168.1.10'),
                ini.readinteger('Server', 'Port'   , 4500));
end;

procedure tmainform.formatcbchange(sender: tobject);
begin
  verticalcb.enabled := true;
  heightse  .enabled := false;
  widthse   .enabled := false;
  case formatcb.itemindex of
    0: begin heightse.value := 841; widthse .value := 1189; end;
    1: begin heightse.value := 594; widthse .value :=  841; end;
    2: begin heightse.value := 420; widthse .value :=  594; end;
    3: begin heightse.value := 297; widthse .value :=  420; end;
    4: begin heightse.value := 210; widthse .value :=  297; end;
    5: begin heightse.value := 148; widthse .value :=  210; end;
  else begin
         verticalcb.enabled := false;
         heightse  .enabled := true;
         widthse   .enabled := true;
       end;
  end;
  verticalcbeditingdone(formatcb);
end;



procedure tmainform.bordersbtnclick(sender: tobject);
begin
   tpc.sendmessage('bordersbtnclick');
end;

procedure tmainform.formdestroy(sender: tobject);
begin
  ini.destroy;
  image.destroy;
  list.destroy;
  strm.destroy;
end;

procedure tmainform.formclose(sender: tobject; var closeaction: tcloseaction);
begin
  if assigned(vpcoder) then
    playorstopbtnclick(stopbtn);

  if assigned(vpcoder) then
    closeaction := canone
  else
    closeaction := cafree;
end;

procedure tmainform.gohomebtnclick(sender: tobject);
begin
   tpc.sendmessage('gohomebtnclick');
end;

procedure tmainform.leftdownbtnclick(sender: tobject);
begin
  tpc.sendmessage('leftdownbtnclick');
end;

procedure tmainform.leftupbtnclick(Sender: TObject);
begin
  tpc.sendmessage('leftupbtnclick');
end;

procedure tmainform.pendownbtnclick(Sender: TObject);
begin
  tpc.sendmessage('pendownbtnclick');
end;

procedure tmainform.penupbtnclick(sender: tobject);
begin
  tpc.sendmessage('penupbtnclick');
end;

procedure tmainform.reloadbtnclick(sender: tobject);
begin
  verticalcbeditingdone(sender);
  // ---
  image.canvas.pen.color   := clltgray;
  image.canvas.brush.color := clltgray;
  image.canvas.brush.style := bssolid;
  image.setsize(
     widthse.value,
    heightse.value);
  image.canvas.fillrect(0,0,
     widthse.value,
    heightse.value);
  // ---
  previewimage.canvas.pen.color   := clltgray;
  previewimage.canvas.brush.color := clltgray;
  previewimage.canvas.brush.style := bssolid;
  previewimage.picture.bitmap.setsize(
     widthse.value,
    heightse.value);
  previewimage.canvas.fillrect(0, 0,
     widthse.value,
    heightse.value);

  previewimage.center            := true;
  previewimage.proportional      := true;
  previewimage.stretchinenabled  := true;
  previewimage.stretchoutenabled := false;
  previewimage.stretch           := true;

  if sender <> nil then
  begin
    vpcoder         := tvpcoder.create(list);
    vpcoder.onstart := @onstart;
    vpcoder.onstop  := @onstop;
    vpcoder.ontick  := @ontick;
    vpcoder.start;
  end;
end;

procedure tmainform.rightdownbtnclick(sender: tobject);
begin
   tpc.sendmessage('rightdownbtnclick');
end;

procedure tmainform.rightupbtnclick(sender: tobject);
begin
  tpc.sendmessage('rightupbtnclick');
end;

procedure tmainform.sethomebtnclick(sender: tobject);
begin
  tpc.sendmessage('sethomebtnclick');
end;

procedure tmainform.startbtnclick(sender: tobject);
begin
  if sender = startbtn then
  begin
    startbtn.enabled := false;
    pausebtn.enabled := true;
    stopbtn .enabled := true;

    tpc.send(strm.size, sizeof(int64));
  end;

end;

procedure tmainform.tpcconnect(asocket: tlsocket);
begin
  serverisbusy   := false;
  conbtn.caption := 'Disconnect';
  manualdrivinggb.enabled := true;

end;

procedure tmainform.tpcdisconnect(asocket: tlsocket);
begin
  serverisbusy   := false;
  conbtn.caption := 'Connect';
  manualdrivinggb.enabled := false;
end;

procedure tmainform.tpcerror(const msg: string; asocket: tlsocket);
begin



end;

procedure tmainform.loadbtnclick(sender: tobject);
begin
  if opendialog.execute then
  begin
    list.clear;
    list.loadfromfile(opendialog.filename);
    reloadbtnclick(loadbtn)
  end;
end;

procedure tmainform.playorstopbtnclick(sender: tobject);
begin
  if assigned(vpcoder) then
  begin
    if sender = stopbtn then
    begin
      vpcoder.terminate;
    end;


    //startbtn .enabled := not vplotcoder.enabled;
    //pausebtn.enabled :=     vplotcoder.enabled;
    //stopbtn .enabled :=     vplotcoder.enabled;
    //aboutbtn .enabled := false;

  end;
end;

procedure tmainform.tpcreceive(asocket: tlsocket);
var
  i: longint;
begin

  tpc.get(i, sizeof(i), asocket);

  showmessage(inttostr(i));
  serverisbusy := false;
end;

procedure tmainform.onstart;
begin
  strm.clear;

  creativecontrolgb.enabled := false;
  papersizegb      .enabled := false;
  drawingcontrolgb .enabled := false;
end;

procedure tmainform.onstop;
begin
  caption := 'VPlot Driver';
  previewimage.canvas.copyrect(
    previewimage.canvas.cliprect,
    image.canvas,
    image.canvas.cliprect);
  previewimage.invalidate;

  creativecontrolgb.enabled := true;
  papersizegb      .enabled := true;
  drawingcontrolgb .enabled := true;
end;

procedure tmainform.ontick;
var
   p: tvppoint;
   x: longint;
  m0: longint;
  m1: longint;
begin
  p.x := ( widthse.value div 2) + offsetxse.value + vpcoder.px;
  p.y := (heightse.value div 2) - offsetyse.value - vpcoder.py;
  if vpcoder.pz < 0 then
    image.canvas.pixels[round(p.x), round(p.y)] := clblack
  else
    image.canvas.pixels[round(p.x), round(p.y)] := clred;

  p.x := vpcoder.px + vplayout.point8.x;
  p.y := vpcoder.py + vplayout.point8.y;
  optimize(p, vplayout, m0, m1);

  x := 2;
  strm.write(x,  sizeof(longint));
  strm.write(m0, sizeof(longint));
  strm.write(m1, sizeof(longint));

  x := round(vpcoder.pz);
  strm.write(x,  sizeof(longint));

  if vpcoder.index mod 10 = 0 then
    caption := format('VPlot Driver - Drawing [%u / %u]',
      [vpcoder.index, vpcoder.count]);
  application.processmessages;
end;

procedure tmainform.verticalcbeditingdone(sender: tobject);
var
  amin, amax: longint;
begin
  amin := min(heightse.value, widthse.value);
  amax := max(heightse.value, widthse.value);
  if verticalcb.checked then
  begin
    heightse.value := amax;
    widthse .value := amin;
  end else
  begin
    heightse.value := amin;
    widthse .value := amax;
  end;
end;

end.

