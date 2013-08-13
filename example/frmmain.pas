unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, regsourcen, regedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    RegEdit1: TRegEdit;
    RegEdit2: TRegEdit;
    RegistrySource1: TRegistrySource;
    SpeedButton1: TSpeedButton;
    procedure RegEdit1BeforeRegistrySettingChange(
      aSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
    procedure RegEdit2BeforeRegistrySettingChange(
      aSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
    procedure RegEdit2Enter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.RegEdit2Enter(Sender: TObject);
begin
  ShowMessage('Test');
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  RegEdit1.RegistrySettings.Section:= 'Test';
end;

procedure TForm1.RegEdit1BeforeRegistrySettingChange(
  aSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
begin
  ShowMessage('test beforeregistrySetttingchang');
end;

procedure TForm1.RegEdit2BeforeRegistrySettingChange(
  aSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
begin
  ShowMessage('Test2');
end;

end.

