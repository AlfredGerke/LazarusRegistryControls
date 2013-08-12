unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, regsourcen,
  regedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    RegEdit1: TRegEdit;
    RegistrySource1: TRegistrySource;
    procedure FormCreate(Sender: TObject);
    procedure RegEdit1RegSettingChange(aSettingInfo: TRegistrySettingValue;
      var aIsOk: boolean);
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

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.RegEdit1RegSettingChange(aSettingInfo: TRegistrySettingValue;
  var aIsOk: boolean);
begin

end;

end.

