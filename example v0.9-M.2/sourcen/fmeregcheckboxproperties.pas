unit fmeregcheckboxproperties;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs, ExtCtrls,
  fmeregcontrolproperties,
  regcheckbox,
  regsourcen;

type

  { TRegCheckBoxProperties }

  TRegCheckBoxProperties = class(TRegControlProperties)
  protected
    function GetRegControlCaptionSettings: TCaptionSettings; override;
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

  { TRegCheckBoxProperties }

function TRegCheckBoxProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  Result := TRegCheckBox(self.RegComponent).CaptionSettings;
end;

procedure TRegCheckBoxProperties._Initialize;
begin
  DoCreateCaptionSettings := True;
end;

end.

