unit fmeregradiobuttonproperties;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  fmeregcontrolproperties,
  regradiobutton,
  regsourcen,
  fmeregistrysettingsbooleandefault;

type

  { TRegRadioButtonProperties }

  TRegRadioButtonProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
    function GetRegControlCaptionSettings: TCaptionSettings; override;
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegRadioButtonProperties }

procedure TRegRadioButtonProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlBooleanDefaultSettings.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegRadioButton(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegRadioButton(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

function TRegRadioButtonProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  Result := TRegRadioButton(self.RegComponent).CaptionSettings;
end;

procedure TRegRadioButtonProperties._Initialize;
begin
  DoCreateCaptionSettings := True;
end;

end.

