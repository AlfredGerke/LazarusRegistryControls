unit fmeregcheckboxproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  regcheckbox,
  regsourcen,
  fmeregistrysettingsbooleandefault;

type

  { TRegCheckBoxProperties }

  TRegCheckBoxProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
    function GetRegControlCaptionSettings: TCaptionSettings; override;
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegCheckBoxProperties }

procedure TRegCheckBoxProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlBooleanDefaultSettings.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegCheckBox(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegCheckBox(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

function TRegCheckBoxProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  Result := TRegCheckBox(self.RegComponent).CaptionSettings;
end;

procedure TRegCheckBoxProperties._Initialize;
begin
  DoCreateCaptionSettings := True;
end;

end.

