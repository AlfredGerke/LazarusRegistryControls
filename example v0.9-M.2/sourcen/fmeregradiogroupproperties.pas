unit fmeregradiogroupproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  regradiogroup,
  fmeregistrylistsettings,
  regsourcen;

type

  { TRegRadioGroupProperties }

  TRegRadioGroupProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
    function GetRegControlCaptionSettings: TCaptionSettings; override;
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegRadioGroupProperties }

procedure TRegRadioGroupProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlListSettings.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegRadioGroup(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegRadioGroup(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

function TRegRadioGroupProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  Result := TRegRadioGroup(self.RegComponent).CaptionSettings;
end;

procedure TRegRadioGroupProperties._Initialize;
begin
  DoCreateCaptionSettings := True;
end;

end.
