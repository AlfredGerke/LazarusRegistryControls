unit fmereglabelproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  fmeregistrysettingsstringdefault,
  reglabel;

type

  { TRegLabelProperties }

  TRegLabelProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;


implementation

{$R *.lfm}

{ TRegLabelProperties }

procedure TRegLabelProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlStringDefault.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegLabel(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegLabel(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

end.

