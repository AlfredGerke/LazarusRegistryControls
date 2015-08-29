unit fmeregeditproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  fmeregistrysettingsstringdefault,
  regedit;

type

  { TRegEditProperties }

  TRegEditProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;

implementation

{$R *.lfm}

{ TRegEditProperties }

procedure TRegEditProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlStringDefault.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegEdit(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegEdit(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

end.

