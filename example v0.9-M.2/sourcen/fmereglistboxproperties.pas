unit fmereglistboxproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  reglistbox,
  fmeregistrylistsettings;

type

  { TRegListBoxProperties }

  TRegListBoxProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;

implementation

{$R *.lfm}

{ TRegListBoxProperties }

procedure TRegListBoxProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlListSettings.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegListBox(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegListBox(self.RegComponent).RegistrySettings);
  end;
end;

end.

