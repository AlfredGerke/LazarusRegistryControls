unit fmeregcomboboxproperties;

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
  regcombobox,
  fmeregistrylistsettings;

type

  { TRegComboBoxProperties }

  TRegComboBoxProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;

implementation

{$R *.lfm}

{ TRegComboBoxProperties }

procedure TRegComboBoxProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlListSettings.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegComboBox(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegComboBox(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

end.

