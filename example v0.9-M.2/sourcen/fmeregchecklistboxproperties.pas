unit fmeregchecklistboxproperties;

{$mode delphi}

interface

uses
  Controls,
  fmeregcontrolproperties,
  regchecklistbox,
  fmeregistrysettingscheckedlist;

type

  { TRegCheckListBoxProperties }

  TRegCheckListBoxProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;

implementation

{$R *.lfm}

{ TRegCheckListBoxProperties }

procedure TRegCheckListBoxProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlSettingsCheckedList.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegCheckListBox(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegCheckListBox(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;


end.

