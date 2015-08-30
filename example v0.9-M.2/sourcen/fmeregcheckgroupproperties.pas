unit fmeregcheckgroupproperties;

{$mode delphi}

interface

uses
  SysUtils, FileUtil, Controls, fmereglistboxproperties, regsourcen, regcheckgroup, fmeregistrysettingscheckedlist;

type

  { TRegCheckGroupProperties }

  TRegCheckGroupProperties = class(TRegListBoxProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
    function GetRegControlCaptionSettings: TCaptionSettings; override;
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegCheckGroupProperties }

procedure TRegCheckGroupProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlSettingsCheckedList.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegCheckGroup(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegCheckGroup(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

function TRegCheckGroupProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  Result := TRegCheckGroup(self.RegComponent).CaptionSettings;
end;

procedure TRegCheckGroupProperties._Initialize;
begin
  DoCreateCaptionSettings := True;
end;


end.

