unit fmeregvaluelisteditorproperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, fmeregcontrolproperties, fmeregistrysettingsvaluelist, regvaluelisteditor;

type

  { TRegValueListEditorProperties }

  TRegValueListEditorProperties = class(TRegControlProperties)
  protected
    procedure CreateRegistrySettingsFrame; override;
  end;

implementation

{$R *.lfm}

{ TRegValueListEditorProperties }

procedure TRegValueListEditorProperties.CreateRegistrySettingsFrame;
begin
  with TRegControlSettingsValueList.Create(pnlRegistrySettings) do
  begin
    Parent := pnlRegistrySettings;
    Align := alClient;

    if Assigned(TRegValueListEditor(self.RegComponent).RegistrySettings) then
      SetRegControlSettings(TRegValueListEditor(self.RegComponent).RegistrySettings);

    OnRefreshSettings := RefreshSettings;
  end;
end;

end.

