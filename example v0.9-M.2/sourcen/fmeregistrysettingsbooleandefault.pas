unit fmeregistrysettingsbooleandefault;

{$mode delphi}

interface

uses
  ValEdit,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlBooleanDefaultSettings }

  TRegControlBooleanDefaultSettings = class(TCustomRegControlSettings<TRegistrySettingsBooleanDefault>)
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsBooleanDefault);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegControlBooleanDefaultSettings }

procedure TRegControlBooleanDefaultSettings.SetSettingsProc(
    aSettings: TRegistrySettingsBooleanDefault);
begin

end;

procedure TRegControlBooleanDefaultSettings._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;

end.

