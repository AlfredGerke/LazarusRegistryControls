unit fmeregistrysettingsbooleandefault;

{$mode delphi}

interface

uses
  ValEdit, StdCtrls,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlBooleanDefaultSettings }

  TRegControlBooleanDefaultSettings = class(TCustomRegControlSettings<TRegistrySettingsBooleanDefault>)
    lblCaptionSettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsBooleanDefault);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

uses
  SysUtils;

{ TRegControlBooleanDefaultSettings }

procedure TRegControlBooleanDefaultSettings.SetSettingsProc(
    aSettings: TRegistrySettingsBooleanDefault);
begin
  with ValueListEditor1.Strings, aSettings do
  begin
    Clear;
    Add('CanRead=' + BoolToStr(CanRead, True));
    Add('CanWrite=' + BoolToStr(CanWrite, True));
    Add('DoWriteAdHoc=' + BoolToStr(DoWriteAdHoc, True));
    Add('GroupIndex=' + IntToStr(GroupIndex));
    Add('DoSyncData=' + BoolToStr(DoSyncData, True));
    Add('Section=' + Section);
    Add('Default=' + BoolToStr(Default, True));
    Add('DoMergeData=' + BoolToStr(DoMergeData, True));
  end;
end;

procedure TRegControlBooleanDefaultSettings._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;

end.

