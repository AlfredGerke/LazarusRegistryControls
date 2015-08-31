unit fmeregistrysettingsvaluelist;

{$mode delphi}

interface

uses
  StdCtrls,
  ValEdit,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlSettingsValueList }

  TRegControlSettingsValueList = class(TCustomRegControlSettings<TRegistrySettingsValueList>)
    lblRegistrySettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsValueList);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

uses
  SysUtils;

procedure TRegControlSettingsValueList.SetSettingsProc(
  aSettings: TRegistrySettingsValueList);
begin
  with ValueListEditor1.Strings, aSettings do
  begin
    Clear;
    Add('CanRead=' + BoolToStr(CanRead, True));
    Add('CanWrite=' + BoolToStr(CanWrite, True));
    Add('DoWriteAdHoc=' + BoolToStr(DoWriteAdHoc, True));
    Add('GroupIndex=' + IntToStr(GroupIndex));
    Add('DoSyncData=' + BoolToStr(DoSyncData, True));
    Add('ListSection=' + ListSection);
    Add('DoMergeData=' + BoolToStr(DoMergeData, True));
  end;
end;

procedure TRegControlSettingsValueList._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;

end.
