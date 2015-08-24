unit fmeregistrylistsettings;

{$mode delphi}

interface

uses
  ValEdit, StdCtrls,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlListSettings }

  TRegControlListSettings = class(TCustomRegControlSettings<TRegistrySettingsList>)
    lblCaptionSettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsList);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

uses
  SysUtils,
  regtype;

{ TRegControlListSettings }

procedure TRegControlListSettings.SetSettingsProc(
    aSettings: TRegistrySettingsList);
var
  source_kind_str: string;
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
    Add('Default=' + IntToStr(Default));
    Add('ItemsByRegistry=' + BoolToStr(ItemsByRegistry, True));
    Add('ListSection=' + ListSection);
    case SourceKind of
      lskUnknown: source_kind_str := 'lskUnknown';
      lskByKey: source_kind_str := 'lskByKey';
      lskByValue: source_kind_str := 'lskByValue';
      lskByKeyValue: source_kind_str := 'lskByKeyValue';
    else
      source_kind_str := 'leer';
    end;
    Add('SourceKind=' + source_kind_str);
    Add('DoMergeData=' + BoolToStr(DoMergeData, True));
  end;
end;

procedure TRegControlListSettings._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;


end.

