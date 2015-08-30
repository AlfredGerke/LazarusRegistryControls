unit fmeregistrysettingscheckedlist;

{$mode delphi}

interface

uses
  StdCtrls,
  ValEdit,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlSettingsCheckedList }

  TRegControlSettingsCheckedList = class(TCustomRegControlSettings<TRegistrySettingsCheckedList>)
    lblRegistrySettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsCheckedList);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

uses
  SysUtils;

{ TRegControlSettingsCheckedList }

procedure TRegControlSettingsCheckedList.SetSettingsProc(
    aSettings: TRegistrySettingsCheckedList);
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
    Add('DoMergeData=' + BoolToStr(DoMergeData, True));
  end;
end;

procedure TRegControlSettingsCheckedList._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;


end.

