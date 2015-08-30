unit fmeregistrysettingscheckedlist;

{$mode delphi}

interface

uses
  Classes,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs, StdCtrls, ValEdit,
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
  SysUtils,
  regtype;

{ TRegControlSettingsCheckedList }

procedure TRegControlSettingsCheckedList.SetSettingsProc(
    aSettings: TRegistrySettingsCheckedList);
var
  source_kind_str: string;
begin
  with ValueListEditor1.Strings, aSettings do
  begin
    Clear;
    {
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
    }
  end;
end;

procedure TRegControlSettingsCheckedList._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;


end.

