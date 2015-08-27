unit fmeregistrysettingsstringdefault;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Controls,
  Graphics,
  StdCtrls,
  ValEdit,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlStringDefault }

  TRegControlStringDefault = class(TCustomRegControlSettings<TRegistrySettingsStringDefault>)
    lblRegistrySettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TRegistrySettingsStringDefault);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegControlStringDefault }

procedure TRegControlStringDefault.SetSettingsProc(
  aSettings: TRegistrySettingsStringDefault);
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
    Add('Default=' + Default);
    Add('Ident=' + Ident);
  end;
end;

procedure TRegControlStringDefault._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;


end.

