unit fmeregcontrolcaptionsettings;

{$mode delphi}

interface

uses
  ExtCtrls,
  ValEdit,
  StdCtrls,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlCaptionSettings }

  TRegControlCaptionSettings = class(TCustomRegControlSettings<TCaptionSettings>)
    lblCaptionSettings: TLabel;
    lblIdent: TBoundLabel;
    lblSection: TBoundLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettingsProc(aSettings: TCaptionSettings);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

uses
  SysUtils;

{ TRegControlCaptionSettings }

procedure TRegControlCaptionSettings.SetSettingsProc(aSettings: TCaptionSettings);
begin
  with ValueListEditor1.Strings, aSettings do
  begin
    Clear;
    Add('Section=' + Section);
    Add('Ident=' + Ident);
    Add('CaptionByRegistry=' + BoolToStr(CaptionByRegistry, True));
  end;
end;

procedure TRegControlCaptionSettings._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;

end.


