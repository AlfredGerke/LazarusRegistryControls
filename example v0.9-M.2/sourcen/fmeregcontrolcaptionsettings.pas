unit fmeregcontrolcaptionsettings;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs, StdCtrls, ExtCtrls,
  fmecustomsettings,
  regsourcen;

type

  { TRegControlCaptionSettings }

  TRegControlCaptionSettings = class(TCustomRegControlSettings<TCaptionSettings>)
    cbxCaptionByRegistry: TCheckBox;
    edtSection: TLabeledEdit;
    edtIdent: TLabeledEdit;
    lblIdent: TBoundLabel;
    lblSection: TBoundLabel;
  private
    procedure SetSettingsProc(aSettings: TCaptionSettings);
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegControlCaptionSettings }

procedure TRegControlCaptionSettings.SetSettingsProc(aSettings: TCaptionSettings);
begin
  edtSection.Text:=aSettings.Section;
  edtIdent.Text := aSettings.Ident;
  cbxCaptionByRegistry.Checked := aSettings.CaptionByRegistry;
end;

procedure TRegControlCaptionSettings._Initialize;
begin
  OnSetSettings := SetSettingsProc;
end;

end.


