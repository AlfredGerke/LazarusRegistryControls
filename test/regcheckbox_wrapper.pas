unit regcheckbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  regcheckbox,
  regsourcen,
  test_wrapper;

type

  { TRegCheckBoxWrapper }

  TRegCheckBoxWrapper = class(TWrapperCS<TRegCheckBox>)
  private
  protected
    procedure DeleteCaptionEntries; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
    procedure SetCaptionSettings; override;
  public
  public
  end;

implementation

uses
  test_const;

{ TRegCheckBoxWrapper }

procedure TRegCheckBoxWrapper.DeleteCaptionEntries;
begin
  inherited DeleteCaptionEntries;

  RegControl.RegistrySource.DeleteKey(SEC_TREGCHECKBOX, IDENT_CAPTION);
end;

procedure TRegCheckBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  RegControl.RegistrySource.WriteBool(SEC_TREGCHECKBOX, IDENT_CHECK_PROPERTY,
    _CHECKED_ENTRY);

  RegControl.RegistrySource.WriteString(SEC_TREGCHECKBOX, IDENT_CAPTION,
    _TREGCHECKBOX_CAPTION_VALUE);
end;

procedure TRegCheckBoxWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := DEFAULT_CHECKED_ENTRY;
  RegControl.RegistrySettings.Section := SEC_TREGCHECKBOX;
  RegControl.RegistrySettings.Ident := IDENT_CHECK_PROPERTY;
end;

procedure TRegCheckBoxWrapper.SetCaptionSettings;
begin
  inherited SetCaptionSettings;

  RegControl.CaptionSettings.Section := SEC_TREGCHECKBOX;
  RegControl.CaptionSettings.Ident := IDENT_CAPTION;
  RegControl.CaptionSettings.CaptionByRegistry := True;

  RegControl.Caption := DEFAULT_CAPTION_VALUE;
end;

end.
